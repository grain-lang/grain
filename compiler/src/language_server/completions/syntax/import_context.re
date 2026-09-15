open Types;
open Grammar;

let rec import_context_node = (tree: parse_tree, pos: position) =>
  switch (Tree.node_at_point(tree, pos)) {
  | Some(node) =>
    switch (Tree.node_kind(node)) {
    | k when k == Kind.program && pos.character > 0 =>
      import_context_node(
        tree,
        {
          ...pos,
          character: pos.character - 1,
        },
      )
    | _ => Some(node)
    }
  | None =>
    if (pos.character > 0) {
      import_context_node(
        tree,
        {
          ...pos,
          character: pos.character - 1,
        },
      );
    } else {
      None;
    }
  };

let cursor_after_include_clause =
    (_source, pos: position, include_decl: Tree.node) =>
  switch (Tree.node_child_by_field_name(include_decl, Field.include_)) {
  | Some(include_clause) =>
    let (start_row, start_col) = Tree.node_start_point(include_clause);
    pos.line > start_row || pos.line == start_row && pos.character >= start_col;
  | None => false
  };

let cursor_past_path_close = (pos: position, path_node: Tree.node) => {
  let (end_row, end_col) = Tree.node_end_point(path_node);
  pos.line > end_row || pos.line == end_row && pos.character > end_col - 1;
};

let import_module_context = (~tree, ~source, ~pos, path_node) => {
  let text = Tree.node_text(tree, path_node);
  let len = String.length(text);
  let import_path =
    if (len >= 2 && text.[0] == '"' && text.[len - 1] == '"') {
      String.sub(text, 1, len - 2);
    } else if (len >= 1 && text.[0] == '"') {
      String.sub(text, 1, len - 1);
    } else {
      "";
    };
  let (prefix, prefix_start, prefix_end) =
    Text.prefix_from_cursor(source, pos);
  let (prefix, prefix_start, prefix_end) =
    prefix == Keyword.include_
      ? ("", prefix_end, prefix_end) : (prefix, prefix_start, prefix_end);
  Some((ImportModuleName(import_path), prefix, prefix_start, prefix_end));
};

let path_node_from_include = include_decl =>
  switch (Tree.node_child_by_field_name(include_decl, Field.from)) {
  | Some(from_clause) =>
    Tree.node_child_by_field_name(from_clause, Field.path)
  | None => Tree.node_child_by_field_name(include_decl, Field.path)
  };

let module_node_from_include = include_decl =>
  switch (Tree.node_child_by_field_name(include_decl, Field.include_)) {
  | Some(include_clause) =>
    Tree.node_child_by_field_name(include_clause, Field.module_)
  | None => Tree.node_child_by_field_name(include_decl, Field.module_)
  };

let file_path_context = (source, pos: position, path_node: Tree.node) =>
  if (cursor_past_path_close(pos, path_node)) {
    None;
  } else {
    let line = Text.line_at(source, pos);
    let character =
      Edit.clamp(
        ~min_value=0,
        ~max_value=String.length(line),
        pos.character,
      );
    let (start_row, start_column) = Tree.node_start_point(path_node);
    let prefix_start = start_column + 1;
    if (pos.line != start_row || character < prefix_start) {
      None;
    } else {
      Some((
        ImportFilePath,
        String.sub(line, prefix_start, character - prefix_start),
        prefix_start,
        character,
      ));
    };
  };

let enclosing_include = node =>
  if (Tree.node_kind(node) == Kind.include_declaration) {
    Some(node);
  } else {
    Util.ancestor_of_kind(node, Kind.include_declaration);
  };

let import_context_from_tree = (tree: parse_tree, pos: position) => {
  let source = Tree.source(tree);
  switch (import_context_node(tree, pos)) {
  | None => None
  | Some(node) =>
    switch (enclosing_include(node)) {
    | None => None
    | Some(include_decl) =>
      let path_node = path_node_from_include(include_decl);
      let module_node = module_node_from_include(include_decl);
      let after_include_keyword =
        cursor_after_include_clause(source, pos, include_decl);
      let after_from_path =
        switch (path_node) {
        | Some(path_node) => cursor_past_path_close(pos, path_node)
        | None => false
        };
      let in_path =
        switch (path_node) {
        | Some(path_node) => Tree.cursor_in_node(pos, node, path_node)
        | None => false
        };
      if (after_from_path && !after_include_keyword) {
        None;
      } else if (in_path) {
        switch (path_node) {
        | Some(path_node) => file_path_context(source, pos, path_node)
        | None => None
        };
      } else {
        switch (path_node) {
        | Some(path_node)
            when after_include_keyword || Option.is_some(module_node) =>
          import_module_context(~tree, ~source, ~pos, path_node)
        | _ => None
        };
      };
    }
  };
};
