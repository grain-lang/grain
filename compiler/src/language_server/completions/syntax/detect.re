open Types;
open Grammar;

let is_incomplete_typed_let_declaration = (node: Tree.node) =>
  Tree.node_kind(node) == Kind.let_declaration
  && Option.is_none(Util.named_child_by_kind(node, Kind.value_bindings))
  && Option.is_some(Tree.node_child_by_field_name(node, Field.pattern));

let incomplete_let_type_on_line = (source, pos: position, decl: Tree.node) => {
  let (row, _) = Tree.node_start_point(decl);
  let (end_row, end_col) = Tree.node_end_point(decl);
  if (pos.line != row && pos.line != end_row) {
    false;
  } else {
    switch (
      Tree.node_child_by_field_name(decl, Field.pattern),
      Tree.node_child_by_field_name(decl, Field.type_),
    ) {
    | (Some(pattern), None) =>
      if (pos.line != row) {
        false;
      } else {
        let (_, pattern_end_col) = Tree.node_end_point(pattern);
        pos.character > pattern_end_col
        || pos.character == pattern_end_col
        && Tree.node_kind(pattern) == Kind.typed_pattern;
      }
    | (Some(_), Some(type_node)) =>
      let (type_end_row, type_end_col) = Tree.node_end_point(type_node);
      Util.cursor_in_node_span(source, pos, type_node)
      || pos.line == type_end_row
      && pos.character > type_end_col
      || end_row == pos.line
      && pos.character > end_col
      && row == end_row;
    | _ => false
    };
  };
};

let rec incomplete_let_type_annotation_on_line =
        (source, pos: position, node: Tree.node) =>
  if (Tree.node_kind(node) == Kind.let_declaration
      && is_incomplete_typed_let_declaration(node)) {
    let (row, _) = Tree.node_start_point(node);
    let (end_row, _) = Tree.node_end_point(node);
    if (row == pos.line || end_row == pos.line) {
      incomplete_let_type_on_line(source, pos, node);
    } else {
      false;
    };
  } else {
    Util.exists_named_child(
      node,
      incomplete_let_type_annotation_on_line(source, pos),
    );
  };

let cursor_in_incomplete_let_type_annotation =
    (tree: parse_tree, source, pos: position, node: Tree.node) =>
  switch (
    Tree.ancestor(node, n =>
      Tree.node_kind(n) == Kind.let_declaration
      && is_incomplete_typed_let_declaration(n)
    )
  ) {
  | Some(decl) => incomplete_let_type_on_line(source, pos, decl)
  | None =>
    incomplete_let_type_annotation_on_line(source, pos, Tree.root(tree))
  };

let cursor_after_incomplete_tuple_start = (pos: position, tuple: Tree.node) => {
  let (start_row, start_col) = Tree.node_start_point(tuple);
  pos.line == start_row && pos.character > start_col;
};

let rec open_incomplete_tuple_in_enum_body =
        (source, pos: position, node: Tree.node) =>
  if (Tree.node_kind(node) == Kind.enum_variant) {
    switch (
      Util.named_child_by_kind(node, Kind.incomplete_data_constructor_tuple)
    ) {
    | Some(tuple) =>
      Util.cursor_in_node_span(source, pos, node)
      && (
        Util.cursor_in_node_span(source, pos, tuple)
        || cursor_after_incomplete_tuple_start(pos, tuple)
      )
    | None => false
    };
  } else {
    Util.exists_named_child(
      node,
      open_incomplete_tuple_in_enum_body(source, pos),
    );
  };

let cursor_in_enum_constructor_tuple =
    (tree: parse_tree, source, pos: position, node: Tree.node) =>
  if (open_incomplete_tuple_in_enum_body(source, pos, Tree.root(tree))) {
    true;
  } else {
    switch (
      Util.ancestor_of_kinds(
        node,
        [Kind.incomplete_data_constructor_tuple, Kind.data_constructor_tuple],
      )
    ) {
    | Some(tuple) =>
      switch (Tree.node_parent(tuple)) {
      | Some(parent) when Tree.node_kind(parent) == Kind.enum_variant =>
        Tree.node_kind(tuple) == Kind.incomplete_data_constructor_tuple
        || Util.cursor_in_node_span(source, pos, tuple)
      | _ => false
      }
    | None => false
    };
  };

let cursor_in_type_reference =
    (tree: parse_tree, source, pos: position, _cursor_byte, node: Tree.node) =>
  if (cursor_in_incomplete_let_type_annotation(tree, source, pos, node)) {
    true;
  } else if (cursor_in_enum_constructor_tuple(tree, source, pos, node)) {
    true;
  } else if (Slot_predicates.suppresses_type_reference(
               tree,
               source,
               pos,
               node,
             )) {
    false;
  } else {
    let constructor_tuple_kinds = [
      Kind.data_constructor_tuple,
      Kind.incomplete_data_constructor_tuple,
    ];
    let type_node_kinds =
      constructor_tuple_kinds
      @ [
        Kind.type_parameters,
        Kind.type_alias,
        Kind.tuple_type,
        Kind.parenthesized_type,
        Kind.arrow_type,
        Kind.qualified_type_identifier,
      ];
    let in_type_node = n =>
      if (Util.node_kind_in(n, type_node_kinds)) {
        true;
      } else if (Tree.node_kind(n) == Kind.type_variable) {
        Option.is_some(
          Util.ancestor_of_kinds(node, constructor_tuple_kinds),
        );
      } else {
        false;
      };
    switch (Tree.ancestor(node, in_type_node)) {
    | None => false
    | Some(type_node) =>
      let kind = Tree.node_kind(type_node);
      if (Util.node_kind_in(type_node, constructor_tuple_kinds)) {
        switch (Tree.node_parent(type_node)) {
        | Some(parent) when Tree.node_kind(parent) == Kind.enum_variant =>
          true
        | _ => false
        };
      } else if (kind == Kind.enum_variant) {
        Tree.cursor_inside(pos, type_node);
      } else if (kind == Kind.qualified_type_identifier) {
        switch (Tree.node_parent(type_node)) {
        | None => false
        | Some(parent) =>
          Util.node_kind_in(
            parent,
            constructor_tuple_kinds
            @ [Kind.type_parameters, Kind.constructor_type],
          )
        };
      } else {
        Util.cursor_in_node_span(source, pos, type_node);
      };
    };
  };

let cursor_in_string_literal = (pos: position, node: Tree.node) => {
  let in_string = n =>
    Util.node_kind_in(
      n,
      [Kind.string, Kind.import_path_string, Kind.string_content],
    );
  switch (Tree.ancestor(node, in_string)) {
  | Some(string_node) =>
    let (end_row, end_col) = Tree.node_end_point(string_node);
    let at_or_after_closing_quote =
      pos.line > end_row || pos.line == end_row && pos.character >= end_col - 1;
    if (at_or_after_closing_quote) {
      false;
    } else {
      Util.cursor_in_node(pos, string_node);
    };
  | None => false
  };
};

let in_match_pattern_context = (tree: parse_tree, pos: position, source, node) =>
  Match_context.in_match_pattern_from_node(tree, source, pos, node)
  || Match_context.tree_contains_match_pattern_cursor(
       tree,
       source,
       pos,
       Tree.root(tree),
     );

let import_context_kind = (tree, pos) =>
  switch (Import_context.import_context_from_tree(tree, pos)) {
  | Some((kind, _, _, _)) => kind
  | None => InScope
  };

let use_qualifier = (tree: parse_tree, use_expr: Tree.node) => {
  let count = Tree.node_named_child_count(use_expr);
  let rec loop = (idx, acc) =>
    if (idx >= count) {
      List.rev(acc);
    } else {
      switch (Tree.node_named_child(use_expr, idx)) {
      | Some(child) when Tree.node_kind(child) == Kind.upper_identifier =>
        loop(idx + 1, [Tree.node_text(tree, child), ...acc])
      | _ => loop(idx + 1, acc)
      };
    };
  switch (loop(0, [])) {
  | [] => None
  | parts => Some(String.concat(".", parts))
  };
};

let use_item_slot = (source, pos: position) => {
  let line = Text.line_at(source, pos);
  let (_word, word_start, _word_end) = Text.prefix_from_cursor(source, pos);
  let before = String.sub(line, 0, word_start);
  let delimiter_index =
    switch (String.rindex_opt(before, '{'), String.rindex_opt(before, ',')) {
    | (Some(a), Some(b)) => Some(max(a, b))
    | (Some(a), None) => Some(a)
    | (None, Some(b)) => Some(b)
    | (None, None) => None
    };
  let region =
    switch (delimiter_index) {
    | Some(idx) =>
      String.sub(before, idx + 1, String.length(before) - idx - 1)
    | None => before
    };
  switch (String.trim(region)) {
  | "type" => UseType
  | "module" => UseModule
  | "exception" => UseException
  | _ => UsePlain
  };
};

let cursor_inside_use_shape =
    (source, pos: position, use_expr: Tree.node, node) =>
  switch (Tree.node_or_ancestor(node, Kind.use_shape)) {
  | Some(_) => true
  | None =>
    let start_byte = Tree.node_start_byte(use_expr);
    let cursor_byte = Text.byte_offset(source, pos);
    if (cursor_byte <= start_byte) {
      false;
    } else {
      let text = String.sub(source, start_byte, cursor_byte - start_byte);
      String.contains(text, '{');
    };
  };

let char_before_cursor_is_dot = (source, pos: position) => {
  let byte = Text.byte_offset(source, pos);
  byte > 0 && source.[byte - 1] == '.';
};

let use_expression_ancestor = (tree: parse_tree, pos: position, node) =>
  switch (Tree.node_or_ancestor(node, Kind.use_expression)) {
  | Some(_) as found => found
  | None =>
    let source = Tree.source(tree);
    let line = Text.line_at(source, pos);
    let start_col = min(pos.character, String.length(line));
    let rec last_token_col = col =>
      if (col <= 0) {
        None;
      } else {
        switch (line.[col - 1]) {
        | ' '
        | '\t' => last_token_col(col - 1)
        | _ => Some(col - 1)
        };
      };
    switch (last_token_col(start_col)) {
    | None => None
    | Some(col) =>
      Option.bind(
        Tree.node_at_point(
          tree,
          {
            ...pos,
            character: col,
          },
        ),
        Tree.node_or_ancestor(_, Kind.use_expression),
      )
    };
  };

let cursor_after_use_dot = (tree: parse_tree, source, pos: position, node) => {
  let use_expr = use_expression_ancestor(tree, pos, node);
  switch (use_expr) {
  | Some(use_expr) when !cursor_inside_use_shape(source, pos, use_expr, node) =>
    Tree.node_end_point(use_expr) == (pos.line, pos.character)
    && char_before_cursor_is_dot(source, pos)
  | _ => false
  };
};

let use_items_context = (tree: parse_tree, source, pos: position, node) =>
  switch (Tree.node_or_ancestor(node, Kind.use_expression)) {
  | None => None
  | Some(use_expr) =>
    if (!cursor_inside_use_shape(source, pos, use_expr, node)) {
      None;
    } else {
      switch (use_qualifier(tree, use_expr)) {
      | None => None
      | Some(qualifier) =>
        Some(UseItems(qualifier, use_item_slot(source, pos)))
      };
    }
  };

let use_path_kind = (source, pos: position) =>
  switch (Text.qualifier_from_line(source, pos)) {
  | Some(qualifier) => UseModulePath(qualifier)
  | None => ImportPath
  };

let context_fallback = (~check_include, tree, pos, cursor_byte, node) =>
  switch (use_expression_ancestor(tree, pos, node)) {
  | Some(_) =>
    switch (use_items_context(tree, Tree.source(tree), pos, node)) {
    | Some(kind) => kind
    | None => use_path_kind(Tree.source(tree), pos)
    }
  | None
      when
        check_include
        && Option.is_some(
             Util.ancestor_of_kind(node, Kind.include_declaration),
           ) =>
    import_context_kind(tree, pos)
  | None =>
    Calls.in_call_argument_label_position(tree, pos, cursor_byte, node)
      ? CallArgument : InScope
  };

let rec detect_kind =
        (
          tree: parse_tree,
          pos: position,
          source,
          cursor_byte,
          node: Tree.node,
        ) =>
  if (cursor_in_string_literal(pos, node)) {
    Suppressed;
  } else if (cursor_after_use_dot(tree, source, pos, node)) {
    UseShape;
  } else if (Keyword_slots.Match_slot.cursor_at_when_keyword_slot(
               tree,
               source,
               pos,
               node,
             )) {
    MatchGuardKeyword;
  } else if (Match_context.cursor_in_constructor_pattern_args(pos, node)) {
    Suppressed;
  } else if (Keyword_slots.Match_slot.cursor_in_when_guard_expression(
               source,
               pos,
               node,
             )) {
    InScope;
  } else if (in_match_pattern_context(tree, pos, source, node)) {
    PatternContext;
  } else if (cursor_in_type_reference(tree, source, pos, cursor_byte, node)) {
    TypeReference;
  } else {
    let kind = Tree.node_kind(node);
    if (kind == Kind.use_expression) {
      switch (use_items_context(tree, source, pos, node)) {
      | Some(kind) => kind
      | None => use_path_kind(source, pos)
      };
    } else if (kind == Kind.include_declaration) {
      import_context_kind(tree, pos);
    } else if (kind == Kind.qualified_identifier) {
      switch (Text.qualifier_from_node(tree, node, cursor_byte)) {
      | Some(qualifier) => MemberAccess(qualifier)
      | None =>
        switch (Tree.node_parent(node)) {
        | None => InScope
        | Some(parent) => detect_kind(tree, pos, source, cursor_byte, parent)
        }
      };
    } else if (kind == Kind.identifier || kind == Kind.upper_identifier) {
      switch (Util.ancestor_of_kind(node, Kind.qualified_identifier)) {
      | Some(qualified) =>
        switch (Text.qualifier_from_node(tree, qualified, cursor_byte)) {
        | Some(qualifier) => MemberAccess(qualifier)
        | None => detect_kind(tree, pos, source, cursor_byte, qualified)
        }
      | None =>
        context_fallback(~check_include=false, tree, pos, cursor_byte, node)
      };
    } else {
      context_fallback(~check_include=true, tree, pos, cursor_byte, node);
    };
  };
