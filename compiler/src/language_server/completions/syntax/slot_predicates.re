open Types;
open Grammar;

let provide_tail_after_header =
    (~exclude_let_header, source, pos: position, provide_decl: Tree.node) =>
  switch (Util.named_child_by_kind(provide_decl, Kind.provide_header)) {
  | Some(header) =>
    let in_let_header =
      exclude_let_header
      && Tree.node_kind(provide_decl) == Kind.provide_declaration
      && (
        switch (Util.named_child_by_kind(provide_decl, Kind.let_header)) {
        | Some(let_header) => Util.cursor_in_node(pos, let_header)
        | None => false
        }
      );
    if (in_let_header) {
      false;
    } else if (Tree.node_kind(provide_decl)
               == Kind.incomplete_provide_declaration) {
      Util.cursor_after_node_end(pos, header)
      || (
        switch (Tree.node_child_by_field_name(provide_decl, Field.name)) {
        | Some(name) => Util.cursor_in_node_span(source, pos, name)
        | None => false
        }
      );
    } else {
      Util.cursor_after_node_end(pos, header);
    };
  | None => false
  };

let record_field_header_in_declaration =
    (source, pos: position, field_decl: Tree.node) =>
  switch (Tree.node_parent(field_decl)) {
  | Some(parent) when Tree.node_kind(parent) == Kind.record_declaration_body =>
    switch (Tree.node_child_by_field_name(field_decl, Field.type_)) {
    | Some(type_node) => Util.cursor_before_node_start(pos, type_node)
    | None => Util.cursor_in_node_span(source, pos, field_decl)
    }
  | _ => false
  };

let record_field_name_without_type =
    (source, pos: position, field_decl: Tree.node) =>
  switch (Tree.node_child_by_field_name(field_decl, Field.type_)) {
  | Some(_) => false
  | None =>
    switch (Tree.node_child_by_field_name(field_decl, Field.name)) {
    | Some(name) => Util.cursor_in_node_span(source, pos, name)
    | None => false
    }
  };

let import_include_tail_after_path = (pos: position, include_decl: Tree.node) =>
  switch (Tree.node_child_by_field_name(include_decl, Field.include_)) {
  | Some(_) => false
  | None =>
    switch (Tree.node_child_by_field_name(include_decl, Field.from)) {
    | Some(from_clause) =>
      switch (Tree.node_child_by_field_name(from_clause, Field.path)) {
      | Some(path) => Util.cursor_at_or_after_node_end(pos, path)
      | None => false
      }
    | None => false
    }
  };

let partial_when_keyword_prefix =
    (~prefix_line, source, pos: position, branch: Tree.node) =>
  switch (Tree.node_child_by_field_name(branch, Field.when_keyword_prefix)) {
  | Some(prefix) =>
    Util.starts_on_line(pos, prefix_line ? prefix : branch)
    && Util.cursor_in_node_span(source, pos, prefix)
  | None => false
  };

let partial_else_keyword_prefix = (source, pos: position, if_expr: Tree.node) =>
  switch (Tree.node_child_by_field_name(if_expr, Field.else_keyword_prefix)) {
  | Some(prefix) =>
    Util.starts_on_line(pos, prefix)
    && Util.cursor_in_node_span(source, pos, prefix)
  | None => false
  };

let in_provide_tail =
    (tree: parse_tree, source, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kinds(node, Kind.provide_declarations)) {
  | Some(provide_decl) =>
    provide_tail_after_header(
      ~exclude_let_header=false,
      source,
      pos,
      provide_decl,
    )
  | None =>
    Util.find_descendant_on_line(
      pos,
      n =>
        Util.node_kind_in(n, Kind.provide_declarations)
        && provide_tail_after_header(
             ~exclude_let_header=false,
             source,
             pos,
             n,
           ),
      Tree.root(tree),
    )
  };

let in_record_field_name = (source, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.record_field_declaration)) {
  | Some(field_decl) =>
    record_field_name_without_type(source, pos, field_decl)
  | None => false
  };

let has_partial_else_prefix = (tree: parse_tree, source, pos: position) =>
  Util.find_descendant_on_line(
    pos,
    n =>
      Tree.node_kind(n) == Kind.if_expression
      && partial_else_keyword_prefix(source, pos, n),
    Tree.root(tree),
  );

let has_partial_when_keyword_prefix =
    (tree: parse_tree, source, pos: position) =>
  Util.find_descendant_on_line(
    pos,
    n =>
      Tree.node_kind(n) == Kind.match_branch
      && partial_when_keyword_prefix(~prefix_line=true, source, pos, n),
    Tree.root(tree),
  );

let suppresses_type_reference =
    (tree: parse_tree, source, pos: position, node: Tree.node) =>
  in_provide_tail(tree, source, pos, node)
  || in_record_field_name(source, pos, node)
  || has_partial_else_prefix(tree, source, pos)
  || has_partial_when_keyword_prefix(tree, source, pos);
