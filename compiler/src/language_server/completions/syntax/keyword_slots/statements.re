open Types;
open Grammar;

let nested_statement_kinds = [
  Kind.block_body,
  Kind.match_body,
  Kind.let_declaration,
  Kind.let_expression,
  Kind.value_binding,
  Kind.include_declaration,
  Kind.provide_declaration,
  Kind.incomplete_provide_declaration,
  Kind.enum_body,
  Kind.record_declaration_body,
];

let nested_statement_ancestor = node =>
  Tree.ancestor(node, n => Util.node_kind_in(n, nested_statement_kinds));

let in_toplevel_statement = (node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.program)) {
  | Some(_) => Option.is_none(nested_statement_ancestor(node))
  | None => false
  };

let in_module_toplevel_statement = (node: Tree.node) =>
  (
    Tree.node_kind(node) == Kind.module_declaration
    || Option.is_some(Util.ancestor_of_kind(node, Kind.module_declaration))
  )
  && Option.is_none(nested_statement_ancestor(node));

let partial_ident_prefix = (source, pos: position) => {
  let (ident, _, _) = Text.prefix_from_cursor(source, pos);
  String.length(ident) > 0 && String.for_all(Text.is_ident_char, ident);
};

let line_starts_with_keyword = (source, pos: position, keyword) => {
  let line = Text.line_at(source, pos);
  let character =
    Edit.clamp(~min_value=0, ~max_value=String.length(line), pos.character);
  let line_prefix = String.sub(line, 0, character);
  let rec skip_whitespace = idx =>
    if (idx >= String.length(line_prefix)) {
      idx;
    } else {
      switch (line_prefix.[idx]) {
      | ' '
      | '\t' => skip_whitespace(idx + 1)
      | _ => idx
      };
    };
  let start = skip_whitespace(0);
  let remaining =
    String.sub(line_prefix, start, String.length(line_prefix) - start);
  String.length(remaining) >= String.length(keyword)
  && String.sub(remaining, 0, String.length(keyword)) == keyword
  && (
    String.length(remaining) == String.length(keyword)
    || remaining.[String.length(keyword)] == ' '
    || remaining.[String.length(keyword)] == '\t'
  );
};

let line_has_leading_indent = (source, pos: position) => {
  let line = Text.line_at(source, pos);
  String.length(line) > 0 && (line.[0] == ' ' || line.[0] == '\t');
};

let rec partial_statement_node_on_line = (pos: position, node: Tree.node) =>
  if (Util.node_kind_in(
        node,
        [Kind.expression_statement, Kind.identifier_expression],
      )) {
    let (row, _) = Tree.node_start_point(node);
    row == pos.line ? Some(node) : None;
  } else {
    Util.find_named_child_map(node, partial_statement_node_on_line(pos));
  };

let partial_toplevel_statement_slot =
    (tree: parse_tree, source, pos: position) =>
  if (!partial_ident_prefix(source, pos)) {
    None;
  } else if (line_has_leading_indent(source, pos)) {
    None;
  } else if (line_starts_with_keyword(source, pos, Keyword.let_)
             || line_starts_with_keyword(source, pos, Keyword.provide)
             || line_starts_with_keyword(source, pos, Keyword.from)) {
    None;
  } else {
    switch (partial_statement_node_on_line(pos, Tree.root(tree))) {
    | None => None
    | Some(expr_stmt) =>
      if (in_toplevel_statement(expr_stmt)) {
        Some(ToplevelStatement);
      } else if (in_module_toplevel_statement(expr_stmt)) {
        Some(ToplevelStatement);
      } else {
        None;
      }
    };
  };

let partial_block_statement_slot = (tree: parse_tree, source, pos: position) =>
  if (!partial_ident_prefix(source, pos)) {
    None;
  } else if (!line_has_leading_indent(source, pos)) {
    None;
  } else {
    switch (partial_statement_node_on_line(pos, Tree.root(tree))) {
    | None => None
    | Some(expr_stmt) =>
      switch (Util.ancestor_of_kinds(expr_stmt, Kind.block_bodies)) {
      | Some(block) when Util.cursor_in_node_span(source, pos, block) =>
        Some(BlockStatement)
      | _ => None
      }
    };
  };

let partial_statement_keyword_slot =
    (~partial_block_slot, tree: parse_tree, source, pos: position) =>
  switch (partial_block_slot) {
  | Some(slot) => Some(slot)
  | None => partial_toplevel_statement_slot(tree, source, pos)
  };

let value_binding_expression_start_slot =
    (source, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.value_binding)) {
  | Some(binding) =>
    switch (
      Tree.node_child_by_field_name(binding, Field.pattern),
      Tree.node_child_by_field_name(binding, Field.value),
    ) {
    | (Some(pattern), Some(value))
        when
          !Util.cursor_in_node(pos, pattern)
          && Util.cursor_in_node_span(source, pos, value) =>
      Some(ExpressionStart)
    | _ => None
    }
  | None => None
  };

let fallback_statement_slot =
    (
      ~partial_block_slot,
      tree: parse_tree,
      source,
      pos: position,
      node: Tree.node,
    ) =>
  if (in_module_toplevel_statement(node)) {
    Some(ToplevelStatement);
  } else {
    switch (Control_flow.block_body_slot(source, pos, node)) {
    | Some(slot) => Some(slot)
    | None =>
      if (in_toplevel_statement(node)) {
        Some(ToplevelStatement);
      } else {
        switch (
          partial_statement_keyword_slot(
            ~partial_block_slot,
            tree,
            source,
            pos,
          )
        ) {
        | Some(slot) => Some(slot)
        | None => value_binding_expression_start_slot(source, pos, node)
        };
      }
    };
  };
