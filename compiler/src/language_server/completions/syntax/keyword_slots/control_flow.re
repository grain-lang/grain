open Types;
open Grammar;

let rec in_loop_body = (pos: position, node: Tree.node) =>
  if (Util.node_kind_in(node, [Kind.while_expression, Kind.for_expression])) {
    switch (Tree.node_child_by_field_name(node, Field.body)) {
    | Some(body) when Util.cursor_in_node(pos, body) => true
    | _ => false
    };
  } else {
    switch (Tree.node_parent(node)) {
    | None => false
    | Some(parent) => in_loop_body(pos, parent)
    };
  };

let in_block_body = (source, pos: position, node: Tree.node) =>
  switch (
    Util.ancestor_of_kinds(
      node,
      [Kind.block_expression, ...Kind.block_bodies],
    )
  ) {
  | Some(block) => Util.cursor_in_node_span(source, pos, block)
  | None => false
  };

let slot_from_block_or_loop = (source, pos: position, node: Tree.node) =>
  if (in_loop_body(pos, node)) {
    Some(LoopBody);
  } else if (in_block_body(source, pos, node)) {
    Some(BlockStatement);
  } else {
    None;
  };

let in_if_tail = (source, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.if_expression)) {
  | Some(if_expr) =>
    switch (Tree.node_child_by_field_name(if_expr, Field.else_keyword_prefix)) {
    | Some(prefix) => Util.cursor_in_node_span(source, pos, prefix)
    | None =>
      switch (
        Tree.node_child_by_field_name(if_expr, Field.consequence),
        Tree.node_child_by_field_name(if_expr, Field.alternative),
      ) {
      | (Some(consequence), None) =>
        Util.cursor_at_or_after_node_end(pos, consequence)
      | (Some(_), Some(alternative)) =>
        Util.cursor_in_node(pos, alternative)
      | _ => false
      }
    }
  | None => false
  };

let if_tail_slot = (tree: parse_tree, source, pos: position, node: Tree.node) =>
  Util.slot_if_ancestor_or_on_line(
    ~tree,
    ~pos,
    ~in_ancestor=in_if_tail(source, pos, node),
    ~on_line=
      n =>
        Tree.node_kind(n) == Kind.if_expression
        && Slot_predicates.partial_else_keyword_prefix(source, pos, n),
    IfTail,
  );

let block_body_slot = (source, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kinds(node, Kind.block_bodies)) {
  | Some(block) when Util.cursor_in_node_span(source, pos, block) =>
    Some(BlockStatement)
  | _ => None
  };
