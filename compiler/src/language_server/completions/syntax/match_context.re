open Types;
open Grammar;

let cursor_on_constructor_name = (pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.qualified_identifier)) {
  | None => false
  | Some(name) =>
    switch (Tree.node_parent(name)) {
    | Some(parent) when Tree.node_kind(parent) == Kind.constructor_pattern =>
      Util.cursor_in_node(pos, name)
    | _ => false
    }
  };

let cursor_in_constructor_pattern_args = (pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.constructor_pattern)) {
  | None => false
  | Some(constructor_pattern) =>
    Util.cursor_in_node(pos, constructor_pattern)
    && !cursor_on_constructor_name(pos, node)
  };

let cursor_in_match_body_pattern_area =
    (tree: parse_tree, source, pos: position, node: Tree.node) => {
  Match_tree.in_match_body_at_cursor(pos, node)
  && !Match_tree.cursor_in_any_branch_body(pos, node)
  && !
       Keyword_slots.Match_slot.cursor_at_when_keyword_slot(
         tree,
         source,
         pos,
         node,
       )
  && !Match_tree.line_has_when_guard(tree, pos, node)
  && Match_tree.cursor_on_pattern_side_of_line(tree, pos, node);
};

let cursor_at_match_arm_end =
    (pos: position, node: Tree.node, branch: Tree.node) =>
  Tree.node_kind(node) == Kind.block_expression
  && (
    switch (Tree.node_child_by_field_name(branch, Field.body)) {
    | Some(body) =>
      Tree.node_start_byte(node) == Tree.node_start_byte(body)
      && Tree.node_end_byte(node) == Tree.node_end_byte(body)
    | None => false
    }
  )
  && {
    let (start_row, start_col) = Tree.node_start_point(node);
    let (end_row, _) = Tree.node_end_point(node);
    pos.line == end_row
    && !(pos.line == start_row && pos.character == start_col);
  };

let in_match_pattern_from_node =
    (tree: parse_tree, source, pos: position, node: Tree.node) =>
  if (cursor_in_constructor_pattern_args(pos, node)) {
    false;
  } else {
    switch (Util.ancestor_of_kind(node, Kind.match_branch)) {
    | Some(branch) =>
      Match_tree.cursor_in_branch_pattern(pos, branch)
      || Keyword_slots.Match_slot.cursor_in_when_guard(
           source,
           pos,
           branch,
           node,
         )
      || cursor_at_match_arm_end(pos, node, branch)
    | None => cursor_in_match_body_pattern_area(tree, source, pos, node)
    };
  };

let rec tree_contains_match_pattern_cursor =
        (tree: parse_tree, source, pos: position, node: Tree.node) => {
  let here =
    if (Tree.node_kind(node) == Kind.match_branch) {
      (
        Match_tree.cursor_in_branch_pattern(pos, node)
        || Keyword_slots.Match_slot.cursor_in_when_guard(
             source,
             pos,
             node,
             node,
           )
      )
      && !
           Keyword_slots.Match_slot.cursor_at_when_keyword_slot(
             tree,
             source,
             pos,
             node,
           );
    } else {
      false;
    };
  if (here) {
    true;
  } else {
    Util.exists_named_child(
      node,
      tree_contains_match_pattern_cursor(tree, source, pos),
    );
  };
};
