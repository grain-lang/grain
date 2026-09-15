open Types;
open Grammar;

let cursor_in_guard_but_not_expression = (pos: position, guard: Tree.node) =>
  switch (Tree.node_named_child(guard, 0)) {
  | Some(expr) when Util.cursor_in_node(pos, expr) => false
  | _ => Util.cursor_in_node(pos, guard)
  };

let cursor_in_when_guard_expression = (source, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.match_branch)) {
  | None => false
  | Some(branch) =>
    switch (Match_tree.when_guard_of_branch(branch)) {
    | None => false
    | Some(guard) =>
      switch (Tree.node_named_child(guard, 0)) {
      | Some(expr) => Util.cursor_in_node_span(source, pos, expr)
      | None => false
      }
    }
  };

let cursor_in_when_guard =
    (source, pos: position, branch: Tree.node, node: Tree.node) =>
  if (cursor_in_when_guard_expression(source, pos, node)) {
    false;
  } else {
    switch (Match_tree.when_guard_of_branch(branch)) {
    | None => false
    | Some(guard) => cursor_in_guard_but_not_expression(pos, guard)
    };
  };

let in_match_guard = (pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.when_guard)) {
  | Some(guard) => cursor_in_guard_but_not_expression(pos, guard)
  | None => false
  };

let cursor_after_branch_pattern = (pos: position, branch: Tree.node) =>
  switch (Tree.node_child_by_field_name(branch, Field.pattern)) {
  | None => false
  | Some(pattern) =>
    let (end_row, end_col) = Tree.node_end_point(pattern);
    pos.line == end_row && pos.character >= end_col;
  };

let cursor_after_pattern_for_when_slot =
    (source, pos: position, branch: Tree.node) =>
  if (cursor_after_branch_pattern(pos, branch)) {
    true;
  } else {
    switch (Tree.node_child_by_field_name(branch, Field.when_keyword_prefix)) {
    | Some(prefix) => Util.cursor_in_node_span(source, pos, prefix)
    | None => false
    };
  };

let when_keyword_allowed = (source, pos: position, branch: Tree.node) =>
  switch (Match_tree.when_guard_of_branch(branch)) {
  | None =>
    switch (Tree.node_child_by_field_name(branch, Field.when_keyword_prefix)) {
    | Some(prefix) => Util.cursor_in_node_span(source, pos, prefix)
    | None => true
    }
  | Some(guard) =>
    switch (Tree.node_named_child(guard, 0)) {
    | Some(expr) => Util.cursor_before_node_start(pos, expr)
    | None => Util.cursor_in_node(pos, guard)
    }
  };

let cursor_at_when_keyword_slot =
    (tree: parse_tree, source, pos: position, node: Tree.node) => {
  let after_pattern_on_line =
    switch (Util.ancestor_of_kind(node, Kind.match_branch)) {
    | Some(branch) =>
      !Match_tree.cursor_in_branch_body(pos, branch)
      && !cursor_in_when_guard_expression(source, pos, node)
      && Match_tree.cursor_on_pattern_side_of_line(tree, pos, node)
      && cursor_after_pattern_for_when_slot(source, pos, branch)
    | None =>
      Match_tree.cursor_in_match_body_before_arrow(tree, pos, node)
      && (
        switch (Match_tree.match_branch_on_line(pos, Tree.root(tree))) {
        | Some(branch) =>
          cursor_after_pattern_for_when_slot(source, pos, branch)
        | None => false
        }
      )
    };
  switch (Util.ancestor_of_kind(node, Kind.match_branch)) {
  | Some(branch) =>
    after_pattern_on_line && when_keyword_allowed(source, pos, branch)
  | None =>
    switch (Match_tree.match_branch_on_line(pos, Tree.root(tree))) {
    | Some(branch) =>
      after_pattern_on_line && when_keyword_allowed(source, pos, branch)
    | None => false
    }
  };
};

let partial_when_keyword_slot = (tree: parse_tree, source, pos: position) =>
  if (Util.find_descendant_on_line(
        pos,
        node => {
          Tree.node_kind(node) == Kind.match_branch
          && Slot_predicates.partial_when_keyword_prefix(
               ~prefix_line=false,
               source,
               pos,
               node,
             )
        },
        Tree.root(tree),
      )) {
    Some(MatchGuard);
  } else {
    None;
  };
