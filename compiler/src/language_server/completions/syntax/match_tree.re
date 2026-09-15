open Types;
open Grammar;

let when_guard_of_branch = branch =>
  Util.named_child_by_kind(branch, Kind.when_guard);

let cursor_in_branch_pattern = (pos: position, branch: Tree.node) =>
  switch (Tree.node_child_by_field_name(branch, Field.pattern)) {
  | None => false
  | Some(pattern) => Util.cursor_in_node(pos, pattern)
  };

let cursor_in_branch_body = (pos: position, branch: Tree.node) =>
  switch (Tree.node_child_by_field_name(branch, Field.body)) {
  | None => false
  | Some(body) => Util.cursor_in_node(pos, body)
  };

let cursor_before_branch_arrow = (pos: position, branch: Tree.node) =>
  switch (Tree.node_child_by_field_name(branch, Field.body)) {
  | Some(body) => Util.cursor_before_node_start(pos, body)
  | None => true
  };

let rec match_branch_on_line = (pos: position, node: Tree.node) =>
  if (Tree.node_kind(node) == Kind.match_branch) {
    let (row, _) = Tree.node_start_point(node);
    row == pos.line ? Some(node) : None;
  } else {
    Util.find_named_child_map(node, match_branch_on_line(pos));
  };

let rec branch_on_line_before_arrow = (pos: position, node: Tree.node) =>
  if (Tree.node_kind(node) == Kind.match_branch) {
    let (row, _) = Tree.node_start_point(node);
    row == pos.line && cursor_before_branch_arrow(pos, node);
  } else {
    Util.exists_named_child(node, branch_on_line_before_arrow(pos));
  };

let in_match_body_at_cursor = (pos: position, node: Tree.node) =>
  if (Tree.node_kind(node) == Kind.match_body) {
    Util.cursor_in_node(pos, node);
  } else {
    switch (Util.ancestor_of_kind(node, Kind.match_body)) {
    | Some(match_body) => Util.cursor_in_node(pos, match_body)
    | None =>
      switch (Util.ancestor_of_kind(node, Kind.match_expression)) {
      | Some(match_expr) =>
        switch (Tree.node_child_by_field_name(match_expr, Field.body)) {
        | Some(body) => Util.cursor_in_node(pos, body)
        | None => false
        }
      | None => false
      }
    };
  };

let cursor_on_pattern_side_of_line =
    (tree: parse_tree, pos: position, node: Tree.node) =>
  if (branch_on_line_before_arrow(pos, Tree.root(tree))) {
    true;
  } else {
    in_match_body_at_cursor(pos, node)
    && Option.is_none(match_branch_on_line(pos, Tree.root(tree)));
  };

let cursor_in_any_branch_body = (pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.match_branch)) {
  | None => false
  | Some(branch) => cursor_in_branch_body(pos, branch)
  };

let line_has_when_guard = (tree: parse_tree, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.match_branch)) {
  | Some(branch) =>
    switch (when_guard_of_branch(branch)) {
    | Some(_) =>
      let (row, _) = Tree.node_start_point(branch);
      row == pos.line;
    | None => false
    }
  | None =>
    switch (match_branch_on_line(pos, Tree.root(tree))) {
    | Some(branch) => Option.is_some(when_guard_of_branch(branch))
    | None => false
    }
  };

let cursor_in_match_body_before_arrow =
    (tree: parse_tree, pos: position, node: Tree.node) => {
  in_match_body_at_cursor(pos, node)
  && !cursor_in_any_branch_body(pos, node)
  && cursor_on_pattern_side_of_line(tree, pos, node);
};

let match_scrutinee_position = (tree: parse_tree, pos: position) => {
  let node = Util.node_at_cursor(tree, pos);
  switch (Util.ancestor_of_kind(node, Kind.match_expression)) {
  | None => None
  | Some(match_expr) =>
    switch (Tree.node_child_by_field_name(match_expr, Field.value)) {
    | None => None
    | Some(scrutinee) =>
      let (row, col) = Tree.node_start_point(scrutinee);
      let scrutinee_pos: position = {
        line: row,
        character: col,
      };
      Some(scrutinee_pos);
    }
  };
};
