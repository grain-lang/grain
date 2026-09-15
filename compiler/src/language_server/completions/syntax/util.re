open Types;

let find_named_child_map = (node, f) => {
  let count = Tree.node_named_child_count(node);
  let rec loop = idx =>
    if (idx >= count) {
      None;
    } else {
      switch (Tree.node_named_child(node, idx)) {
      | None => loop(idx + 1)
      | Some(child) =>
        switch (f(child)) {
        | Some(_) as found => found
        | None => loop(idx + 1)
        }
      };
    };
  loop(0);
};

let find_named_child = (node, predicate) =>
  find_named_child_map(node, child => predicate(child) ? Some(child) : None);

let named_child_by_kind = (node, kind) =>
  find_named_child(node, child => Tree.node_kind(child) == kind);

let exists_named_child = (node, predicate) =>
  Option.is_some(find_named_child(node, predicate));

let node_kind_in = (node, kinds) => List.mem(Tree.node_kind(node), kinds);

let ancestor_of_kind = (node, kind) =>
  Tree.ancestor(node, n => Tree.node_kind(n) == kind);

let ancestor_of_kinds = (node, kinds) =>
  Tree.ancestor(node, n => node_kind_in(n, kinds));

let starts_on_line = (pos: position, node: Tree.node) => {
  let (row, _) = Tree.node_start_point(node);
  row == pos.line;
};

let cursor_at_or_after_node_end = (pos: position, node: Tree.node) => {
  let (end_row, end_col) = Tree.node_end_point(node);
  pos.line > end_row || pos.line == end_row && pos.character >= end_col;
};

let cursor_after_node_end = (pos: position, node: Tree.node) => {
  let (end_row, end_col) = Tree.node_end_point(node);
  pos.line > end_row || pos.line == end_row && pos.character > end_col;
};

let cursor_before_node_start = (pos: position, node: Tree.node) => {
  let (start_row, start_col) = Tree.node_start_point(node);
  pos.line < start_row || pos.line == start_row && pos.character < start_col;
};

let cursor_in_node = (pos: position, node: Tree.node) => {
  let (start_row, start_column) = Tree.node_start_point(node);
  let (end_row, end_column) = Tree.node_end_point(node);
  !Tree.cursor_before(pos, start_row, start_column)
  && !Tree.cursor_after(pos, end_row, end_column);
};

let cursor_in_node_span = (source, pos: position, node: Tree.node) => {
  let (start_row, start_col) = Tree.node_start_point(node);
  let (end_row, end_col) = Tree.node_end_point(node);
  if (Tree.cursor_before(pos, start_row, start_col)) {
    false;
  } else if (pos.line < end_row
             || pos.line == end_row
             + 1
             && pos.character <= end_col) {
    true;
  } else if (pos.line > end_row) {
    false;
  } else {
    let line = Text.line_at(source, pos);
    let character =
      Edit.clamp(
        ~min_value=0,
        ~max_value=String.length(line),
        pos.character,
      );
    if (character <= end_col) {
      true;
    } else {
      let between = String.sub(line, end_col, character - end_col);
      String.for_all(c => c == ' ' || c == '\t', between);
    };
  };
};

let node_at_cursor = (tree: parse_tree, pos: position) =>
  switch (Tree.node_at_point(tree, pos)) {
  | Some(node) => node
  | None =>
    switch (Tree.node_descendant_for_point(tree, pos)) {
    | Some(node) => node
    | None => Tree.root(tree)
    }
  };

let rec find_descendant_on_line =
        (pos: position, predicate: Tree.node => bool, node: Tree.node) => {
  let (row, _) = Tree.node_start_point(node);
  if (row == pos.line && predicate(node)) {
    true;
  } else {
    exists_named_child(node, find_descendant_on_line(pos, predicate));
  };
};

let slot_if_ancestor_or_on_line =
    (
      ~tree,
      ~pos: position,
      ~in_ancestor: bool,
      ~on_line: Tree.node => bool,
      slot,
    ) =>
  if (in_ancestor) {
    Some(slot);
  } else if (find_descendant_on_line(pos, on_line, Tree.root(tree))) {
    Some(slot);
  } else {
    None;
  };
