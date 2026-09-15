open Types;
open Grammar;

let pattern_name = (tree: parse_tree, node: Tree.node) =>
  if (Util.node_kind_in(
        node,
        [Kind.variable_pattern, Kind.identifier, Kind.upper_identifier],
      )) {
    Some(Tree.node_text(tree, node));
  } else {
    None;
  };

let binding_from_value_binding = (tree: parse_tree, node: Tree.node) =>
  switch (Tree.node_child_by_field_name(node, Field.pattern)) {
  | None => None
  | Some(pattern) => pattern_name(tree, pattern)
  };

let binding_from_module = (tree: parse_tree, node: Tree.node) =>
  switch (Tree.node_child_by_field_name(node, Field.name)) {
  | None => None
  | Some(name_node) => Some(Tree.node_text(tree, name_node))
  };

let is_root_module_declaration = node =>
  switch (Tree.node_parent(node)) {
  | Some(parent) => Tree.node_kind(parent) == Kind.program
  | None => false
  };

let has_named_child_of_kind = (node, kind) => {
  let count = Tree.node_named_child_count(node);
  let rec loop = idx =>
    if (idx >= count) {
      false;
    } else {
      switch (Tree.node_named_child(node, idx)) {
      | Some(child) when Tree.node_kind(child) == kind => true
      | _ => loop(idx + 1)
      };
    };
  loop(0);
};

let is_module_scope = node => {
  let kind = Tree.node_kind(node);
  kind == Kind.module_declaration
  || kind == Kind.provide_declaration
  && has_named_child_of_kind(node, Kind.upper_identifier);
};

let local_bindings_before =
    (tree: parse_tree, pos: position): list(local_binding) => {
  let source = Tree.source(tree);
  let cursor_byte = Text.byte_offset(source, pos);
  let root = Tree.root(tree);
  let node_contains_cursor = node =>
    Tree.node_start_byte(node) <= cursor_byte
    && cursor_byte <= Tree.node_end_byte(node);
  let rec collect =
          (tree, node: Tree.node, cursor_byte, acc: list(local_binding)) => {
    let acc =
      if (Tree.node_start_byte(node) >= cursor_byte) {
        acc;
      } else {
        switch (Tree.node_kind(node)) {
        | k when k == Kind.value_binding =>
          switch (binding_from_value_binding(tree, node)) {
          | Some(name) => [{name: name}, ...acc]
          | None => acc
          }
        | k
            when
              k == Kind.module_declaration
              && !is_root_module_declaration(node) =>
          switch (binding_from_module(tree, node)) {
          | Some(name) => [{name: name}, ...acc]
          | None => acc
          }
        | _ => acc
        };
      };
    // If the cursor is not inside the module's scope, skip it to avoid offering its bindings
    if (is_module_scope(node) && !node_contains_cursor(node)) {
      acc;
    } else {
      let count = Tree.node_named_child_count(node);
      let rec loop = (idx, acc) =>
        if (idx >= count) {
          acc;
        } else {
          switch (Tree.node_named_child(node, idx)) {
          | None => loop(idx + 1, acc)
          | Some(child) =>
            loop(idx + 1, collect(tree, child, cursor_byte, acc))
          };
        };
      loop(0, acc);
    };
  };
  collect(tree, root, cursor_byte, []);
};
