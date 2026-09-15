open Types;
open Grammar;

let in_label_of_argument = (pos, cursor_byte, arg) =>
  switch (Tree.node_child_by_field_name(arg, Field.label)) {
  | None => false
  | Some(label) =>
    switch (Tree.node_child_by_field_name(label, Field.value)) {
    | None => true
    | Some(value) =>
      Tree.cursor_inside(pos, value)
      || cursor_byte < Tree.node_start_byte(value)
    }
  };

let after_callee_before_positional = (pos, cursor_byte, ~node, ~app, ~callee) =>
  switch (Tree.node_or_ancestor(node, Kind.positional_application_argument)) {
  | Some(_) => false
  | None =>
    switch (Tree.node_child_by_field_name(app, Field.arguments)) {
    | Some(args) => Tree.cursor_inside(pos, args)
    | None =>
      cursor_byte > Tree.node_end_byte(callee)
      && cursor_byte < Tree.node_end_byte(app)
    }
  };

let in_call_argument_label_position =
    (_tree: parse_tree, pos: position, cursor_byte: int, node: Tree.node) =>
  switch (Tree.node_or_ancestor(node, Kind.application_expression)) {
  | None => false
  | Some(app) =>
    switch (Tree.node_child_by_field_name(app, Field.function_)) {
    | None => false
    | Some(callee) when cursor_byte < Tree.node_end_byte(callee) => false
    | Some(callee) =>
      switch (Tree.node_or_ancestor(node, Kind.labeled_application_argument)) {
      | Some(arg) => in_label_of_argument(pos, cursor_byte, arg)
      | None =>
        after_callee_before_positional(pos, cursor_byte, ~node, ~app, ~callee)
      }
    }
  };

let callee_name_before_call = (tree: parse_tree, pos: position) => {
  let source = Tree.source(tree);
  let cursor_byte = Text.byte_offset(source, pos);
  let node_at_column = column => {
    let point = {
      ...pos,
      character: column,
    };
    switch (Tree.node_at_point(tree, point)) {
    | Some(node) => Some(node)
    | None => Tree.node_descendant_for_point(tree, point)
    };
  };
  let rec find_call_at_or_before = column =>
    if (column < 0) {
      None;
    } else {
      switch (node_at_column(column)) {
      | None => find_call_at_or_before(column - 1)
      | Some(node) =>
        switch (Tree.node_or_ancestor(node, Kind.application_expression)) {
        | Some(app) => Some(app)
        | None => find_call_at_or_before(column - 1)
        }
      };
    };
  switch (find_call_at_or_before(pos.character)) {
  | None => None
  | Some(app) =>
    switch (Tree.node_child_by_field_name(app, Field.function_)) {
    | None => None
    | Some(callee) =>
      if (cursor_byte <= Tree.node_end_byte(callee)) {
        None;
      } else {
        let rec callee_name_from_node = (node: Tree.node) =>
          if (Util.node_kind_in(
                node,
                [
                  Kind.identifier,
                  Kind.upper_identifier,
                  Kind.qualified_identifier,
                ],
              )) {
            Some(Tree.node_text(tree, node));
          } else if (Tree.node_kind(node) == Kind.parenthesized_expression) {
            switch (Tree.node_child_by_field_name(node, Field.expression)) {
            | None => None
            | Some(expr) => callee_name_from_node(expr)
            };
          } else {
            switch (String.trim(Tree.node_text(tree, node))) {
            | "" => None
            | callee => Some(callee)
            };
          };
        callee_name_from_node(callee);
      }
    }
  };
};
