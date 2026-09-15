open Types;
open Grammar;

let is_incomplete_let_declaration = (node: Tree.node) =>
  Tree.node_kind(node) == Kind.let_declaration
  && Option.is_none(Util.named_child_by_kind(node, Kind.value_bindings));

let let_header_slot = (source, pos: position, header: Tree.node) => {
  let has_rec =
    Option.is_some(Tree.node_child_by_field_name(header, Field.rec_));
  let has_mut =
    Option.is_some(Tree.node_child_by_field_name(header, Field.mut));
  if ((has_rec || has_mut) && Util.cursor_in_node_span(source, pos, header)) {
    Some(LetAfterModifier);
  } else if (Util.cursor_in_node_span(source, pos, header)) {
    Some(LetHeader);
  } else {
    None;
  };
};

let binding_body_slot =
    (source, pos: position, cursor_node, ~header, pattern_opt, value_opt) => {
  let header_slot = () =>
    switch (header) {
    | Some(header) when Util.cursor_in_node(pos, header) =>
      let_header_slot(source, pos, header)
    | _ => None
    };
  switch (pattern_opt, value_opt) {
  | (Some(pattern), Some(value)) =>
    if (Util.cursor_in_node(pos, pattern)) {
      None;
    } else if (Util.cursor_in_node_span(source, pos, value)) {
      switch (Control_flow.slot_from_block_or_loop(source, pos, cursor_node)) {
      | Some(slot) => Some(slot)
      | None => Some(ExpressionStart)
      };
    } else {
      header_slot();
    }
  | (Some(pattern), None) =>
    if (Util.cursor_in_node(pos, pattern)) {
      None;
    } else {
      header_slot();
    }
  | _ =>
    switch (header) {
    | Some(header) => let_header_slot(source, pos, header)
    | None => None
    }
  };
};

let rec let_binding_slot =
        (
          tree: parse_tree,
          source,
          pos: position,
          cursor_node: Tree.node,
          node: Tree.node,
        ) => {
  let kind = Tree.node_kind(node);
  if (kind == Kind.let_declaration || kind == Kind.let_expression) {
    switch (Util.named_child_by_kind(node, Kind.let_header)) {
    | Some(header) =>
      let binding =
        Option.bind(
          Util.named_child_by_kind(node, Kind.value_bindings), bindings =>
          Tree.node_named_child(bindings, 0)
        );
      switch (binding) {
      | Some(binding) =>
        binding_body_slot(
          source,
          pos,
          cursor_node,
          ~header=Some(header),
          Tree.node_child_by_field_name(binding, Field.pattern),
          Tree.node_child_by_field_name(binding, Field.value),
        )
      | None => let_header_slot(source, pos, header)
      };
    | None =>
      switch (Tree.node_child_by_field_name(node, Field.modifier_prefix)) {
      | Some(_) when Util.cursor_in_node_span(source, pos, node) =>
        Some(LetHeader)
      | _ => None
      }
    };
  } else if (kind == Kind.value_binding) {
    binding_body_slot(
      source,
      pos,
      cursor_node,
      ~header=None,
      Tree.node_child_by_field_name(node, Field.pattern),
      Tree.node_child_by_field_name(node, Field.value),
    );
  } else if (kind == Kind.let_header) {
    let_header_slot(source, pos, node);
  } else {
    switch (Tree.node_parent(node)) {
    | None => None
    | Some(parent) =>
      let_binding_slot(tree, source, pos, cursor_node, parent)
    };
  };
};

let rec incomplete_let_slot_on_line = (source, pos: position, node: Tree.node) =>
  if (Tree.node_kind(node) == Kind.let_declaration
      && is_incomplete_let_declaration(node)) {
    let (row, _) = Tree.node_start_point(node);
    if (row == pos.line) {
      switch (Tree.node_child_by_field_name(node, Field.modifier_prefix)) {
      | Some(_) when Util.cursor_in_node_span(source, pos, node) =>
        Some(LetHeader)
      | Some(_) => None
      | None =>
        switch (Util.named_child_by_kind(node, Kind.let_header)) {
        | Some(header) => let_header_slot(source, pos, header)
        | None => None
        }
      };
    } else {
      None;
    };
  } else {
    Util.find_named_child_map(
      node,
      incomplete_let_slot_on_line(source, pos),
    );
  };

let incomplete_let_slot_from_tree = (tree: parse_tree, source, pos: position) =>
  incomplete_let_slot_on_line(source, pos, Tree.root(tree));

let rec expression_start_on_line = (source, pos: position, node: Tree.node) =>
  if (Tree.node_kind(node) == Kind.value_binding) {
    let (row, _) = Tree.node_start_point(node);
    if (row == pos.line) {
      switch (
        Tree.node_child_by_field_name(node, Field.pattern),
        Tree.node_child_by_field_name(node, Field.value),
      ) {
      | (Some(pattern), Some(value)) =>
        !Util.cursor_in_node(pos, pattern)
        && Util.cursor_in_node_span(source, pos, value)
      | _ => false
      };
    } else {
      false;
    };
  } else {
    Util.exists_named_child(node, expression_start_on_line(source, pos));
  };

let expression_start_slot =
    (tree: parse_tree, source, pos: position, node: Tree.node) =>
  switch (let_binding_slot(tree, source, pos, node, node)) {
  | Some(ExpressionStart) as slot => slot
  | _ =>
    if (expression_start_on_line(source, pos, Tree.root(tree))) {
      Some(ExpressionStart);
    } else {
      None;
    }
  };
