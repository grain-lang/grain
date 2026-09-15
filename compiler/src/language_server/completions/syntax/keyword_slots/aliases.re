open Types;
open Grammar;

let gap_is_alias_boundary = (~source, ~node_end_byte, ~cursor_byte) =>
  if (cursor_byte <= node_end_byte) {
    false;
  } else {
    let segment =
      String.sub(source, node_end_byte, cursor_byte - node_end_byte);
    String.for_all(
      c => c == ' ' || c == '\t' || Text.is_ident_char(c),
      segment,
    )
    && String.exists(c => c == ' ' || c == '\t', segment);
  };

let qualifying_use_item = (~source, ~cursor_byte, item) =>
  Tree.node_kind(item) == Kind.use_item
  && Tree.node_named_child_count(item) == 1
  && gap_is_alias_boundary(
       ~source,
       ~node_end_byte=Tree.node_end_byte(item),
       ~cursor_byte,
     );

let use_item_alias = (~source, ~cursor_byte, node) => {
  let from_ancestor =
    switch (Util.ancestor_of_kind(node, Kind.use_item)) {
    | Some(item) => qualifying_use_item(~source, ~cursor_byte, item)
    | None => false
    };
  let enclosing_shape =
    Tree.node_kind(node) == Kind.use_shape
      ? Some(node) : Util.ancestor_of_kind(node, Kind.use_shape);
  let from_shape =
    switch (enclosing_shape) {
    | Some(shape) =>
      Option.is_some(
        Util.find_named_child(
          shape,
          qualifying_use_item(~source, ~cursor_byte),
        ),
      )
    | None => false
    };
  from_ancestor || from_shape;
};

let rec enclosing_of_kind = (tree: parse_tree, pos: position, kind) =>
  switch (Tree.node_at_point(tree, pos)) {
  | Some(node) =>
    switch (Tree.node_or_ancestor(node, kind)) {
    | Some(_) as found => found
    | None when pos.character > 0 =>
      enclosing_of_kind(
        tree,
        {
          ...pos,
          character: pos.character - 1,
        },
        kind,
      )
    | None => None
    }
  | None when pos.character > 0 =>
    enclosing_of_kind(
      tree,
      {
        ...pos,
        character: pos.character - 1,
      },
      kind,
    )
  | None => None
  };

let include_module_alias = (~tree, ~source, ~pos, ~cursor_byte) =>
  switch (enclosing_of_kind(tree, pos, Kind.include_declaration)) {
  | None => false
  | Some(include_decl) =>
    switch (Tree.node_child_by_field_name(include_decl, Field.include_)) {
    | None => false
    | Some(clause) =>
      switch (Tree.node_child_by_field_name(clause, Field.module_)) {
      | None => false
      | Some(module_node) =>
        Option.is_none(Tree.node_child_by_field_name(clause, Field.alias))
        && gap_is_alias_boundary(
             ~source,
             ~node_end_byte=Tree.node_end_byte(module_node),
             ~cursor_byte,
           )
      }
    }
  };

let foreign_wasm_alias = (~tree, ~source, ~pos: position, ~cursor_byte) =>
  switch (enclosing_of_kind(tree, pos, Kind.foreign_declaration)) {
  | None => false
  | Some(decl) =>
    switch (Tree.node_child_by_field_name(decl, Field.type_)) {
    | None => false
    | Some(type_node) =>
      Option.is_none(Tree.node_child_by_field_name(decl, Field.alias))
      && gap_is_alias_boundary(
           ~source,
           ~node_end_byte=Tree.node_end_byte(type_node),
           ~cursor_byte,
         )
      && (
        switch (Tree.node_child_by_field_name(decl, Field.module_)) {
        | Some(module_node) =>
          Util.cursor_before_node_start(pos, module_node)
        | None => true
        }
      )
    }
  };

let destructuring_pattern_kinds = [
  Kind.list_pattern,
  Kind.tuple_pattern,
  Kind.record_pattern,
  Kind.array_pattern,
  Kind.constructor_pattern,
];

let pattern_is_destructuring = p =>
  Util.node_kind_in(p, destructuring_pattern_kinds);

let let_binding_pattern_alias = (~tree, ~source, ~pos: position, ~cursor_byte) =>
  switch (enclosing_of_kind(tree, pos, Kind.value_binding)) {
  | None => false
  | Some(binding) =>
    switch (Tree.node_child_by_field_name(binding, Field.pattern)) {
    | None => false
    | Some(pattern) =>
      pattern_is_destructuring(pattern)
      && gap_is_alias_boundary(
           ~source,
           ~node_end_byte=Tree.node_end_byte(pattern),
           ~cursor_byte,
         )
      && (
        switch (Tree.node_child_by_field_name(binding, Field.value)) {
        | Some(value) => Util.cursor_before_node_start(pos, value)
        | None => true
        }
      )
    }
  };

let lambda_param_pattern_alias =
    (~tree, ~source, ~pos: position, ~cursor_byte) =>
  switch (enclosing_of_kind(tree, pos, Kind.lambda_argument)) {
  | None => false
  | Some(arg) =>
    switch (Tree.node_named_child(arg, 0)) {
    | Some(pattern) =>
      pattern_is_destructuring(pattern)
      && gap_is_alias_boundary(
           ~source,
           ~node_end_byte=Tree.node_end_byte(pattern),
           ~cursor_byte,
         )
    | None => false
    }
  };

let destructuring_expression_kinds = [
  Kind.tuple_expression,
  Kind.record_expression,
  Kind.list_expression,
  Kind.array_expression,
];

// `let f = ((a, b) as point)` should still suggest it even though the arrow has not been inserted
let unarrowed_lambda_param_alias =
    (~tree, ~source, ~pos: position, ~cursor_byte) =>
  switch (enclosing_of_kind(tree, pos, Kind.parenthesized_expression)) {
  | None => false
  | Some(paren) =>
    let parent_is_binding =
      switch (Tree.node_parent(paren)) {
      | Some(parent) => Tree.node_kind(parent) == Kind.value_binding
      | None => false
      };
    parent_is_binding
    && (
      switch (Tree.node_named_child(paren, 0)) {
      | Some(inner) =>
        Util.node_kind_in(inner, destructuring_expression_kinds)
        && gap_is_alias_boundary(
             ~source,
             ~node_end_byte=Tree.node_end_byte(inner),
             ~cursor_byte,
           )
      | None => false
      }
    );
  };

let alias_slot = (tree: parse_tree, source, pos: position, node: Tree.node) => {
  let cursor_byte = Text.byte_offset(source, pos);
  if (use_item_alias(~source, ~cursor_byte, node)
      || include_module_alias(~tree, ~source, ~pos, ~cursor_byte)
      || foreign_wasm_alias(~tree, ~source, ~pos, ~cursor_byte)
      || let_binding_pattern_alias(~tree, ~source, ~pos, ~cursor_byte)
      || lambda_param_pattern_alias(~tree, ~source, ~pos, ~cursor_byte)
      || unarrowed_lambda_param_alias(~tree, ~source, ~pos, ~cursor_byte)) {
    Some(AliasKeyword);
  } else {
    None;
  };
};
