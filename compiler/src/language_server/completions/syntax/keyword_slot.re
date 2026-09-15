open Types;

let structural_slot =
    (
      ~partial_block_slot,
      tree: parse_tree,
      source,
      pos: position,
      node: Tree.node,
    ) => {
  let partial_when =
    Keyword_slots.Match_slot.partial_when_keyword_slot(tree, source, pos);
  let if_tail =
    Keyword_slots.Control_flow.if_tail_slot(tree, source, pos, node);
  let import_include_tail =
    Keyword_slots.Declarations.import_include_tail_slot(
      tree,
      source,
      pos,
      node,
    );
  let provide_type_tail =
    Keyword_slots.Declarations.provide_type_tail_slot(tree, pos, node);
  let provide_tail =
    Keyword_slots.Declarations.provide_tail_slot(tree, source, pos, node);
  let record_field_header =
    Keyword_slots.Declarations.record_field_header_slot(
      tree,
      source,
      pos,
      node,
    );
  if (Keyword_slots.Match_slot.in_match_guard(pos, node)) {
    Some(MatchGuard);
  } else if (Option.is_some(partial_when)) {
    partial_when;
  } else if (Option.is_some(if_tail)) {
    if_tail;
  } else if (Option.is_some(record_field_header)) {
    record_field_header;
  } else if (Option.is_some(import_include_tail)) {
    import_include_tail;
  } else if (Option.is_some(provide_type_tail)) {
    provide_type_tail;
  } else if (Option.is_some(provide_tail)) {
    provide_tail;
  } else if (Keyword_slots.Control_flow.in_loop_body(pos, node)) {
    Some(LoopBody);
  } else if (Keyword_slots.Control_flow.in_block_body(source, pos, node)) {
    Some(BlockStatement);
  } else {
    switch (
      Keyword_slots.Let_binding.incomplete_let_slot_from_tree(
        tree,
        source,
        pos,
      )
    ) {
    | Some(slot) => Some(slot)
    | None =>
      switch (
        Keyword_slots.Let_binding.let_binding_slot(
          tree,
          source,
          pos,
          node,
          node,
        )
      ) {
      | Some(slot) => Some(slot)
      | None =>
        switch (
          Keyword_slots.Let_binding.expression_start_slot(
            tree,
            source,
            pos,
            node,
          )
        ) {
        | Some(slot) => Some(slot)
        | None =>
          Keyword_slots.Statements.fallback_statement_slot(
            ~partial_block_slot,
            tree,
            source,
            pos,
            node,
          )
        }
      }
    };
  };
};

let resolve = (tree: parse_tree, pos: position, source) => {
  let node = Util.node_at_cursor(tree, pos);
  let partial_block_slot =
    Keyword_slots.Statements.partial_block_statement_slot(tree, source, pos);
  let tree_slot =
    structural_slot(~partial_block_slot, tree, source, pos, node);
  switch (partial_block_slot) {
  | Some(BlockStatement) => Some(BlockStatement)
  | _ =>
    switch (tree_slot) {
    | None =>
      Keyword_slots.Statements.partial_toplevel_statement_slot(
        tree,
        source,
        pos,
      )
    | Some(slot) => Some(slot)
    }
  };
};
