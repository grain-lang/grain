open Types;
open Grammar;

let record_field_header_slot =
    (tree: parse_tree, source, pos: position, node: Tree.node) => {
  let matches = n =>
    Tree.node_kind(n) == Kind.record_field_declaration
    && Slot_predicates.record_field_header_in_declaration(source, pos, n);
  let in_ancestor =
    switch (Util.ancestor_of_kind(node, Kind.record_field_declaration)) {
    | Some(field_decl) =>
      Slot_predicates.record_field_header_in_declaration(
        source,
        pos,
        field_decl,
      )
    | None => false
    };
  Util.slot_if_ancestor_or_on_line(
    ~tree,
    ~pos,
    ~in_ancestor,
    ~on_line=matches,
    RecordFieldHeader,
  );
};

let in_import_include_tail = (source, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kind(node, Kind.include_declaration)) {
  | Some(include_decl) =>
    switch (
      Tree.node_child_by_field_name(
        include_decl,
        Field.include_keyword_prefix,
      )
    ) {
    | Some(prefix) => Util.cursor_in_node_span(source, pos, prefix)
    | None =>
      Slot_predicates.import_include_tail_after_path(pos, include_decl)
    }
  | None => false
  };

let import_include_tail_slot =
    (tree: parse_tree, source, pos: position, node: Tree.node) => {
  let matches = n =>
    Tree.node_kind(n) == Kind.include_declaration
    && (
      switch (Tree.node_child_by_field_name(n, Field.include_keyword_prefix)) {
      | Some(prefix) => Util.cursor_in_node_span(source, pos, prefix)
      | None => Slot_predicates.import_include_tail_after_path(pos, n)
      }
    );
  Util.slot_if_ancestor_or_on_line(
    ~tree,
    ~pos,
    ~in_ancestor=in_import_include_tail(source, pos, node),
    ~on_line=matches,
    ImportIncludeTail,
  );
};

let provide_type_tail_after_rec = (pos: position, node: Tree.node) =>
  switch (Util.named_child_by_kind(node, Kind.provide_type_header)) {
  | Some(header) =>
    switch (Tree.node_child_by_field_name(header, Field.rec_)) {
    | Some(rec_node) => Util.cursor_at_or_after_node_end(pos, rec_node)
    | None => false
    }
  | None => false
  };

let is_provide_type_tail_node = node =>
  Tree.node_kind(node) == Kind.type_alias
  || Tree.node_kind(node) == Kind.incomplete_type_alias
  || Tree.node_kind(node) == Kind.incomplete_provide_declaration
  && Option.is_some(Util.named_child_by_kind(node, Kind.provide_type_header));

let provide_type_tail_slot =
    (tree: parse_tree, pos: position, node: Tree.node) => {
  let in_ancestor =
    switch (Tree.ancestor(node, is_provide_type_tail_node)) {
    | Some(type_node) => provide_type_tail_after_rec(pos, type_node)
    | None => false
    };
  Util.slot_if_ancestor_or_on_line(
    ~tree,
    ~pos,
    ~in_ancestor,
    ~on_line=
      n =>
        is_provide_type_tail_node(n) && provide_type_tail_after_rec(pos, n),
    ProvideTypeTail,
  );
};

let in_provide_tail = (source, pos: position, node: Tree.node) =>
  switch (Util.ancestor_of_kinds(node, Kind.provide_declarations)) {
  | Some(provide_decl) =>
    Slot_predicates.provide_tail_after_header(
      ~exclude_let_header=true,
      source,
      pos,
      provide_decl,
    )
  | None => false
  };

let provide_tail_slot =
    (tree: parse_tree, source, pos: position, node: Tree.node) => {
  let matches = n =>
    Util.node_kind_in(n, Kind.provide_declarations)
    && Slot_predicates.provide_tail_after_header(
         ~exclude_let_header=true,
         source,
         pos,
         n,
       );
  Util.slot_if_ancestor_or_on_line(
    ~tree,
    ~pos,
    ~in_ancestor=in_provide_tail(source, pos, node),
    ~on_line=matches,
    ProvideTail,
  );
};
