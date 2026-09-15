open Types;
open Grammar;

let normalize_keyword_prefix =
    (
      source,
      pos: position,
      keyword_slot: option(keyword_slot),
      prefix,
      prefix_start,
      prefix_end,
    ) => {
  switch (keyword_slot) {
  | Some(LetHeader) when prefix == Keyword.let_ => (
      "",
      prefix_end,
      prefix_end,
    )
  | Some(LetAfterModifier)
      when prefix == Keyword.rec_ || prefix == Keyword.mut => (
      "",
      prefix_end,
      prefix_end,
    )
  | Some(LetHeader)
  | Some(LetAfterModifier) => (prefix, prefix_start, prefix_end)
  | Some(ImportIncludeTail) when prefix == Keyword.include_ => (
      "",
      prefix_end,
      prefix_end,
    )
  | Some(ProvideTail)
  | Some(MatchGuard)
  | Some(IfTail)
  | Some(RecordFieldHeader)
  | Some(BlockStatement)
  | Some(LoopBody) when prefix != "" => (prefix, prefix_start, prefix_end)
  | _ => (prefix, prefix_start, prefix_end)
  };
};

let previous_non_whitespace = (source, pos: position) => {
  let cursor_byte = Text.byte_offset(source, pos);
  let rec loop = idx =>
    if (idx <= 0) {
      None;
    } else {
      switch (source.[idx - 1]) {
      | ' '
      | '\t'
      | '\n'
      | '\r' => loop(idx - 1)
      | c => Some(c)
      };
    };
  loop(cursor_byte);
};

let expression_start_after_delimiter = (source, pos: position) =>
  switch (previous_non_whitespace(source, pos)) {
  | Some('[')
  | Some('(')
  | Some(',') => true
  | _ => false
  };

let prefix_context = (tree: parse_tree, source, pos: position) => {
  let in_doc_comment = Doc_comment.cursor_in_doc_comment(tree, pos);
  let (doc_prefix, doc_prefix_start, doc_prefix_end) =
    Text.prefix_from_doc_attribute(source, pos);
  let editing_doc_attribute =
    in_doc_comment && String.contains(doc_prefix, '@');
  let import_context =
    editing_doc_attribute
      ? None : Import_context.import_context_from_tree(tree, pos);
  let prefix_range =
    if (editing_doc_attribute) {
      (doc_prefix, doc_prefix_start, doc_prefix_end);
    } else {
      switch (import_context) {
      | Some((_kind, prefix, prefix_start, prefix_end)) => (
          prefix,
          prefix_start,
          prefix_end,
        )
      | None => Text.prefix_from_cursor(source, pos)
      };
    };
  (editing_doc_attribute, import_context, prefix_range);
};

let apply_import_context = (detected_kind, import_context) =>
  switch (import_context) {
  | Some((kind, _prefix, _prefix_start, _)) => kind
  | None => detected_kind
  };

let apply_member_access_fallback = (source, pos: position, kind) =>
  switch (kind) {
  | InScope =>
    switch (Text.qualifier_from_line(source, pos)) {
    | Some(qualifier) => MemberAccess(qualifier)
    | None => kind
    }
  | _ => kind
  };

let resolve_kind = (tree: parse_tree, source, pos: position, import_context) => {
  let cursor_byte = Text.byte_offset(source, pos);
  let node = Util.node_at_cursor(tree, pos);
  let detected_kind =
    Detect.detect_kind(tree, pos, source, cursor_byte, node);
  let kind = apply_import_context(detected_kind, import_context);
  apply_member_access_fallback(source, pos, kind);
};

let resolve_keyword_slot = (tree: parse_tree, source, pos: position, kind) => {
  let node = Util.node_at_cursor(tree, pos);
  let alias = Keyword_slots.Aliases.alias_slot(tree, source, pos, node);
  let keyword_slot =
    switch (alias) {
    | Some(_) => alias
    | None =>
      switch (kind) {
      | Suppressed => None
      | UseItems(_, _) => None
      | UseModulePath(_) => None
      | UseShape => None
      | MatchGuardKeyword => Some(MatchGuard)
      | _ => Keyword_slot.resolve(tree, pos, source)
      }
    };
  switch (keyword_slot, kind) {
  | (None, InScope) when expression_start_after_delimiter(source, pos) =>
    Some(ExpressionStart)
  | _ => keyword_slot
  };
};

let make_context = (pos: position, kind, keyword_slot, prefix_range) => {
  let (prefix, prefix_start, prefix_end) = prefix_range;
  let edit_range = Text.replace_range(pos, prefix_start, prefix_end);
  {
    kind,
    keyword_slot,
    prefix,
    replace_range: edit_range,
  };
};

let at = (tree: parse_tree, pos: position) => {
  let source = Tree.source(tree);
  let (editing_doc_attribute, import_context, prefix_range) =
    prefix_context(tree, source, pos);
  if (editing_doc_attribute) {
    make_context(pos, DocblockContext, None, prefix_range);
  } else {
    let kind = resolve_kind(tree, source, pos, import_context);
    let keyword_slot = resolve_keyword_slot(tree, source, pos, kind);
    let (prefix, prefix_start, prefix_end) = prefix_range;
    let normalized_prefix =
      normalize_keyword_prefix(
        source,
        pos,
        keyword_slot,
        prefix,
        prefix_start,
        prefix_end,
      );
    make_context(pos, kind, keyword_slot, normalized_prefix);
  };
};
