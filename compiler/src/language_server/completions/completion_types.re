// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = Protocol.text_document_position_params;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem
module ResponseResult = {
  let enum_to_yojson = (to_enum, value) =>
    to_enum(value) |> [%to_yojson: int];
  let enum_of_yojson = (of_enum, json) =>
    Result.bind(json |> [%of_yojson: int], value =>
      switch (of_enum(value)) {
      | Some(v) => Ok(v)
      | None => Result.Error("Invalid enum value")
      }
    );

  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
  [@deriving (enum, yojson)]
  type completion_item_kind =
    | [@value 1] Text
    | [@value 2] Method
    | [@value 3] Function
    | [@value 4] Constructor
    | [@value 5] Field
    | [@value 6] Variable
    | [@value 7] Class
    | [@value 8] Interface
    | [@value 9] Module
    | [@value 10] Property
    | [@value 11] Unit
    | [@value 12] Value
    | [@value 13] Enum
    | [@value 14] Keyword
    | [@value 15] Snippet
    | [@value 16] Color
    | [@value 17] File
    | [@value 18] Reference
    | [@value 19] Folder
    | [@value 20] EnumMember
    | [@value 21] Constant
    | [@value 22] Struct
    | [@value 23] Event
    | [@value 24] Operator
    | [@value 25] TypeParameter;
  let completion_item_kind_to_yojson =
    enum_to_yojson(completion_item_kind_to_enum);
  let completion_item_kind_of_yojson =
    enum_of_yojson(completion_item_kind_of_enum);

  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#insertTextFormat
  [@deriving (enum, yojson)]
  type insert_text_format =
    | [@value 1] PlainText
    | [@value 2] SnippetFormat;
  let insert_text_format_to_yojson =
    enum_to_yojson(insert_text_format_to_enum);
  let insert_text_format_of_yojson =
    enum_of_yojson(insert_text_format_of_enum);

  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemTag
  [@deriving (enum, yojson)]
  type completion_item_tag =
    | [@value 1] Deprecated;
  let completion_item_tag_to_yojson =
    enum_to_yojson(completion_item_tag_to_enum);
  let completion_item_tag_of_yojson =
    enum_of_yojson(completion_item_tag_of_enum);

  [@deriving yojson({strict: false})]
  type item = {
    label: string,
    kind: completion_item_kind,
    [@default None]
    detail: option(string),
    [@key "sortText"]
    sort_text: string,
    [@default None] [@key "filterText"]
    filter_text: option(string),
    [@default None] [@key "insertText"]
    insert_text: option(string),
    [@default None] [@key "insertTextFormat"]
    insert_text_format: option(insert_text_format),
    [@default None] [@key "textEdit"]
    text_edit: option(Protocol.text_edit),
    [@default None]
    command: option(Protocol.command),
    [@default None]
    tags: option(list(completion_item_tag)),
  };

  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
  [@deriving yojson({strict: false})]
  type t = {
    [@key "isIncomplete"]
    is_incomplete: bool,
    items: list(item),
  };
};

type source =
  | TreeSitter
  | Filesystem
  | Typedtree
  | KeywordSource
  | DocblockAttributeSource;

// Source rank is used to break ties between candidates from the same source during deduplication.
let source_rank =
  fun
  // Since compiler results are more authoritative, they should be highest
  | Typedtree => 0
  // Docblocks are domain specific and are not guesses so also share this priority
  | DocblockAttributeSource => 0
  // Tree sitter results are primarily to attempt to complete incomplete or missing information and carries less metadata
  | TreeSitter => 1
  // Filesystem results come from directory listings rather than the parse tree
  | Filesystem => 1
  // Keywords are language tokens so shouldn't collide with other sources as they don't share the same kind
  | KeywordSource => 2;

// Completion item ordering for LSP sortText and sorting. Lower ranks appear first.
module Sort_group = {
  type t =
    | Typed
    | Import
    | ModuleMember
    | Operator
    | Local
    | Keyword;

  let ordered: list(t) = [
    Typed,
    Import,
    ModuleMember,
    Local,
    Keyword,
    Operator,
  ];

  let rank = group =>
    switch (List.find_index((==)(group), ordered)) {
    | Some(index) => index
    | None => Int.max_int
    };
};

module Relevance = {
  type t = {
    score: int,
    is_exact_prefix: bool,
    remaining_chars: int,
    boost: int,
  };

  let score_for_sort_group =
    fun
    | Sort_group.Typed => 5000
    | Sort_group.Import => 4500
    | Sort_group.ModuleMember => 4000
    | Sort_group.Local => 3000
    | Sort_group.Keyword => 2000
    | Sort_group.Operator => 1000;

  let score_for_source =
    fun
    | Typedtree => 150
    | DocblockAttributeSource => 140
    | TreeSitter => 100
    | Filesystem => 90
    | KeywordSource => 50;

  let remaining_chars = (~prefix, text) => {
    let prefix_len = String.length(prefix);
    let text_len = String.length(text);
    text_len > prefix_len ? text_len - prefix_len : 0;
  };

  let prefix_score = (~prefix, text) =>
    if (prefix == "") {
      0;
    } else {
      let remaining = remaining_chars(~prefix, text);
      let closeness = max(0, 300 - min(remaining, 30) * 10);
      (text == prefix ? 1000 : 0) + closeness;
    };

  let make =
      (~source, ~sort_group, ~label, ~filter_text=?, ~prefix, ~boost=0, ()) => {
    let text = Option.value(~default=label, filter_text);
    let remaining_chars = remaining_chars(~prefix, text);
    {
      score:
        score_for_sort_group(sort_group)
        + score_for_source(source)
        + boost
        + prefix_score(~prefix, text),
      is_exact_prefix: prefix != "" && text == prefix,
      remaining_chars,
      boost,
    };
  };

  let compare = (left, right) =>
    switch (Int.compare(right.score, left.score)) {
    | 0 =>
      switch (left.is_exact_prefix, right.is_exact_prefix) {
      | (true, false) => (-1)
      | (false, true) => 1
      | _ =>
        switch (Int.compare(left.remaining_chars, right.remaining_chars)) {
        | 0 => Int.compare(right.boost, left.boost)
        | cmp => cmp
        }
      }
    | cmp => cmp
    };

  let max_sort_score = 9999999;
  let sort_width = String.length(string_of_int(max_sort_score));

  let to_sort_text = (~relevance: t, ~label) =>
    Printf.sprintf(
      "%0*d_%s",
      sort_width,
      max(0, max_sort_score - relevance.score),
      String.lowercase_ascii(label),
    );
};

type candidate = {
  item: ResponseResult.item,
  source,
  sort_group: Sort_group.t,
  relevance: Relevance.t,
};
