open Grain_typed;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
module RequestParams: {
  [@deriving yojson({strict: false})]
  type t;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
module ResponseResult: {
  [@deriving yojson({strict: false})]
  type t;
};

let process:
  (
    ~id: Protocol.message_id,
    ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
    ~documents: Hashtbl.t(Protocol.uri, string),
    ~parse_trees:
      Hashtbl.t(Protocol.uri, (int, Grain_tree_sitter.parse_tree)),
    RequestParams.t
  ) =>
  unit;
