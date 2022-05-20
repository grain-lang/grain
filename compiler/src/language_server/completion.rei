open Grain_typed;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
module RequestParams: {
  [@deriving yojson({strict: false})]
  type t;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
module ResponseResult: {
  [@deriving yojson]
  type t;
};

let process:
  (
    ~id: Protocol.message_id,
    ~compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
    ~cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
    ~documents: Hashtbl.t(Protocol.uri, string),
    RequestParams.t
  ) =>
  unit;

module Resolution: {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem
  module RequestParams: {
    [@deriving yojson({strict: false})]
    type t;
  };

  let process:
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~documents: Hashtbl.t(Protocol.uri, string),
      RequestParams.t
    ) =>
    unit;
};
