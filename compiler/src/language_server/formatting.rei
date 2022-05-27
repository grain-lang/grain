open Grain_typed;

//https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentFormattingParams
module RequestParams: {
  [@deriving yojson({strict: false})]
  type formatting_options;

  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    options: formatting_options,
  };
};

//https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textEdit
module ResponseResult: {
  [@deriving yojson]
  type t;
};

let process:
  (
    ~id: Protocol.message_id,
    ~uri: Protocol.uri,
    ~compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
    ~cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
    ~documents: Hashtbl.t(Protocol.uri, string),
    RequestParams.t
  ) =>
  unit;
