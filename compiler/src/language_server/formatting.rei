open Grain_typed;

//https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentFormattingParams
module RequestParams: {
  [@deriving yojson({strict: false})]
  type t;
};

//https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textEdit
module ResponseResult: {
  [@deriving yojson]
  type t;
};

let process:
  (
    ~id: Protocol.message_id,
    ~documents: Hashtbl.t(Protocol.uri, string),
    RequestParams.t
  ) =>
  unit;
