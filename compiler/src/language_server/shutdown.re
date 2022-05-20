open Grain_typed;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#shutdown
module RequestParams = {
  [@deriving yojson]
  type t = unit;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#shutdown
module ResponseResult = {
  [@deriving yojson]
  type t = unit;
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  Protocol.response(~id, ResponseResult.to_yojson());
};
