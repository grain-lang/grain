open Grain_typed;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#exit
module RequestParams = {
  [@deriving yojson]
  type t = unit;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#exit
module ResponseResult = {
  [@deriving yojson]
  type t = unit;
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  Protocol.response(~id, ResponseResult.to_yojson());
};
