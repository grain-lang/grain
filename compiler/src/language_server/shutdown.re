open Grain_typed;

[@deriving yojson]
type null_response = {
  jsonrpc: Rpc.version,
  id: Rpc.message_id,
  result: option(string),
};

let process =
    (
      ~id: Rpc.message_id,
      ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
      ~documents,
      request,
    ) => {
  let empty_response: null_response = {
    jsonrpc: Rpc.version,
    id,
    result: None,
  };
  let res = null_response_to_yojson(empty_response);
  let str_json = Yojson.Safe.to_string(res);
  Rpc.send(stdout, str_json);
};
