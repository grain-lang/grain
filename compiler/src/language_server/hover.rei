open Grain_typed;

let process:
  (
    ~id: Rpc.message_id,
    ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
    ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
    ~documents: Hashtbl.t(string, string),
    Yojson.Safe.t
  ) =>
  unit;
