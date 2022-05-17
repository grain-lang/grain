open Grain_typed;

let process:
  (
    ~id: Rpc.message_id,
    ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
    Yojson.Safe.t
  ) =>
  unit;
