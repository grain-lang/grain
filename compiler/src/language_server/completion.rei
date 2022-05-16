open Grain_typed;

let get_module_exports:
  (~path: Path.t, Typedtree.typed_program) => list(Rpc.completion_item);

module Resolution: {
  let process:
    (
      ~id: Rpc.msg_id,
      ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
      ~documents: Hashtbl.t(string, string),
      Yojson.Safe.t
    ) =>
    unit;
};

let process:
  (
    ~id: Rpc.msg_id,
    ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
    ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
    ~documents: Hashtbl.t(string, string),
    Yojson.Safe.t
  ) =>
  unit;
