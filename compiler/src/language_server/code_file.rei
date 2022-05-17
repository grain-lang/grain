open Grain_typed;

let process:
  (
    ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
    ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
    ~documents: Hashtbl.t(string, string),
    Yojson.Safe.t
  ) =>
  unit;
