open Grain_typed;

let process:
  (
    ~documents: Hashtbl.t(string, string),
    ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
    ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
    Yojson.Safe.t
  ) =>
  unit;
