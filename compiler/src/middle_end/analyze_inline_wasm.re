// This module still exists to support the --no-bulk-memory flag.

open Anftree;
open Anf_iterator;
open Grain_typed;
open Grain_utils;

type inline_type =
  | WasmPrimN(primn);

type analysis +=
  | InlineableWasmTable(Ident.tbl(inline_type));

let inline_wasm_tbl: ref(Ident.tbl(inline_type)) = (
  ref(Ident.empty): ref(Ident.tbl(inline_type))
);

let mod_has_inlineable_wasm = ref(false);

let rec get_inlineable_wasm_tbl = lst =>
  switch (lst) {
  | [] => raise(Not_found)
  | [InlineableWasmTable(t), ..._] => t
  | [_, ...tl] => get_inlineable_wasm_tbl(tl)
  };

let set_inlineable_wasm = (id, inline_type) =>
  inline_wasm_tbl := Ident.add(id, inline_type, inline_wasm_tbl^);

let get_inline_wasm_type = (id: Ident.t): inline_type =>
  Ident.find_same(id, inline_wasm_tbl^);

let has_inline_wasm_type = (id: Ident.t): bool =>
  Ident.find_same_opt(id, inline_wasm_tbl^) |> Option.is_some;

let get_primitive = prim =>
  Option.bind(prim, prim => {
    Translprim.(
      switch (PrimMap.find_opt(prim_map, prim)) {
      | Some(PrimitiveN(prim)) => Some(WasmPrimN(prim))
      | _ => None
      }
    )
  });

let get_primitive_memory = id => {
  let prim =
    switch (id) {
    | "copy" when Config.bulk_memory^ => Some("@wasm.memory_copy")
    | "fill" when Config.bulk_memory^ => Some("@wasm.memory_fill")
    | _ => None
    };

  get_primitive(prim);
};

let analyze = ({imports, body, analyses}) => {
  inline_wasm_tbl := Ident.empty;
  mod_has_inlineable_wasm := false;
  let process_import = ({imp_use_id, imp_desc}) => {
    switch (imp_desc) {
    | GrainValue("runtime/unsafe/memory", name) =>
      mod_has_inlineable_wasm := true;
      switch (get_primitive_memory(name)) {
      | Some(prim) => set_inlineable_wasm(imp_use_id, prim)
      | None => ()
      };
    | GrainValue(_)
    | WasmFunction(_)
    | WasmValue(_)
    | JSFunction(_) => ()
    };
  };
  List.iter(process_import, imports);
  analyses := [InlineableWasmTable(inline_wasm_tbl^), ...analyses^];
};
