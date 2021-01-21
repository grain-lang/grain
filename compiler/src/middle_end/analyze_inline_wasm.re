open Anftree;
open Anf_iterator;
open Grain_typed;

type inline_type =
  | WasmPrim1(prim1)
  | WasmPrim2(prim2)
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

module StringHash =
  Hashtbl.Make({
    type t = string;
    let hash = x => Hashtbl.hash(x);
    let equal = (a, b) => String.compare(a, b) === 0;
  });

let primitive_map_i32 =
  StringHash.of_seq(
    List.to_seq([
      ("ofGrain", WasmPrim1(WasmOfGrain)),
      ("toGrain", WasmPrim1(WasmToGrain)),
      ("load", WasmPrim2(WasmLoadI32)),
      ("store", WasmPrimN(WasmStoreI32)),
      ("clz", WasmPrim1(WasmUnaryI32({op: "clz_i32", boolean: false}))),
      ("ctz", WasmPrim1(WasmUnaryI32({op: "ctz_i32", boolean: false}))),
      (
        "popcnt",
        WasmPrim1(WasmUnaryI32({op: "popcnt_i32", boolean: false})),
      ),
      ("eqz", WasmPrim1(WasmUnaryI32({op: "eq_z_i32", boolean: true}))),
      ("add", WasmPrim2(WasmBinaryI32({op: "add_i32", boolean: false}))),
      ("sub", WasmPrim2(WasmBinaryI32({op: "sub_i32", boolean: false}))),
      ("mul", WasmPrim2(WasmBinaryI32({op: "mul_i32", boolean: false}))),
      ("divS", WasmPrim2(WasmBinaryI32({op: "div_s_i32", boolean: false}))),
      ("divU", WasmPrim2(WasmBinaryI32({op: "div_u_i32", boolean: false}))),
      ("remS", WasmPrim2(WasmBinaryI32({op: "rem_s_i32", boolean: false}))),
      ("remU", WasmPrim2(WasmBinaryI32({op: "rem_u_i32", boolean: false}))),
      ("and", WasmPrim2(WasmBinaryI32({op: "and_i32", boolean: false}))),
      ("or", WasmPrim2(WasmBinaryI32({op: "or_i32", boolean: false}))),
      ("xor", WasmPrim2(WasmBinaryI32({op: "xor_i32", boolean: false}))),
      ("shl", WasmPrim2(WasmBinaryI32({op: "shl_i32", boolean: false}))),
      ("shrU", WasmPrim2(WasmBinaryI32({op: "shr_u_i32", boolean: false}))),
      ("shrS", WasmPrim2(WasmBinaryI32({op: "shr_s_i32", boolean: false}))),
      ("rotl", WasmPrim2(WasmBinaryI32({op: "rot_l_i32", boolean: false}))),
      ("rotr", WasmPrim2(WasmBinaryI32({op: "rot_r_i32", boolean: false}))),
      ("eq", WasmPrim2(WasmBinaryI32({op: "eq_i32", boolean: true}))),
      ("ne", WasmPrim2(WasmBinaryI32({op: "ne_i32", boolean: true}))),
      ("ltS", WasmPrim2(WasmBinaryI32({op: "lt_s_i32", boolean: true}))),
      ("ltU", WasmPrim2(WasmBinaryI32({op: "lt_u_i32", boolean: true}))),
      ("leS", WasmPrim2(WasmBinaryI32({op: "le_s_i32", boolean: true}))),
      ("leU", WasmPrim2(WasmBinaryI32({op: "le_u_i32", boolean: true}))),
      ("gtS", WasmPrim2(WasmBinaryI32({op: "gt_s_i32", boolean: true}))),
      ("gtU", WasmPrim2(WasmBinaryI32({op: "gt_u_i32", boolean: true}))),
      ("geS", WasmPrim2(WasmBinaryI32({op: "ge_s_i32", boolean: true}))),
      ("geU", WasmPrim2(WasmBinaryI32({op: "ge_u_i32", boolean: true}))),
    ]),
  );

let primitive_map_i64 =
  StringHash.of_seq(
    List.to_seq([
      ("load", WasmPrim2(WasmLoadI64)),
      ("store", WasmPrimN(WasmStoreI64)),
      ("clz", WasmPrim1(WasmUnaryI64({op: "clz_i64", boolean: false}))),
      ("ctz", WasmPrim1(WasmUnaryI64({op: "ctz_i64", boolean: false}))),
      (
        "popcnt",
        WasmPrim1(WasmUnaryI64({op: "popcnt_i64", boolean: false})),
      ),
      ("eqz", WasmPrim1(WasmUnaryI64({op: "eq_z_i64", boolean: true}))),
      ("add", WasmPrim2(WasmBinaryI64({op: "add_i64", boolean: false}))),
      ("sub", WasmPrim2(WasmBinaryI64({op: "sub_i64", boolean: false}))),
      ("mul", WasmPrim2(WasmBinaryI64({op: "mul_i64", boolean: false}))),
      ("divS", WasmPrim2(WasmBinaryI64({op: "div_s_i64", boolean: false}))),
      ("divU", WasmPrim2(WasmBinaryI64({op: "div_u_i64", boolean: false}))),
      ("remS", WasmPrim2(WasmBinaryI64({op: "rem_s_i64", boolean: false}))),
      ("remU", WasmPrim2(WasmBinaryI64({op: "rem_u_i64", boolean: false}))),
      ("and", WasmPrim2(WasmBinaryI64({op: "and_i64", boolean: false}))),
      ("or", WasmPrim2(WasmBinaryI64({op: "or_i64", boolean: false}))),
      ("xor", WasmPrim2(WasmBinaryI64({op: "xor_i64", boolean: false}))),
      ("shl", WasmPrim2(WasmBinaryI64({op: "shl_i64", boolean: false}))),
      ("shrU", WasmPrim2(WasmBinaryI64({op: "shr_u_i64", boolean: false}))),
      ("shrS", WasmPrim2(WasmBinaryI64({op: "shr_s_i64", boolean: false}))),
      ("rotl", WasmPrim2(WasmBinaryI64({op: "rot_l_i64", boolean: false}))),
      ("rotr", WasmPrim2(WasmBinaryI64({op: "rot_r_i64", boolean: false}))),
      ("eq", WasmPrim2(WasmBinaryI64({op: "eq_i64", boolean: true}))),
      ("ne", WasmPrim2(WasmBinaryI64({op: "ne_i64", boolean: true}))),
      ("ltS", WasmPrim2(WasmBinaryI64({op: "lt_s_i64", boolean: true}))),
      ("ltU", WasmPrim2(WasmBinaryI64({op: "lt_u_i64", boolean: true}))),
      ("leS", WasmPrim2(WasmBinaryI64({op: "le_s_i64", boolean: true}))),
      ("leU", WasmPrim2(WasmBinaryI64({op: "le_u_i64", boolean: true}))),
      ("gtS", WasmPrim2(WasmBinaryI64({op: "gt_s_i64", boolean: true}))),
      ("gtU", WasmPrim2(WasmBinaryI64({op: "gt_u_i64", boolean: true}))),
      ("geS", WasmPrim2(WasmBinaryI64({op: "ge_s_i64", boolean: true}))),
      ("geU", WasmPrim2(WasmBinaryI64({op: "ge_u_i64", boolean: true}))),
    ]),
  );

let get_primitive_i32 = id => StringHash.find_opt(primitive_map_i32, id);
let get_primitive_i64 = id => StringHash.find_opt(primitive_map_i64, id);

let analyze = ({imports, body, analyses}) => {
  inline_wasm_tbl := Ident.empty;
  mod_has_inlineable_wasm := false;
  let process_import = ({imp_use_id, imp_desc}) => {
    switch (imp_desc) {
    | GrainValue("wasmi32", name) =>
      mod_has_inlineable_wasm := true;
      switch (get_primitive_i32(name)) {
      | Some(prim) => set_inlineable_wasm(imp_use_id, prim)
      | None => ()
      };
    | GrainValue("wasmi64", name) =>
      mod_has_inlineable_wasm := true;
      switch (get_primitive_i64(name)) {
      | Some(prim) => set_inlineable_wasm(imp_use_id, prim)
      | None => ()
      };
    | GrainValue(_)
    | WasmFunction(_)
    | JSFunction(_) => ()
    };
  };
  List.iter(process_import, imports);
  analyses := [InlineableWasmTable(inline_wasm_tbl^), ...analyses^];
};
