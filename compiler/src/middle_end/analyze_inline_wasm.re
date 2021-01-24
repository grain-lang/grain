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
      ("ofGrain", "@wasm.ofGrain"),
      ("toGrain", "@wasm.toGrain"),
      ("load", "@wasm.load_int32"),
      ("store", "@wasm.store_int32"),
      ("clz", "@wasm.clz_int32"),
      ("ctz", "@wasm.ctz_int32"),
      ("popcnt", "@wasm.popcnt_int32"),
      ("eqz", "@wasm.eq_z_int32"),
      ("add", "@wasm.add_int32"),
      ("sub", "@wasm.sub_int32"),
      ("mul", "@wasm.mul_int32"),
      ("divS", "@wasm.div_s_int32"),
      ("divU", "@wasm.div_u_int32"),
      ("remS", "@wasm.rem_s_int32"),
      ("remU", "@wasm.rem_u_int32"),
      ("and", "@wasm.and_int32"),
      ("or", "@wasm.or_int32"),
      ("xor", "@wasm.xor_int32"),
      ("shl", "@wasm.shl_int32"),
      ("shrU", "@wasm.shr_u_int32"),
      ("shrS", "@wasm.shr_s_int32"),
      ("rotl", "@wasm.rot_l_int32"),
      ("rotr", "@wasm.rot_r_int32"),
      ("eq", "@wasm.eq_int32"),
      ("ne", "@wasm.ne_int32"),
      ("ltS", "@wasm.lt_s_int32"),
      ("ltU", "@wasm.lt_u_int32"),
      ("leS", "@wasm.le_s_int32"),
      ("leU", "@wasm.le_u_int32"),
      ("gtS", "@wasm.gt_s_int32"),
      ("gtU", "@wasm.gt_u_int32"),
      ("geS", "@wasm.ge_s_int32"),
      ("geU", "@wasm.ge_u_int32"),
      ("wrapI64", "@wasm.wrap_int64"),
      ("truncF32S", "@wasm.trunc_s_float32_to_int32"),
      ("truncF32U", "@wasm.trunc_u_float32_to_int32"),
      ("truncF64S", "@wasm.trunc_s_float64_to_int32"),
      ("truncF64U", "@wasm.trunc_u_float64_to_int32"),
      ("truncF64U", "@wasm.trunc_u_float64_to_int32"),
      ("reinterpretF32", "@wasm.reinterpret_float32"),
      ("extendS8", "@wasm.extend_s8_int32"),
      ("extendS16", "@wasm.extend_s16_int32"),
    ]),
  );

let primitive_map_i64 =
  StringHash.of_seq(
    List.to_seq([
      ("load", "@wasm.load_int64"),
      ("store", "@wasm.store_int64"),
      ("clz", "@wasm.clz_int64"),
      ("ctz", "@wasm.ctz_int64"),
      ("popcnt", "@wasm.popcnt_int64"),
      ("eqz", "@wasm.eq_z_int64"),
      ("add", "@wasm.add_int64"),
      ("sub", "@wasm.sub_int64"),
      ("mul", "@wasm.mul_int64"),
      ("divS", "@wasm.div_s_int64"),
      ("divU", "@wasm.div_u_int64"),
      ("remS", "@wasm.rem_s_int64"),
      ("remU", "@wasm.rem_u_int64"),
      ("and", "@wasm.and_int64"),
      ("or", "@wasm.or_int64"),
      ("xor", "@wasm.xor_int64"),
      ("shl", "@wasm.shl_int64"),
      ("shrU", "@wasm.shr_u_int64"),
      ("shrS", "@wasm.shr_s_int64"),
      ("rotl", "@wasm.rot_l_int64"),
      ("rotr", "@wasm.rot_r_int64"),
      ("eq", "@wasm.eq_int64"),
      ("ne", "@wasm.ne_int64"),
      ("ltS", "@wasm.lt_s_int64"),
      ("ltU", "@wasm.lt_u_int64"),
      ("leS", "@wasm.le_s_int64"),
      ("leU", "@wasm.le_u_int64"),
      ("gtS", "@wasm.gt_s_int64"),
      ("gtU", "@wasm.gt_u_int64"),
      ("geS", "@wasm.ge_s_int64"),
      ("geU", "@wasm.ge_u_int64"),
      ("extendI32S", "@wasm.extend_s_int32"),
      ("extendI32U", "@wasm.extend_u_int32"),
      ("truncF32S", "@wasm.trunc_s_float32_to_int64"),
      ("truncF32U", "@wasm.trunc_u_float32_to_int64"),
      ("truncF64S", "@wasm.trunc_s_float64_to_int64"),
      ("truncF64U", "@wasm.trunc_u_float64_to_int64"),
      ("reinterpretF64", "@wasm.reinterpret_float64"),
      ("extendS8", "@wasm.extend_s8_int64"),
      ("extendS16", "@wasm.extend_s16_int64"),
      ("extendS32", "@wasm.extend_s16_int64"),
    ]),
  );

let get_primitive = (primitive_map, id) => {
  Translprim.(
    switch (StringHash.find_opt(primitive_map, id)) {
    | Some(prim) =>
      switch (PrimMap.find_opt(prim_map, prim)) {
      | Some(Primitive1(prim)) => Some(WasmPrim1(prim))
      | Some(Primitive2(prim)) => Some(WasmPrim2(prim))
      | Some(PrimitiveN(prim)) => Some(WasmPrimN(prim))
      | None => None
      }
    | None => None
    }
  );
};

let get_primitive_i32 = get_primitive(primitive_map_i32);
let get_primitive_i64 = get_primitive(primitive_map_i64);

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
