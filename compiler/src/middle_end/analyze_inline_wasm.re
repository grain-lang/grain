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
      ("fromGrain", "@wasm.fromGrain"),
      ("toGrain", "@wasm.toGrain"),
      ("load", "@wasm.load_int32"),
      ("load8S", "@wasm.load_8_s_int32"),
      ("load8U", "@wasm.load_8_u_int32"),
      ("load16S", "@wasm.load_16_s_int32"),
      ("load16U", "@wasm.load_16_u_int32"),
      ("store", "@wasm.store_int32"),
      ("store8", "@wasm.store_8_int32"),
      ("store16", "@wasm.store_16_int32"),
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
      ("load8S", "@wasm.load_8_s_int64"),
      ("load8U", "@wasm.load_8_u_int64"),
      ("load16S", "@wasm.load_16_s_int64"),
      ("load16U", "@wasm.load_16_u_int64"),
      ("load32S", "@wasm.load_32_s_int64"),
      ("load32U", "@wasm.load_32_u_int64"),
      ("store", "@wasm.store_int64"),
      ("store8", "@wasm.store_8_int64"),
      ("store16", "@wasm.store_16_int64"),
      ("store32", "@wasm.store_32_int64"),
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
      ("extendS32", "@wasm.extend_s32_int64"),
    ]),
  );

let primitive_map_f32 =
  StringHash.of_seq(
    List.to_seq([
      ("load", "@wasm.load_float32"),
      ("store", "@wasm.store_float32"),
      ("neg", "@wasm.neg_float32"),
      ("abs", "@wasm.abs_float32"),
      ("ceil", "@wasm.ceil_float32"),
      ("floor", "@wasm.floor_float32"),
      ("trunc", "@wasm.trunc_float32"),
      ("nearest", "@wasm.nearest_float32"),
      ("sqrt", "@wasm.sqrt_float32"),
      ("add", "@wasm.add_float32"),
      ("sub", "@wasm.sub_float32"),
      ("mul", "@wasm.mul_float32"),
      ("div", "@wasm.div_float32"),
      ("copySign", "@wasm.copy_sign_float32"),
      ("min", "@wasm.min_float32"),
      ("max", "@wasm.max_float32"),
      ("eq", "@wasm.eq_float32"),
      ("ne", "@wasm.ne_float32"),
      ("lt", "@wasm.lt_float32"),
      ("le", "@wasm.le_float32"),
      ("gt", "@wasm.gt_float32"),
      ("ge", "@wasm.ge_float32"),
      ("reinterpretI32", "@wasm.reinterpret_int32"),
      ("convertI32S", "@wasm.convert_s_int32_to_float32"),
      ("convertI32U", "@wasm.convert_u_int32_to_float32"),
      ("convertI64S", "@wasm.convert_s_int64_to_float32"),
      ("convertI64U", "@wasm.convert_u_int64_to_float32"),
      ("demoteF64", "@wasm.demote_float64"),
    ]),
  );

let primitive_map_f64 =
  StringHash.of_seq(
    List.to_seq([
      ("load", "@wasm.load_float64"),
      ("store", "@wasm.store_float64"),
      ("neg", "@wasm.neg_float64"),
      ("abs", "@wasm.abs_float64"),
      ("ceil", "@wasm.ceil_float64"),
      ("floor", "@wasm.floor_float64"),
      ("trunc", "@wasm.trunc_float64"),
      ("nearest", "@wasm.nearest_float64"),
      ("sqrt", "@wasm.sqrt_float64"),
      ("add", "@wasm.add_float64"),
      ("sub", "@wasm.sub_float64"),
      ("mul", "@wasm.mul_float64"),
      ("div", "@wasm.div_float64"),
      ("copySign", "@wasm.copy_sign_float64"),
      ("min", "@wasm.min_float64"),
      ("max", "@wasm.max_float64"),
      ("eq", "@wasm.eq_float64"),
      ("ne", "@wasm.ne_float64"),
      ("lt", "@wasm.lt_float64"),
      ("le", "@wasm.le_float64"),
      ("gt", "@wasm.gt_float64"),
      ("ge", "@wasm.ge_float64"),
      ("reinterpretI64", "@wasm.reinterpret_int64"),
      ("convertI32S", "@wasm.convert_s_int32_to_float64"),
      ("convertI32U", "@wasm.convert_u_int32_to_float64"),
      ("convertI64S", "@wasm.convert_s_int64_to_float64"),
      ("convertI64U", "@wasm.convert_u_int64_to_float64"),
      ("promoteF32", "@wasm.promote_float32"),
    ]),
  );

let primitive_map_memory =
  StringHash.of_seq(
    List.to_seq([
      ("copy", "@wasm.memory_copy"),
      ("fill", "@wasm.memory_fill"),
      ("grow", "@wasm.memory_grow"),
      ("size", "@wasm.memory_size"),
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
let get_primitive_f32 = get_primitive(primitive_map_f32);
let get_primitive_f64 = get_primitive(primitive_map_f64);
let get_primitive_memory = get_primitive(primitive_map_memory);

let analyze = ({imports, body, analyses}) => {
  inline_wasm_tbl := Ident.empty;
  mod_has_inlineable_wasm := false;
  let process_import = ({imp_use_id, imp_desc}) => {
    switch (imp_desc) {
    | GrainValue("runtime/unsafe/wasmi32", name) =>
      mod_has_inlineable_wasm := true;
      switch (get_primitive_i32(name)) {
      | Some(prim) => set_inlineable_wasm(imp_use_id, prim)
      | None => ()
      };
    | GrainValue("runtime/unsafe/wasmi64", name) =>
      mod_has_inlineable_wasm := true;
      switch (get_primitive_i64(name)) {
      | Some(prim) => set_inlineable_wasm(imp_use_id, prim)
      | None => ()
      };
    | GrainValue("runtime/unsafe/wasmf32", name) =>
      mod_has_inlineable_wasm := true;
      switch (get_primitive_f32(name)) {
      | Some(prim) => set_inlineable_wasm(imp_use_id, prim)
      | None => ()
      };
    | GrainValue("runtime/unsafe/wasmf64", name) =>
      mod_has_inlineable_wasm := true;
      switch (get_primitive_f64(name)) {
      | Some(prim) => set_inlineable_wasm(imp_use_id, prim)
      | None => ()
      };
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
