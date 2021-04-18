open Anftree;
open Anf_iterator;
open Grain_typed;
open Grain_utils;

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

let get_primitive = prim =>
  Option.bind(prim, prim => {
    Translprim.(
      switch (PrimMap.find_opt(prim_map, prim)) {
      | Some(Primitive1(prim)) => Some(WasmPrim1(prim))
      | Some(Primitive2(prim)) => Some(WasmPrim2(prim))
      | Some(PrimitiveN(prim)) => Some(WasmPrimN(prim))
      | None => None
      }
    )
  });

let get_primitive_i32 = id => {
  let prim =
    switch (id) {
    | "fromGrain" => Some("@wasm.fromGrain")
    | "toGrain" => Some("@wasm.toGrain")
    | "load" => Some("@wasm.load_int32")
    | "load8S" => Some("@wasm.load_8_s_int32")
    | "load8U" => Some("@wasm.load_8_u_int32")
    | "load16S" => Some("@wasm.load_16_s_int32")
    | "load16U" => Some("@wasm.load_16_u_int32")
    | "store" => Some("@wasm.store_int32")
    | "store8" => Some("@wasm.store_8_int32")
    | "store16" => Some("@wasm.store_16_int32")
    | "clz" => Some("@wasm.clz_int32")
    | "ctz" => Some("@wasm.ctz_int32")
    | "popcnt" => Some("@wasm.popcnt_int32")
    | "eqz" => Some("@wasm.eq_z_int32")
    | "add" => Some("@wasm.add_int32")
    | "sub" => Some("@wasm.sub_int32")
    | "mul" => Some("@wasm.mul_int32")
    | "divS" => Some("@wasm.div_s_int32")
    | "divU" => Some("@wasm.div_u_int32")
    | "remS" => Some("@wasm.rem_s_int32")
    | "remU" => Some("@wasm.rem_u_int32")
    | "and" => Some("@wasm.and_int32")
    | "or" => Some("@wasm.or_int32")
    | "xor" => Some("@wasm.xor_int32")
    | "shl" => Some("@wasm.shl_int32")
    | "shrU" => Some("@wasm.shr_u_int32")
    | "shrS" => Some("@wasm.shr_s_int32")
    | "rotl" => Some("@wasm.rot_l_int32")
    | "rotr" => Some("@wasm.rot_r_int32")
    | "eq" => Some("@wasm.eq_int32")
    | "ne" => Some("@wasm.ne_int32")
    | "ltS" => Some("@wasm.lt_s_int32")
    | "ltU" => Some("@wasm.lt_u_int32")
    | "leS" => Some("@wasm.le_s_int32")
    | "leU" => Some("@wasm.le_u_int32")
    | "gtS" => Some("@wasm.gt_s_int32")
    | "gtU" => Some("@wasm.gt_u_int32")
    | "geS" => Some("@wasm.ge_s_int32")
    | "geU" => Some("@wasm.ge_u_int32")
    | "wrapI64" => Some("@wasm.wrap_int64")
    | "truncF32S" => Some("@wasm.trunc_s_float32_to_int32")
    | "truncF32U" => Some("@wasm.trunc_u_float32_to_int32")
    | "truncF64S" => Some("@wasm.trunc_s_float64_to_int32")
    | "truncF64U" => Some("@wasm.trunc_u_float64_to_int32")
    | "reinterpretF32" => Some("@wasm.reinterpret_float32")
    | "extendS8" => Some("@wasm.extend_s8_int32")
    | "extendS16" => Some("@wasm.extend_s16_int32")
    | _ => None
    };

  get_primitive(prim);
};
let get_primitive_i64 = id => {
  let prim =
    switch (id) {
    | "load" => Some("@wasm.load_int64")
    | "load8S" => Some("@wasm.load_8_s_int64")
    | "load8U" => Some("@wasm.load_8_u_int64")
    | "load16S" => Some("@wasm.load_16_s_int64")
    | "load16U" => Some("@wasm.load_16_u_int64")
    | "load32S" => Some("@wasm.load_32_s_int64")
    | "load32U" => Some("@wasm.load_32_u_int64")
    | "store" => Some("@wasm.store_int64")
    | "store8" => Some("@wasm.store_8_int64")
    | "store16" => Some("@wasm.store_16_int64")
    | "store32" => Some("@wasm.store_32_int64")
    | "clz" => Some("@wasm.clz_int64")
    | "ctz" => Some("@wasm.ctz_int64")
    | "popcnt" => Some("@wasm.popcnt_int64")
    | "eqz" => Some("@wasm.eq_z_int64")
    | "add" => Some("@wasm.add_int64")
    | "sub" => Some("@wasm.sub_int64")
    | "mul" => Some("@wasm.mul_int64")
    | "divS" => Some("@wasm.div_s_int64")
    | "divU" => Some("@wasm.div_u_int64")
    | "remS" => Some("@wasm.rem_s_int64")
    | "remU" => Some("@wasm.rem_u_int64")
    | "and" => Some("@wasm.and_int64")
    | "or" => Some("@wasm.or_int64")
    | "xor" => Some("@wasm.xor_int64")
    | "shl" => Some("@wasm.shl_int64")
    | "shrU" => Some("@wasm.shr_u_int64")
    | "shrS" => Some("@wasm.shr_s_int64")
    | "rotl" => Some("@wasm.rot_l_int64")
    | "rotr" => Some("@wasm.rot_r_int64")
    | "eq" => Some("@wasm.eq_int64")
    | "ne" => Some("@wasm.ne_int64")
    | "ltS" => Some("@wasm.lt_s_int64")
    | "ltU" => Some("@wasm.lt_u_int64")
    | "leS" => Some("@wasm.le_s_int64")
    | "leU" => Some("@wasm.le_u_int64")
    | "gtS" => Some("@wasm.gt_s_int64")
    | "gtU" => Some("@wasm.gt_u_int64")
    | "geS" => Some("@wasm.ge_s_int64")
    | "geU" => Some("@wasm.ge_u_int64")
    | "extendI32S" => Some("@wasm.extend_s_int32")
    | "extendI32U" => Some("@wasm.extend_u_int32")
    | "truncF32S" => Some("@wasm.trunc_s_float32_to_int64")
    | "truncF32U" => Some("@wasm.trunc_u_float32_to_int64")
    | "truncF64S" => Some("@wasm.trunc_s_float64_to_int64")
    | "truncF64U" => Some("@wasm.trunc_u_float64_to_int64")
    | "reinterpretF64" => Some("@wasm.reinterpret_float64")
    | "extendS8" => Some("@wasm.extend_s8_int64")
    | "extendS16" => Some("@wasm.extend_s16_int64")
    | "extendS32" => Some("@wasm.extend_s32_int64")
    | _ => None
    };

  get_primitive(prim);
};
let get_primitive_f32 = id => {
  let prim =
    switch (id) {
    | "load" => Some("@wasm.load_float32")
    | "store" => Some("@wasm.store_float32")
    | "neg" => Some("@wasm.neg_float32")
    | "abs" => Some("@wasm.abs_float32")
    | "ceil" => Some("@wasm.ceil_float32")
    | "floor" => Some("@wasm.floor_float32")
    | "trunc" => Some("@wasm.trunc_float32")
    | "nearest" => Some("@wasm.nearest_float32")
    | "sqrt" => Some("@wasm.sqrt_float32")
    | "add" => Some("@wasm.add_float32")
    | "sub" => Some("@wasm.sub_float32")
    | "mul" => Some("@wasm.mul_float32")
    | "div" => Some("@wasm.div_float32")
    | "copySign" => Some("@wasm.copy_sign_float32")
    | "min" => Some("@wasm.min_float32")
    | "max" => Some("@wasm.max_float32")
    | "eq" => Some("@wasm.eq_float32")
    | "ne" => Some("@wasm.ne_float32")
    | "lt" => Some("@wasm.lt_float32")
    | "le" => Some("@wasm.le_float32")
    | "gt" => Some("@wasm.gt_float32")
    | "ge" => Some("@wasm.ge_float32")
    | "reinterpretI32" => Some("@wasm.reinterpret_int32")
    | "convertI32S" => Some("@wasm.convert_s_int32_to_float32")
    | "convertI32U" => Some("@wasm.convert_u_int32_to_float32")
    | "convertI64S" => Some("@wasm.convert_s_int64_to_float32")
    | "convertI64U" => Some("@wasm.convert_u_int64_to_float32")
    | "demoteF64" => Some("@wasm.demote_float64")
    | _ => None
    };

  get_primitive(prim);
};
let get_primitive_f64 = id => {
  let prim =
    switch (id) {
    | "load" => Some("@wasm.load_float64")
    | "store" => Some("@wasm.store_float64")
    | "neg" => Some("@wasm.neg_float64")
    | "abs" => Some("@wasm.abs_float64")
    | "ceil" => Some("@wasm.ceil_float64")
    | "floor" => Some("@wasm.floor_float64")
    | "trunc" => Some("@wasm.trunc_float64")
    | "nearest" => Some("@wasm.nearest_float64")
    | "sqrt" => Some("@wasm.sqrt_float64")
    | "add" => Some("@wasm.add_float64")
    | "sub" => Some("@wasm.sub_float64")
    | "mul" => Some("@wasm.mul_float64")
    | "div" => Some("@wasm.div_float64")
    | "copySign" => Some("@wasm.copy_sign_float64")
    | "min" => Some("@wasm.min_float64")
    | "max" => Some("@wasm.max_float64")
    | "eq" => Some("@wasm.eq_float64")
    | "ne" => Some("@wasm.ne_float64")
    | "lt" => Some("@wasm.lt_float64")
    | "le" => Some("@wasm.le_float64")
    | "gt" => Some("@wasm.gt_float64")
    | "ge" => Some("@wasm.ge_float64")
    | "reinterpretI64" => Some("@wasm.reinterpret_int64")
    | "convertI32S" => Some("@wasm.convert_s_int32_to_float64")
    | "convertI32U" => Some("@wasm.convert_u_int32_to_float64")
    | "convertI64S" => Some("@wasm.convert_s_int64_to_float64")
    | "convertI64U" => Some("@wasm.convert_u_int64_to_float64")
    | "promoteF32" => Some("@wasm.promote_float32")
    | _ => None
    };

  get_primitive(prim);
};
let get_primitive_memory = id => {
  let prim =
    switch (id) {
    | "grow" => Some("@wasm.memory_grow")
    | "size" => Some("@wasm.memory_size")
    | "compare" => Some("@wasm.memory_compare")
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
