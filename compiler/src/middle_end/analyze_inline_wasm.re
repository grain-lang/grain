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
      (
        "clz",
        WasmPrim1(WasmUnaryI32({wasm_op: Op_clz_int32, boolean: false})),
      ),
      (
        "ctz",
        WasmPrim1(WasmUnaryI32({wasm_op: Op_ctz_int32, boolean: false})),
      ),
      (
        "popcnt",
        WasmPrim1(WasmUnaryI32({wasm_op: Op_popcnt_int32, boolean: false})),
      ),
      (
        "eqz",
        WasmPrim1(WasmUnaryI32({wasm_op: Op_eq_z_int32, boolean: true})),
      ),
      (
        "add",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_add_int32, boolean: false})),
      ),
      (
        "sub",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_sub_int32, boolean: false})),
      ),
      (
        "mul",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_mul_int32, boolean: false})),
      ),
      (
        "divS",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_div_s_int32, boolean: false})),
      ),
      (
        "divU",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_div_u_int32, boolean: false})),
      ),
      (
        "remS",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_rem_s_int32, boolean: false})),
      ),
      (
        "remU",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_rem_u_int32, boolean: false})),
      ),
      (
        "and",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_and_int32, boolean: false})),
      ),
      (
        "or",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_or_int32, boolean: false})),
      ),
      (
        "xor",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_xor_int32, boolean: false})),
      ),
      (
        "shl",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_shl_int32, boolean: false})),
      ),
      (
        "shrU",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_shr_u_int32, boolean: false})),
      ),
      (
        "shrS",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_shr_s_int32, boolean: false})),
      ),
      (
        "rotl",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_rot_l_int32, boolean: false})),
      ),
      (
        "rotr",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_rot_r_int32, boolean: false})),
      ),
      (
        "eq",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_eq_int32, boolean: true})),
      ),
      (
        "ne",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_ne_int32, boolean: true})),
      ),
      (
        "ltS",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_lt_s_int32, boolean: true})),
      ),
      (
        "ltU",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_lt_u_int32, boolean: true})),
      ),
      (
        "leS",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_le_s_int32, boolean: true})),
      ),
      (
        "leU",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_le_u_int32, boolean: true})),
      ),
      (
        "gtS",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_gt_s_int32, boolean: true})),
      ),
      (
        "gtU",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_gt_u_int32, boolean: true})),
      ),
      (
        "geS",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_ge_s_int32, boolean: true})),
      ),
      (
        "geU",
        WasmPrim2(WasmBinaryI32({wasm_op: Op_ge_u_int32, boolean: true})),
      ),
    ]),
  );

let primitive_map_i64 =
  StringHash.of_seq(
    List.to_seq([
      ("load", WasmPrim2(WasmLoadI64)),
      ("store", WasmPrimN(WasmStoreI64)),
      (
        "clz",
        WasmPrim1(WasmUnaryI64({wasm_op: Op_clz_int64, boolean: false})),
      ),
      (
        "ctz",
        WasmPrim1(WasmUnaryI64({wasm_op: Op_ctz_int64, boolean: false})),
      ),
      (
        "popcnt",
        WasmPrim1(WasmUnaryI64({wasm_op: Op_popcnt_int64, boolean: false})),
      ),
      (
        "eqz",
        WasmPrim1(WasmUnaryI64({wasm_op: Op_eq_z_int64, boolean: true})),
      ),
      (
        "add",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_add_int64, boolean: false})),
      ),
      (
        "sub",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_sub_int64, boolean: false})),
      ),
      (
        "mul",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_mul_int64, boolean: false})),
      ),
      (
        "divS",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_div_s_int64, boolean: false})),
      ),
      (
        "divU",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_div_u_int64, boolean: false})),
      ),
      (
        "remS",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_rem_s_int64, boolean: false})),
      ),
      (
        "remU",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_rem_u_int64, boolean: false})),
      ),
      (
        "and",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_and_int64, boolean: false})),
      ),
      (
        "or",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_or_int64, boolean: false})),
      ),
      (
        "xor",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_xor_int64, boolean: false})),
      ),
      (
        "shl",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_shl_int64, boolean: false})),
      ),
      (
        "shrU",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_shr_u_int64, boolean: false})),
      ),
      (
        "shrS",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_shr_s_int64, boolean: false})),
      ),
      (
        "rotl",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_rot_l_int64, boolean: false})),
      ),
      (
        "rotr",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_rot_r_int64, boolean: false})),
      ),
      (
        "eq",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_eq_int64, boolean: true})),
      ),
      (
        "ne",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_ne_int64, boolean: true})),
      ),
      (
        "ltS",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_lt_s_int64, boolean: true})),
      ),
      (
        "ltU",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_lt_u_int64, boolean: true})),
      ),
      (
        "leS",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_le_s_int64, boolean: true})),
      ),
      (
        "leU",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_le_u_int64, boolean: true})),
      ),
      (
        "gtS",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_gt_s_int64, boolean: true})),
      ),
      (
        "gtU",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_gt_u_int64, boolean: true})),
      ),
      (
        "geS",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_ge_s_int64, boolean: true})),
      ),
      (
        "geU",
        WasmPrim2(WasmBinaryI64({wasm_op: Op_ge_u_int64, boolean: true})),
      ),
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
