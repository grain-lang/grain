open Grain_parsing;
open Ast_helper;
open Typedtree;
open Parsetree;

type primitive =
  | Primitive1(prim1)
  | Primitive2(prim2)
  | PrimitiveN(primn);

module PrimMap =
  Hashtbl.Make({
    type t = string;
    let hash = x => Hashtbl.hash(x);
    let equal = (a, b) => String.compare(a, b) === 0;
  });

let default_loc = default_loc_src^();

let mkident = name =>
  Exp.ident(Location.mkloc(Identifier.IdentName(name), default_loc));
let mkpatvar = name => {
  ppat_desc: PPatVar(Location.mkloc(name, default_loc)),
  ppat_loc: default_loc,
};

let id_a = mkident("a");
let id_b = mkident("b");
let id_c = mkident("c");
let pat_a = mkpatvar("a");
let pat_b = mkpatvar("b");
let pat_c = mkpatvar("c");

let prim_map =
  PrimMap.of_seq(
    List.to_seq([
      ("@incr", Primitive1(Incr)),
      ("@decr", Primitive1(Decr)),
      ("@not", Primitive1(Not)),
      ("@box", Primitive1(Box)),
      ("@unbox", Primitive1(Unbox)),
      ("@ignore", Primitive1(Ignore)),
      ("@assert", Primitive1(Assert)),
      ("@fail", Primitive1(FailWith)),
      ("@plus", Primitive2(Plus)),
      ("@minus", Primitive2(Minus)),
      ("@times", Primitive2(Times)),
      ("@divides", Primitive2(Divide)),
      ("@modulo", Primitive2(Mod)),
      ("@less", Primitive2(Less)),
      ("@greater", Primitive2(Greater)),
      ("@lesseq", Primitive2(LessEq)),
      ("@greatereq", Primitive2(GreaterEq)),
      ("@is", Primitive2(Is)),
      ("@eq", Primitive2(Eq)),
      ("@and", Primitive2(And)),
      ("@or", Primitive2(Or)),
      ("@array.length", Primitive1(ArrayLength)),
      ("@array.make", Primitive2(ArrayMake)),
      ("@array.init", Primitive2(ArrayInit)),
      ("@int64.fromNumber", Primitive1(Int64FromNumber)),
      ("@int64.toNumber", Primitive1(Int64ToNumber)),
      ("@int64.lnot", Primitive1(Int64Lnot)),
      ("@int64.land", Primitive2(Int64Land)),
      ("@int64.lor", Primitive2(Int64Lor)),
      ("@int64.lxor", Primitive2(Int64Lxor)),
      ("@int64.lsl", Primitive2(Int64Lsl)),
      ("@int64.lsr", Primitive2(Int64Lsr)),
      ("@int64.asr", Primitive2(Int64Asr)),
      ("@int64.gt", Primitive2(Int64Gt)),
      ("@int64.gte", Primitive2(Int64Gte)),
      ("@int64.lt", Primitive2(Int64Lt)),
      ("@int64.lte", Primitive2(Int64Lte)),
      ("@int32.toNumber", Primitive1(Int32ToNumber)),
      ("@float32.toNumber", Primitive1(Float32ToNumber)),
      ("@float64.toNumber", Primitive1(Float64ToNumber)),
      ("@wasm.load_int32", Primitive2(WasmLoadI32({sz: 4, signed: false}))),
      (
        "@wasm.load_8_s_int32",
        Primitive2(WasmLoadI32({sz: 1, signed: true})),
      ),
      (
        "@wasm.load_8_u_int32",
        Primitive2(WasmLoadI32({sz: 1, signed: false})),
      ),
      (
        "@wasm.load_16_s_int32",
        Primitive2(WasmLoadI32({sz: 2, signed: true})),
      ),
      (
        "@wasm.load_16_u_int32",
        Primitive2(WasmLoadI32({sz: 2, signed: false})),
      ),
      ("@wasm.store_int32", PrimitiveN(WasmStoreI32({sz: 4}))),
      ("@wasm.store_8_int32", PrimitiveN(WasmStoreI32({sz: 1}))),
      ("@wasm.store_16_int32", PrimitiveN(WasmStoreI32({sz: 2}))),
      ("@wasm.load_int64", Primitive2(WasmLoadI64({sz: 8, signed: false}))),
      (
        "@wasm.load_8_s_int64",
        Primitive2(WasmLoadI64({sz: 1, signed: true})),
      ),
      (
        "@wasm.load_8_u_int64",
        Primitive2(WasmLoadI64({sz: 1, signed: false})),
      ),
      (
        "@wasm.load_16_s_int64",
        Primitive2(WasmLoadI64({sz: 2, signed: true})),
      ),
      (
        "@wasm.load_16_u_int64",
        Primitive2(WasmLoadI64({sz: 2, signed: false})),
      ),
      (
        "@wasm.load_32_s_int64",
        Primitive2(WasmLoadI64({sz: 4, signed: true})),
      ),
      (
        "@wasm.load_32_u_int64",
        Primitive2(WasmLoadI64({sz: 4, signed: false})),
      ),
      ("@wasm.store_int64", PrimitiveN(WasmStoreI64({sz: 8}))),
      ("@wasm.store_8_int64", PrimitiveN(WasmStoreI32({sz: 1}))),
      ("@wasm.store_16_int64", PrimitiveN(WasmStoreI32({sz: 2}))),
      ("@wasm.store_32_int64", PrimitiveN(WasmStoreI32({sz: 4}))),
      (
        "@wasm.clz_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_clz_int32,
            arg_type: Wasm_int32,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.ctz_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_ctz_int32,
            arg_type: Wasm_int32,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.popcnt_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_popcnt_int32,
            arg_type: Wasm_int32,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.eq_z_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_eq_z_int32,
            arg_type: Wasm_int32,
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.add_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_add_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.sub_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_sub_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.mul_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_mul_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.div_s_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_div_s_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.div_u_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_div_u_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.rem_s_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_rem_s_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.rem_u_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_rem_u_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.and_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_and_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.or_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_or_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.xor_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_xor_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.shl_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_shl_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.shr_u_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_shr_u_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.shr_s_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_shr_s_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.rot_l_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_rot_l_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.rot_r_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_rot_r_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.eq_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_eq_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ne_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_ne_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.lt_s_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_lt_s_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.lt_u_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_lt_u_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.le_s_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_le_s_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.le_u_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_le_u_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.gt_s_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_gt_s_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.gt_u_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_gt_u_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ge_s_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_ge_s_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ge_u_int32",
        Primitive2(
          WasmBinaryI32({
            wasm_op: Op_ge_u_int32,
            arg_types: (Wasm_int32, Wasm_int32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.wrap_int64",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_wrap_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.trunc_s_float32_to_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_trunc_s_float32_to_int32,
            arg_type: Wasm_float32,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.trunc_u_float32_to_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_trunc_u_float32_to_int32,
            arg_type: Wasm_float32,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.trunc_s_float64_to_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_trunc_s_float64_to_int32,
            arg_type: Wasm_float64,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.trunc_u_float64_to_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_trunc_u_float64_to_int32,
            arg_type: Wasm_float64,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.trunc_u_float64_to_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_trunc_u_float64_to_int32,
            arg_type: Wasm_float64,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.reinterpret_float32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_reinterpret_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.extend_s8_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_extend_s8_int32,
            arg_type: Wasm_int32,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.extend_s16_int32",
        Primitive1(
          WasmUnaryI32({
            wasm_op: Op_extend_s16_int32,
            arg_type: Wasm_int32,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.clz_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_clz_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.ctz_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_ctz_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.popcnt_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_popcnt_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.eq_z_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_eq_z_int64,
            arg_type: Wasm_int64,
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.add_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_add_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.sub_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_sub_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.mul_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_mul_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.div_s_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_div_s_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.div_u_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_div_u_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.rem_s_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_rem_s_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.rem_u_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_rem_u_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.and_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_and_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.or_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_or_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.xor_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_xor_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.shl_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_shl_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.shr_u_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_shr_u_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.shr_s_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_shr_s_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.rot_l_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_rot_l_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.rot_r_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_rot_r_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.eq_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_eq_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ne_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_ne_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.lt_s_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_lt_s_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.lt_u_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_lt_u_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.le_s_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_le_s_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.le_u_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_le_u_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.gt_s_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_gt_s_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.gt_u_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_gt_u_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ge_s_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_ge_s_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ge_u_int64",
        Primitive2(
          WasmBinaryI64({
            wasm_op: Op_ge_u_int64,
            arg_types: (Wasm_int64, Wasm_int64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.extend_s_int32",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_extend_s_int32,
            arg_type: Wasm_int32,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.extend_u_int32",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_extend_u_int32,
            arg_type: Wasm_int32,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.trunc_s_float32_to_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_trunc_s_float32_to_int64,
            arg_type: Wasm_float32,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.trunc_u_float32_to_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_trunc_u_float32_to_int64,
            arg_type: Wasm_float32,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.trunc_s_float64_to_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_trunc_s_float64_to_int64,
            arg_type: Wasm_float64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.trunc_u_float64_to_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_trunc_u_float64_to_int64,
            arg_type: Wasm_float64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.reinterpret_float64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_reinterpret_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.extend_s8_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_extend_s8_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.extend_s16_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_extend_s16_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.extend_s16_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_extend_s16_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      ("@wasm.ofGrain", Primitive1(WasmOfGrain)),
      ("@wasm.toGrain", Primitive1(WasmToGrain)),
    ]),
  );

let transl_prim = (env, desc) => {
  let loc = desc.tvd_loc;

  let prim =
    try(PrimMap.find(prim_map, List.hd(desc.tvd_prim))) {
    | Not_found => failwith("This primitive does not exist.")
    };

  let diable_gc = [Location.mknoloc("disableGC")];

  let value =
    switch (prim) {
    | Primitive1((WasmUnaryI32(_) | WasmUnaryI64(_)) as p) =>
      Exp.lambda(
        ~loc,
        ~attributes=diable_gc,
        [pat_a],
        Exp.prim1(~loc, p, id_a),
      )
    | Primitive1(p) => Exp.lambda(~loc, [pat_a], Exp.prim1(~loc, p, id_a))
    | Primitive2(
        (WasmBinaryI32(_) | WasmBinaryI64(_) | WasmLoadI32(_) | WasmLoadI64(_)) as p,
      ) =>
      Exp.lambda(
        ~loc,
        ~attributes=diable_gc,
        [pat_a, pat_b],
        Exp.prim2(~loc, p, id_a, id_b),
      )
    | Primitive2(p) =>
      Exp.lambda(~loc, [pat_a, pat_b], Exp.prim2(~loc, p, id_a, id_b))
    | PrimitiveN((WasmStoreI32(_) | WasmStoreI64(_)) as p) =>
      Exp.lambda(
        ~loc,
        ~attributes=diable_gc,
        [pat_a, pat_b, pat_c],
        Exp.primn(~loc, p, [id_a, id_b, id_c]),
      )
    };

  let binds = [
    {
      pvb_pat: {
        ppat_desc: PPatVar(desc.tvd_name),
        ppat_loc: loc,
      },
      pvb_expr: value,
      pvb_loc: loc,
    },
  ];
  let mut_flag = desc.tvd_val.val_mutable ? Mutable : Immutable;
  Typecore.type_binding(env, Nonrecursive, mut_flag, binds, None);
};
