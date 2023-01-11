open Grain_parsing;
open Ast_helper;
open Typedtree;
open Parsetree;

type primitive_constant =
  | HeapBase
  | HeapStart
  | HeapTypeMetadata;

type primitive =
  | PrimitiveConstant(primitive_constant)
  | Primitive0(prim0)
  | Primitive1(prim1)
  | Primitive2(prim2)
  | PrimitiveN(primn);

module PrimMap =
  Hashtbl.Make({
    type t = string;
    let hash = x => Hashtbl.hash(x);
    let equal = (a, b) => String.compare(a, b) === 0;
  });

let default_loc = Location.dummy_loc;

let mkident = name =>
  Exp.ident(
    Location.mkloc(
      Identifier.IdentName(Location.mkloc(name, default_loc)),
      default_loc,
    ),
  );
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
      ("@heap.base", PrimitiveConstant(HeapBase)),
      ("@heap.start", PrimitiveConstant(HeapStart)),
      ("@heap.type_metadata", PrimitiveConstant(HeapTypeMetadata)),
      ("@allocate.int32", Primitive0(AllocateInt32)),
      ("@allocate.int64", Primitive0(AllocateInt64)),
      ("@allocate.float32", Primitive0(AllocateFloat32)),
      ("@allocate.float64", Primitive0(AllocateFloat64)),
      ("@allocate.rational", Primitive0(AllocateRational)),
      ("@allocate.array", Primitive1(AllocateArray)),
      ("@allocate.tuple", Primitive1(AllocateTuple)),
      ("@allocate.bytes", Primitive1(AllocateBytes)),
      ("@allocate.string", Primitive1(AllocateString)),
      ("@allocate.bigInt", Primitive1(AllocateBigInt)),
      ("@new.int32", Primitive1(NewInt32)),
      ("@new.int64", Primitive1(NewInt64)),
      ("@new.float32", Primitive1(NewFloat32)),
      ("@new.float64", Primitive1(NewFloat64)),
      ("@builtin.id", Primitive1(BuiltinId)),
      ("@adt.load_variant", Primitive1(LoadAdtVariant)),
      ("@string.size", Primitive1(StringSize)),
      ("@bytes.size", Primitive1(BytesSize)),
      ("@tag.simple_number", Primitive1(TagSimpleNumber)),
      ("@untag.simple_number", Primitive1(UntagSimpleNumber)),
      ("@tag.char", Primitive1(TagChar)),
      ("@untag.char", Primitive1(UntagChar)),
      ("@not", Primitive1(Not)),
      ("@box", Primitive1(Box)),
      ("@unbox", Primitive1(Unbox)),
      ("@ignore", Primitive1(Ignore)),
      ("@assert", Primitive1(Assert)),
      ("@throw", Primitive1(Throw)),
      ("@unreachable", Primitive0(Unreachable)),
      ("@is", Primitive2(Is)),
      ("@eq", Primitive2(Eq)),
      ("@and", Primitive2(And)),
      ("@or", Primitive2(Or)),
      ("@array.length", Primitive1(ArrayLength)),
      ("@new.rational", Primitive2(NewRational)),
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
      ("@wasm.load_float32", Primitive2(WasmLoadF32)),
      ("@wasm.store_float32", PrimitiveN(WasmStoreF32)),
      ("@wasm.load_float64", Primitive2(WasmLoadF64)),
      ("@wasm.store_float64", PrimitiveN(WasmStoreF64)),
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
        "@wasm.extend_s32_int64",
        Primitive1(
          WasmUnaryI64({
            wasm_op: Op_extend_s32_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_int64,
          }),
        ),
      ),
      (
        "@wasm.neg_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_neg_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.abs_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_abs_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.ceil_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_ceil_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.floor_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_floor_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.trunc_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_trunc_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.nearest_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_nearest_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.sqrt_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_sqrt_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.add_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_add_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.sub_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_sub_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.mul_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_mul_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.div_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_div_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.copy_sign_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_copy_sign_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.min_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_min_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.max_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_max_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.eq_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_eq_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ne_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_ne_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.lt_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_lt_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.le_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_le_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.gt_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_gt_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ge_float32",
        Primitive2(
          WasmBinaryF32({
            wasm_op: Op_ge_float32,
            arg_types: (Wasm_float32, Wasm_float32),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.reinterpret_int32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_reinterpret_int32,
            arg_type: Wasm_int32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.convert_s_int32_to_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_convert_s_int32_to_float32,
            arg_type: Wasm_int32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.convert_u_int32_to_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_convert_u_int32_to_float32,
            arg_type: Wasm_int32,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.convert_s_int64_to_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_convert_s_int64_to_float32,
            arg_type: Wasm_int64,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.convert_u_int64_to_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_convert_u_int64_to_float32,
            arg_type: Wasm_int64,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.demote_float64",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_demote_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_float32,
          }),
        ),
      ),
      (
        "@wasm.neg_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_neg_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.abs_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_abs_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.ceil_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_ceil_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.floor_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_floor_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.trunc_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_trunc_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.nearest_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_nearest_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.sqrt_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_sqrt_float64,
            arg_type: Wasm_float64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.add_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_add_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.sub_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_sub_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.mul_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_mul_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.div_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_div_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.copy_sign_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_copy_sign_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.min_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_min_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.max_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_max_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.eq_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_eq_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ne_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_ne_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.lt_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_lt_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.le_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_le_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.gt_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_gt_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.ge_float64",
        Primitive2(
          WasmBinaryF64({
            wasm_op: Op_ge_float64,
            arg_types: (Wasm_float64, Wasm_float64),
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.reinterpret_int64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_reinterpret_int64,
            arg_type: Wasm_int64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.convert_s_int32_to_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_convert_s_int32_to_float64,
            arg_type: Wasm_int32,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.convert_u_int32_to_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_convert_u_int32_to_float64,
            arg_type: Wasm_int32,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.convert_s_int64_to_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_convert_s_int64_to_float64,
            arg_type: Wasm_int64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.convert_u_int64_to_float64",
        Primitive1(
          WasmUnaryF64({
            wasm_op: Op_convert_u_int64_to_float64,
            arg_type: Wasm_int64,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      (
        "@wasm.promote_float32",
        Primitive1(
          WasmUnaryF32({
            wasm_op: Op_promote_float32,
            arg_type: Wasm_float32,
            ret_type: Wasm_float64,
          }),
        ),
      ),
      ("@wasm.fromGrain", Primitive1(WasmFromGrain)),
      ("@wasm.toGrain", Primitive1(WasmToGrain)),
      ("@wasm.memory_grow", Primitive1(WasmMemoryGrow)),
      ("@wasm.memory_size", PrimitiveN(WasmMemorySize)),
      ("@wasm.memory_copy", PrimitiveN(WasmMemoryCopy)),
      ("@wasm.memory_fill", PrimitiveN(WasmMemoryFill)),
      ("@wasm.memory_compare", PrimitiveN(WasmMemoryCompare)),
    ]),
  );

let active_memory_base = () => {
  switch (Grain_utils.Config.memory_base^) {
  | Some(x) => x
  | None => Grain_utils.Config.default_memory_base
  };
};

let transl_prim = (env, desc) => {
  let loc = desc.tvd_loc;

  let prim =
    try(PrimMap.find(prim_map, List.hd(desc.tvd_prim))) {
    | Not_found => failwith("This primitive does not exist.")
    };

  let disable_gc = [(Location.mknoloc("disableGC"), [])];

  // `attrs` are attributes which should be applied to the `let` which gets implicitly generated.
  //
  // Specifically, consider:
  //
  // primitive equal: (a, a) -> Bool = "@equal"
  //
  // This will get pseudo-generated into something like (note: not syntactically correct):
  //
  // let equal = @disableGC (a, b) => <@equal>(a, b)
  //
  // For *constant* values, this is not good enough. Consider:
  //
  // primitive heapBase = "@heap.base"
  //
  // Since the RHS is just the constant value (inlined here), this binding effectively becomes
  //
  // let heapBase = 0x400n
  //
  // which means that we need to put the `@disableGC` on the *outside* of the `heapBase` binding.
  // This is what the `attrs` list here controls; by returning `attrs == [Typedtree.Disable_gc]`,
  // we will generate
  //
  // @disableGC let heapBase = 0x400n
  //
  // Which can pass through the rest of the compilation without issue.
  //
  let (value, attrs) =
    switch (prim) {
    | PrimitiveConstant(const) =>
      let (value, needs_disablegc) =
        switch (const) {
        // [NOTE] should be kept in sync with `runtime_heap_ptr` and friends in `compcore.re`
        | HeapBase => (
            Const.wasmi32(string_of_int(active_memory_base())),
            true,
          )
        | HeapStart => (
            Const.wasmi32(string_of_int(active_memory_base() + 0x10)),
            true,
          )
        | HeapTypeMetadata => (
            Const.wasmi32(string_of_int(active_memory_base() + 0x8)),
            true,
          )
        };
      let attrs =
        if (needs_disablegc) {
          [Typedtree.Disable_gc];
        } else {
          [];
        };
      (Exp.constant(~loc, ~attributes=disable_gc, value), attrs);
    | Primitive0(
        (
          AllocateInt32 | AllocateInt64 | AllocateFloat32 | AllocateFloat64 |
          AllocateRational |
          Unreachable
        ) as p,
      ) => (
        Exp.lambda(~loc, ~attributes=disable_gc, [], Exp.prim0(~loc, p)),
        [],
      )
    | Primitive1(
        (
          WasmUnaryI32(_) | WasmUnaryI64(_) | WasmUnaryF32(_) | WasmUnaryF64(_) |
          WasmMemoryGrow |
          WasmFromGrain |
          WasmToGrain
        ) as p,
      ) => (
        Exp.lambda(
          ~loc,
          ~attributes=disable_gc,
          [pat_a],
          Exp.prim1(~loc, p, id_a),
        ),
        [],
      )
    | Primitive1(BuiltinId) =>
      // This primitive must always be inlined, so we do not generate a lambda
      (Exp.constant(PConstVoid), [])
    | Primitive1(p) => (
        Exp.lambda(~loc, [pat_a], Exp.prim1(~loc, p, id_a)),
        [],
      )
    | Primitive2(
        (
          WasmBinaryI32(_) | WasmBinaryI64(_) | WasmBinaryF32(_) |
          WasmBinaryF64(_) |
          WasmLoadI32(_) |
          WasmLoadI64(_) |
          WasmLoadF32 |
          WasmLoadF64
        ) as p,
      ) => (
        Exp.lambda(
          ~loc,
          ~attributes=disable_gc,
          [pat_a, pat_b],
          Exp.prim2(~loc, p, id_a, id_b),
        ),
        [],
      )
    | Primitive2(p) => (
        Exp.lambda(~loc, [pat_a, pat_b], Exp.prim2(~loc, p, id_a, id_b)),
        [],
      )
    | PrimitiveN(WasmMemorySize as p) => (
        Exp.lambda(~loc, ~attributes=disable_gc, [], Exp.primn(~loc, p, [])),
        [],
      )
    | PrimitiveN(
        (
          WasmStoreI32(_) | WasmStoreI64(_) | WasmStoreF32 | WasmStoreF64 |
          WasmMemoryCopy |
          WasmMemoryFill |
          WasmMemoryCompare
        ) as p,
      ) => (
        Exp.lambda(
          ~loc,
          ~attributes=disable_gc,
          [pat_a, pat_b, pat_c],
          Exp.primn(~loc, p, [id_a, id_b, id_c]),
        ),
        [],
      )
    };

  let value = Typecore.type_expression(env, value);
  let binds = [
    {
      vb_pat: {
        pat_desc: TPatVar(desc.tvd_id, desc.tvd_name),
        pat_loc: loc,
        pat_extra: [],
        pat_type: desc.tvd_val.val_type,
        pat_env: env,
      },
      vb_expr: value,
      vb_loc: loc,
    },
  ];
  (binds, env, attrs);
};
