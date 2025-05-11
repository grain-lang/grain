open Grain_parsing;
open Ast_helper;
open Typedtree;
open Parsetree;

type primitive_constant =
  | HeapTypeMetadata
  | ElideTypeInfo;

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
  Expression.ident(
    ~loc=Location.dummy_loc,
    ~core_loc=Location.dummy_loc,
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
let id_d = mkident("d");
let id_e = mkident("e");
let id_f = mkident("f");
let id_g = mkident("g");
let id_h = mkident("h");
let id_i = mkident("i");
let id_j = mkident("j");
let id_k = mkident("k");
let id_l = mkident("l");
let id_m = mkident("m");
let id_n = mkident("n");
let id_o = mkident("o");
let id_p = mkident("p");

let pat_a = mkpatvar("a");
let pat_b = mkpatvar("b");
let pat_c = mkpatvar("c");
let pat_d = mkpatvar("d");
let pat_e = mkpatvar("e");
let pat_f = mkpatvar("f");
let pat_g = mkpatvar("g");
let pat_h = mkpatvar("h");
let pat_i = mkpatvar("i");
let pat_j = mkpatvar("j");
let pat_k = mkpatvar("k");
let pat_l = mkpatvar("l");
let pat_m = mkpatvar("m");
let pat_n = mkpatvar("n");
let pat_o = mkpatvar("o");
let pat_p = mkpatvar("p");

let prim_map =
  PrimMap.of_seq(
    List.to_seq([
      ("@heap.start", Primitive0(HeapStart)),
      ("@heap.type_metadata", Primitive0(HeapTypeMetadata)),
      ("@meta.elide_type_info", PrimitiveConstant(ElideTypeInfo)),
      ("@allocate.int32", Primitive0(AllocateInt32)),
      ("@allocate.int64", Primitive0(AllocateInt64)),
      ("@allocate.uint32", Primitive0(AllocateUint32)),
      ("@allocate.uint64", Primitive0(AllocateUint64)),
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
      ("@new.uint32", Primitive1(NewUint32)),
      ("@new.uint64", Primitive1(NewUint64)),
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
      ("@tag.int8", Primitive1(TagInt8)),
      ("@untag.int8", Primitive1(UntagInt8)),
      ("@tag.int16", Primitive1(TagInt16)),
      ("@untag.int16", Primitive1(UntagInt16)),
      ("@tag.uint8", Primitive1(TagUint8)),
      ("@untag.uint8", Primitive1(UntagUint8)),
      ("@tag.uint16", Primitive1(TagUint16)),
      ("@untag.uint16", Primitive1(UntagUint16)),
      ("@not", Primitive1(Not)),
      ("@box", Primitive1(Box)),
      ("@unbox", Primitive1(Unbox)),
      ("@ignore", Primitive1(Ignore)),
      ("@assert", Primitive1(Assert)),
      ("@throw", Primitive1(Throw)),
      ("@magic", Primitive1(Magic)),
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
      ("@wasm.store_8_int64", PrimitiveN(WasmStoreI64({sz: 1}))),
      ("@wasm.store_16_int64", PrimitiveN(WasmStoreI64({sz: 2}))),
      ("@wasm.store_32_int64", PrimitiveN(WasmStoreI64({sz: 4}))),
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
      ("@wasm.simd.load", Primitive2(WasmLoadV128)),
      ("@wasm.simd.store", PrimitiveN(WasmStoreV128)),
      (
        "@wasm.simd.load8_splat",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load8_splat_vec128})),
      ),
      (
        "@wasm.simd.load16_splat",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load16_splat_vec128})),
      ),
      (
        "@wasm.simd.load32_splat",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load32_splat_vec128})),
      ),
      (
        "@wasm.simd.load64_splat",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load64_splat_vec128})),
      ),
      (
        "@wasm.simd.load8x8_s",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load8x8_s_vec128})),
      ),
      (
        "@wasm.simd.load8x8_u",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load8x8_u_vec128})),
      ),
      (
        "@wasm.simd.load16x4_s",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load16x4_s_vec128})),
      ),
      (
        "@wasm.simd.load16x4_u",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load16x4_u_vec128})),
      ),
      (
        "@wasm.simd.load32x2_s",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load32x2_s_vec128})),
      ),
      (
        "@wasm.simd.load32x2_u",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load32x2_u_vec128})),
      ),
      (
        "@wasm.simd.load32_zero",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load32_zero_vec128})),
      ),
      (
        "@wasm.simd.load64_zero",
        PrimitiveN(WasmSimdLoad({wasm_op: Op_load64_zero_vec128})),
      ),
      (
        "@wasm.simd.load8_lane",
        PrimitiveN(
          WasmSimdLoadStoreLane({
            wasm_op: Op_load8_lane_vec128,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.load16_lane",
        PrimitiveN(
          WasmSimdLoadStoreLane({
            wasm_op: Op_load16_lane_vec128,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.load32_lane",
        PrimitiveN(
          WasmSimdLoadStoreLane({
            wasm_op: Op_load32_lane_vec128,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.load64_lane",
        PrimitiveN(
          WasmSimdLoadStoreLane({
            wasm_op: Op_load64_lane_vec128,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.store8_lane",
        PrimitiveN(
          WasmSimdLoadStoreLane({
            wasm_op: Op_store8_lane_vec128,
            ret_type: Grain_void,
          }),
        ),
      ),
      (
        "@wasm.simd.store16_lane",
        PrimitiveN(
          WasmSimdLoadStoreLane({
            wasm_op: Op_store16_lane_vec128,
            ret_type: Grain_void,
          }),
        ),
      ),
      (
        "@wasm.simd.store32_lane",
        PrimitiveN(
          WasmSimdLoadStoreLane({
            wasm_op: Op_store32_lane_vec128,
            ret_type: Grain_void,
          }),
        ),
      ),
      (
        "@wasm.simd.store64_lane",
        PrimitiveN(
          WasmSimdLoadStoreLane({
            wasm_op: Op_store64_lane_vec128,
            ret_type: Grain_void,
          }),
        ),
      ),
      (
        "@wasm.simd.not",
        Primitive1(
          WasmUnaryV128({
            wasm_op: Op_not_vec128,
            arg_type: Wasm_vec128,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.any_true",
        Primitive1(
          WasmUnaryV128({
            wasm_op: Op_any_true_vec128,
            arg_type: Wasm_vec128,
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.simd.and",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_and_vec128,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.or",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_or_vec128,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.xor",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_xor_vec128,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.and_not",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_and_not_vec128,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.bitselect",
        PrimitiveN(WasmTernaryV128({wasm_op: Op_bitselect_vec128})),
      ),
      ("@wasm.simd.i8x16.const", PrimitiveN(WasmSimdConstI8x16)),
      ("@wasm.simd.i8x16.shuffle", PrimitiveN(WasmSimdShuffle)),
      (
        "@wasm.simd.i8x16.swizzle",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_swizzle_vec8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.splat",
        Primitive1(
          WasmUnaryV128({
            wasm_op: Op_splat_vec_i8x16,
            arg_type: Wasm_int32,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.extract_lane_s",
        Primitive2(
          WasmSimdExtract({
            wasm_op: Op_extract_lane_s_vec_i8x16,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.extract_lane_u",
        Primitive2(
          WasmSimdExtract({
            wasm_op: Op_extract_lane_u_vec_i8x16,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.replace_lane",
        PrimitiveN(
          WasmSimdReplace({
            wasm_op: Op_replace_lane_vec_i8x16,
            arg_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.eq",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_eq_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.ne",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_ne_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.lt_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_lt_s_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.lt_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_lt_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.gt_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_gt_s_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.gt_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_gt_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.le_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_le_s_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.le_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_le_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.ge_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_ge_s_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.ge_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_ge_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.abs",
        Primitive1(
          WasmUnaryV128({
            wasm_op: Op_abs_vec_i8x16,
            arg_type: Wasm_vec128,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.neg",
        Primitive1(
          WasmUnaryV128({
            wasm_op: Op_neg_vec_i8x16,
            arg_type: Wasm_vec128,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.all_true",
        Primitive1(
          WasmUnaryV128({
            wasm_op: Op_all_true_vec_i8x16,
            arg_type: Wasm_vec128,
            ret_type: Grain_bool,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.bitmask",
        Primitive1(
          WasmUnaryV128({
            wasm_op: Op_bitmask_vec_i8x16,
            arg_type: Wasm_vec128,
            ret_type: Wasm_int32,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.popcnt",
        Primitive1(
          WasmUnaryV128({
            wasm_op: Op_popcnt_vec_i8x16,
            arg_type: Wasm_vec128,
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.shl",
        Primitive2(
          WasmSimdShift({
            wasm_op: Op_shl_vec_i8x16,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.shr_s",
        Primitive2(
          WasmSimdShift({
            wasm_op: Op_shr_s_vec_i8x16,
          }), 
        ),
      ),
      (
        "@wasm.simd.i8x16.shr_u",
        Primitive2(
          WasmSimdShift({
            wasm_op: Op_shr_u_vec_i8x16,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.add",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_add_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.add_sat_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_add_sat_s_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.add_sat_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_add_sat_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.sub",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_sub_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.sub_sat_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_sub_sat_s_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.sub_sat_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_sub_sat_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.min_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_min_s_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.min_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_min_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.max_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_max_s_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.max_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_max_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.avgr_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_avgr_u_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.narrow_i16x8_s",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_narrow_s_vec_i16x8_to_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      (
        "@wasm.simd.i8x16.narrow_i16x8_u",
        Primitive2(
          WasmBinaryV128({
            wasm_op: Op_narrow_u_vec_i16x8_to_vec_i8x16,
            arg_types: (Wasm_vec128, Wasm_vec128),
            ret_type: Wasm_vec128,
          }),
        ),
      ),
      ("@wasm.simd.i16x8.const", PrimitiveN(WasmSimdConstI16x8)),
      ("@wasm.simd.i16x8.splat", Primitive1(WasmUnaryV128({wasm_op: Op_splat_vec_i16x8, arg_type: Wasm_int32, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.extract_lane_s", Primitive2(WasmSimdExtract({wasm_op: Op_extract_lane_s_vec_i16x8, ret_type: Wasm_int32}))),
      ("@wasm.simd.i16x8.extract_lane_u", Primitive2(WasmSimdExtract({wasm_op: Op_extract_lane_u_vec_i16x8, ret_type: Wasm_int32}))),
      ("@wasm.simd.i16x8.replace_lane", PrimitiveN(WasmSimdReplace({wasm_op: Op_replace_lane_vec_i16x8, arg_type: Wasm_int32}))),
      ("@wasm.simd.i16x8.eq", Primitive2(WasmBinaryV128({wasm_op: Op_eq_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.ne", Primitive2(WasmBinaryV128({wasm_op: Op_ne_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.lt_s", Primitive2(WasmBinaryV128({wasm_op: Op_lt_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.lt_u", Primitive2(WasmBinaryV128({wasm_op: Op_lt_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.gt_s", Primitive2(WasmBinaryV128({wasm_op: Op_gt_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.gt_u", Primitive2(WasmBinaryV128({wasm_op: Op_gt_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.le_s", Primitive2(WasmBinaryV128({wasm_op: Op_le_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.le_u", Primitive2(WasmBinaryV128({wasm_op: Op_le_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.ge_s", Primitive2(WasmBinaryV128({wasm_op: Op_ge_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.ge_u", Primitive2(WasmBinaryV128({wasm_op: Op_ge_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.abs", Primitive1(WasmUnaryV128({wasm_op: Op_abs_vec_i16x8, arg_type: Wasm_vec128, ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.neg", Primitive1(WasmUnaryV128({wasm_op: Op_neg_vec_i16x8, arg_type: Wasm_vec128, ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.all_true", Primitive1(WasmUnaryV128({wasm_op: Op_all_true_vec_i16x8, arg_type: Wasm_vec128, ret_type:Grain_bool}))),
      ("@wasm.simd.i16x8.bitmask", Primitive1(WasmUnaryV128({wasm_op: Op_bitmask_vec_i16x8, arg_type: Wasm_vec128, ret_type:Wasm_int32}))),
      ("@wasm.simd.i16x8.shl", Primitive2(WasmSimdShift({wasm_op: Op_shl_vec_i16x8}))),
      ("@wasm.simd.i16x8.shr_s", Primitive2(WasmSimdShift({wasm_op: Op_shr_s_vec_i16x8}))),
      ("@wasm.simd.i16x8.shr_u", Primitive2(WasmSimdShift({wasm_op: Op_shr_u_vec_i16x8}))),
      ("@wasm.simd.i16x8.add", Primitive2(WasmBinaryV128({wasm_op: Op_add_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.add_sat_s", Primitive2(WasmBinaryV128({wasm_op: Op_add_sat_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.add_sat_u", Primitive2(WasmBinaryV128({wasm_op: Op_add_sat_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.sub", Primitive2(WasmBinaryV128({wasm_op: Op_sub_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.sub_sat_s", Primitive2(WasmBinaryV128({wasm_op: Op_sub_sat_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.sub_sat_u", Primitive2(WasmBinaryV128({wasm_op: Op_sub_sat_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.mul", Primitive2(WasmBinaryV128({wasm_op: Op_mul_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.min_s", Primitive2(WasmBinaryV128({wasm_op: Op_min_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.min_u", Primitive2(WasmBinaryV128({wasm_op: Op_min_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.max_s", Primitive2(WasmBinaryV128({wasm_op: Op_max_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.max_u", Primitive2(WasmBinaryV128({wasm_op: Op_max_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.avgr_u", Primitive2(WasmBinaryV128({wasm_op: Op_avgr_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.q15_mulr_sat_s", Primitive2(WasmBinaryV128({wasm_op: Op_q15_mulr_sat_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.ext_mul_low_s", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_low_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.ext_mul_high_s", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_high_s_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.ext_mul_low_u", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_low_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.ext_mul_high_u", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_high_u_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type:Wasm_vec128}))),
      ("@wasm.simd.i16x8.ext_add_pairwise_s_i8x16", Primitive1(WasmUnaryV128({wasm_op: Op_ext_add_pairwise_s_vec_i8x16_to_i16x8, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.ext_add_pairwise_u_i8x16", Primitive1(WasmUnaryV128({wasm_op: Op_ext_add_pairwise_u_vec_i8x16_to_i16x8, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.narrow_i32x4_s", Primitive2(WasmBinaryV128({wasm_op: Op_narrow_s_vec_i32x4_to_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.narrow_i32x4_u", Primitive2(WasmBinaryV128({wasm_op: Op_narrow_u_vec_i32x4_to_vec_i16x8, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.extend_low_i8x16_s", Primitive1(WasmUnaryV128({wasm_op: Op_extend_low_s_vec_i8x16_to_vec_i16x8, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.extend_high_i8x16_s", Primitive1(WasmUnaryV128({wasm_op: Op_extend_high_s_vec_i8x16_to_vec_i16x8, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.extend_low_i8x16_u", Primitive1(WasmUnaryV128({wasm_op: Op_extend_low_u_vec_i8x16_to_vec_i16x8, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i16x8.extend_high_i8x16_u", Primitive1(WasmUnaryV128({wasm_op: Op_extend_high_u_vec_i8x16_to_vec_i16x8, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.const", PrimitiveN(WasmSimdConstI32x4)),
      ("@wasm.simd.i32x4.splat", Primitive1(WasmUnaryV128({wasm_op: Op_splat_vec_i32x4, arg_type: Wasm_int32, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.extract_lane", Primitive2(WasmSimdExtract({wasm_op: Op_extract_lane_vec_i32x4, ret_type: Wasm_int32}))),
      ("@wasm.simd.i32x4.replace_lane", PrimitiveN(WasmSimdReplace({wasm_op: Op_replace_lane_vec_i32x4, arg_type: Wasm_int32}))),
      ("@wasm.simd.i32x4.eq", Primitive2(WasmBinaryV128({wasm_op: Op_eq_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ne", Primitive2(WasmBinaryV128({wasm_op: Op_ne_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.lt_s", Primitive2(WasmBinaryV128({wasm_op: Op_lt_s_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.lt_u", Primitive2(WasmBinaryV128({wasm_op: Op_lt_u_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.gt_s", Primitive2(WasmBinaryV128({wasm_op: Op_gt_s_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.gt_u", Primitive2(WasmBinaryV128({wasm_op: Op_gt_u_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.le_s", Primitive2(WasmBinaryV128({wasm_op: Op_le_s_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.le_u", Primitive2(WasmBinaryV128({wasm_op: Op_le_u_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ge_s", Primitive2(WasmBinaryV128({wasm_op: Op_ge_s_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ge_u", Primitive2(WasmBinaryV128({wasm_op: Op_ge_u_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.abs", Primitive1(WasmUnaryV128({wasm_op: Op_abs_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.neg", Primitive1(WasmUnaryV128({wasm_op: Op_neg_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.all_true", Primitive1(WasmUnaryV128({wasm_op: Op_all_true_vec_i32x4, arg_type: Wasm_vec128, ret_type: Grain_bool}))),
      ("@wasm.simd.i32x4.bitmask", Primitive1(WasmUnaryV128({wasm_op: Op_bitmask_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_int32}))),
      ("@wasm.simd.i32x4.shl", Primitive2(WasmSimdShift({wasm_op: Op_shl_vec_i32x4}))),
      ("@wasm.simd.i32x4.shr_s", Primitive2(WasmSimdShift({wasm_op: Op_shr_s_vec_i32x4}))),
      ("@wasm.simd.i32x4.shr_u", Primitive2(WasmSimdShift({wasm_op: Op_shr_u_vec_i32x4}))),
      ("@wasm.simd.i32x4.add", Primitive2(WasmBinaryV128({wasm_op: Op_add_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.sub", Primitive2(WasmBinaryV128({wasm_op: Op_sub_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.mul", Primitive2(WasmBinaryV128({wasm_op: Op_mul_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.min_s", Primitive2(WasmBinaryV128({wasm_op: Op_min_s_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.min_u", Primitive2(WasmBinaryV128({wasm_op: Op_min_u_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.max_s", Primitive2(WasmBinaryV128({wasm_op: Op_max_s_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.max_u", Primitive2(WasmBinaryV128({wasm_op: Op_max_u_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.dot_i16x8_s", Primitive2(WasmBinaryV128({wasm_op: Op_dot_s_vec_i16x8_to_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ext_mul_low_s", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_low_s_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ext_mul_high_s", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_high_s_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ext_mul_low_u", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_low_u_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ext_mul_high_u", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_high_u_vec_i32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ext_add_pairwise_i16x8_s", Primitive1(WasmUnaryV128({wasm_op: Op_ext_add_pairwise_s_vec_i16x8_to_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.ext_add_pairwise_i16x8_u", Primitive1(WasmUnaryV128({wasm_op: Op_ext_add_pairwise_u_vec_i16x8_to_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.trunc_sat_f32x4_s", Primitive1(WasmUnaryV128({wasm_op: Op_trunc_sat_s_vec_f32x4_to_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.trunc_sat_f32x4_u", Primitive1(WasmUnaryV128({wasm_op: Op_trunc_sat_u_vec_f32x4_to_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.extend_low_i16x8_s", Primitive1(WasmUnaryV128({wasm_op: Op_extend_low_s_vec_i16x8_to_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.extend_high_i16x8_s", Primitive1(WasmUnaryV128({wasm_op: Op_extend_high_s_vec_i16x8_to_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.extend_low_i16x8_u", Primitive1(WasmUnaryV128({wasm_op: Op_extend_low_u_vec_i16x8_to_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.extend_high_i16x8_u", Primitive1(WasmUnaryV128({wasm_op: Op_extend_high_u_vec_i16x8_to_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.trunc_sat_f64x2_s_zero", Primitive1(WasmUnaryV128({wasm_op: Op_trunc_sat_zero_s_vec_f64x2_to_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i32x4.trunc_sat_f64x2_u_zero", Primitive1(WasmUnaryV128({wasm_op: Op_trunc_sat_zero_u_vec_f64x2_to_vec_i32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.const", Primitive2(WasmSimdConstI64x2)),
      ("@wasm.simd.i64x2.splat", Primitive1(WasmUnaryV128({wasm_op: Op_splat_vec_i64x2, arg_type: Wasm_int64, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.extract_lane", Primitive2(WasmSimdExtract({wasm_op: Op_extract_lane_vec_i64x2, ret_type: Wasm_int64}))),
      ("@wasm.simd.i64x2.replace_lane", PrimitiveN(WasmSimdReplace({wasm_op: Op_replace_lane_vec_i64x2, arg_type: Wasm_int64}))),
      ("@wasm.simd.i64x2.eq", Primitive2(WasmBinaryV128({wasm_op: Op_eq_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.ne", Primitive2(WasmBinaryV128({wasm_op: Op_ne_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.lt_s", Primitive2(WasmBinaryV128({wasm_op: Op_lt_s_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.gt_s", Primitive2(WasmBinaryV128({wasm_op: Op_gt_s_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.le_s", Primitive2(WasmBinaryV128({wasm_op: Op_le_s_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.ge_s", Primitive2(WasmBinaryV128({wasm_op: Op_ge_s_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.abs", Primitive1(WasmUnaryV128({wasm_op: Op_abs_vec_i64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.neg", Primitive1(WasmUnaryV128({wasm_op: Op_neg_vec_i64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.all_true", Primitive1(WasmUnaryV128({wasm_op: Op_all_true_vec_i64x2, arg_type: Wasm_vec128, ret_type: Grain_bool}))),
      ("@wasm.simd.i64x2.bitmask", Primitive1(WasmUnaryV128({wasm_op: Op_bitmask_vec_i64x2, arg_type: Wasm_vec128, ret_type: Wasm_int32}))),
      ("@wasm.simd.i64x2.shl", Primitive2(WasmSimdShift({wasm_op: Op_shl_vec_i64x2}))),
      ("@wasm.simd.i64x2.shr_s", Primitive2(WasmSimdShift({wasm_op: Op_shr_s_vec_i64x2}))),
      ("@wasm.simd.i64x2.shr_u", Primitive2(WasmSimdShift({wasm_op: Op_shr_u_vec_i64x2}))),
      ("@wasm.simd.i64x2.add", Primitive2(WasmBinaryV128({wasm_op: Op_add_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.sub", Primitive2(WasmBinaryV128({wasm_op: Op_sub_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.mul", Primitive2(WasmBinaryV128({wasm_op: Op_mul_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.ext_mul_low_s", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_low_s_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.ext_mul_high_s", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_high_s_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.ext_mul_low_u", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_low_u_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.ext_mul_high_u", Primitive2(WasmBinaryV128({wasm_op: Op_ext_mul_high_u_vec_i64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.extend_low_i32x4_s", Primitive1(WasmUnaryV128({wasm_op: Op_extend_low_s_vec_i32x4_to_vec_i64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.extend_high_i32x4_s", Primitive1(WasmUnaryV128({wasm_op: Op_extend_high_s_vec_i32x4_to_vec_i64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.extend_low_i32x4_u", Primitive1(WasmUnaryV128({wasm_op: Op_extend_low_u_vec_i32x4_to_vec_i64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.i64x2.extend_high_i32x4_u", Primitive1(WasmUnaryV128({wasm_op: Op_extend_high_u_vec_i32x4_to_vec_i64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.const", PrimitiveN(WasmSimdConstF32x4)),
      ("@wasm.simd.f32x4.splat", Primitive1(WasmUnaryV128({wasm_op: Op_splat_vec_f32x4, arg_type: Wasm_float32, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.extract_lane", Primitive2(WasmSimdExtract({wasm_op: Op_extract_lane_vec_f32x4, ret_type: Wasm_float32}))),
      ("@wasm.simd.f32x4.replace_lane", PrimitiveN(WasmSimdReplace({wasm_op: Op_replace_lane_vec_f32x4, arg_type: Wasm_float32}))),
      ("@wasm.simd.f32x4.eq", Primitive2(WasmBinaryV128({wasm_op: Op_eq_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.ne", Primitive2(WasmBinaryV128({wasm_op: Op_ne_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.lt", Primitive2(WasmBinaryV128({wasm_op: Op_lt_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.gt", Primitive2(WasmBinaryV128({wasm_op: Op_gt_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.le", Primitive2(WasmBinaryV128({wasm_op: Op_le_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.ge", Primitive2(WasmBinaryV128({wasm_op: Op_ge_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.abs", Primitive1(WasmUnaryV128({wasm_op: Op_abs_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.neg", Primitive1(WasmUnaryV128({wasm_op: Op_neg_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.sqrt", Primitive1(WasmUnaryV128({wasm_op: Op_sqrt_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.add", Primitive2(WasmBinaryV128({wasm_op: Op_add_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.sub", Primitive2(WasmBinaryV128({wasm_op: Op_sub_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.mul", Primitive2(WasmBinaryV128({wasm_op: Op_mul_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.div", Primitive2(WasmBinaryV128({wasm_op: Op_div_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.min", Primitive2(WasmBinaryV128({wasm_op: Op_min_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.max", Primitive2(WasmBinaryV128({wasm_op: Op_max_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.p_min", Primitive2(WasmBinaryV128({wasm_op: Op_p_min_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.p_max", Primitive2(WasmBinaryV128({wasm_op: Op_p_max_vec_f32x4, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.ceil", Primitive1(WasmUnaryV128({wasm_op: Op_ceil_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.floor", Primitive1(WasmUnaryV128({wasm_op: Op_floor_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.trunc", Primitive1(WasmUnaryV128({wasm_op: Op_trunc_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.nearest", Primitive1(WasmUnaryV128({wasm_op: Op_nearest_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.convert_i32x4_s", Primitive1(WasmUnaryV128({wasm_op: Op_convert_s_vec_i32x4_to_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.convert_i32x4_u", Primitive1(WasmUnaryV128({wasm_op: Op_convert_u_vec_i32x4_to_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f32x4.demote_f64x2_zero", Primitive1(WasmUnaryV128({wasm_op: Op_demote_zero_vec_f64x2_to_vec_f32x4, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.const", Primitive2(WasmSimdConstF64x2)),
      ("@wasm.simd.f64x2.splat", Primitive1(WasmUnaryV128({wasm_op: Op_splat_vec_f64x2, arg_type: Wasm_float64, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.extract_lane", Primitive2(WasmSimdExtract({wasm_op: Op_extract_lane_vec_f64x2, ret_type: Wasm_float64}))),
      ("@wasm.simd.f64x2.replace_lane", PrimitiveN(WasmSimdReplace({wasm_op: Op_replace_lane_vec_f64x2, arg_type: Wasm_float64}))),
      ("@wasm.simd.f64x2.eq", Primitive2(WasmBinaryV128({wasm_op: Op_eq_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.ne", Primitive2(WasmBinaryV128({wasm_op: Op_ne_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.lt", Primitive2(WasmBinaryV128({wasm_op: Op_lt_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.gt", Primitive2(WasmBinaryV128({wasm_op: Op_gt_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.le", Primitive2(WasmBinaryV128({wasm_op: Op_le_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.ge", Primitive2(WasmBinaryV128({wasm_op: Op_ge_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.abs", Primitive1(WasmUnaryV128({wasm_op: Op_abs_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.neg", Primitive1(WasmUnaryV128({wasm_op: Op_neg_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.sqrt", Primitive1(WasmUnaryV128({wasm_op: Op_sqrt_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.add", Primitive2(WasmBinaryV128({wasm_op: Op_add_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.sub", Primitive2(WasmBinaryV128({wasm_op: Op_sub_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.mul", Primitive2(WasmBinaryV128({wasm_op: Op_mul_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.div", Primitive2(WasmBinaryV128({wasm_op: Op_div_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.min", Primitive2(WasmBinaryV128({wasm_op: Op_min_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.max", Primitive2(WasmBinaryV128({wasm_op: Op_max_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.p_min", Primitive2(WasmBinaryV128({wasm_op: Op_p_min_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.p_max", Primitive2(WasmBinaryV128({wasm_op: Op_p_max_vec_f64x2, arg_types: (Wasm_vec128, Wasm_vec128), ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.ceil", Primitive1(WasmUnaryV128({wasm_op: Op_ceil_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.floor", Primitive1(WasmUnaryV128({wasm_op: Op_floor_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.trunc", Primitive1(WasmUnaryV128({wasm_op: Op_trunc_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.nearest", Primitive1(WasmUnaryV128({wasm_op: Op_nearest_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.convert_low_i32x4_s", Primitive1(WasmUnaryV128({wasm_op: Op_convert_low_s_vec_i32x4_to_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.convert_low_i32x4_u", Primitive1(WasmUnaryV128({wasm_op: Op_convert_low_u_vec_i32x4_to_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.simd.f64x2.promote_low_f32x4", Primitive1(WasmUnaryV128({wasm_op: Op_promote_low_vec_f32x4_to_vec_f64x2, arg_type: Wasm_vec128, ret_type: Wasm_vec128}))),
      ("@wasm.fromGrain", Primitive1(WasmFromGrain)),
      ("@wasm.toGrain", Primitive1(WasmToGrain)),
      ("@wasm.memory_grow", Primitive1(WasmMemoryGrow)),
      ("@wasm.memory_size", Primitive0(WasmMemorySize)),
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
  let loc = desc.pprim_loc;
  let core_loc = desc.pprim_loc;

  let prim =
    try(PrimMap.find(prim_map, desc.pprim_name.txt)) {
    | Not_found => failwith("This primitive does not exist.")
    };

  let disable_gc = [
    {
      Asttypes.attr_name: Location.mknoloc("disableGC"),
      attr_args: [],
      attr_loc: Location.dummy_loc,
    },
  ];

  let lambda_arg = pat => {
    pla_label: Unlabeled,
    pla_pattern: pat,
    pla_default: None,
    pla_loc: Location.dummy_loc,
  };

  let (value, typ) =
    switch (prim) {
    | PrimitiveConstant(const) =>
      let (value, typ, attributes) =
        switch (const) {
        // [NOTE] should be kept in sync with `runtime_heap_ptr` and friends in `compcore.re`
        | HeapTypeMetadata => (
            Constant.wasmi32(
              Location.mknoloc(string_of_int(active_memory_base() + 0x8)),
            ),
            Builtin_types.type_wasmi32,
            disable_gc,
          )
        | ElideTypeInfo => (
            Constant.bool(Grain_utils.Config.elide_type_info^),
            Builtin_types.type_bool,
            [],
          )
        };
      (Expression.constant(~loc, ~core_loc, ~attributes, value), typ);
    | Primitive0(p) =>
      let attributes =
        switch (p) {
        | AllocateInt32
        | AllocateInt64
        | AllocateUint32
        | AllocateUint64
        | AllocateFloat32
        | AllocateFloat64
        | AllocateRational
        | WasmMemorySize
        | Unreachable
        | HeapStart
        | HeapTypeMetadata => disable_gc
        };
      (
        Expression.lambda(
          ~loc,
          ~core_loc,
          ~attributes,
          [],
          Expression.prim0(~loc, ~core_loc, p),
        ),
        Typecore.prim0_type(p),
      );
    | Primitive1(BuiltinId as p) =>
      // This primitive must always be inlined, so we do not generate a lambda
      (
        Expression.constant(~loc, ~core_loc, PConstVoid),
        Typecore.prim1_type(p),
      )
    | Primitive1(p) =>
      let attributes =
        switch (p) {
        | WasmUnaryI32(_)
        | WasmUnaryI64(_)
        | WasmUnaryF32(_)
        | WasmUnaryF64(_)
        | WasmUnaryV128(_)
        | WasmMemoryGrow
        | WasmFromGrain
        | WasmToGrain => disable_gc
        | AllocateArray
        | AllocateTuple
        | AllocateBytes
        | AllocateString
        | AllocateBigInt
        | StringSize
        | BytesSize
        | ArrayLength
        | NewInt32
        | NewInt64
        | NewUint32
        | NewUint64
        | NewFloat32
        | NewFloat64
        | LoadAdtVariant
        | TagSimpleNumber
        | UntagSimpleNumber
        | TagChar
        | UntagChar
        | TagInt8
        | UntagInt8
        | TagInt16
        | UntagInt16
        | TagUint8
        | UntagUint8
        | TagUint16
        | UntagUint16
        | Not
        | Box
        | BoxBind
        | Unbox
        | UnboxBind
        | Ignore
        | Assert
        | Throw
        | Magic
        | BuiltinId => []
        };
      (
        Expression.lambda(
          ~loc,
          ~core_loc,
          ~attributes,
          [lambda_arg(pat_a)],
          Expression.prim1(~loc, ~core_loc, p, id_a),
        ),
        Typecore.prim1_type(p),
      );
    | Primitive2(p) =>
      let attributes =
        switch (p) {
        | WasmBinaryI32(_)
        | WasmBinaryI64(_)
        | WasmBinaryF32(_)
        | WasmBinaryF64(_)
        | WasmBinaryV128(_)
        | WasmSimdExtract(_)
        | WasmSimdShift(_)|WasmSimdConstI64x2|WasmSimdConstF64x2
        | WasmLoadI32(_)
        | WasmLoadI64(_)
        | WasmLoadF32
        | WasmLoadF64
        | WasmLoadV128
        | NewRational => disable_gc
        | Is
        | Eq
        | And
        | Or => []
        };
      (
        Expression.lambda( 
          ~loc,
          ~core_loc,
          ~attributes,
          [lambda_arg(pat_a), lambda_arg(pat_b)],
          Expression.prim2(~loc, ~core_loc, p, id_a, id_b),
        ),
        Typecore.prim2_type(p),
      );
    | PrimitiveN(p) =>
      let attributes =
        switch (p) {
        | WasmStoreI32(_)
        | WasmStoreI64(_)
        | WasmStoreF32
        | WasmStoreF64
        | WasmStoreV128
        | WasmMemoryCopy
        | WasmMemoryFill
        | WasmMemoryCompare
        | WasmSimdShuffle
        | WasmTernaryV128(_)
        | WasmSimdReplace(_)
        | WasmSimdLoad(_)
        | WasmSimdLoadStoreLane(_)| WasmSimdConstI8x16
    | WasmSimdConstI16x8
    | WasmSimdConstI32x4
    | WasmSimdConstF32x4 => disable_gc
        };
      let (args, ids) =
        switch (p) {
        | WasmStoreI32(_)
        | WasmStoreI64(_)
        | WasmStoreF32
        | WasmStoreF64
        | WasmStoreV128
        | WasmMemoryCopy
        | WasmMemoryFill
        | WasmMemoryCompare
        | WasmSimdShuffle
        | WasmTernaryV128(_)
        | WasmSimdReplace(_)
        | WasmSimdLoad(_) => (
            [lambda_arg(pat_a), lambda_arg(pat_b), lambda_arg(pat_c)],
            [id_a, id_b, id_c],
          )
        | WasmSimdLoadStoreLane(_) => (
            [
              lambda_arg(pat_a),
              lambda_arg(pat_b),
              lambda_arg(pat_c),
              lambda_arg(pat_d),
              lambda_arg(pat_e),
            ],
            [id_a, id_b, id_c, id_d, id_e],
          )
          | WasmSimdConstI8x16 => (
            [lambda_arg(pat_a), lambda_arg(pat_b), lambda_arg(pat_c),
              lambda_arg(pat_d), lambda_arg(pat_e), lambda_arg(pat_f), lambda_arg(pat_g), lambda_arg(pat_h), lambda_arg(pat_i), lambda_arg(pat_j), lambda_arg(pat_k), lambda_arg(pat_l), lambda_arg(pat_m), lambda_arg(pat_n), lambda_arg(pat_o), lambda_arg(pat_p)],
            [id_a, id_b, id_c, id_d, id_e, id_f, id_g, id_h, id_i, id_j, id_k, id_l, id_m, id_n, id_o, id_p],
          )
    | WasmSimdConstI16x8 => (
            [lambda_arg(pat_a), lambda_arg(pat_b), lambda_arg(pat_c),
              lambda_arg(pat_d), lambda_arg(pat_e), lambda_arg(pat_f), lambda_arg(pat_g), lambda_arg(pat_h)],
            [id_a, id_b, id_c, id_d, id_e, id_f, id_g, id_h],
          )
    | WasmSimdConstI32x4
    | WasmSimdConstF32x4 => (
            [lambda_arg(pat_a), lambda_arg(pat_b), lambda_arg(pat_c),
              lambda_arg(pat_d)],
            [id_a, id_b, id_c, id_d],
          )
        };
      (
        Expression.lambda(
          ~loc,
          ~core_loc,
          ~attributes,
          args,
          Expression.primn(~loc, ~core_loc, p, ids),
        ),
        Typecore.primn_type(p),
      );
    };

  let id = Ident.create(desc.pprim_ident.txt);
  let value_description = {
    Types.val_type: typ,
    val_repr: Type_utils.repr_of_type(env, typ),
    val_kind: TValPrim(desc.pprim_name.txt),
    val_loc: loc,
    val_internalpath: PIdent(id),
    val_fullpath: Path.PIdent(id),
    val_mutable: false,
    val_global: true,
  };

  let value = Typecore.type_expression(env, value);
  let env = Env.add_value(id, value_description, env);
  let binds = [
    {
      vb_pat: {
        pat_desc: TPatVar(id, desc.pprim_ident),
        pat_loc: loc,
        pat_extra: [],
        pat_type: typ,
        pat_env: env,
      },
      vb_expr: value,
      vb_loc: loc,
    },
  ];
  (binds, id, value_description, env);
};
