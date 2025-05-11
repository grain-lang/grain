/** Low-level IR, suitable for direct translation into WASM */
open Sexplib.Conv;

open Grain_parsing;
open Grain_typed;
open Value_tags;
open Runtime_errors;

let sexp_locs_disabled = _ => ! Grain_utils.Config.sexp_locs_enabled^;

type tag_type = Value_tags.tag_type;
type heap_tag_type = Value_tags.heap_tag_type;

[@deriving sexp]
type attributes = Typedtree.attributes;

type grain_error = Runtime_errors.grain_error;
let (prim0_of_sexp, sexp_of_prim0) = (
  Parsetree.prim0_of_sexp,
  Parsetree.sexp_of_prim0,
);
let (prim1_of_sexp, sexp_of_prim1) = (
  Parsetree.prim1_of_sexp,
  Parsetree.sexp_of_prim1,
);
let (prim2_of_sexp, sexp_of_prim2) = (
  Parsetree.prim2_of_sexp,
  Parsetree.sexp_of_prim2,
);
let (primn_of_sexp, sexp_of_primn) = (
  Parsetree.primn_of_sexp,
  Parsetree.sexp_of_primn,
);

type wasm_prim_type =
  Parsetree.wasm_prim_type =
    | Wasm_int32
    | Wasm_int64
    | Wasm_float32
    | Wasm_float64
    | Wasm_vec128
    | Grain_bool
    | Grain_void;

type wasm_op =
  Parsetree.wasm_op =
    | Op_clz_int32
    | Op_ctz_int32
    | Op_popcnt_int32
    | Op_neg_float32
    | Op_abs_float32
    | Op_ceil_float32
    | Op_floor_float32
    | Op_trunc_float32
    | Op_nearest_float32
    | Op_sqrt_float32
    | Op_eq_z_int32
    | Op_clz_int64
    | Op_ctz_int64
    | Op_popcnt_int64
    | Op_neg_float64
    | Op_abs_float64
    | Op_ceil_float64
    | Op_floor_float64
    | Op_trunc_float64
    | Op_nearest_float64
    | Op_sqrt_float64
    | Op_eq_z_int64
    | Op_extend_s_int32
    | Op_extend_u_int32
    | Op_wrap_int64
    | Op_trunc_s_float32_to_int32
    | Op_trunc_s_float32_to_int64
    | Op_trunc_u_float32_to_int32
    | Op_trunc_u_float32_to_int64
    | Op_trunc_s_float64_to_int32
    | Op_trunc_s_float64_to_int64
    | Op_trunc_u_float64_to_int32
    | Op_trunc_u_float64_to_int64
    | Op_reinterpret_float32
    | Op_reinterpret_float64
    | Op_convert_s_int32_to_float32
    | Op_convert_s_int32_to_float64
    | Op_convert_u_int32_to_float32
    | Op_convert_u_int32_to_float64
    | Op_convert_s_int64_to_float32
    | Op_convert_s_int64_to_float64
    | Op_convert_u_int64_to_float32
    | Op_convert_u_int64_to_float64
    | Op_promote_float32
    | Op_demote_float64
    | Op_reinterpret_int32
    | Op_reinterpret_int64
    | Op_extend_s8_int32
    | Op_extend_s16_int32
    | Op_extend_s8_int64
    | Op_extend_s16_int64
    | Op_extend_s32_int64
    | Op_add_int32
    | Op_sub_int32
    | Op_mul_int32
    | Op_div_s_int32
    | Op_div_u_int32
    | Op_rem_s_int32
    | Op_rem_u_int32
    | Op_and_int32
    | Op_or_int32
    | Op_xor_int32
    | Op_shl_int32
    | Op_shr_u_int32
    | Op_shr_s_int32
    | Op_rot_l_int32
    | Op_rot_r_int32
    | Op_eq_int32
    | Op_ne_int32
    | Op_lt_s_int32
    | Op_lt_u_int32
    | Op_le_s_int32
    | Op_le_u_int32
    | Op_gt_s_int32
    | Op_gt_u_int32
    | Op_ge_s_int32
    | Op_ge_u_int32
    | Op_add_int64
    | Op_sub_int64
    | Op_mul_int64
    | Op_div_s_int64
    | Op_div_u_int64
    | Op_rem_s_int64
    | Op_rem_u_int64
    | Op_and_int64
    | Op_or_int64
    | Op_xor_int64
    | Op_shl_int64
    | Op_shr_u_int64
    | Op_shr_s_int64
    | Op_rot_l_int64
    | Op_rot_r_int64
    | Op_eq_int64
    | Op_ne_int64
    | Op_lt_s_int64
    | Op_lt_u_int64
    | Op_le_s_int64
    | Op_le_u_int64
    | Op_gt_s_int64
    | Op_gt_u_int64
    | Op_ge_s_int64
    | Op_ge_u_int64
    | Op_add_float32
    | Op_sub_float32
    | Op_mul_float32
    | Op_div_float32
    | Op_copy_sign_float32
    | Op_min_float32
    | Op_max_float32
    | Op_eq_float32
    | Op_ne_float32
    | Op_lt_float32
    | Op_le_float32
    | Op_gt_float32
    | Op_ge_float32
    | Op_add_float64
    | Op_sub_float64
    | Op_mul_float64
    | Op_div_float64
    | Op_copy_sign_float64
    | Op_min_float64
    | Op_max_float64
    | Op_eq_float64
    | Op_ne_float64
    | Op_lt_float64
    | Op_le_float64
    | Op_gt_float64
    | Op_ge_float64
    | Op_atomic_rmw_add
    | Op_atomic_rmw_sub
    | Op_atomic_rmw_and
    | Op_atomic_rmw_or
    | Op_atomic_rmw_xor
    | Op_atomic_rmw_xchg
    | Op_trunc_sat_s_float32_to_int32
    | Op_trunc_sat_s_float32_to_int64
    | Op_trunc_sat_u_float32_to_int32
    | Op_trunc_sat_u_float32_to_int64
    | Op_trunc_sat_s_float64_to_int32
    | Op_trunc_sat_s_float64_to_int64
    | Op_trunc_sat_u_float64_to_int32
    | Op_trunc_sat_u_float64_to_int64
    | Op_splat_vec_i8x16
    | Op_extract_lane_s_vec_i8x16
    | Op_extract_lane_u_vec_i8x16
    | Op_replace_lane_vec_i8x16
    | Op_splat_vec_i16x8
    | Op_extract_lane_s_vec_i16x8
    | Op_extract_lane_u_vec_i16x8
    | Op_replace_lane_vec_i16x8
    | Op_splat_vec_i32x4
    | Op_extract_lane_vec_i32x4
    | Op_replace_lane_vec_i32x4
    | Op_splat_vec_i64x2
    | Op_extract_lane_vec_i64x2
    | Op_replace_lane_vec_i64x2
    | Op_splat_vec_f32x4
    | Op_extract_lane_vec_f32x4
    | Op_replace_lane_vec_f32x4
    | Op_splat_vec_f64x2
    | Op_extract_lane_vec_f64x2
    | Op_replace_lane_vec_f64x2
    | Op_eq_vec_i8x16
    | Op_ne_vec_i8x16
    | Op_lt_s_vec_i8x16
    | Op_lt_u_vec_i8x16
    | Op_gt_s_vec_i8x16
    | Op_gt_u_vec_i8x16
    | Op_le_s_vec_i8x16
    | Op_le_u_vec_i8x16
    | Op_ge_s_vec_i8x16
    | Op_ge_u_vec_i8x16
    | Op_eq_vec_i16x8
    | Op_ne_vec_i16x8
    | Op_lt_s_vec_i16x8
    | Op_lt_u_vec_i16x8
    | Op_gt_s_vec_i16x8
    | Op_gt_u_vec_i16x8
    | Op_le_s_vec_i16x8
    | Op_le_u_vec_i16x8
    | Op_ge_s_vec_i16x8
    | Op_ge_u_vec_i16x8
    | Op_eq_vec_i32x4
    | Op_ne_vec_i32x4
    | Op_lt_s_vec_i32x4
    | Op_lt_u_vec_i32x4
    | Op_gt_s_vec_i32x4
    | Op_gt_u_vec_i32x4
    | Op_le_s_vec_i32x4
    | Op_le_u_vec_i32x4
    | Op_ge_s_vec_i32x4
    | Op_ge_u_vec_i32x4
    | Op_eq_vec_i64x2
    | Op_ne_vec_i64x2
    | Op_lt_s_vec_i64x2
    | Op_gt_s_vec_i64x2
    | Op_le_s_vec_i64x2
    | Op_ge_s_vec_i64x2
    | Op_eq_vec_f32x4
    | Op_ne_vec_f32x4
    | Op_lt_vec_f32x4
    | Op_gt_vec_f32x4
    | Op_le_vec_f32x4
    | Op_ge_vec_f32x4
    | Op_eq_vec_f64x2
    | Op_ne_vec_f64x2
    | Op_lt_vec_f64x2
    | Op_gt_vec_f64x2
    | Op_le_vec_f64x2
    | Op_ge_vec_f64x2
    | Op_not_vec128
    | Op_and_vec128
    | Op_or_vec128
    | Op_xor_vec128
    | Op_and_not_vec128
    | Op_bitselect_vec128
    | Op_relaxed_fma_vec_f32x4
    | Op_relaxed_fms_vec_f32x4
    | Op_relaxed_fma_vec_f64x4
    | Op_relaxed_fms_vec_f64x4
    | Op_laneselect_i8x16
    | Op_laneselect_i16x8
    | Op_laneselect_i32x4
    | Op_laneselect_i64x2
    | Op_dot_i8x16_i7x16_add_s_to_vec_i32x4
    | Op_any_true_vec128
    | Op_popcnt_vec_i8x16
    | Op_abs_vec_i8x16
    | Op_neg_vec_i8x16
    | Op_all_true_vec_i8x16
    | Op_bitmask_vec_i8x16
    | Op_shl_vec_i8x16
    | Op_shr_s_vec_i8x16
    | Op_shr_u_vec_i8x16
    | Op_add_vec_i8x16
    | Op_add_sat_s_vec_i8x16
    | Op_add_sat_u_vec_i8x16
    | Op_sub_vec_i8x16
    | Op_sub_sat_s_vec_i8x16
    | Op_sub_sat_u_vec_i8x16
    | Op_min_s_vec_i8x16
    | Op_min_u_vec_i8x16
    | Op_max_s_vec_i8x16
    | Op_max_u_vec_i8x16
    | Op_avgr_u_vec_i8x16
    | Op_abs_vec_i16x8
    | Op_neg_vec_i16x8
    | Op_all_true_vec_i16x8
    | Op_bitmask_vec_i16x8
    | Op_shl_vec_i16x8
    | Op_shr_s_vec_i16x8
    | Op_shr_u_vec_i16x8
    | Op_add_vec_i16x8
    | Op_add_sat_s_vec_i16x8
    | Op_add_sat_u_vec_i16x8
    | Op_sub_vec_i16x8
    | Op_sub_sat_s_vec_i16x8
    | Op_sub_sat_u_vec_i16x8
    | Op_mul_vec_i16x8
    | Op_min_s_vec_i16x8
    | Op_min_u_vec_i16x8
    | Op_max_s_vec_i16x8
    | Op_max_u_vec_i16x8
    | Op_avgr_u_vec_i16x8
    | Op_q15_mulr_sat_s_vec_i16x8
    | Op_ext_mul_low_s_vec_i16x8
    | Op_ext_mul_high_s_vec_i16x8
    | Op_ext_mul_low_u_vec_i16x8
    | Op_ext_mul_high_u_vec_i16x8
    | Op_abs_vec_i32x4
    | Op_neg_vec_i32x4
    | Op_all_true_vec_i32x4
    | Op_bitmask_vec_i32x4
    | Op_shl_vec_i32x4
    | Op_shr_s_vec_i32x4
    | Op_shr_u_vec_i32x4
    | Op_add_vec_i32x4
    | Op_sub_vec_i32x4
    | Op_mul_vec_i32x4
    | Op_min_s_vec_i32x4
    | Op_min_u_vec_i32x4
    | Op_max_s_vec_i32x4
    | Op_max_u_vec_i32x4
    | Op_dot_s_vec_i16x8_to_vec_i32x4
    | Op_ext_mul_low_s_vec_i32x4
    | Op_ext_mul_high_s_vec_i32x4
    | Op_ext_mul_low_u_vec_i32x4
    | Op_ext_mul_high_u_vec_i32x4
    | Op_abs_vec_i64x2
    | Op_neg_vec_i64x2
    | Op_all_true_vec_i64x2
    | Op_bitmask_vec_i64x2
    | Op_shl_vec_i64x2
    | Op_shr_s_vec_i64x2
    | Op_shr_u_vec_i64x2
    | Op_add_vec_i64x2
    | Op_sub_vec_i64x2
    | Op_mul_vec_i64x2
    | Op_ext_mul_low_s_vec_i64x2
    | Op_ext_mul_high_s_vec_i64x2
    | Op_ext_mul_low_u_vec_i64x2
    | Op_ext_mul_high_u_vec_i64x2
    | Op_abs_vec_f32x4
    | Op_neg_vec_f32x4
    | Op_sqrt_vec_f32x4
    | Op_add_vec_f32x4
    | Op_sub_vec_f32x4
    | Op_mul_vec_f32x4
    | Op_div_vec_f32x4
    | Op_min_vec_f32x4
    | Op_max_vec_f32x4
    | Op_p_min_vec_f32x4
    | Op_p_max_vec_f32x4
    | Op_ceil_vec_f32x4
    | Op_floor_vec_f32x4
    | Op_trunc_vec_f32x4
    | Op_nearest_vec_f32x4
    | Op_abs_vec_f64x2
    | Op_neg_vec_f64x2
    | Op_sqrt_vec_f64x2
    | Op_add_vec_f64x2
    | Op_sub_vec_f64x2
    | Op_mul_vec_f64x2
    | Op_div_vec_f64x2
    | Op_min_vec_f64x2
    | Op_max_vec_f64x2
    | Op_p_min_vec_f64x2
    | Op_p_max_vec_f64x2
    | Op_ceil_vec_f64x2
    | Op_floor_vec_f64x2
    | Op_trunc_vec_f64x2
    | Op_nearest_vec_f64x2
    | Op_ext_add_pairwise_s_vec_i8x16_to_i16x8
    | Op_ext_add_pairwise_u_vec_i8x16_to_i16x8
    | Op_ext_add_pairwise_s_vec_i16x8_to_i32x4
    | Op_ext_add_pairwise_u_vec_i16x8_to_i32x4
    | Op_trunc_sat_s_vec_f32x4_to_vec_i32x4
    | Op_trunc_sat_u_vec_f32x4_to_vec_i32x4
    | Op_convert_s_vec_i32x4_to_vec_f32x4
    | Op_convert_u_vec_i32x4_to_vec_f32x4
    | Op_load8_splat_vec128
    | Op_load16_splat_vec128
    | Op_load32_splat_vec128
    | Op_load64_splat_vec128
    | Op_load8x8_s_vec128
    | Op_load8x8_u_vec128
    | Op_load16x4_s_vec128
    | Op_load16x4_u_vec128
    | Op_load32x2_s_vec128
    | Op_load32x2_u_vec128
    | Op_load32_zero_vec128
    | Op_load64_zero_vec128
    | Op_load8_lane_vec128
    | Op_load16_lane_vec128
    | Op_load32_lane_vec128
    | Op_load64_lane_vec128
    | Op_store8_lane_vec128
    | Op_store16_lane_vec128
    | Op_store32_lane_vec128
    | Op_store64_lane_vec128
    | Op_narrow_s_vec_i16x8_to_vec_i8x16
    | Op_narrow_u_vec_i16x8_to_vec_i8x16
    | Op_narrow_s_vec_i32x4_to_vec_i16x8
    | Op_narrow_u_vec_i32x4_to_vec_i16x8
    | Op_extend_low_s_vec_i8x16_to_vec_i16x8
    | Op_extend_high_s_vec_i8x16_to_vec_i16x8
    | Op_extend_low_u_vec_i8x16_to_vec_i16x8
    | Op_extend_high_u_vec_i8x16_to_vec_i16x8
    | Op_extend_low_s_vec_i16x8_to_vec_i32x4
    | Op_extend_high_s_vec_i16x8_to_vec_i32x4
    | Op_extend_low_u_vec_i16x8_to_vec_i32x4
    | Op_extend_high_u_vec_i16x8_to_vec_i32x4
    | Op_extend_low_s_vec_i32x4_to_vec_i64x2
    | Op_extend_high_s_vec_i32x4_to_vec_i64x2
    | Op_extend_low_u_vec_i32x4_to_vec_i64x2
    | Op_extend_high_u_vec_i32x4_to_vec_i64x2
    | Op_convert_low_s_vec_i32x4_to_vec_f64x2
    | Op_convert_low_u_vec_i32x4_to_vec_f64x2
    | Op_trunc_sat_zero_s_vec_f64x2_to_vec_i32x4
    | Op_trunc_sat_zero_u_vec_f64x2_to_vec_i32x4
    | Op_demote_zero_vec_f64x2_to_vec_f32x4
    | Op_promote_low_vec_f32x4_to_vec_f64x2
    | Op_relaxed_trunc_s_vec_f32x4_to_vec_i32x4
    | Op_relaxed_trunc_u_vec_f32x4_to_vec_i32x4
    | Op_relaxed_trunc_zero_s_vec_f64x2_to_vec_i32x4
    | Op_relaxed_trunc_zero_u_vec_f64x2_to_vec_i32x4
    | Op_swizzle_vec8x16
    | Op_relaxed_swizzle_vec_i8x16
    | Op_relaxed_min_vec_f32x4
    | Op_relaxed_max_vec_f32x4
    | Op_relaxed_min_vec_f64x2
    | Op_relaxed_max_vec_f64x2
    | Op_relaxed_q15_mulr_s_vec_i16x8
    | Op_dot_i8x16_i7x16_s_to_vec_i16x8
    | Op_ref_as_non_null
    | Op_ref_as_extern_internalize
    | Op_ref_as_extern_externalize
    | Op_br_on_null
    | Op_br_on_non_null
    | Op_br_on_cast
    | Op_br_on_cast_fail;

type prim0 =
  Parsetree.prim0 =
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
    | HeapTypeMetadata;

type prim1 =
  Parsetree.prim1 =
    | AllocateArray
    | AllocateTuple
    | AllocateBytes
    | AllocateString
    | AllocateBigInt
    | NewInt32
    | NewInt64
    | NewUint32
    | NewUint64
    | NewFloat32
    | NewFloat64
    | BuiltinId
    | LoadAdtVariant
    | StringSize
    | BytesSize
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
    | Unbox
    | BoxBind
    | UnboxBind
    | Ignore
    | ArrayLength
    | Assert
    | Throw
    | Magic
    | WasmFromGrain
    | WasmToGrain
    | WasmUnaryI32({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryI64({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryF32({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryF64({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryV128({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmMemoryGrow;

type prim2 =
  Parsetree.prim2 =
    | NewRational
    | Is
    | Eq
    | And
    | Or
    | WasmLoadI32({
        sz: int,
        signed: bool,
      })
    | WasmLoadI64({
        sz: int,
        signed: bool,
      })
    | WasmLoadF32
    | WasmLoadF64
    | WasmLoadV128
    | WasmBinaryI32({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryI64({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryF32({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryF64({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryV128({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmSimdExtract({
        wasm_op,
        ret_type: wasm_prim_type,
      })
    | WasmSimdShift({wasm_op})
    | WasmSimdConstI64x2
    | WasmSimdConstF64x2;

type primn =
  Parsetree.primn =
    | WasmStoreI32({sz: int})
    | WasmStoreI64({sz: int})
    | WasmStoreF32
    | WasmStoreF64
    | WasmStoreV128
    | WasmMemoryCopy
    | WasmMemoryFill
    | WasmMemoryCompare
    | WasmTernaryV128({wasm_op})
    | WasmSimdReplace({
        wasm_op,
        arg_type: wasm_prim_type,
      })
    | WasmSimdShuffle
    | WasmSimdLoad({wasm_op})
    | WasmSimdLoadStoreLane({
        wasm_op,
        ret_type: wasm_prim_type,
      })
    | WasmSimdConstI8x16
    | WasmSimdConstI16x8
    | WasmSimdConstI32x4
    | WasmSimdConstF32x4;

[@deriving sexp]
type constant =
  | MConstI8(int32)
  | MConstI16(int32)
  | MConstI32(int32)
  | MConstI64(int64)
  | MConstU8(int32)
  | MConstU16(int32)
  | MConstU32(int32)
  | MConstU64(int64)
  /*
   * jsoo cannot safely marshal floats into a consistent representation because
   * of how JS numbers work; we work with the bits of the float instead.
   */
  | MConstF32(int64)
  | MConstF64(int64)
  | MConstV128(int32, int32, int32, int32)
  | MConstChar(string)
  | MConstLiteral(constant); /* Special case for things which should not be encoded */

[@deriving sexp]
type binding =
  | MArgBind(int32, Types.allocation_type)
  | MLocalBind(int32, Types.allocation_type)
  | MGlobalBind(string, Types.allocation_type)
  | MClosureBind(int32)
  | MSwapBind(int32, Types.allocation_type); /* Used like a register would be */

[@deriving sexp]
type immediate = {
  immediate_desc,
  immediate_analyses,
}

and immediate_desc =
  | MImmConst(constant)
  | MImmBinding(binding)
  | MIncRef(immediate)
  | MImmTrap

and immediate_analyses = {mutable last_usage}

and last_usage =
  | Last
  | TailCallLast
  | NotLast
  | Unknown;

[@deriving sexp]
type closure_data = {
  func_idx: option(int32),
  global_offset: string,
  arity: int32,
  variables: list(immediate),
};

[@deriving sexp]
type allocation_type =
  | MClosure(closure_data)
  | MTuple(list(immediate))
  | MBox(immediate)
  | MArray(list(immediate))
  | MRecord(immediate, immediate, list((option(string), immediate)))
  | MADT(immediate, immediate, immediate, list(immediate)) /* Type hash, Type Tag, Variant Tag, Elements */
  | MBytes(bytes)
  | MString(string)
  | MInt32(int32)
  | MInt64(int64)
  | MUint32(int32)
  | MUint64(int64)
  /*
   * jsoo cannot safely marshal floats into a consistent representation because
   * of how JS numbers work; we work with the bits of the float instead.
   */
  | MFloat32(int64)
  | MFloat64(int64)
  | MRational({
      numerator_flags: list(Bigint_flags.t),
      numerator_limbs: array(int64),
      denominator_flags: list(Bigint_flags.t),
      denominator_limbs: array(int64),
    })
  | MBigInt({
      flags: list(Bigint_flags.t),
      limbs: array(int64),
    });

[@deriving sexp]
type tag_op =
  | MCheckTag
  | MAssertTag
  | MAddTag
  | MRemoveTag;

[@deriving sexp]
type arity_operand =
  | MLambdaArity
  | MTupleArity;

[@deriving sexp]
type arity_op =
  | MGetArity
  | MAssertArity(int32);

[@deriving sexp]
type tuple_op =
  | MTupleGet(int32)
  | MTupleSet(int32, immediate);

[@deriving sexp]
type box_op =
  | MBoxUnbox
  | MBoxUpdate(immediate);

[@deriving sexp]
type array_op =
  | MArrayGet(immediate)
  | MArraySet(immediate, immediate)
  | MArrayLength;

[@deriving sexp]
type adt_op =
  | MAdtGet(int32)
  | MAdtGetModule
  | MAdtGetTag;

[@deriving sexp]
type record_op =
  | MRecordGet(int32)
  | MRecordSet(int32, immediate);

[@deriving sexp]
type closure_op =
  | MClosureSetPtr(string, int32);

[@deriving sexp]
type instr = {
  instr_desc,
  [@sexp_drop_if sexp_locs_disabled]
  instr_loc: Location.t,
} /* Optimized path for statically-known function names */ /* value, branches, default */ /* Items in the same list have their backpatching delayed until the end of that list */ /* Ignore the result of an expression. Used for sequences. */ /* Prints a message to the console; for compiler debugging */
[@deriving sexp]
and instr_desc =
  | MImmediate(immediate)
  | MCallRaw({
      func: string,
      func_type: (list(Types.allocation_type), list(Types.allocation_type)),
      args: list(immediate),
    })
  | MCallKnown({
      func: string,
      closure: immediate,
      func_type: (list(Types.allocation_type), list(Types.allocation_type)),
      args: list(immediate),
    })
  | MReturnCallKnown({
      func: string,
      closure: immediate,
      func_type: (list(Types.allocation_type), list(Types.allocation_type)),
      args: list(immediate),
    })
  | MCallIndirect({
      func: immediate,
      func_type: (list(Types.allocation_type), list(Types.allocation_type)),
      args: list(immediate),
    })
  | MReturnCallIndirect({
      func: immediate,
      func_type: (list(Types.allocation_type), list(Types.allocation_type)),
      args: list(immediate),
    })
  | MError(grain_error, list(immediate))
  | MAllocate(allocation_type)
  | MTagOp(tag_op, tag_type, immediate)
  | MArityOp(arity_operand, arity_op, immediate)
  | MIf(immediate, block, block)
  | MFor(option(block), option(block), block)
  | MContinue
  | MBreak
  | MReturn(option(immediate))
  | MSwitch(immediate, list((int32, block)), block, Types.allocation_type) /* value, branches, default, return type */
  | MPrim0(prim0)
  | MPrim1(prim1, immediate)
  | MPrim2(prim2, immediate, immediate)
  | MPrimN(primn, list(immediate))
  | MTupleOp(tuple_op, immediate)
  | MBoxOp(box_op, immediate)
  | MArrayOp(array_op, immediate)
  | MAdtOp(adt_op, immediate)
  | MRecordOp(record_op, immediate)
  | MClosureOp(closure_op, immediate)
  | MStore(list((binding, instr))) /* Items in the same list have their backpatching delayed until the end of that list */
  | MSet(binding, instr)
  | MDrop(instr) /* Ignore the result of an expression. Used for sequences. */
  | MCleanup(option(instr), list(immediate)) /* Calls decRef on items to be cleaned up. instr is evaluated first, cleanup occurs, and the value of instr is returned */

[@deriving sexp]
and block = list(instr);

[@deriving sexp]
type import_type =
  | MFuncImport(list(Types.allocation_type), list(Types.allocation_type))
  | MGlobalImport(Types.allocation_type, bool);

[@deriving sexp]
type import_kind =
  | MImportWasm
  | MImportGrain;

[@deriving sexp]
type import_setup =
  | MCallGetter
  | MWrap(int32)
  | MSetupNone;

[@deriving sexp]
type import = {
  mimp_id: Ident.t,
  mimp_mod: string,
  mimp_name: string,
  mimp_type: import_type,
  mimp_kind: import_kind,
  mimp_setup: import_setup,
  mutable mimp_used: bool,
};

[@deriving sexp]
type export =
  | WasmFunctionExport({
      ex_function_name: string,
      ex_function_internal_name: string,
    })
  | WasmGlobalExport({
      ex_global_name: string,
      ex_global_internal_name: string,
    });

[@deriving sexp]
type mash_function = {
  id: Ident.t,
  name: option(string),
  args: list(Types.allocation_type),
  return_type: list(Types.allocation_type),
  closure: option(int),
  body: block,
  stack_size,
  attrs: attributes,
  [@sexp_drop_if sexp_locs_disabled]
  func_loc: Location.t,
}
and stack_size = {
  stack_size_ptr: int,
  stack_size_i32: int,
  stack_size_i64: int,
  stack_size_f32: int,
  stack_size_f64: int,
  stack_size_v128: int,
};

[@deriving sexp]
type mash_program = {
  mash_code,
  signature: [@sexp.opaque] Cmi_format.cmi_infos,
}

and mash_code = {
  functions: list(mash_function),
  imports: list(import),
  exports: list(export),
  main_body: block,
  main_body_stack_size: stack_size,
  globals: list(mash_global),
  function_table_elements: list(string),
  global_function_table_offset: Ident.t,
  compilation_mode: Grain_utils.Config.compilation_mode,
  type_metadata: [@sexp.opaque] list(Types.type_metadata),
  [@sexp_drop_if sexp_locs_disabled]
  prog_loc: Location.t,
}

and mash_global = {
  id: Ident.t,
  mutable_: bool,
  allocation_type: Types.allocation_type,
  initial_value: option(constant),
};

let const_true = MConstLiteral(MConstI32(Int32.of_int(0xFFFFFFFE)));
let const_false = MConstLiteral(MConstI32(Int32.of_int(0x7FFFFFFE)));
let const_void = MConstLiteral(MConstI32(Int32.of_int(0x6FFFFFFE)));
