/* Stripped-down version of OCaml's typedtree. Original copyright: */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/** Typed variant of the AST. */

open Grain_parsing;
open Types;

let sexp_locs_disabled: 'a => bool;

type loc('a) = Location.loc('a);

[@deriving sexp]
type attributes = list(loc(attribute))

[@deriving sexp]
and attribute =
  | Disable_gc
  | Unsafe
  | External_name(loc(string));

type partial =
  | Partial
  | Total;

type provide_flag =
  Asttypes.provide_flag = | NotProvided | Provided | Abstract;
type rec_flag = Asttypes.rec_flag = | Nonrecursive | Recursive;
type mut_flag = Asttypes.mut_flag = | Mutable | Immutable;
type argument_label =
  Asttypes.argument_label =
    | Unlabeled | Labeled(loc(string)) | Default(loc(string));

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
    | WasmSimdShift({wasm_op});

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
      });

type core_type = {
  ctyp_desc: core_type_desc,
  ctyp_type: type_expr,
  ctyp_env: Env.t,
  ctyp_loc: Location.t,
}

and core_type_desc =
  | TTyAny
  | TTyVar(string)
  | TTyArrow(list((argument_label, core_type)), core_type)
  | TTyTuple(list(core_type))
  | TTyRecord(list((loc(Identifier.t), core_type)))
  | TTyConstr(Path.t, loc(Identifier.t), list(core_type))
  | TTyPoly(list(string), core_type);

[@deriving sexp]
type record_field = {
  rf_name: Ident.t,
  rf_type: core_type,
  rf_mutable: bool,
  [@sexp_drop_if sexp_locs_disabled]
  rf_loc: Location.t,
};

type constructor_arguments =
  | TConstrTuple(list(core_type))
  | TConstrRecord(list(record_field))
  | TConstrSingleton

[@deriving sexp]
and type_extension = {
  tyext_path: Path.t,
  tyext_txt: loc(Identifier.t),
  tyext_params: list(core_type),
  tyext_constructors: list(extension_constructor),
  tyext_loc: Location.t,
}

[@deriving sexp]
and type_exception = {
  tyexn_constructor: extension_constructor,
  tyexn_loc: Location.t,
}

[@deriving sexp]
and extension_constructor = {
  ext_id: Ident.t,
  ext_name: loc(string),
  ext_type: Types.extension_constructor,
  ext_kind: extension_constructor_kind,
  ext_loc: Location.t,
}

[@deriving sexp]
and extension_constructor_kind =
  | TExtDecl(constructor_arguments)
  | TExtRebind(Path.t, loc(Identifier.t));

[@deriving sexp]
type constructor_declaration = {
  cd_id: Ident.t,
  cd_name: loc(string),
  cd_args: constructor_arguments,
  cd_res: option(core_type),
  cd_loc: Location.t,
};

type data_kind =
  | TDataAbstract
  | TDataVariant(list(constructor_declaration))
  | TDataRecord(list(record_field));

[@deriving sexp]
type data_declaration = {
  data_id: Ident.t,
  data_name: loc(string),
  data_params: list(core_type),
  data_type: Types.type_declaration,
  data_kind,
  data_manifest: option(core_type),
  data_provided: provide_flag,
  data_loc: Location.t,
};

[@deriving sexp]
type pattern = {
  pat_desc: pattern_desc,
  pat_loc: Location.t,
  pat_extra: list((pat_extra, Location.t)),
  pat_type: type_expr,
  mutable pat_env: Env.t,
}

and pat_extra =
  | TPatConstraint(core_type)

and pattern_desc =
  | TPatAny
  | TPatVar(Ident.t, loc(string))
  | TPatConstant(constant)
  | TPatTuple(list(pattern))
  | TPatArray(list(pattern))
  | TPatRecord(
      list((loc(Identifier.t), label_description, pattern)),
      closed_flag,
    )
  | TPatConstruct(loc(Identifier.t), constructor_description, list(pattern))
  | TPatAlias(pattern, Ident.t, loc(string))
  | TPatOr(pattern, pattern);

[@deriving sexp]
type expression = {
  exp_desc: expression_desc,
  exp_loc: Location.t,
  exp_extra: list((exp_extra, Location.t)),
  exp_attributes: attributes,
  exp_type: type_expr,
  exp_env: Env.t,
}

and exp_extra =
  | TExpConstraint(core_type)

and expression_desc =
  | TExpIdent(Path.t, loc(Identifier.t), Types.value_description)
  | TExpConstant(constant)
  | TExpTuple(list(expression))
  | TExpList({
      items: list(expression),
      spread: option(expression),
    })
  | TExpArray(list(expression))
  | TExpArrayGet(expression, expression)
  | TExpArraySet({
      array: expression,
      index: expression,
      value: expression,
      infix_op: option(expression),
    })
  | TExpRecord(
      option(expression),
      array((Types.label_description, record_label_definition)),
    )
  | TExpRecordGet(expression, loc(Identifier.t), Types.label_description)
  | TExpRecordSet(
      expression,
      loc(Identifier.t),
      Types.label_description,
      expression,
    )
  | TExpLet(rec_flag, mut_flag, list(value_binding))
  | TExpMatch(expression, list(match_branch), partial)
  | TExpUse(loc(Path.t), use_items)
  | TExpPrim0(prim0)
  | TExpPrim1(prim1, expression)
  | TExpPrim2(prim2, expression, expression)
  | TExpPrimN(primn, list(expression))
  | TExpBoxAssign(expression, expression)
  | TExpAssign(expression, expression)
  | TExpIf(expression, expression, expression)
  | TExpWhile(expression, expression)
  | TExpFor(
      option(expression),
      option(expression),
      option(expression),
      expression,
    )
  | TExpContinue
  | TExpBreak
  | TExpReturn(option(expression))
  | TExpLambda(list(match_branch), partial)
  | TExpApp(expression, list(argument_label), list(argument_value))
  | TExpConstruct(
      loc(Identifier.t),
      constructor_description,
      constructor_expression,
    )
  | TExpBlock(list(expression))

and constructor_expression =
  | TExpConstrTuple(list(expression))
  | TExpConstrRecord(
      array((Types.label_description, record_label_definition)),
    )

and record_label_definition =
  | Kept
  | Overridden(loc(Identifier.t), expression)

and value_binding = {
  vb_pat: pattern,
  vb_expr: expression,
  vb_loc: Location.t,
}

and match_branch = {
  mb_pat: pattern,
  mb_body: expression,
  mb_guard: option(expression),
  mb_loc: Location.t,
}

[@deriving sexp]
and argument_value = {
  arg_label: argument_label,
  arg_label_specified: bool,
  arg_expr: expression,
};

[@deriving sexp]
type include_declaration = {
  tinc_path: Path.t,
  tinc_loc: Location.t,
};

[@deriving sexp]
type provide_declaration = {
  tex_id: Ident.t,
  tex_path: Path.t,
  [@sexp_drop_if sexp_locs_disabled]
  tex_loc: Location.t,
};

[@deriving sexp]
type value_description = {
  tvd_id: Ident.t,
  tvd_mod: loc(string),
  tvd_name: loc(string),
  tvd_desc: core_type,
  tvd_val: Types.value_description,
  [@sexp_drop_if sexp_locs_disabled]
  tvd_loc: Location.t,
};

[@deriving sexp]
type module_declaration = {
  tmod_id: Ident.t,
  tmod_decl: Types.module_declaration,
  tmod_statements: list(toplevel_stmt),
  tmod_provided: provide_flag,
  [@sexp_drop_if sexp_locs_disabled]
  tmod_loc: Location.t,
}

and toplevel_stmt_desc =
  | TTopForeign(value_description)
  | TTopInclude(include_declaration)
  | TTopProvide(list(provide_declaration))
  | TTopData(list(data_declaration))
  | TTopModule(module_declaration)
  | TTopLet(rec_flag, mut_flag, list(value_binding))
  | TTopException(extension_constructor)
  | TTopExpr(expression)

[@deriving sexp]
and toplevel_stmt = {
  ttop_desc: toplevel_stmt_desc,
  ttop_attributes: attributes,
  ttop_loc: Location.t,
  ttop_env: Env.t,
};

[@deriving (sexp, yojson)]
type comment_desc =
  Parsetree.comment_desc = {
    cmt_content: string,
    cmt_source: string,
    cmt_loc: Location.t,
  };

[@deriving (sexp, yojson)]
type comment =
  Parsetree.comment =
    | Line(comment_desc)
    | Shebang(comment_desc)
    | Block(comment_desc)
    | Doc(comment_desc);

[@deriving sexp]
type typed_program = {
  module_name: loc(string),
  statements: list(toplevel_stmt),
  env: Env.t,
  signature: Cmi_format.cmi_infos,
  comments: list(comment),
  prog_loc: Location.t,
};

/* Auxiliary functions over the AST */

let iter_pattern_desc: (pattern => unit, pattern_desc) => unit;
let map_pattern_desc: (pattern => pattern, pattern_desc) => pattern_desc;

let exists_pattern: (pattern => bool, pattern) => bool;

let let_bound_idents: list(value_binding) => list(Ident.t);
let rev_let_bound_idents: list(value_binding) => list(Ident.t);

let let_bound_idents_with_loc:
  list(value_binding) => list((Ident.t, loc(string)));

/** Alpha conversion of patterns */

let alpha_pat: (list((Ident.t, Ident.t)), pattern) => pattern;

let mknoloc: 'a => Asttypes.loc('a);
let mkloc: ('a, Location.t) => Asttypes.loc('a);

let pattern_bound_idents: pattern => list(Ident.t);
