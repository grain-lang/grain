open Binaryen;
open Comp_utils;

module OpHash =
  Hashtbl.Make({
    type t = Mashtree.wasm_op;
    let hash = x => Hashtbl.hash(x);
    let equal = Stdlib.(==);
  });

let op_map = {
  Mashtree.(
    OpHash.of_seq(
      List.to_seq([
        (Op_clz_int32, Op.clz_int32),
        (Op_ctz_int32, Op.ctz_int32),
        (Op_popcnt_int32, Op.popcnt_int32),
        (Op_neg_float32, Op.neg_float32),
        (Op_abs_float32, Op.abs_float32),
        (Op_ceil_float32, Op.ceil_float32),
        (Op_floor_float32, Op.floor_float32),
        (Op_trunc_float32, Op.trunc_float32),
        (Op_nearest_float32, Op.nearest_float32),
        (Op_sqrt_float32, Op.sqrt_float32),
        (Op_eq_z_int32, Op.eq_z_int32),
        (Op_clz_int64, Op.clz_int64),
        (Op_ctz_int64, Op.ctz_int64),
        (Op_popcnt_int64, Op.popcnt_int64),
        (Op_neg_float64, Op.neg_float64),
        (Op_abs_float64, Op.abs_float64),
        (Op_ceil_float64, Op.ceil_float64),
        (Op_floor_float64, Op.floor_float64),
        (Op_trunc_float64, Op.trunc_float64),
        (Op_nearest_float64, Op.nearest_float64),
        (Op_sqrt_float64, Op.sqrt_float64),
        (Op_eq_z_int64, Op.eq_z_int64),
        (Op_extend_s_int32, Op.extend_s_int32),
        (Op_extend_u_int32, Op.extend_u_int32),
        (Op_wrap_int64, Op.wrap_int64),
        (Op_trunc_s_float32_to_int32, Op.trunc_s_float32_to_int32),
        (Op_trunc_s_float32_to_int64, Op.trunc_s_float32_to_int64),
        (Op_trunc_u_float32_to_int32, Op.trunc_u_float32_to_int32),
        (Op_trunc_u_float32_to_int64, Op.trunc_u_float32_to_int64),
        (Op_trunc_s_float64_to_int32, Op.trunc_s_float64_to_int32),
        (Op_trunc_s_float64_to_int64, Op.trunc_s_float64_to_int64),
        (Op_trunc_u_float64_to_int32, Op.trunc_u_float64_to_int32),
        (Op_trunc_u_float64_to_int64, Op.trunc_u_float64_to_int64),
        (Op_reinterpret_float32, Op.reinterpret_float32),
        (Op_reinterpret_float64, Op.reinterpret_float64),
        (Op_convert_s_int32_to_float32, Op.convert_s_int32_to_float32),
        (Op_convert_s_int32_to_float64, Op.convert_s_int32_to_float64),
        (Op_convert_u_int32_to_float32, Op.convert_u_int32_to_float32),
        (Op_convert_u_int32_to_float64, Op.convert_u_int32_to_float64),
        (Op_convert_s_int64_to_float32, Op.convert_s_int64_to_float32),
        (Op_convert_s_int64_to_float64, Op.convert_s_int64_to_float64),
        (Op_convert_u_int64_to_float32, Op.convert_u_int64_to_float32),
        (Op_convert_u_int64_to_float64, Op.convert_u_int64_to_float64),
        (Op_promote_float32, Op.promote_float32),
        (Op_demote_float64, Op.demote_float64),
        (Op_reinterpret_int32, Op.reinterpret_int32),
        (Op_reinterpret_int64, Op.reinterpret_int64),
        (Op_extend_s8_int32, Op.extend_s8_int32),
        (Op_extend_s16_int32, Op.extend_s16_int32),
        (Op_extend_s8_int64, Op.extend_s8_int64),
        (Op_extend_s16_int64, Op.extend_s16_int64),
        (Op_extend_s32_int64, Op.extend_s32_int64),
        (Op_add_int32, Op.add_int32),
        (Op_sub_int32, Op.sub_int32),
        (Op_mul_int32, Op.mul_int32),
        (Op_div_s_int32, Op.div_s_int32),
        (Op_div_u_int32, Op.div_u_int32),
        (Op_rem_s_int32, Op.rem_s_int32),
        (Op_rem_u_int32, Op.rem_u_int32),
        (Op_and_int32, Op.and_int32),
        (Op_or_int32, Op.or_int32),
        (Op_xor_int32, Op.xor_int32),
        (Op_shl_int32, Op.shl_int32),
        (Op_shr_u_int32, Op.shr_u_int32),
        (Op_shr_s_int32, Op.shr_s_int32),
        (Op_rot_l_int32, Op.rot_l_int32),
        (Op_rot_r_int32, Op.rot_r_int32),
        (Op_eq_int32, Op.eq_int32),
        (Op_ne_int32, Op.ne_int32),
        (Op_lt_s_int32, Op.lt_s_int32),
        (Op_lt_u_int32, Op.lt_u_int32),
        (Op_le_s_int32, Op.le_s_int32),
        (Op_le_u_int32, Op.le_u_int32),
        (Op_gt_s_int32, Op.gt_s_int32),
        (Op_gt_u_int32, Op.gt_u_int32),
        (Op_ge_s_int32, Op.ge_s_int32),
        (Op_ge_u_int32, Op.ge_u_int32),
        (Op_add_int64, Op.add_int64),
        (Op_sub_int64, Op.sub_int64),
        (Op_mul_int64, Op.mul_int64),
        (Op_div_s_int64, Op.div_s_int64),
        (Op_div_u_int64, Op.div_u_int64),
        (Op_rem_s_int64, Op.rem_s_int64),
        (Op_rem_u_int64, Op.rem_u_int64),
        (Op_and_int64, Op.and_int64),
        (Op_or_int64, Op.or_int64),
        (Op_xor_int64, Op.xor_int64),
        (Op_shl_int64, Op.shl_int64),
        (Op_shr_u_int64, Op.shr_u_int64),
        (Op_shr_s_int64, Op.shr_s_int64),
        (Op_rot_l_int64, Op.rot_l_int64),
        (Op_rot_r_int64, Op.rot_r_int64),
        (Op_eq_int64, Op.eq_int64),
        (Op_ne_int64, Op.ne_int64),
        (Op_lt_s_int64, Op.lt_s_int64),
        (Op_lt_u_int64, Op.lt_u_int64),
        (Op_le_s_int64, Op.le_s_int64),
        (Op_le_u_int64, Op.le_u_int64),
        (Op_gt_s_int64, Op.gt_s_int64),
        (Op_gt_u_int64, Op.gt_u_int64),
        (Op_ge_s_int64, Op.ge_s_int64),
        (Op_ge_u_int64, Op.ge_u_int64),
        (Op_add_float32, Op.add_float32),
        (Op_sub_float32, Op.sub_float32),
        (Op_mul_float32, Op.mul_float32),
        (Op_div_float32, Op.div_float32),
        (Op_copy_sign_float32, Op.copy_sign_float32),
        (Op_min_float32, Op.min_float32),
        (Op_max_float32, Op.max_float32),
        (Op_eq_float32, Op.eq_float32),
        (Op_ne_float32, Op.ne_float32),
        (Op_lt_float32, Op.lt_float32),
        (Op_le_float32, Op.le_float32),
        (Op_gt_float32, Op.gt_float32),
        (Op_ge_float32, Op.ge_float32),
        (Op_add_float64, Op.add_float64),
        (Op_sub_float64, Op.sub_float64),
        (Op_mul_float64, Op.mul_float64),
        (Op_div_float64, Op.div_float64),
        (Op_copy_sign_float64, Op.copy_sign_float64),
        (Op_min_float64, Op.min_float64),
        (Op_max_float64, Op.max_float64),
        (Op_eq_float64, Op.eq_float64),
        (Op_ne_float64, Op.ne_float64),
        (Op_lt_float64, Op.lt_float64),
        (Op_le_float64, Op.le_float64),
        (Op_gt_float64, Op.gt_float64),
        (Op_ge_float64, Op.ge_float64),
        (Op_atomic_rmw_add, Op.atomic_rmw_add),
        (Op_atomic_rmw_sub, Op.atomic_rmw_sub),
        (Op_atomic_rmw_and, Op.atomic_rmw_and),
        (Op_atomic_rmw_or, Op.atomic_rmw_or),
        (Op_atomic_rmw_xor, Op.atomic_rmw_xor),
        (Op_atomic_rmw_xchg, Op.atomic_rmw_xchg),
        (Op_trunc_sat_s_float32_to_int32, Op.trunc_sat_s_float32_to_int32),
        (Op_trunc_sat_s_float32_to_int64, Op.trunc_sat_s_float32_to_int64),
        (Op_trunc_sat_u_float32_to_int32, Op.trunc_sat_u_float32_to_int32),
        (Op_trunc_sat_u_float32_to_int64, Op.trunc_sat_u_float32_to_int64),
        (Op_trunc_sat_s_float64_to_int32, Op.trunc_sat_s_float64_to_int32),
        (Op_trunc_sat_s_float64_to_int64, Op.trunc_sat_s_float64_to_int64),
        (Op_trunc_sat_u_float64_to_int32, Op.trunc_sat_u_float64_to_int32),
        (Op_trunc_sat_u_float64_to_int64, Op.trunc_sat_u_float64_to_int64),
        (Op_splat_vec_i8x16, Op.splat_vec_i8x16),
        (Op_extract_lane_s_vec_i8x16, Op.extract_lane_s_vec_i8x16),
        (Op_extract_lane_u_vec_i8x16, Op.extract_lane_u_vec_i8x16),
        (Op_replace_lane_vec_i8x16, Op.replace_lane_vec_i8x16),
        (Op_splat_vec_i16x8, Op.splat_vec_i16x8),
        (Op_extract_lane_s_vec_i16x8, Op.extract_lane_s_vec_i16x8),
        (Op_extract_lane_u_vec_i16x8, Op.extract_lane_u_vec_i16x8),
        (Op_replace_lane_vec_i16x8, Op.replace_lane_vec_i16x8),
        (Op_splat_vec_i32x4, Op.splat_vec_i32x4),
        (Op_extract_lane_vec_i32x4, Op.extract_lane_vec_i32x4),
        (Op_replace_lane_vec_i32x4, Op.replace_lane_vec_i32x4),
        (Op_splat_vec_i64x2, Op.splat_vec_i64x2),
        (Op_extract_lane_vec_i64x2, Op.extract_lane_vec_i64x2),
        (Op_replace_lane_vec_i64x2, Op.replace_lane_vec_i64x2),
        (Op_splat_vec_f32x4, Op.splat_vec_f32x4),
        (Op_extract_lane_vec_f32x4, Op.extract_lane_vec_f32x4),
        (Op_replace_lane_vec_f32x4, Op.replace_lane_vec_f32x4),
        (Op_splat_vec_f64x2, Op.splat_vec_f64x2),
        (Op_extract_lane_vec_f64x2, Op.extract_lane_vec_f64x2),
        (Op_replace_lane_vec_f64x2, Op.replace_lane_vec_f64x2),
        (Op_eq_vec_i8x16, Op.eq_vec_i8x16),
        (Op_ne_vec_i8x16, Op.ne_vec_i8x16),
        (Op_lt_s_vec_i8x16, Op.lt_s_vec_i8x16),
        (Op_lt_u_vec_i8x16, Op.lt_u_vec_i8x16),
        (Op_gt_s_vec_i8x16, Op.gt_s_vec_i8x16),
        (Op_gt_u_vec_i8x16, Op.gt_u_vec_i8x16),
        (Op_le_s_vec_i8x16, Op.le_s_vec_i8x16),
        (Op_le_u_vec_i8x16, Op.le_u_vec_i8x16),
        (Op_ge_s_vec_i8x16, Op.ge_s_vec_i8x16),
        (Op_ge_u_vec_i8x16, Op.ge_u_vec_i8x16),
        (Op_eq_vec_i16x8, Op.eq_vec_i16x8),
        (Op_ne_vec_i16x8, Op.ne_vec_i16x8),
        (Op_lt_s_vec_i16x8, Op.lt_s_vec_i16x8),
        (Op_lt_u_vec_i16x8, Op.lt_u_vec_i16x8),
        (Op_gt_s_vec_i16x8, Op.gt_s_vec_i16x8),
        (Op_gt_u_vec_i16x8, Op.gt_u_vec_i16x8),
        (Op_le_s_vec_i16x8, Op.le_s_vec_i16x8),
        (Op_le_u_vec_i16x8, Op.le_u_vec_i16x8),
        (Op_ge_s_vec_i16x8, Op.ge_s_vec_i16x8),
        (Op_ge_u_vec_i16x8, Op.ge_u_vec_i16x8),
        (Op_eq_vec_i32x4, Op.eq_vec_i32x4),
        (Op_ne_vec_i32x4, Op.ne_vec_i32x4),
        (Op_lt_s_vec_i32x4, Op.lt_s_vec_i32x4),
        (Op_lt_u_vec_i32x4, Op.lt_u_vec_i32x4),
        (Op_gt_s_vec_i32x4, Op.gt_s_vec_i32x4),
        (Op_gt_u_vec_i32x4, Op.gt_u_vec_i32x4),
        (Op_le_s_vec_i32x4, Op.le_s_vec_i32x4),
        (Op_le_u_vec_i32x4, Op.le_u_vec_i32x4),
        (Op_ge_s_vec_i32x4, Op.ge_s_vec_i32x4),
        (Op_ge_u_vec_i32x4, Op.ge_u_vec_i32x4),
        (Op_eq_vec_f32x4, Op.eq_vec_f32x4),
        (Op_ne_vec_f32x4, Op.ne_vec_f32x4),
        (Op_lt_vec_f32x4, Op.lt_vec_f32x4),
        (Op_gt_vec_f32x4, Op.gt_vec_f32x4),
        (Op_le_vec_f32x4, Op.le_vec_f32x4),
        (Op_ge_vec_f32x4, Op.ge_vec_f32x4),
        (Op_eq_vec_f64x2, Op.eq_vec_f64x2),
        (Op_ne_vec_f64x2, Op.ne_vec_f64x2),
        (Op_lt_vec_f64x2, Op.lt_vec_f64x2),
        (Op_gt_vec_f64x2, Op.gt_vec_f64x2),
        (Op_le_vec_f64x2, Op.le_vec_f64x2),
        (Op_ge_vec_f64x2, Op.ge_vec_f64x2),
        (Op_not_vec128, Op.not_vec128),
        (Op_and_vec128, Op.and_vec128),
        (Op_or_vec128, Op.or_vec128),
        (Op_xor_vec128, Op.xor_vec128),
        (Op_and_not_vec128, Op.and_not_vec128),
        (Op_bitselect_vec128, Op.bitselect_vec128),
        (Op_relaxed_fma_vec_f32x4, Op.relaxed_fma_vec_f32x4),
        (Op_relaxed_fms_vec_f32x4, Op.relaxed_fms_vec_f32x4),
        (Op_relaxed_fma_vec_f64x4, Op.relaxed_fma_vec_f64x4),
        (Op_relaxed_fms_vec_f64x4, Op.relaxed_fms_vec_f64x4),
        (Op_laneselect_i8x16, Op.laneselect_i8x16),
        (Op_laneselect_i16x8, Op.laneselect_i16x8),
        (Op_laneselect_i32x4, Op.laneselect_i32x4),
        (Op_laneselect_i64x2, Op.laneselect_i64x2),
        (
          Op_dot_i8x16_i7x16_add_s_to_vec_i32x4,
          Op.dot_i8x16_i7x16_add_s_to_vec_i32x4,
        ),
        (Op_any_true_vec128, Op.any_true_vec128),
        (Op_popcnt_vec_i8x16, Op.popcnt_vec_i8x16),
        (Op_abs_vec_i8x16, Op.abs_vec_i8x16),
        (Op_neg_vec_i8x16, Op.neg_vec_i8x16),
        (Op_all_true_vec_i8x16, Op.all_true_vec_i8x16),
        (Op_bitmask_vec_i8x16, Op.bitmask_vec_i8x16),
        (Op_shl_vec_i8x16, Op.shl_vec_i8x16),
        (Op_shr_s_vec_i8x16, Op.shr_s_vec_i8x16),
        (Op_shr_u_vec_i8x16, Op.shr_u_vec_i8x16),
        (Op_add_vec_i8x16, Op.add_vec_i8x16),
        (Op_add_sat_s_vec_i8x16, Op.add_sat_s_vec_i8x16),
        (Op_add_sat_u_vec_i8x16, Op.add_sat_u_vec_i8x16),
        (Op_sub_vec_i8x16, Op.sub_vec_i8x16),
        (Op_sub_sat_s_vec_i8x16, Op.sub_sat_s_vec_i8x16),
        (Op_sub_sat_u_vec_i8x16, Op.sub_sat_u_vec_i8x16),
        (Op_min_s_vec_i8x16, Op.min_s_vec_i8x16),
        (Op_min_u_vec_i8x16, Op.min_u_vec_i8x16),
        (Op_max_s_vec_i8x16, Op.max_s_vec_i8x16),
        (Op_max_u_vec_i8x16, Op.max_u_vec_i8x16),
        (Op_avgr_u_vec_i8x16, Op.avgr_u_vec_i8x16),
        (Op_abs_vec_i16x8, Op.abs_vec_i16x8),
        (Op_neg_vec_i16x8, Op.neg_vec_i16x8),
        (Op_all_true_vec_i16x8, Op.all_true_vec_i16x8),
        (Op_bitmask_vec_i16x8, Op.bitmask_vec_i16x8),
        (Op_shl_vec_i16x8, Op.shl_vec_i16x8),
        (Op_shr_s_vec_i16x8, Op.shr_s_vec_i16x8),
        (Op_shr_u_vec_i16x8, Op.shr_u_vec_i16x8),
        (Op_add_vec_i16x8, Op.add_vec_i16x8),
        (Op_add_sat_s_vec_i16x8, Op.add_sat_s_vec_i16x8),
        (Op_add_sat_u_vec_i16x8, Op.add_sat_u_vec_i16x8),
        (Op_sub_vec_i16x8, Op.sub_vec_i16x8),
        (Op_sub_sat_s_vec_i16x8, Op.sub_sat_s_vec_i16x8),
        (Op_sub_sat_u_vec_i16x8, Op.sub_sat_u_vec_i16x8),
        (Op_mul_vec_i16x8, Op.mul_vec_i16x8),
        (Op_min_s_vec_i16x8, Op.min_s_vec_i16x8),
        (Op_min_u_vec_i16x8, Op.min_u_vec_i16x8),
        (Op_max_s_vec_i16x8, Op.max_s_vec_i16x8),
        (Op_max_u_vec_i16x8, Op.max_u_vec_i16x8),
        (Op_avgr_u_vec_i16x8, Op.avgr_u_vec_i16x8),
        (Op_q15_mulr_sat_s_vec_i16x8, Op.q15_mulr_sat_s_vec_i16x8),
        (Op_ext_mul_low_s_vec_i16x8, Op.ext_mul_low_s_vec_i16x8),
        (Op_ext_mul_high_s_vec_i16x8, Op.ext_mul_high_s_vec_i16x8),
        (Op_ext_mul_low_u_vec_i16x8, Op.ext_mul_low_u_vec_i16x8),
        (Op_ext_mul_high_u_vec_i16x8, Op.ext_mul_high_u_vec_i16x8),
        (Op_abs_vec_i32x4, Op.abs_vec_i32x4),
        (Op_neg_vec_i32x4, Op.neg_vec_i32x4),
        (Op_all_true_vec_i32x4, Op.all_true_vec_i32x4),
        (Op_bitmask_vec_i32x4, Op.bitmask_vec_i32x4),
        (Op_shl_vec_i32x4, Op.shl_vec_i32x4),
        (Op_shr_s_vec_i32x4, Op.shr_s_vec_i32x4),
        (Op_shr_u_vec_i32x4, Op.shr_u_vec_i32x4),
        (Op_add_vec_i32x4, Op.add_vec_i32x4),
        (Op_sub_vec_i32x4, Op.sub_vec_i32x4),
        (Op_mul_vec_i32x4, Op.mul_vec_i32x4),
        (Op_min_s_vec_i32x4, Op.min_s_vec_i32x4),
        (Op_min_u_vec_i32x4, Op.min_u_vec_i32x4),
        (Op_max_s_vec_i32x4, Op.max_s_vec_i32x4),
        (Op_max_u_vec_i32x4, Op.max_u_vec_i32x4),
        (Op_dot_s_vec_i16x8_to_vec_i32x4, Op.dot_s_vec_i16x8_to_vec_i32x4),
        (Op_neg_vec_i64x2, Op.neg_vec_i64x2),
        (Op_shl_vec_i64x2, Op.shl_vec_i64x2),
        (Op_shr_s_vec_i64x2, Op.shr_s_vec_i64x2),
        (Op_shr_u_vec_i64x2, Op.shr_u_vec_i64x2),
        (Op_add_vec_i64x2, Op.add_vec_i64x2),
        (Op_sub_vec_i64x2, Op.sub_vec_i64x2),
        (Op_mul_vec_i64x2, Op.mul_vec_i64x2),
        (Op_abs_vec_f32x4, Op.abs_vec_f32x4),
        (Op_neg_vec_f32x4, Op.neg_vec_f32x4),
        (Op_sqrt_vec_f32x4, Op.sqrt_vec_f32x4),
        (Op_add_vec_f32x4, Op.add_vec_f32x4),
        (Op_sub_vec_f32x4, Op.sub_vec_f32x4),
        (Op_mul_vec_f32x4, Op.mul_vec_f32x4),
        (Op_div_vec_f32x4, Op.div_vec_f32x4),
        (Op_min_vec_f32x4, Op.min_vec_f32x4),
        (Op_max_vec_f32x4, Op.max_vec_f32x4),
        (Op_p_min_vec_f32x4, Op.p_min_vec_f32x4),
        (Op_p_max_vec_f32x4, Op.p_max_vec_f32x4),
        (Op_ceil_vec_f32x4, Op.ceil_vec_f32x4),
        (Op_floor_vec_f32x4, Op.floor_vec_f32x4),
        (Op_trunc_vec_f32x4, Op.trunc_vec_f32x4),
        (Op_nearest_vec_f32x4, Op.nearest_vec_f32x4),
        (Op_abs_vec_f64x2, Op.abs_vec_f64x2),
        (Op_neg_vec_f64x2, Op.neg_vec_f64x2),
        (Op_sqrt_vec_f64x2, Op.sqrt_vec_f64x2),
        (Op_add_vec_f64x2, Op.add_vec_f64x2),
        (Op_sub_vec_f64x2, Op.sub_vec_f64x2),
        (Op_mul_vec_f64x2, Op.mul_vec_f64x2),
        (Op_div_vec_f64x2, Op.div_vec_f64x2),
        (Op_min_vec_f64x2, Op.min_vec_f64x2),
        (Op_max_vec_f64x2, Op.max_vec_f64x2),
        (Op_p_min_vec_f64x2, Op.p_min_vec_f64x2),
        (Op_p_max_vec_f64x2, Op.p_max_vec_f64x2),
        (Op_ceil_vec_f64x2, Op.ceil_vec_f64x2),
        (Op_floor_vec_f64x2, Op.floor_vec_f64x2),
        (Op_trunc_vec_f64x2, Op.trunc_vec_f64x2),
        (Op_nearest_vec_f64x2, Op.nearest_vec_f64x2),
        (
          Op_ext_add_pairwise_s_vec_i8x16_to_i16x8,
          Op.ext_add_pairwise_s_vec_i8x16_to_i16x8,
        ),
        (
          Op_ext_add_pairwise_u_vec_i8x16_to_i16x8,
          Op.ext_add_pairwise_u_vec_i8x16_to_i16x8,
        ),
        (
          Op_ext_add_pairwise_s_vec_i16x8_to_i32x4,
          Op.ext_add_pairwise_s_vec_i16x8_to_i32x4,
        ),
        (
          Op_ext_add_pairwise_u_vec_i16x8_to_i32x4,
          Op.ext_add_pairwise_u_vec_i16x8_to_i32x4,
        ),
        (
          Op_trunc_sat_s_vec_f32x4_to_vec_i32x4,
          Op.trunc_sat_s_vec_f32x4_to_vec_i32x4,
        ),
        (
          Op_trunc_sat_u_vec_f32x4_to_vec_i32x4,
          Op.trunc_sat_u_vec_f32x4_to_vec_i32x4,
        ),
        (
          Op_convert_s_vec_i32x4_to_vec_f32x4,
          Op.convert_s_vec_i32x4_to_vec_f32x4,
        ),
        (
          Op_convert_u_vec_i32x4_to_vec_f32x4,
          Op.convert_u_vec_i32x4_to_vec_f32x4,
        ),
        (Op_load8_splat_vec128, Op.load8_splat_vec128),
        (Op_load16_splat_vec128, Op.load16_splat_vec128),
        (Op_load32_splat_vec128, Op.load32_splat_vec128),
        (Op_load64_splat_vec128, Op.load64_splat_vec128),
        (Op_load8x8_s_vec128, Op.load8x8_s_vec128),
        (Op_load8x8_u_vec128, Op.load8x8_u_vec128),
        (Op_load16x4_s_vec128, Op.load16x4_s_vec128),
        (Op_load16x4_u_vec128, Op.load16x4_u_vec128),
        (Op_load32x2_s_vec128, Op.load32x2_s_vec128),
        (Op_load32x2_u_vec128, Op.load32x2_u_vec128),
        (Op_load32_zero_vec128, Op.load32_zero_vec128),
        (Op_load64_zero_vec128, Op.load64_zero_vec128),
        (Op_load8_lane_vec128, Op.load8_lane_vec128),
        (Op_load16_lane_vec128, Op.load16_lane_vec128),
        (Op_load32_lane_vec128, Op.load32_lane_vec128),
        (Op_load64_lane_vec128, Op.load64_lane_vec128),
        (Op_store8_lane_vec128, Op.store8_lane_vec128),
        (Op_store16_lane_vec128, Op.store16_lane_vec128),
        (Op_store32_lane_vec128, Op.store32_lane_vec128),
        (Op_store64_lane_vec128, Op.store64_lane_vec128),
        (
          Op_narrow_s_vec_i16x8_to_vec_i8x16,
          Op.narrow_s_vec_i16x8_to_vec_i8x16,
        ),
        (
          Op_narrow_u_vec_i16x8_to_vec_i8x16,
          Op.narrow_u_vec_i16x8_to_vec_i8x16,
        ),
        (
          Op_narrow_s_vec_i32x4_to_vec_i16x8,
          Op.narrow_s_vec_i32x4_to_vec_i16x8,
        ),
        (
          Op_narrow_u_vec_i32x4_to_vec_i16x8,
          Op.narrow_u_vec_i32x4_to_vec_i16x8,
        ),
        (
          Op_extend_low_s_vec_i8x16_to_vec_i16x8,
          Op.extend_low_s_vec_i8x16_to_vec_i16x8,
        ),
        (
          Op_extend_high_s_vec_i8x16_to_vec_i16x8,
          Op.extend_high_s_vec_i8x16_to_vec_i16x8,
        ),
        (
          Op_extend_low_u_vec_i8x16_to_vec_i16x8,
          Op.extend_low_u_vec_i8x16_to_vec_i16x8,
        ),
        (
          Op_extend_high_u_vec_i8x16_to_vec_i16x8,
          Op.extend_high_u_vec_i8x16_to_vec_i16x8,
        ),
        (
          Op_extend_low_s_vec_i16x8_to_vec_i32x4,
          Op.extend_low_s_vec_i16x8_to_vec_i32x4,
        ),
        (
          Op_extend_high_s_vec_i16x8_to_vec_i32x4,
          Op.extend_high_s_vec_i16x8_to_vec_i32x4,
        ),
        (
          Op_extend_low_u_vec_i16x8_to_vec_i32x4,
          Op.extend_low_u_vec_i16x8_to_vec_i32x4,
        ),
        (
          Op_extend_high_u_vec_i16x8_to_vec_i32x4,
          Op.extend_high_u_vec_i16x8_to_vec_i32x4,
        ),
        (
          Op_extend_low_s_vec_i32x4_to_vec_i64x2,
          Op.extend_low_s_vec_i32x4_to_vec_i64x2,
        ),
        (
          Op_extend_high_s_vec_i32x4_to_vec_i64x2,
          Op.extend_high_s_vec_i32x4_to_vec_i64x2,
        ),
        (
          Op_extend_low_u_vec_i32x4_to_vec_i64x2,
          Op.extend_low_u_vec_i32x4_to_vec_i64x2,
        ),
        (
          Op_extend_high_u_vec_i32x4_to_vec_i64x2,
          Op.extend_high_u_vec_i32x4_to_vec_i64x2,
        ),
        (
          Op_convert_low_s_vec_i32x4_to_vec_f64x2,
          Op.convert_low_s_vec_i32x4_to_vec_f64x2,
        ),
        (
          Op_convert_low_u_vec_i32x4_to_vec_f64x2,
          Op.convert_low_u_vec_i32x4_to_vec_f64x2,
        ),
        (
          Op_trunc_sat_zero_s_vec_f64x2_to_vec_i32x4,
          Op.trunc_sat_zero_s_vec_f64x2_to_vec_i32x4,
        ),
        (
          Op_trunc_sat_zero_u_vec_f64x2_to_vec_i32x4,
          Op.trunc_sat_zero_u_vec_f64x2_to_vec_i32x4,
        ),
        (
          Op_demote_zero_vec_f64x2_to_vec_f32x4,
          Op.demote_zero_vec_f64x2_to_vec_f32x4,
        ),
        (
          Op_promote_low_vec_f32x4_to_vec_f64x2,
          Op.promote_low_vec_f32x4_to_vec_f64x2,
        ),
        (
          Op_relaxed_trunc_s_vec_f32x4_to_vec_i32x4,
          Op.relaxed_trunc_s_vec_f32x4_to_vec_i32x4,
        ),
        (
          Op_relaxed_trunc_u_vec_f32x4_to_vec_i32x4,
          Op.relaxed_trunc_u_vec_f32x4_to_vec_i32x4,
        ),
        (
          Op_relaxed_trunc_zero_s_vec_f64x2_to_vec_i32x4,
          Op.relaxed_trunc_zero_s_vec_f64x2_to_vec_i32x4,
        ),
        (
          Op_relaxed_trunc_zero_u_vec_f64x2_to_vec_i32x4,
          Op.relaxed_trunc_zero_u_vec_f64x2_to_vec_i32x4,
        ),
        (Op_swizzle_vec8x16, Op.swizzle_vec8x16),
        (Op_relaxed_swizzle_vec_i8x16, Op.relaxed_swizzle_vec_i8x16),
        (Op_relaxed_min_vec_f32x4, Op.relaxed_min_vec_f32x4),
        (Op_relaxed_max_vec_f32x4, Op.relaxed_max_vec_f32x4),
        (Op_relaxed_min_vec_f64x2, Op.relaxed_min_vec_f64x2),
        (Op_relaxed_max_vec_f64x2, Op.relaxed_max_vec_f64x2),
        (Op_relaxed_q15_mulr_s_vec_i16x8, Op.relaxed_q15_mulr_s_vec_i16x8),
        (
          Op_dot_i8x16_i7x16_s_to_vec_i16x8,
          Op.dot_i8x16_i7x16_s_to_vec_i16x8,
        ),
        (Op_ref_as_non_null, Op.ref_as_non_null),
        (Op_ref_as_extern_internalize, Op.ref_as_extern_internalize),
        (Op_ref_as_extern_externalize, Op.ref_as_extern_externalize),
        (Op_br_on_null, Op.br_on_null),
        (Op_br_on_non_null, Op.br_on_non_null),
        (Op_br_on_cast, Op.br_on_cast),
        (Op_br_on_cast_fail, Op.br_on_cast_fail),
      ]),
    )
  );
};

let get_op = name =>
  try(OpHash.find(op_map, name)) {
  | Not_found =>
    failwith(Printf.sprintf("internal: Wasm instruction not registered"))
  };

let compile_wasm_prim1 = (wasm_mod, env, instr, ret_type, arg) => {
  let op = Expression.Unary.make(wasm_mod, get_op(instr), arg);
  switch (ret_type) {
  | Mashtree.Grain_bool =>
    Expression.Select.make(
      wasm_mod,
      op,
      Expression.Const.make(wasm_mod, const_true()),
      Expression.Const.make(wasm_mod, const_false()),
    )
  | _ => op
  };
};

let compile_wasm_prim2 = (wasm_mod, env, instr, ret_type, arg1, arg2) => {
  let op = Expression.Binary.make(wasm_mod, get_op(instr), arg1, arg2);
  switch (ret_type) {
  | Mashtree.Grain_bool =>
    Expression.Select.make(
      wasm_mod,
      op,
      Expression.Const.make(wasm_mod, const_true()),
      Expression.Const.make(wasm_mod, const_false()),
    )
  | _ => op
  };
};

let compile_wasm_simd_prim3 = (wasm_mod, instr, arg1, arg2, arg3) => {
  Expression.SIMD_ternary.make(wasm_mod, get_op(instr), arg1, arg2, arg3);
};

let compile_wasm_simd_extract = (wasm_mod, instr, vec, lane) => {
  Mashtree.(
    switch (lane.immediate_desc) {
    | MImmConst(MConstLiteral(MConstI32(lane))) =>
      Expression.SIMD_extract.make(
        wasm_mod,
        get_op(instr),
        vec,
        Int32.to_int(lane),
      )

    // The wrapper function generated for this can never provide an immediate
    // lane value, but not generating the wrapper function would require
    // completely refactoring the way primitives work, which isn't worth the
    // effort since this is always inlined.
    | _ => Expression.Unreachable.make(wasm_mod)
    }
  );
};

let compile_wasm_simd_replace = (wasm_mod, instr, vec, lane, value) => {
  Mashtree.(
    switch (lane.immediate_desc) {
    | MImmConst(MConstLiteral(MConstI32(lane))) =>
      Expression.SIMD_replace.make(
        wasm_mod,
        get_op(instr),
        vec,
        Int32.to_int(lane),
        value,
      )

    // The wrapper function generated for this can never provide an immediate
    // lane value, but not generating the wrapper function would require
    // completely refactoring the way primitives work, which isn't worth the
    // effort since this is always inlined.
    | _ => Expression.Unreachable.make(wasm_mod)
    }
  );
};

let compile_wasm_simd_shuffle = (wasm_mod, vec1, vec2, mask) => {
  Mashtree.(
    switch (mask.immediate_desc) {
    | MImmConst(MConstLiteral(MConstV128(low, low_mid, high_mid, high))) =>
      let mask = {
        let bytes = Bytes.create(16);
        Bytes.set_int32_le(bytes, 0, low);
        Bytes.set_int32_le(bytes, 4, low_mid);
        Bytes.set_int32_le(bytes, 8, high_mid);
        Bytes.set_int32_le(bytes, 12, high);
        Array.init(16, Bytes.get_uint8(bytes));
      };
      Expression.SIMD_shuffle.make(wasm_mod, vec1, vec2, mask);

    // The wrapper function generated for this can never provide an immediate
    // mask value, but not generating the wrapper function would require
    // completely refactoring the way primitives work, which isn't worth the
    // effort since this is always inlined.
    | _ => Expression.Unreachable.make(wasm_mod)
    }
  );
};

let compile_wasm_simd_load = (wasm_mod, instr, ptr, offset, align) => {
  Mashtree.(
    switch (offset.immediate_desc, align.immediate_desc) {
    | (
        MImmConst(MConstLiteral(MConstI32(offset))),
        MImmConst(MConstLiteral(MConstI32(align))),
      ) =>
      Expression.SIMD_load.make(
        wasm_mod,
        get_op(instr),
        Int32.to_int(offset),
        Int32.to_int(align),
        ptr,
        grain_memory,
      )

    // The wrapper function generated for this can never provide immediate
    // offset/alignment values, but not generating the wrapper function would
    // require completely refactoring the way primitives work, which isn't
    // worth the effort since this is always inlined.
    | _ => Expression.Unreachable.make(wasm_mod)
    }
  );
};

let compile_wasm_simd_load_store_lane =
    (wasm_mod, instr, ret_type, ptr, offset, align, lane, vec) => {
  Mashtree.(
    switch (offset.immediate_desc, align.immediate_desc, lane.immediate_desc) {
    | (
        MImmConst(MConstLiteral(MConstI32(offset))),
        MImmConst(MConstLiteral(MConstI32(align))),
        MImmConst(MConstLiteral(MConstI32(lane))),
      ) =>
      let exp =
        Expression.SIMD_load_store_lane.make(
          wasm_mod,
          get_op(instr),
          Int32.to_int(offset),
          Int32.to_int(align),
          Int32.to_int(lane),
          ptr,
          vec,
          grain_memory,
        );
      switch (ret_type) {
      | Grain_void =>
        Expression.Block.make(
          wasm_mod,
          gensym_label("wasm_simd_load_store_lane"),
          [exp],
        )
      | _ => exp
      };

    // The wrapper function generated for this can never provide immediate
    // offset/alignment/lane values, but not generating the wrapper function
    // would require completely refactoring the way primitives work, which
    // isn't worth the effort since this is always inlined.
    | _ => Expression.Unreachable.make(wasm_mod)
    }
  );
};