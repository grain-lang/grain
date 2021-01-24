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
        (Op_memory_size, Op.memory_size),
        (Op_memory_grow, Op.memory_grow),
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
  let op = Expression.unary(wasm_mod, get_op(instr), arg);
  switch (ret_type) {
  | Mashtree.Grain_bool =>
    Expression.select(
      wasm_mod,
      op,
      Expression.const(wasm_mod, const_true()),
      Expression.const(wasm_mod, const_false()),
    )
  | _ => op
  };
};

let compile_wasm_prim2 = (wasm_mod, env, instr, ret_type, arg1, arg2) => {
  let op = Expression.binary(wasm_mod, get_op(instr), arg1, arg2);
  switch (ret_type) {
  | Mashtree.Grain_bool =>
    Expression.select(
      wasm_mod,
      op,
      Expression.const(wasm_mod, const_true()),
      Expression.const(wasm_mod, const_false()),
    )
  | _ => op
  };
};
