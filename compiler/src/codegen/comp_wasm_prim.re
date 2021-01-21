open Binaryen;
open Comp_utils;

module StringHash =
  Hashtbl.Make({
    type t = string;
    let hash = x => Hashtbl.hash(x);
    let equal = (a, b) => String.compare(a, b) === 0;
  });

let op_map =
  StringHash.of_seq(
    List.to_seq([
      ("clz_i32", Op.clz_int32),
      ("ctz_i32", Op.ctz_int32),
      ("popcnt_i32", Op.popcnt_int32),
      ("eq_z_i32", Op.eq_z_int32),
      ("add_i32", Op.add_int32),
      ("sub_i32", Op.sub_int32),
      ("mul_i32", Op.mul_int32),
      ("div_s_i32", Op.div_s_int32),
      ("div_u_i32", Op.div_u_int32),
      ("rem_s_i32", Op.rem_s_int32),
      ("rem_u_i32", Op.rem_u_int32),
      ("and_i32", Op.and_int32),
      ("or_i32", Op.or_int32),
      ("xor_i32", Op.xor_int32),
      ("shl_i32", Op.shl_int32),
      ("shr_u_i32", Op.shr_u_int32),
      ("shr_s_i32", Op.shr_s_int32),
      ("rot_l_i32", Op.rot_l_int32),
      ("rot_r_i32", Op.rot_r_int32),
      ("eq_i32", Op.eq_int32),
      ("ne_i32", Op.ne_int32),
      ("lt_s_i32", Op.lt_s_int32),
      ("lt_u_i32", Op.lt_u_int32),
      ("le_s_i32", Op.le_s_int32),
      ("le_u_i32", Op.le_u_int32),
      ("gt_s_i32", Op.gt_s_int32),
      ("gt_u_i32", Op.gt_u_int32),
      ("ge_s_i32", Op.ge_s_int32),
      ("ge_u_i32", Op.ge_u_int32),
      ("clz_i64", Op.clz_int64),
      ("ctz_i64", Op.ctz_int64),
      ("popcnt_i64", Op.popcnt_int64),
      ("eq_z_i64", Op.eq_z_int64),
      ("add_i64", Op.add_int64),
      ("sub_i64", Op.sub_int64),
      ("mul_i64", Op.mul_int64),
      ("div_s_i64", Op.div_s_int64),
      ("div_u_i64", Op.div_u_int64),
      ("rem_s_i64", Op.rem_s_int64),
      ("rem_u_i64", Op.rem_u_int64),
      ("and_i64", Op.and_int64),
      ("or_i64", Op.or_int64),
      ("xor_i64", Op.xor_int64),
      ("shl_i64", Op.shl_int64),
      ("shr_u_i64", Op.shr_u_int64),
      ("shr_s_i64", Op.shr_s_int64),
      ("rot_l_i64", Op.rot_l_int64),
      ("rot_r_i64", Op.rot_r_int64),
      ("eq_i64", Op.eq_int64),
      ("ne_i64", Op.ne_int64),
      ("lt_s_i64", Op.lt_s_int64),
      ("lt_u_i64", Op.lt_u_int64),
      ("le_s_i64", Op.le_s_int64),
      ("le_u_i64", Op.le_u_int64),
      ("gt_s_i64", Op.gt_s_int64),
      ("gt_u_i64", Op.gt_u_int64),
      ("ge_s_i64", Op.ge_s_int64),
      ("ge_u_i64", Op.ge_u_int64),
    ]),
  );

let get_op = name =>
  try(StringHash.find(op_map, name)) {
  | Not_found =>
    failwith(
      Printf.sprintf("internal: Wasm instruction `%s` not found", name),
    )
  };

let compile_wasm_prim1 = (wasm_mod, env, instr, ~boolean, arg) => {
  let op = Expression.unary(wasm_mod, get_op(instr), arg);
  if (boolean) {
    Expression.select(
      wasm_mod,
      op,
      Expression.const(wasm_mod, const_true()),
      Expression.const(wasm_mod, const_false()),
    );
  } else {
    op;
  };
};

let compile_wasm_prim2 = (wasm_mod, env, instr, ~boolean, arg1, arg2) => {
  let op = Expression.binary(wasm_mod, get_op(instr), arg1, arg2);
  if (boolean) {
    Expression.select(
      wasm_mod,
      op,
      Expression.const(wasm_mod, const_true()),
      Expression.const(wasm_mod, const_false()),
    );
  } else {
    op;
  };
};
