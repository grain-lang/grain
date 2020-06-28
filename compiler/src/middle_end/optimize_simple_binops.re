open Anftree;
open Grain_typed;
open Types;

let get_int =
  fun
  | Const_int(n) => n
  | Const_int32(n) => Int32.to_int(n)
  | Const_int64(n) => Int64.to_int(n)
  | _ => failwith("Operand was not an integer");

let get_bool =
  fun
  | Const_bool(b) => b
  | _ => failwith("Operand was not a boolean");

let in_valid_int_range = (op, x, y) => {
  let n = op(get_int(x), get_int(y));
  /* Numbers in Grain are stored double their value, so we need to check if the representation overflows */
  let n = n * 2;
  n < Int32.to_int(Int32.max_int) && n > Int32.to_int(Int32.min_int);
};

module ConstantFoldingArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_comp_expression = ({comp_desc: desc} as c) =>
    switch (desc) {
    | [@implicit_arity]
      CApp(
        {imm_desc: ImmId({name})},
        [{imm_desc: ImmConst(x)} as i, {imm_desc: ImmConst(y)}],
      ) =>
      let wrap_imm = imm => {
        ...c,
        comp_desc: CImmExpr({...i, imm_desc: ImmConst(imm)}),
      };
      switch (name) {
      /* in_valid_int_range check to make sure we don't overflow.
         If we will overflow, don't optimize and allow the error at runtime. */
      | "+" when in_valid_int_range((+), x, y) =>
        wrap_imm @@ Const_int(get_int(x) + get_int(y))
      | "-" when in_valid_int_range((-), x, y) =>
        wrap_imm @@ Const_int(get_int(x) - get_int(y))
      | "*" when in_valid_int_range(( * ), x, y) =>
        wrap_imm @@ Const_int(get_int(x) * get_int(y))
      | "<" => wrap_imm @@ Const_bool(get_int(x) < get_int(y))
      | "<=" => wrap_imm @@ Const_bool(get_int(x) <= get_int(y))
      | ">" => wrap_imm @@ Const_bool(get_int(x) > get_int(y))
      | ">=" => wrap_imm @@ Const_bool(get_int(x) >= get_int(y))
      | "==" => wrap_imm @@ Const_bool(x == y)
      | "&&" => wrap_imm @@ Const_bool(get_bool(x) && get_bool(y))
      | "||" => wrap_imm @@ Const_bool(get_bool(x) || get_bool(y))
      | _ => c
      };
    | _ => c
    };
};

module ConstantFoldingMapper = Anf_mapper.MakeMap(ConstantFoldingArg);

let optimize = anfprog => ConstantFoldingMapper.map_anf_program(anfprog);
