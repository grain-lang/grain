open Anftree;
open Grain_typed;
open Types;

let get_bool =
  fun
  | Const_bool(b) => b
  | _ => failwith("Operand was not a boolean");

module ConstantFoldingArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_comp_expression = ({comp_desc: desc} as c) =>
    switch (desc) {
    | CPrim1(Not, {imm_desc: ImmConst(Const_bool(b))} as i) => {
        ...c,
        comp_desc: CImmExpr({...i, imm_desc: ImmConst(Const_bool(!b))}),
      }
    | CPrim2(prim2, {imm_desc: ImmConst(x)} as i, {imm_desc: ImmConst(y)}) =>
      let wrap_imm = imm => {
        ...c,
        comp_desc: CImmExpr({...i, imm_desc: ImmConst(imm)}),
      };
      switch (prim2) {
      | And => wrap_imm @@ Const_bool(get_bool(x) && get_bool(y))
      | Or => wrap_imm @@ Const_bool(get_bool(x) || get_bool(y))
      | _ => c
      };
    | _ => c
    };
};

module ConstantFoldingMapper = Anf_mapper.MakeMap(ConstantFoldingArg);

let optimize = anfprog => ConstantFoldingMapper.map_anf_program(anfprog);
