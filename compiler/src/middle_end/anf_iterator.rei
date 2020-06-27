open Anftree;
open Grain_parsing;
open Grain_typed;
open Types;

module type IterArgument = {
  let enter_imm_expression: imm_expression => unit;
  let leave_imm_expression: imm_expression => unit;

  let enter_comp_expression: comp_expression => unit;
  let leave_comp_expression: comp_expression => unit;

  let enter_anf_expression: anf_expression => unit;
  let leave_anf_expression: anf_expression => unit;

  let enter_anf_program: anf_program => unit;
  let leave_anf_program: anf_program => unit;
};

module DefaultIterArgument: IterArgument;

module MakeIter:
  (Iter: IterArgument) =>
   {
    let iter_imm_expression: imm_expression => unit;
    let iter_comp_expression: comp_expression => unit;
    let iter_anf_expression: anf_expression => unit;
    let iter_anf_program: anf_program => unit;
  };
