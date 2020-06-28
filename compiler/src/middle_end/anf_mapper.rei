open Anftree;
open Grain_parsing;
open Grain_typed;
open Types;

module type MapArgument = {
  let enter_imm_expression: imm_expression => imm_expression;
  let leave_imm_expression: imm_expression => imm_expression;

  let enter_comp_expression: comp_expression => comp_expression;
  let leave_comp_expression: comp_expression => comp_expression;

  let enter_anf_expression: anf_expression => anf_expression;
  let leave_anf_expression: anf_expression => anf_expression;

  let enter_anf_program: anf_program => anf_program;
  let leave_anf_program: anf_program => anf_program;
};

module DefaultMapArgument: MapArgument;

module MakeMap:
  (Iter: MapArgument) =>
   {
    let map_imm_expression: imm_expression => imm_expression;
    let map_comp_expression: comp_expression => comp_expression;
    let map_anf_expression: anf_expression => anf_expression;
    let map_anf_program: anf_program => anf_program;
  };
