open Anftree
open Grain_parsing
open Grain_typed
open Types

module type MapArgument = sig
  val enter_imm_expression : imm_expression -> imm_expression
  val leave_imm_expression : imm_expression -> imm_expression

  val enter_comp_expression : comp_expression -> comp_expression
  val leave_comp_expression : comp_expression -> comp_expression

  val enter_anf_expression : anf_expression -> anf_expression
  val leave_anf_expression : anf_expression -> anf_expression

  val enter_anf_program : anf_program -> anf_program
  val leave_anf_program : anf_program -> anf_program
end

module DefaultMapArgument : MapArgument

module MakeMap :
  functor
    (Iter : MapArgument) ->
sig
  val map_imm_expression : imm_expression -> imm_expression
  val map_comp_expression : comp_expression -> comp_expression
  val map_anf_expression : anf_expression -> anf_expression
  val map_anf_program : anf_program -> anf_program
end

