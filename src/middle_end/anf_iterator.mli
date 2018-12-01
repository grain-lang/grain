open Anftree
open Grain_parsing
open Grain_typed
open Types


module type IterArgument = sig
  val enter_imm_expression : imm_expression -> unit
  val leave_imm_expression : imm_expression -> unit

  val enter_comp_expression : comp_expression -> unit
  val leave_comp_expression : comp_expression -> unit

  val enter_anf_expression : anf_expression -> unit
  val leave_anf_expression : anf_expression -> unit

  val enter_anf_program : anf_program -> unit
  val leave_anf_program : anf_program -> unit
end

module DefaultIterArgument : IterArgument

module MakeIter :
  functor
    (Iter : IterArgument) ->
sig
  val iter_imm_expression : imm_expression -> unit
  val iter_comp_expression : comp_expression -> unit
  val iter_anf_expression : anf_expression -> unit
  val iter_anf_program : anf_program -> unit
end

