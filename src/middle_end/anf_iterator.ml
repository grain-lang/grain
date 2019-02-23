(* Modelled off of typedtreeIter.ml; see note about OCaml copyright *)
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

module DefaultIterArgument : IterArgument = struct
  let enter_imm_expression _ = ()
  let leave_imm_expression _ = ()

  let enter_comp_expression _ = ()
  let leave_comp_expression _ = ()

  let enter_anf_expression _ = ()
  let leave_anf_expression _ = ()

  let enter_anf_program _ = ()
  let leave_anf_program _ = ()
end


module MakeIter(Iter : IterArgument) = struct
  let rec iter_imm_expression i =
    Iter.enter_imm_expression i;
    Iter.leave_imm_expression i

  and iter_comp_expression ({comp_desc = desc} as c) =
    Iter.enter_comp_expression c;
    begin match desc with
      | CImmExpr(i) ->
        iter_imm_expression i
      | CPrim1(_, arg) ->
        iter_imm_expression arg
      | CPrim2(_, arg1, arg2) ->
        iter_imm_expression arg1;
        iter_imm_expression arg2
      | CAssign(lhs, rhs) ->
        iter_imm_expression lhs;
        iter_imm_expression rhs
      | CTuple(elts) ->
        List.iter (iter_imm_expression) elts
      | CAdt(ttag, vtag, elts) ->
        iter_imm_expression ttag;
        iter_imm_expression vtag;
        List.iter (iter_imm_expression) elts
      | CGetTupleItem(_, tup) ->
        iter_imm_expression tup
      | CSetTupleItem(_, tup, value) ->
        iter_imm_expression tup;
        iter_imm_expression value
      | CGetAdtItem(_, adt)
      | CGetAdtTag(adt) ->
        iter_imm_expression adt
      | CIf(c, t, f) ->
        iter_imm_expression c;
        iter_anf_expression t;
        iter_anf_expression f
      | CWhile(c, body) ->
        iter_anf_expression c;
        iter_anf_expression body
      | CSwitch(c, branches) ->
        iter_imm_expression c;
        List.iter (fun (_, body) -> iter_anf_expression body) branches
      | CApp(f, args) ->
        iter_imm_expression f;
        List.iter (iter_imm_expression) args
      | CAppBuiltin(_, _, args) ->
        List.iter (iter_imm_expression) args
      | CLambda(idents, expr) ->
        iter_anf_expression expr
      | CString(s) -> ()
    end;
    Iter.leave_comp_expression c

  and iter_anf_expression ({anf_desc = desc} as anf) =
    Iter.enter_anf_expression anf;
    begin match desc with
      | AELet(_, _, bindings, body) ->
        List.iter (fun (ident, bind) -> iter_comp_expression bind) bindings;
        iter_anf_expression body
      | AESeq(hd, tl) ->
        iter_comp_expression hd;
        iter_anf_expression tl
      | AEComp(c) ->
        iter_comp_expression c
    end;
    Iter.leave_anf_expression anf

  and iter_anf_program ({body} as prog) =
    Iter.enter_anf_program prog;
    iter_anf_expression body;
    Iter.leave_anf_program prog
end
