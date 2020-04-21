(* Modelled off of typetreeMap.ml; see note about OCaml copyright *)
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

module DefaultMapArgument : MapArgument = struct
  let enter_imm_expression e = e
  let leave_imm_expression e = e

  let enter_comp_expression e = e
  let leave_comp_expression e = e

  let enter_anf_expression e = e
  let leave_anf_expression e = e

  let enter_anf_program p = p
  let leave_anf_program p = p
end

module MakeMap(Iter : MapArgument) = struct
  let rec map_imm_expression i =
    Iter.leave_imm_expression (Iter.enter_imm_expression i)

  and map_comp_expression c =
    let ({comp_desc = desc} as c) = Iter.enter_comp_expression c in
    let d = match desc with
      | CImmExpr(i) ->
        CImmExpr(map_imm_expression i)
      | CPrim1(p1, arg) ->
        CPrim1(p1, map_imm_expression arg)
      | CPrim2(p2, arg1, arg2) ->
        let arg1 = map_imm_expression arg1 in
        let arg2 = map_imm_expression arg2 in
        CPrim2(p2, arg1, arg2)
      | CAssign(lhs, rhs) ->
        let lhs = map_imm_expression lhs in
        let rhs = map_imm_expression rhs in
        CAssign(lhs, rhs)
      | CTuple(elts) ->
        CTuple(List.map (map_imm_expression) elts)
      | CArray(elts) ->
        CArray(List.map (map_imm_expression) elts)
      | CArrayGet(arg1, arg2) ->
        CArrayGet(map_imm_expression arg1, map_imm_expression arg2)
      | CArraySet(arg1, arg2, arg3) ->
        CArraySet(map_imm_expression arg1, map_imm_expression arg2, map_imm_expression arg3)
      | CRecord(ttag, elts) ->
        CRecord(map_imm_expression ttag, List.map (fun (name, elt) -> name, map_imm_expression elt) elts)
      | CAdt(ttag, vtag, elts) ->
        CAdt(map_imm_expression ttag, map_imm_expression vtag, List.map (map_imm_expression) elts)
      | CGetTupleItem(idx, tup) ->
        CGetTupleItem(idx, map_imm_expression tup)
      | CSetTupleItem(idx, tup, value) ->
        let tup = map_imm_expression tup in
        let value = map_imm_expression value in
        CSetTupleItem(idx, tup, value)
      | CGetAdtItem(idx, adt) ->
        CGetAdtItem(idx, map_imm_expression adt)
      | CGetAdtTag(adt) ->
        CGetAdtTag(map_imm_expression adt)
      | CGetRecordItem(idx, record) ->
        CGetRecordItem(idx, map_imm_expression record)
      | CIf(c, t, f) ->
        let c = map_imm_expression c in
        let t = map_anf_expression t in
        let f = map_anf_expression f in
        CIf(c, t, f)
      | CWhile(c, body) ->
        let c = map_anf_expression c in
        let body = map_anf_expression body in
        CWhile(c, body)
      | CSwitch(c, branches) ->
        let c = map_imm_expression c in
        let branches = List.map (fun (tag, body) -> (tag, map_anf_expression body)) branches in
        CSwitch(c, branches)
      | CApp(f, args) ->
        let f = map_imm_expression f in
        let args = List.map (map_imm_expression) args in
        CApp(f, args)
      | CAppBuiltin(mod_, f, args) ->
        CAppBuiltin(mod_, f, List.map (map_imm_expression) args)
      | CLambda(idents, expr) ->
        let expr = map_anf_expression expr in
        CLambda(idents, expr)
      | CString(s) -> CString(s)
      | CInt32(i) -> CInt32(i)
      | CInt64(i) -> CInt64(i)
    in
    Iter.leave_comp_expression {c with comp_desc = d}

  and map_anf_expression anf =
    let ({anf_desc = desc} as anf) = Iter.enter_anf_expression anf in
    let d = match desc with
      | AELet(g, r, bindings, body) ->
        let bindings = List.map (fun (ident, bind) ->
            let b = map_comp_expression bind in
            (ident, b)) bindings in
        let body = map_anf_expression body in
        AELet(g, r, bindings, body)
      | AESeq(hd, tl) ->
        let hd = map_comp_expression hd in
        let tl = map_anf_expression tl in
        AESeq(hd, tl)
      | AEComp(c) ->
        AEComp(map_comp_expression c)
    in
    Iter.leave_anf_expression {anf with anf_desc=d}

  and map_anf_program prog =
    let ({body} as prog) = Iter.enter_anf_program prog in
    let body = map_anf_expression body in
    Iter.leave_anf_program {prog with body}
end
