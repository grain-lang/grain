/* Modelled off of typedtreeIter.ml; see note about OCaml copyright */
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

module DefaultIterArgument: IterArgument = {
  let enter_imm_expression = _ => ();
  let leave_imm_expression = _ => ();

  let enter_comp_expression = _ => ();
  let leave_comp_expression = _ => ();

  let enter_anf_expression = _ => ();
  let leave_anf_expression = _ => ();

  let enter_anf_program = _ => ();
  let leave_anf_program = _ => ();
};

module MakeIter = (Iter: IterArgument) => {
  let rec iter_imm_expression = i => {
    Iter.enter_imm_expression(i);
    Iter.leave_imm_expression(i);
  }

  and iter_comp_expression = ({comp_desc: desc} as c) => {
    Iter.enter_comp_expression(c);
    switch (desc) {
    | CImmExpr(i) => iter_imm_expression(i)
    | CPrim0(_) => ()
    | CPrim1(_, arg) => iter_imm_expression(arg)
    | CPrim2(_, arg1, arg2) =>
      iter_imm_expression(arg1);
      iter_imm_expression(arg2);
    | CPrimN(_, elts) => List.iter(iter_imm_expression, elts)
    | CBoxAssign(lhs, rhs) =>
      iter_imm_expression(lhs);
      iter_imm_expression(rhs);
    | CAssign(lhs, rhs) =>
      iter_imm_expression(lhs);
      iter_imm_expression(rhs);
    | CLocalAssign(lhs, rhs) => iter_imm_expression(rhs)
    | CTuple(elts) => List.iter(iter_imm_expression, elts)
    | CArray(elts) => List.iter(iter_imm_expression, elts)
    | CArrayGet(arg1, arg2) =>
      iter_imm_expression(arg1);
      iter_imm_expression(arg2);
    | CArraySet(arg1, arg2, arg3) =>
      iter_imm_expression(arg1);
      iter_imm_expression(arg2);
      iter_imm_expression(arg3);
    | CRecord(type_hash, ttag, elts) =>
      iter_imm_expression(type_hash);
      iter_imm_expression(ttag);
      List.iter(((_, elt)) => iter_imm_expression(elt), elts);
    | CAdt(type_hash, ttag, vtag, elts) =>
      iter_imm_expression(type_hash);
      iter_imm_expression(ttag);
      iter_imm_expression(vtag);
      List.iter(iter_imm_expression, elts);
    | CGetTupleItem(_, tup) => iter_imm_expression(tup)
    | CSetTupleItem(_, tup, value) =>
      iter_imm_expression(tup);
      iter_imm_expression(value);
    | CGetAdtItem(_, adt)
    | CGetAdtTag(adt) => iter_imm_expression(adt)
    | CGetRecordItem(_, record) => iter_imm_expression(record)
    | CSetRecordItem(_, record, arg) =>
      iter_imm_expression(record);
      iter_imm_expression(arg);
    | CIf(c, t, f) =>
      iter_imm_expression(c);
      iter_anf_expression(t);
      iter_anf_expression(f);
    | CFor(c, inc, body) =>
      Option.iter(iter_anf_expression, c);
      Option.iter(iter_anf_expression, inc);
      iter_anf_expression(body);
    | CContinue
    | CBreak => ()
    | CReturn(e) => Option.iter(iter_imm_expression, e)
    | CSwitch(c, branches, _) =>
      iter_imm_expression(c);
      List.iter(((_, body)) => iter_anf_expression(body), branches);
    | CApp((f, _), args, _) =>
      iter_imm_expression(f);
      List.iter(iter_imm_expression, args);
    | CLambda(_, idents, (expr, _), _) => iter_anf_expression(expr)
    | CBytes(s) => ()
    | CString(s) => ()
    | CNumber(i) => ()
    | CInt32(i) => ()
    | CInt64(i) => ()
    | CUint32(i) => ()
    | CUint64(i) => ()
    | CFloat32(f) => ()
    | CFloat64(f) => ()
    };
    Iter.leave_comp_expression(c);
  }

  and iter_anf_expression = ({anf_desc: desc} as anf) => {
    Iter.enter_anf_expression(anf);
    switch (desc) {
    | AELet(_, _, _, bindings, body) =>
      List.iter(((ident, bind)) => iter_comp_expression(bind), bindings);
      iter_anf_expression(body);
    | AESeq(hd, tl) =>
      iter_comp_expression(hd);
      iter_anf_expression(tl);
    | AEComp(c) => iter_comp_expression(c)
    };
    Iter.leave_anf_expression(anf);
  }

  and iter_anf_program = ({body} as prog) => {
    Iter.enter_anf_program(prog);
    iter_anf_expression(body);
    Iter.leave_anf_program(prog);
  };
};
