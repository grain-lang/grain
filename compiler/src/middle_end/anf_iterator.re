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
    | [@implicit_arity] CPrim1(_, arg) => iter_imm_expression(arg)
    | [@implicit_arity] CPrim2(_, arg1, arg2) =>
      iter_imm_expression(arg1);
      iter_imm_expression(arg2);
    | [@implicit_arity] CBoxAssign(lhs, rhs) =>
      iter_imm_expression(lhs);
      iter_imm_expression(rhs);
    | [@implicit_arity] CAssign(lhs, rhs) =>
      iter_imm_expression(lhs);
      iter_imm_expression(rhs);
    | CTuple(elts) => List.iter(iter_imm_expression, elts)
    | CArray(elts) => List.iter(iter_imm_expression, elts)
    | [@implicit_arity] CArrayGet(arg1, arg2) =>
      iter_imm_expression(arg1);
      iter_imm_expression(arg2);
    | [@implicit_arity] CArraySet(arg1, arg2, arg3) =>
      iter_imm_expression(arg1);
      iter_imm_expression(arg2);
      iter_imm_expression(arg3);
    | [@implicit_arity] CRecord(ttag, elts) =>
      iter_imm_expression(ttag);
      List.iter(((_, elt)) => iter_imm_expression(elt), elts);
    | [@implicit_arity] CAdt(ttag, vtag, elts) =>
      iter_imm_expression(ttag);
      iter_imm_expression(vtag);
      List.iter(iter_imm_expression, elts);
    | [@implicit_arity] CGetTupleItem(_, tup) => iter_imm_expression(tup)
    | [@implicit_arity] CSetTupleItem(_, tup, value) =>
      iter_imm_expression(tup);
      iter_imm_expression(value);
    | [@implicit_arity] CGetAdtItem(_, adt)
    | CGetAdtTag(adt) => iter_imm_expression(adt)
    | [@implicit_arity] CGetRecordItem(_, record) =>
      iter_imm_expression(record)
    | [@implicit_arity] CSetRecordItem(_, record, arg) =>
      iter_imm_expression(record);
      iter_imm_expression(arg);
    | [@implicit_arity] CIf(c, t, f) =>
      iter_imm_expression(c);
      iter_anf_expression(t);
      iter_anf_expression(f);
    | [@implicit_arity] CWhile(c, body) =>
      iter_anf_expression(c);
      iter_anf_expression(body);
    | [@implicit_arity] CSwitch(c, branches) =>
      iter_imm_expression(c);
      List.iter(((_, body)) => iter_anf_expression(body), branches);
    | [@implicit_arity] CApp(f, args) =>
      iter_imm_expression(f);
      List.iter(iter_imm_expression, args);
    | [@implicit_arity] CAppBuiltin(_, _, args) =>
      List.iter(iter_imm_expression, args)
    | [@implicit_arity] CLambda(idents, expr) => iter_anf_expression(expr)
    | CString(s) => ()
    | CInt32(i) => ()
    | CInt64(i) => ()
    };
    Iter.leave_comp_expression(c);
  }

  and iter_anf_expression = ({anf_desc: desc} as anf) => {
    Iter.enter_anf_expression(anf);
    switch (desc) {
    | [@implicit_arity] AELet(_, _, bindings, body) =>
      List.iter(((ident, bind)) => iter_comp_expression(bind), bindings);
      iter_anf_expression(body);
    | [@implicit_arity] AESeq(hd, tl) =>
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
