/* Modelled off of typetreeMap.ml; see note about OCaml copyright */
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

module DefaultMapArgument: MapArgument = {
  let enter_imm_expression = e => e;
  let leave_imm_expression = e => e;

  let enter_comp_expression = e => e;
  let leave_comp_expression = e => e;

  let enter_anf_expression = e => e;
  let leave_anf_expression = e => e;

  let enter_anf_program = p => p;
  let leave_anf_program = p => p;
};

module MakeMap = (Iter: MapArgument) => {
  let rec map_imm_expression = i =>
    Iter.leave_imm_expression(Iter.enter_imm_expression(i))

  and map_comp_expression = c => {
    let {comp_desc: desc} as c = Iter.enter_comp_expression(c);
    let d =
      switch (desc) {
      | CImmExpr(i) => CImmExpr(map_imm_expression(i))
      | [@implicit_arity] CPrim1(p1, arg) =>
        [@implicit_arity] CPrim1(p1, map_imm_expression(arg))
      | [@implicit_arity] CPrim2(p2, arg1, arg2) =>
        let arg1 = map_imm_expression(arg1);
        let arg2 = map_imm_expression(arg2);
        [@implicit_arity] CPrim2(p2, arg1, arg2);
      | [@implicit_arity] CBoxAssign(lhs, rhs) =>
        let lhs = map_imm_expression(lhs);
        let rhs = map_imm_expression(rhs);
        [@implicit_arity] CBoxAssign(lhs, rhs);
      | [@implicit_arity] CAssign(lhs, rhs) =>
        let rhs = map_imm_expression(rhs);
        [@implicit_arity] CAssign(lhs, rhs);
      | CTuple(elts) => CTuple(List.map(map_imm_expression, elts))
      | CArray(elts) => CArray(List.map(map_imm_expression, elts))
      | [@implicit_arity] CArrayGet(arg1, arg2) =>
        [@implicit_arity]
        CArrayGet(map_imm_expression(arg1), map_imm_expression(arg2))
      | [@implicit_arity] CArraySet(arg1, arg2, arg3) =>
        [@implicit_arity]
        CArraySet(
          map_imm_expression(arg1),
          map_imm_expression(arg2),
          map_imm_expression(arg3),
        )
      | [@implicit_arity] CRecord(ttag, elts) =>
        [@implicit_arity]
        CRecord(
          map_imm_expression(ttag),
          List.map(
            ((name, elt)) => (name, map_imm_expression(elt)),
            elts,
          ),
        )
      | [@implicit_arity] CAdt(ttag, vtag, elts) =>
        [@implicit_arity]
        CAdt(
          map_imm_expression(ttag),
          map_imm_expression(vtag),
          List.map(map_imm_expression, elts),
        )
      | [@implicit_arity] CGetTupleItem(idx, tup) =>
        [@implicit_arity] CGetTupleItem(idx, map_imm_expression(tup))
      | [@implicit_arity] CSetTupleItem(idx, tup, value) =>
        let tup = map_imm_expression(tup);
        let value = map_imm_expression(value);
        [@implicit_arity] CSetTupleItem(idx, tup, value);
      | [@implicit_arity] CGetAdtItem(idx, adt) =>
        [@implicit_arity] CGetAdtItem(idx, map_imm_expression(adt))
      | CGetAdtTag(adt) => CGetAdtTag(map_imm_expression(adt))
      | [@implicit_arity] CGetRecordItem(idx, record) =>
        [@implicit_arity] CGetRecordItem(idx, map_imm_expression(record))
      | [@implicit_arity] CSetRecordItem(idx, record, arg) =>
        [@implicit_arity]
        CSetRecordItem(
          idx,
          map_imm_expression(record),
          map_imm_expression(arg),
        )
      | [@implicit_arity] CIf(c, t, f) =>
        let c = map_imm_expression(c);
        let t = map_anf_expression(t);
        let f = map_anf_expression(f);
        [@implicit_arity] CIf(c, t, f);
      | [@implicit_arity] CWhile(c, body) =>
        let c = map_anf_expression(c);
        let body = map_anf_expression(body);
        [@implicit_arity] CWhile(c, body);
      | [@implicit_arity] CSwitch(c, branches) =>
        let c = map_imm_expression(c);
        let branches =
          List.map(
            ((tag, body)) => (tag, map_anf_expression(body)),
            branches,
          );
        [@implicit_arity] CSwitch(c, branches);
      | [@implicit_arity] CApp(f, args) =>
        let f = map_imm_expression(f);
        let args = List.map(map_imm_expression, args);
        [@implicit_arity] CApp(f, args);
      | [@implicit_arity] CAppBuiltin(mod_, f, args) =>
        [@implicit_arity]
        CAppBuiltin(mod_, f, List.map(map_imm_expression, args))
      | [@implicit_arity] CLambda(idents, expr) =>
        let expr = map_anf_expression(expr);
        [@implicit_arity] CLambda(idents, expr);
      | CString(s) => CString(s)
      | CInt32(i) => CInt32(i)
      | CInt64(i) => CInt64(i)
      };

    Iter.leave_comp_expression({...c, comp_desc: d});
  }

  and map_anf_expression = anf => {
    let {anf_desc: desc} as anf = Iter.enter_anf_expression(anf);
    let d =
      switch (desc) {
      | [@implicit_arity] AELet(g, r, bindings, body) =>
        let bindings =
          List.map(
            ((ident, bind)) => {
              let b = map_comp_expression(bind);
              (ident, b);
            },
            bindings,
          );
        let body = map_anf_expression(body);
        [@implicit_arity] AELet(g, r, bindings, body);
      | [@implicit_arity] AESeq(hd, tl) =>
        let hd = map_comp_expression(hd);
        let tl = map_anf_expression(tl);
        [@implicit_arity] AESeq(hd, tl);
      | AEComp(c) => AEComp(map_comp_expression(c))
      };

    Iter.leave_anf_expression({...anf, anf_desc: d});
  }

  and map_anf_program = prog => {
    let {body} as prog = Iter.enter_anf_program(prog);
    let body = map_anf_expression(body);
    Iter.leave_anf_program({...prog, body});
  };
};
