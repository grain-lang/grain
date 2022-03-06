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
      | CPrim0(p0) => CPrim0(p0)
      | CPrim1(p1, arg) => CPrim1(p1, map_imm_expression(arg))
      | CPrim2(p2, arg1, arg2) =>
        let arg1 = map_imm_expression(arg1);
        let arg2 = map_imm_expression(arg2);
        CPrim2(p2, arg1, arg2);
      | CPrimN(p, args) => CPrimN(p, List.map(map_imm_expression, args))
      | CBoxAssign(lhs, rhs) =>
        let lhs = map_imm_expression(lhs);
        let rhs = map_imm_expression(rhs);
        CBoxAssign(lhs, rhs);
      | CAssign(lhs, rhs) =>
        let lhs = map_imm_expression(lhs);
        let rhs = map_imm_expression(rhs);
        CAssign(lhs, rhs);
      | CLocalAssign(id, rhs) =>
        let rhs = map_imm_expression(rhs);
        CLocalAssign(id, rhs);
      | CTuple(elts) => CTuple(List.map(map_imm_expression, elts))
      | CArray(elts) => CArray(List.map(map_imm_expression, elts))
      | CArrayGet(arg1, arg2) =>
        CArrayGet(map_imm_expression(arg1), map_imm_expression(arg2))
      | CArraySet(arg1, arg2, arg3) =>
        CArraySet(
          map_imm_expression(arg1),
          map_imm_expression(arg2),
          map_imm_expression(arg3),
        )
      | CRecord(ttag, elts) =>
        CRecord(
          map_imm_expression(ttag),
          List.map(
            ((name, elt)) => (name, map_imm_expression(elt)),
            elts,
          ),
        )
      | CAdt(ttag, vtag, elts) =>
        CAdt(
          map_imm_expression(ttag),
          map_imm_expression(vtag),
          List.map(map_imm_expression, elts),
        )
      | CGetTupleItem(idx, tup) =>
        CGetTupleItem(idx, map_imm_expression(tup))
      | CSetTupleItem(idx, tup, value) =>
        let tup = map_imm_expression(tup);
        let value = map_imm_expression(value);
        CSetTupleItem(idx, tup, value);
      | CGetAdtItem(idx, adt) => CGetAdtItem(idx, map_imm_expression(adt))
      | CGetAdtTag(adt) => CGetAdtTag(map_imm_expression(adt))
      | CGetRecordItem(idx, record) =>
        CGetRecordItem(idx, map_imm_expression(record))
      | CSetRecordItem(idx, record, arg) =>
        CSetRecordItem(
          idx,
          map_imm_expression(record),
          map_imm_expression(arg),
        )
      | CIf(c, t, f) =>
        let c = map_imm_expression(c);
        let t = map_anf_expression(t);
        let f = map_anf_expression(f);
        CIf(c, t, f);
      | CFor(c, inc, body) =>
        let c = Option.map(map_anf_expression, c);
        let inc = Option.map(map_anf_expression, inc);
        let body = map_anf_expression(body);
        CFor(c, inc, body);
      | CContinue => CContinue
      | CBreak => CBreak
      | CSwitch(c, branches, partial) =>
        let c = map_imm_expression(c);
        let branches =
          List.map(
            ((tag, body)) => (tag, map_anf_expression(body)),
            branches,
          );
        CSwitch(c, branches, partial);
      | CApp((f, fty), args, tail) =>
        let f = map_imm_expression(f);
        let args = List.map(map_imm_expression, args);
        CApp((f, fty), args, tail);
      | CAppBuiltin(mod_, f, args) =>
        CAppBuiltin(mod_, f, List.map(map_imm_expression, args))
      | CLambda(name, idents, (expr, alloc_ty)) =>
        let expr = map_anf_expression(expr);
        CLambda(name, idents, (expr, alloc_ty));
      | CBytes(b) => CBytes(b)
      | CString(s) => CString(s)
      | CChar(c) => CChar(c)
      | CNumber(i) => CNumber(i)
      | CInt32(i) => CInt32(i)
      | CInt64(i) => CInt64(i)
      | CFloat32(f) => CFloat32(f)
      | CFloat64(f) => CFloat64(f)
      };

    Iter.leave_comp_expression({...c, comp_desc: d});
  }

  and map_anf_expression = anf => {
    let {anf_desc: desc} as anf = Iter.enter_anf_expression(anf);
    let d =
      switch (desc) {
      | AELet(g, r, m, bindings, body) =>
        let bindings =
          List.map(
            ((ident, bind)) => {
              let b = map_comp_expression(bind);
              (ident, b);
            },
            bindings,
          );
        let body = map_anf_expression(body);
        AELet(g, r, m, bindings, body);
      | AESeq(hd, tl) =>
        let hd = map_comp_expression(hd);
        let tl = map_anf_expression(tl);
        AESeq(hd, tl);
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
