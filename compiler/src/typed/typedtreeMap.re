/* Modified from OCaml's source. Original copyright below. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                   Fabrice Le Fessant, INRIA Saclay                     */
/*                                                                        */
/*   Copyright 2012 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Typedtree;

module type MapArgument = {
  let enter_typed_program: typed_program => typed_program;
  let enter_data_declaration: data_declaration => data_declaration;

  let enter_pattern: pattern => pattern;
  let enter_expression: expression => expression;

  let enter_core_type: core_type => core_type;
  let enter_toplevel_stmt: toplevel_stmt => toplevel_stmt;

  let leave_typed_program: typed_program => typed_program;
  let leave_data_declaration: data_declaration => data_declaration;
  let leave_pattern: pattern => pattern;
  let leave_expression: expression => expression;

  let leave_core_type: core_type => core_type;
  let leave_toplevel_stmt: toplevel_stmt => toplevel_stmt;
};

module MakeMap =
       (Map: MapArgument)
       : {
         let map_typed_program: typed_program => typed_program;
         let map_pattern: pattern => pattern;
         let map_toplevel_stmt: toplevel_stmt => toplevel_stmt;
         let map_expression: expression => expression;
       } => {
  let rec map_typed_program = tp => {
    let tp = Map.enter_typed_program(tp);
    let statements = List.map(map_toplevel_stmt, tp.statements);
    Map.leave_typed_program({...tp, statements});
  }

  and map_core_type = ct => {
    let ct = Map.enter_core_type(ct);
    let ctyp_desc =
      switch (ct.ctyp_desc) {
      | TTyAny
      | TTyVar(_) => ct.ctyp_desc
      | TTyArrow(args, ret) =>
        let args = List.map(((l, arg)) => (l, map_core_type(arg)), args);
        let ret = map_core_type(ret);
        TTyArrow(args, ret);
      | TTyConstr(a, b, args) =>
        TTyConstr(a, b, List.map(map_core_type, args))
      | TTyTuple(args) => TTyTuple(List.map(map_core_type, args))
      | TTyRecord(args) =>
        TTyRecord(
          List.map(((name, arg)) => (name, map_core_type(arg)), args),
        )
      | TTyPoly(args, typ) =>
        let typ = map_core_type(typ);
        TTyPoly(args, typ);
      };
    Map.leave_core_type({...ct, ctyp_desc});
  }

  and map_binding = vb => {
    let vb_pat = map_pattern(vb.vb_pat);
    let vb_expr = map_expression(vb.vb_expr);
    {...vb, vb_pat, vb_expr};
  }

  and map_bindings = (rec_flag, mut_flag, binds) =>
    List.map(map_binding, binds)

  and map_match_branch = ({mb_pat, mb_body, mb_guard} as mb) => {
    let mb_pat = map_pattern(mb_pat);
    let mb_body = map_expression(mb_body);
    let mb_guard = Option.map(map_expression, mb_guard);
    {...mb, mb_pat, mb_body, mb_guard};
  }

  and map_match_branches = branches => List.map(map_match_branch, branches)

  and map_constructor_arguments =
    fun
    | TConstrTuple(args) => TConstrTuple(List.map(map_core_type, args))
    | TConstrRecord(rfs) => TConstrRecord(List.map(map_record_field, rfs))
    | TConstrSingleton as ca => ca

  and map_constructor_declaration = ({cd_args, cd_res} as cd) => {
    let cd_args = map_constructor_arguments(cd_args);
    let cd_res = Option.map(map_core_type, cd_res);
    {...cd, cd_args, cd_res};
  }

  and map_record_field = ({rf_type} as rf) => {
    let rf_type = map_core_type(rf_type);
    {...rf, rf_type};
  }

  and map_type_parameter = ct => map_core_type(ct)

  and map_data_declaration = decl => {
    let decl = Map.enter_data_declaration(decl);
    let data_params = List.map(map_type_parameter, decl.data_params);
    let data_kind =
      switch (decl.data_kind) {
      | TDataAbstract => TDataAbstract
      | TDataVariant(cstrs) =>
        TDataVariant(List.map(map_constructor_declaration, cstrs))
      | TDataRecord(lbls) => TDataRecord(List.map(map_record_field, lbls))
      };
    Map.leave_data_declaration({...decl, data_params, data_kind});
  }

  and map_toplevel_stmt = stmt => {
    let stmt = Map.enter_toplevel_stmt(stmt);
    let ttop_desc =
      switch (stmt.ttop_desc) {
      | TTopData(decls) => TTopData(List.map(map_data_declaration, decls))
      | TTopException(_)
      | TTopForeign(_)
      | TTopInclude(_)
      | TTopProvide(_) => stmt.ttop_desc
      | TTopModule(decl) =>
        TTopModule({
          ...decl,
          tmod_statements: map_toplevel_stmts(decl.tmod_statements),
        })
      | TTopLet(recflag, mutflag, binds) =>
        TTopLet(recflag, mutflag, map_bindings(recflag, mutflag, binds))
      | TTopExpr(e) => TTopExpr(map_expression(e))
      };
    Map.leave_toplevel_stmt({...stmt, ttop_desc});
  }

  and map_toplevel_stmts = stmts => List.map(map_toplevel_stmt, stmts)

  and map_pat_extra = ((cstr, loc)) => {
    let cstr =
      switch (cstr) {
      | TPatConstraint(ct) => TPatConstraint(map_core_type(ct))
      };
    (cstr, loc);
  }

  and map_pattern = pat => {
    let pat = Map.enter_pattern(pat);
    let pat_extra = List.map(map_pat_extra, pat.pat_extra);
    let pat_desc =
      switch (pat.pat_desc) {
      | TPatAny
      | TPatVar(_)
      | TPatConstant(_) => pat.pat_desc
      | TPatAlias(p1, a, b) => TPatAlias(map_pattern(p1), a, b)
      | TPatConstruct(a, b, args) =>
        TPatConstruct(a, b, List.map(map_pattern, args))
      | TPatTuple(args) => TPatTuple(List.map(map_pattern, args))
      | TPatArray(args) => TPatArray(List.map(map_pattern, args))
      | TPatRecord(fields, c) =>
        TPatRecord(
          List.map(((id, ld, pat)) => (id, ld, map_pattern(pat)), fields),
          c,
        )
      | TPatOr(p1, p2) => TPatOr(map_pattern(p1), map_pattern(p2))
      };
    Map.leave_pattern({...pat, pat_extra, pat_desc});
  }

  and map_exp_extra = ((cstr, loc)) => {
    let cstr =
      switch (cstr) {
      | TExpConstraint(ct) => TExpConstraint(map_core_type(ct))
      };
    (cstr, loc);
  }

  and map_record_fields = rfs => {
    Array.map(
      rf =>
        switch (rf) {
        | (desc, Overridden(name, expr)) => (
            desc,
            Overridden(name, map_expression(expr)),
          )
        | (desc, def) => (desc, def)
        },
      rfs,
    );
  }

  and map_expression = exp => {
    let exp = Map.enter_expression(exp);
    let exp_extra = List.map(map_exp_extra, exp.exp_extra);
    let exp_desc =
      switch (exp.exp_desc) {
      | TExpUse(_)
      | TExpIdent(_)
      | TExpConstant(_) => exp.exp_desc
      | TExpLet(recflag, mutflag, binds) =>
        TExpLet(recflag, mutflag, map_bindings(recflag, mutflag, binds))
      | TExpLambda(branches, p) =>
        TExpLambda(map_match_branches(branches), p)
      | TExpApp(exp, labels, args) =>
        TExpApp(
          map_expression(exp),
          labels,
          List.map(((l, arg)) => (l, map_expression(arg)), args),
        )
      | TExpPrim0(o) => TExpPrim0(o)
      | TExpPrim1(o, e) => TExpPrim1(o, map_expression(e))
      | TExpPrim2(o, e1, e2) =>
        TExpPrim2(o, map_expression(e1), map_expression(e2))
      | TExpPrimN(o, args) => TExpPrimN(o, List.map(map_expression, args))
      | TExpBoxAssign(e1, e2) =>
        TExpBoxAssign(map_expression(e1), map_expression(e2))
      | TExpAssign(e1, e2) =>
        TExpAssign(map_expression(e1), map_expression(e2))
      | TExpMatch(value, branches, p) =>
        TExpMatch(map_expression(value), map_match_branches(branches), p)
      | TExpTuple(args) => TExpTuple(List.map(map_expression, args))
      | TExpArray(args) => TExpArray(List.map(map_expression, args))
      | TExpArrayGet(a1, a2) =>
        TExpArrayGet(map_expression(a1), map_expression(a2))
      | TExpArraySet({array, index, value, infix_op}) =>
        TExpArraySet({
          array: map_expression(array),
          index: map_expression(index),
          value: map_expression(value),
          infix_op: Option.map(map_expression, infix_op),
        })
      | TExpRecord(b, args) =>
        TExpRecord(Option.map(map_expression, b), map_record_fields(args))
      | TExpRecordGet(record, field, ld) =>
        TExpRecordGet(map_expression(record), field, ld)
      | TExpRecordSet(record, field, ld, arg) =>
        TExpRecordSet(
          map_expression(record),
          field,
          ld,
          map_expression(arg),
        )
      | TExpBlock(args) => TExpBlock(List.map(map_expression, args))
      | TExpConstruct(a, b, TExpConstrTuple(args)) =>
        TExpConstruct(a, b, TExpConstrTuple(List.map(map_expression, args)))
      | TExpConstruct(a, b, TExpConstrRecord(args)) =>
        TExpConstruct(a, b, TExpConstrRecord(map_record_fields(args)))
      | TExpIf(c, t, f) =>
        TExpIf(map_expression(c), map_expression(t), map_expression(f))
      | TExpWhile(c, b) => TExpWhile(map_expression(c), map_expression(b))
      | TExpFor(i, c, inc, b) =>
        TExpFor(
          Option.map(map_expression, i),
          Option.map(map_expression, c),
          Option.map(map_expression, inc),
          map_expression(b),
        )
      | TExpContinue => TExpContinue
      | TExpBreak => TExpBreak
      | TExpReturn(e) => TExpReturn(Option.map(map_expression, e))
      };
    Map.leave_expression({...exp, exp_extra, exp_desc});
  };
};

module DefaultMapArgument: MapArgument = {
  let enter_typed_program = a => a;
  let enter_data_declaration = a => a;
  let enter_pattern = a => a;
  let enter_expression = a => a;
  let enter_core_type = a => a;
  let enter_toplevel_stmt = a => a;
  let leave_typed_program = a => a;
  let leave_data_declaration = a => a;
  let leave_pattern = a => a;
  let leave_expression = a => a;
  let leave_core_type = a => a;
  let leave_toplevel_stmt = a => a;
};
