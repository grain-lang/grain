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
      | [@implicit_arity] TTyArrow(args, ret) =>
        let args = List.map(map_core_type, args);
        let ret = map_core_type(ret);
        [@implicit_arity] TTyArrow(args, ret);
      | [@implicit_arity] TTyConstr(a, b, args) =>
        [@implicit_arity] TTyConstr(a, b, List.map(map_core_type, args))
      | TTyTuple(args) => TTyTuple(List.map(map_core_type, args))
      | TTyRecord(args) =>
        TTyRecord(
          List.map(((name, arg)) => (name, map_core_type(arg)), args),
        )
      | [@implicit_arity] TTyPoly(args, typ) =>
        let typ = map_core_type(typ);
        [@implicit_arity] TTyPoly(args, typ);
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

  and map_match_branch = ({mb_pat, mb_body} as mb) => {
    let mb_pat = map_pattern(mb_pat);
    let mb_body = map_expression(mb_body);
    {...mb, mb_pat, mb_body};
  }

  and map_match_branches = branches => List.map(map_match_branch, branches)

  and map_constructor_arguments =
    fun
    | TConstrTuple(args) => TConstrTuple(List.map(map_core_type, args))
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
      | TTopForeign(_)
      | TTopImport(_)
      | TTopExport(_) => stmt.ttop_desc
      | [@implicit_arity] TTopLet(exportflag, recflag, mutflag, binds) =>
        [@implicit_arity]
        TTopLet(
          exportflag,
          recflag,
          mutflag,
          map_bindings(recflag, mutflag, binds),
        )
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
      | [@implicit_arity] TPatAlias(p1, a, b) =>
        [@implicit_arity] TPatAlias(map_pattern(p1), a, b)
      | [@implicit_arity] TPatConstruct(a, b, args) =>
        [@implicit_arity] TPatConstruct(a, b, List.map(map_pattern, args))
      | TPatTuple(args) => TPatTuple(List.map(map_pattern, args))
      | [@implicit_arity] TPatRecord(fields, c) =>
        [@implicit_arity]
        TPatRecord(
          List.map(((id, ld, pat)) => (id, ld, map_pattern(pat)), fields),
          c,
        )
      | [@implicit_arity] TPatOr(p1, p2) =>
        [@implicit_arity] TPatOr(map_pattern(p1), map_pattern(p2))
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

  and map_expression = exp => {
    let exp = Map.enter_expression(exp);
    let exp_extra = List.map(map_exp_extra, exp.exp_extra);
    let exp_desc =
      switch (exp.exp_desc) {
      | TExpNull
      | TExpIdent(_)
      | TExpConstant(_) => exp.exp_desc
      | [@implicit_arity] TExpLet(recflag, mutflag, binds, body) =>
        [@implicit_arity]
        TExpLet(
          recflag,
          mutflag,
          map_bindings(recflag, mutflag, binds),
          map_expression(body),
        )
      | [@implicit_arity] TExpLambda(branches, p) =>
        [@implicit_arity] TExpLambda(map_match_branches(branches), p)
      | [@implicit_arity] TExpApp(exp, args) =>
        [@implicit_arity]
        TExpApp(map_expression(exp), List.map(map_expression, args))
      | [@implicit_arity] TExpPrim1(o, e) =>
        [@implicit_arity] TExpPrim1(o, map_expression(e))
      | [@implicit_arity] TExpPrim2(o, e1, e2) =>
        [@implicit_arity]
        TExpPrim2(o, map_expression(e1), map_expression(e2))
      | [@implicit_arity] TExpBoxAssign(e1, e2) =>
        [@implicit_arity]
        TExpBoxAssign(map_expression(e1), map_expression(e2))
      | [@implicit_arity] TExpAssign(e1, e2) =>
        [@implicit_arity] TExpAssign(map_expression(e1), map_expression(e2))
      | [@implicit_arity] TExpMatch(value, branches, p) =>
        [@implicit_arity]
        TExpMatch(map_expression(value), map_match_branches(branches), p)
      | TExpTuple(args) => TExpTuple(List.map(map_expression, args))
      | TExpArray(args) => TExpArray(List.map(map_expression, args))
      | [@implicit_arity] TExpArrayGet(a1, a2) =>
        [@implicit_arity]
        TExpArrayGet(map_expression(a1), map_expression(a2))
      | [@implicit_arity] TExpArraySet(a1, a2, a3) =>
        [@implicit_arity]
        TExpArraySet(
          map_expression(a1),
          map_expression(a2),
          map_expression(a3),
        )
      | TExpRecord(args) =>
        TExpRecord(
          Array.map(
            fun
            | (desc, [@implicit_arity] Overridden(name, expr)) => (
                desc,
                [@implicit_arity] Overridden(name, map_expression(expr)),
              )
            | (desc, def) => (desc, def),
            args,
          ),
        )
      | [@implicit_arity] TExpRecordGet(record, field, ld) =>
        [@implicit_arity] TExpRecordGet(map_expression(record), field, ld)
      | [@implicit_arity] TExpRecordSet(record, field, ld, arg) =>
        [@implicit_arity]
        TExpRecordSet(
          map_expression(record),
          field,
          ld,
          map_expression(arg),
        )
      | TExpBlock(args) => TExpBlock(List.map(map_expression, args))
      | [@implicit_arity] TExpConstruct(a, b, args) =>
        [@implicit_arity] TExpConstruct(a, b, List.map(map_expression, args))
      | [@implicit_arity] TExpIf(c, t, f) =>
        [@implicit_arity]
        TExpIf(map_expression(c), map_expression(t), map_expression(f))
      | [@implicit_arity] TExpWhile(c, b) =>
        [@implicit_arity] TExpWhile(map_expression(c), map_expression(b))
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
