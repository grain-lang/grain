/* Modified from OCaml's source. Original copyright below. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Typedtree;

module type IteratorArgument = {
  let enter_typed_program: typed_program => unit;

  let enter_pattern: pattern => unit;
  let enter_expression: expression => unit;
  let enter_core_type: core_type => unit;
  let enter_toplevel_stmt: toplevel_stmt => unit;

  let leave_typed_program: typed_program => unit;
  let leave_pattern: pattern => unit;
  let leave_expression: expression => unit;
  let leave_core_type: core_type => unit;
  let leave_toplevel_stmt: toplevel_stmt => unit;

  let enter_bindings: (rec_flag, mut_flag) => unit;
  let enter_binding: value_binding => unit;
  let leave_binding: value_binding => unit;
  let leave_bindings: (rec_flag, mut_flag) => unit;

  let enter_data_declarations: unit => unit;
  let enter_data_declaration: data_declaration => unit;
  let leave_data_declaration: data_declaration => unit;
  let leave_data_declarations: unit => unit;
};

module MakeIterator =
       (Iter: IteratorArgument)
       : {
         let iter_typed_program: typed_program => unit;
         let iter_toplevel_stmt: toplevel_stmt => unit;
         let iter_expression: expression => unit;
         let iter_pattern: pattern => unit;
       } => {
  let may_iter = Option.iter;

  let rec iter_typed_program = ({statements} as tp) => {
    Iter.enter_typed_program(tp);
    iter_toplevel_stmts(statements);
    Iter.leave_typed_program(tp);
  }

  and iter_core_type = ct => {
    Iter.enter_core_type(ct);
    switch (ct.ctyp_desc) {
    | TTyAny
    | TTyVar(_) => ()
    | TTyArrow(args, ret) =>
      List.iter(((_, a)) => iter_core_type(a), args);
      iter_core_type(ret);
    | TTyConstr(_, _, args)
    | TTyTuple(args) => List.iter(iter_core_type, args)
    | TTyRecord(args) =>
      List.iter(((_, arg)) => iter_core_type(arg), args)
    | TTyPoly(_, typ) => iter_core_type(typ)
    };
    Iter.leave_core_type(ct);
  }

  and iter_binding = ({vb_pat, vb_expr} as vb) => {
    Iter.enter_binding(vb);
    iter_pattern(vb_pat);
    iter_expression(vb_expr);
    Iter.leave_binding(vb);
  }

  and iter_bindings = (rec_flag, mut_flag, binds) => {
    Iter.enter_bindings(rec_flag, mut_flag);
    List.iter(iter_binding, binds);
    Iter.leave_bindings(rec_flag, mut_flag);
  }

  and iter_match_branch = ({mb_pat, mb_body, mb_guard}) => {
    iter_pattern(mb_pat);
    iter_expression(mb_body);
    Option.iter(iter_expression, mb_guard);
  }

  and iter_match_branches = branches => List.iter(iter_match_branch, branches)

  and iter_constructor_arguments =
    fun
    | TConstrTuple(args) => List.iter(iter_core_type, args)
    | TConstrRecord(rfs) => List.iter(iter_record_field, rfs)
    | TConstrSingleton => ()

  and iter_constructor_declaration = ({cd_args, cd_res}) => {
    iter_constructor_arguments(cd_args);
    may_iter(iter_core_type, cd_res);
  }

  and iter_record_field = ({rf_type}) => iter_core_type(rf_type)

  and iter_type_parameter = ct => iter_core_type(ct)

  and iter_data_declaration = decl => {
    Iter.enter_data_declaration(decl);
    List.iter(iter_type_parameter, decl.data_params);
    switch (decl.data_kind) {
    | TDataAbstract => ()
    | TDataVariant(cstrs) => List.iter(iter_constructor_declaration, cstrs)
    | TDataRecord(labels) => List.iter(iter_record_field, labels)
    };
    Iter.leave_data_declaration(decl);
  }

  and iter_toplevel_stmt = stmt => {
    Iter.enter_toplevel_stmt(stmt);
    switch (stmt.ttop_desc) {
    | TTopData(decls) => List.iter(iter_data_declaration, decls)
    | TTopException(_)
    | TTopForeign(_)
    | TTopInclude(_)
    | TTopProvide(_) => ()
    | TTopModule({tmod_statements}) => iter_toplevel_stmts(tmod_statements)
    | TTopExpr(e) => iter_expression(e)
    | TTopLet(recflag, mutflag, binds) =>
      iter_bindings(recflag, mutflag, binds)
    };
    Iter.leave_toplevel_stmt(stmt);
  }
  and iter_toplevel_stmts = stmts =>
    List.iter(
      cur =>
        switch (cur.ttop_desc) {
        | TTopException(_)
        | TTopForeign(_)
        | TTopInclude(_)
        | TTopProvide(_)
        | TTopExpr(_)
        | TTopModule(_)
        | TTopLet(_) => iter_toplevel_stmt(cur)
        | TTopData(_) =>
          Iter.enter_data_declarations();
          iter_toplevel_stmt(cur);
          Iter.leave_data_declarations();
        },
      stmts,
    )

  and iter_pattern = pat => {
    Iter.enter_pattern(pat);
    List.iter(
      ((cstr, _)) =>
        switch (cstr) {
        | TPatConstraint(ct) => iter_core_type(ct)
        },
      pat.pat_extra,
    );
    switch (pat.pat_desc) {
    | TPatAny
    | TPatVar(_)
    | TPatConstant(_) => ()
    | TPatAlias(p1, _, _) => iter_pattern(p1)
    | TPatConstruct(_, _, args)
    | TPatTuple(args) => List.iter(iter_pattern, args)
    | TPatArray(args) => List.iter(iter_pattern, args)
    | TPatRecord(fields, _) =>
      List.iter(((_, _, pat)) => iter_pattern(pat), fields)
    | TPatOr(p1, p2) =>
      iter_pattern(p1);
      iter_pattern(p2);
    };
    Iter.leave_pattern(pat);
  }

  and iter_record_fields = rfs => {
    Array.iter(
      expr =>
        switch (expr) {
        | (_, Overridden(_, expr)) => iter_expression(expr)
        | _ => ()
        },
      rfs,
    );
  }

  and iter_expression = ({exp_desc, exp_extra} as exp) => {
    Iter.enter_expression(exp);
    List.iter(
      ((cstr, _)) =>
        switch (cstr) {
        | TExpConstraint(ct) => iter_core_type(ct)
        },
      exp_extra,
    );
    switch (exp_desc) {
    | TExpUse(_)
    | TExpIdent(_)
    | TExpConstant(_) => ()
    | TExpLet(recflag, mutflag, binds) =>
      iter_bindings(recflag, mutflag, binds)
    | TExpLambda(branches, _) => iter_match_branches(branches)
    | TExpApp(exp, _, args) =>
      iter_expression(exp);
      List.iter(((_, arg)) => iter_expression(arg), args);
    | TExpPrim0(_) => ()
    | TExpPrim1(_, e) => iter_expression(e)
    | TExpPrim2(_, e1, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | TExpPrimN(_, args) => List.iter(iter_expression, args)
    | TExpBoxAssign(e1, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | TExpAssign(e1, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | TExpMatch(value, branches, _) =>
      iter_expression(value);
      iter_match_branches(branches);
    | TExpRecord(b, args) =>
      Option.iter(iter_expression, b);
      iter_record_fields(args);
    | TExpRecordGet(expr, _, _) => iter_expression(expr)
    | TExpRecordSet(e1, _, _, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | TExpTuple(args)
    | TExpArray(args)
    | TExpBlock(args)
    | TExpConstruct(_, _, TExpConstrTuple(args)) =>
      List.iter(iter_expression, args)
    | TExpConstruct(_, _, TExpConstrRecord(args)) =>
      iter_record_fields(args)
    | TExpArrayGet(a1, a2) =>
      iter_expression(a1);
      iter_expression(a2);
    | TExpArraySet({array: a1, index: a2, value: a3, infix_op: a4}) =>
      iter_expression(a1);
      iter_expression(a2);
      iter_expression(a3);
      Option.iter(iter_expression, a4);
    | TExpIf(c, t, f) =>
      iter_expression(c);
      iter_expression(t);
      iter_expression(f);
    | TExpWhile(c, b) =>
      iter_expression(c);
      iter_expression(b);
    | TExpFor(i, c, inc, b) =>
      Option.iter(iter_expression, i);
      Option.iter(iter_expression, c);
      Option.iter(iter_expression, inc);
      iter_expression(b);
    | TExpContinue
    | TExpBreak => ()
    | TExpReturn(e) => Option.iter(iter_expression, e)
    };
    Iter.leave_expression(exp);
  };
};

module DefaultIteratorArgument: IteratorArgument = {
  let enter_typed_program = _ => ();
  let enter_pattern = _ => ();
  let enter_expression = _ => ();
  let enter_core_type = _ => ();
  let enter_toplevel_stmt = _ => ();
  let enter_bindings = (_, _) => ();
  let enter_binding = _ => ();
  let enter_data_declaration = _ => ();
  let enter_data_declarations = () => ();

  let leave_typed_program = _ => ();
  let leave_pattern = _ => ();
  let leave_expression = _ => ();
  let leave_core_type = _ => ();
  let leave_toplevel_stmt = _ => ();
  let leave_binding = _ => ();
  let leave_bindings = (_, _) => ();
  let leave_data_declaration = _ => ();
  let leave_data_declarations = () => ();
};
