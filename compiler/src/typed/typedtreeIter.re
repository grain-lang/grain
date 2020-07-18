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

open Grain_parsing;
open Asttypes;
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

  let enter_bindings: (export_flag, rec_flag, mut_flag) => unit;
  let enter_binding: value_binding => unit;
  let leave_binding: value_binding => unit;
  let leave_bindings: (export_flag, rec_flag, mut_flag) => unit;

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
    | [@implicit_arity] TTyArrow(args, ret) =>
      List.iter(iter_core_type, args);
      iter_core_type(ret);
    | [@implicit_arity] TTyConstr(_, _, args)
    | TTyTuple(args) => List.iter(iter_core_type, args)
    | TTyRecord(args) =>
      List.iter(((_, arg)) => iter_core_type(arg), args)
    | [@implicit_arity] TTyPoly(_, typ) => iter_core_type(typ)
    };
    Iter.leave_core_type(ct);
  }

  and iter_binding = ({vb_pat, vb_expr} as vb) => {
    Iter.enter_binding(vb);
    iter_pattern(vb_pat);
    iter_expression(vb_expr);
    Iter.leave_binding(vb);
  }

  and iter_bindings = (export_flag, rec_flag, mut_flag, binds) => {
    Iter.enter_bindings(export_flag, rec_flag, mut_flag);
    List.iter(iter_binding, binds);
    Iter.leave_bindings(export_flag, rec_flag, mut_flag);
  }

  and iter_match_branch = ({mb_pat, mb_body}) => {
    iter_pattern(mb_pat);
    iter_expression(mb_body);
  }

  and iter_match_branches = branches => List.iter(iter_match_branch, branches)

  and iter_constructor_arguments =
    fun
    | TConstrTuple(args) => List.iter(iter_core_type, args)
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
    | TDataVariant(cstrs) => List.iter(iter_constructor_declaration, cstrs)
    | TDataRecord(labels) => List.iter(iter_record_field, labels)
    };
    Iter.leave_data_declaration(decl);
  }

  /* FIXME: These two functions are gross */
  and iter_toplevel_stmt = stmt => {
    Iter.enter_toplevel_stmt(stmt);
    switch (stmt.ttop_desc) {
    | TTopData(decls) => List.iter(iter_data_declaration, decls)
    | TTopForeign(_)
    | TTopImport(_)
    | TTopExport(_) => ()
    | TTopExpr(e) => iter_expression(e)
    | [@implicit_arity] TTopLet(exportflag, recflag, mutflag, binds) =>
      iter_bindings(exportflag, recflag, mutflag, binds)
    };
    Iter.leave_toplevel_stmt(stmt);
  }

  and iter_toplevel_stmts = stmts =>
    List.iter(
      cur =>
        switch (cur.ttop_desc) {
        | TTopForeign(_)
        | TTopImport(_)
        | TTopExport(_)
        | TTopExpr(_)
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
    | [@implicit_arity] TPatAlias(p1, _, _) => iter_pattern(p1)
    | [@implicit_arity] TPatConstruct(_, _, args)
    | TPatTuple(args) => List.iter(iter_pattern, args)
    | [@implicit_arity] TPatRecord(fields, _) =>
      List.iter(((_, _, pat)) => iter_pattern(pat), fields)
    | [@implicit_arity] TPatOr(p1, p2) =>
      iter_pattern(p1);
      iter_pattern(p2);
    };
    Iter.leave_pattern(pat);
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
    | TExpNull
    | TExpIdent(_)
    | TExpConstant(_) => ()
    | [@implicit_arity] TExpLet(recflag, mutflag, binds, body) =>
      iter_bindings(Nonexported, recflag, mutflag, binds);
      iter_expression(body);
    | [@implicit_arity] TExpLambda(branches, _) =>
      iter_match_branches(branches)
    | [@implicit_arity] TExpApp(exp, args) =>
      iter_expression(exp);
      List.iter(iter_expression, args);
    | [@implicit_arity] TExpPrim1(_, e) => iter_expression(e)
    | [@implicit_arity] TExpPrim2(_, e1, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | [@implicit_arity] TExpBoxAssign(e1, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | [@implicit_arity] TExpAssign(e1, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | [@implicit_arity] TExpMatch(value, branches, _) =>
      iter_expression(value);
      iter_match_branches(branches);
    | TExpRecord(args) =>
      Array.iter(
        fun
        | (_, [@implicit_arity] Overridden(_, expr)) =>
          iter_expression(expr)
        | _ => (),
        args,
      )
    | [@implicit_arity] TExpRecordGet(expr, _, _) => iter_expression(expr)
    | [@implicit_arity] TExpRecordSet(e1, _, _, e2) =>
      iter_expression(e1);
      iter_expression(e2);
    | TExpTuple(args)
    | TExpArray(args)
    | TExpBlock(args)
    | [@implicit_arity] TExpConstruct(_, _, args) =>
      List.iter(iter_expression, args)
    | [@implicit_arity] TExpArrayGet(a1, a2) =>
      iter_expression(a1);
      iter_expression(a2);
    | [@implicit_arity] TExpArraySet(a1, a2, a3) =>
      iter_expression(a1);
      iter_expression(a2);
      iter_expression(a3);
    | [@implicit_arity] TExpIf(c, t, f) =>
      iter_expression(c);
      iter_expression(t);
      iter_expression(f);
    | [@implicit_arity] TExpWhile(c, b) =>
      iter_expression(c);
      iter_expression(b);
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
  let enter_bindings = (_, _, _) => ();
  let enter_binding = _ => ();
  let enter_data_declaration = _ => ();
  let enter_data_declarations = () => ();

  let leave_typed_program = _ => ();
  let leave_pattern = _ => ();
  let leave_expression = _ => ();
  let leave_core_type = _ => ();
  let leave_toplevel_stmt = _ => ();
  let leave_binding = _ => ();
  let leave_bindings = (_, _, _) => ();
  let leave_data_declaration = _ => ();
  let leave_data_declarations = () => ();
};
