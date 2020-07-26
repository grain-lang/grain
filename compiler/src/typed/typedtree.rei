/* Stripped-down version of OCaml's typedtree. Original copyright: */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/** Typed variant of the AST. */

open Grain_parsing;
open Types;

let sexp_locs_disabled: 'a => bool;

type loc('a) = Location.loc('a);
type partial =
  | Partial
  | Total;

type rec_flag = Asttypes.rec_flag = | Nonrecursive | Recursive;
type mut_flag = Asttypes.mut_flag = | Mutable | Immutable;

type prim1 =
  Parsetree.prim1 =
    | Incr
    | Decr
    | Not
    | Box
    | Unbox
    | Ignore
    | ArrayLength
    | Assert
    | FailWith
    | Int64FromNumber
    | Int64ToNumber
    | Int64Lnot;

type prim2 =
  Parsetree.prim2 =
    | Plus
    | Minus
    | Times
    | Divide
    | Mod
    | Less
    | Greater
    | LessEq
    | GreaterEq
    | Eq
    | And
    | Or
    | ArrayMake
    | ArrayInit
    | Int64Land
    | Int64Lor
    | Int64Lxor
    | Int64Lsl
    | Int64Lsr
    | Int64Asr
    | Int64Gt
    | Int64Gte
    | Int64Lt
    | Int64Lte;

type core_type = {
  ctyp_desc: core_type_desc,
  ctyp_type: type_expr,
  ctyp_env: Env.t,
  ctyp_loc: Location.t,
}

and core_type_desc =
  | TTyAny
  | TTyVar(string)
  | TTyArrow(list(core_type), core_type)
  | TTyTuple(list(core_type))
  | TTyRecord(list((loc(Identifier.t), core_type)))
  | TTyConstr(Path.t, loc(Identifier.t), list(core_type))
  | TTyPoly(list(string), core_type);

type constructor_arguments =
  | TConstrTuple(list(core_type))
  | TConstrSingleton;

[@deriving sexp]
type constructor_declaration = {
  cd_id: Ident.t,
  cd_name: loc(string),
  cd_args: constructor_arguments,
  cd_res: option(core_type),
  cd_loc: Location.t,
};

[@deriving sexp]
type record_field = {
  rf_name: Ident.t,
  rf_type: core_type,
  rf_mutable: bool,
  [@sexp_drop_if sexp_locs_disabled]
  rf_loc: Location.t,
};

type data_kind =
  | TDataVariant(list(constructor_declaration))
  | TDataRecord(list(record_field));

[@deriving sexp]
type data_declaration = {
  data_id: Ident.t,
  data_name: loc(string),
  data_params: list(core_type),
  data_type: Types.type_declaration,
  data_kind,
  data_loc: Location.t,
};

[@deriving sexp]
type pattern = {
  pat_desc: pattern_desc,
  pat_loc: Location.t,
  pat_extra: list((pat_extra, Location.t)),
  pat_type: type_expr,
  mutable pat_env: Env.t,
}

and pat_extra =
  | TPatConstraint(core_type)

and pattern_desc =
  | TPatAny
  | TPatVar(Ident.t, loc(string))
  | TPatConstant(constant)
  | TPatTuple(list(pattern))
  | TPatRecord(
      list((loc(Identifier.t), label_description, pattern)),
      closed_flag,
    )
  | TPatConstruct(loc(Identifier.t), constructor_description, list(pattern))
  | TPatAlias(pattern, Ident.t, loc(string))
  | TPatOr(pattern, pattern);

[@deriving sexp]
type expression = {
  exp_desc: expression_desc,
  exp_loc: Location.t,
  exp_extra: list((exp_extra, Location.t)),
  exp_type: type_expr,
  exp_env: Env.t,
}

and exp_extra =
  | TExpConstraint(core_type)

and expression_desc =
  | TExpIdent(Path.t, loc(Identifier.t), Types.value_description)
  | TExpConstant(constant)
  | TExpTuple(list(expression))
  | TExpArray(list(expression))
  | TExpArrayGet(expression, expression)
  | TExpArraySet(expression, expression, expression)
  | TExpRecord(array((Types.label_description, record_label_definition)))
  | TExpRecordGet(expression, loc(Identifier.t), Types.label_description)
  | TExpRecordSet(
      expression,
      loc(Identifier.t),
      Types.label_description,
      expression,
    )
  | TExpLet(rec_flag, mut_flag, list(value_binding), expression)
  | TExpMatch(expression, list(match_branch), partial)
  | TExpPrim1(prim1, expression)
  | TExpPrim2(prim2, expression, expression)
  | TExpBoxAssign(expression, expression)
  | TExpAssign(expression, expression)
  | TExpIf(expression, expression, expression)
  | TExpWhile(expression, expression)
  | TExpLambda(list(match_branch), partial)
  | TExpApp(expression, list(expression))
  | TExpConstruct(
      loc(Identifier.t),
      constructor_description,
      list(expression),
    ) /* TODO: Decide if needed */
  | TExpBlock(list(expression))
  | TExpNull

and record_label_definition =
  | Kept(Types.type_expr)
  | Overridden(loc(Identifier.t), expression)

and value_binding = {
  vb_pat: pattern,
  vb_expr: expression,
  vb_loc: Location.t,
}

and match_branch = {
  mb_pat: pattern,
  mb_body: expression,
  mb_loc: Location.t,
};

[@deriving sexp]
type import_declaration = {
  timp_path: Path.t,
  timp_loc: Location.t,
};

[@deriving sexp]
type export_declaration = {
  tex_path: Path.t,
  [@sexp_drop_if sexp_locs_disabled]
  tex_loc: Location.t,
};

[@deriving sexp]
type value_description = {
  tvd_id: Ident.t,
  tvd_mod: loc(string),
  tvd_name: loc(string),
  tvd_desc: core_type,
  tvd_val: Types.value_description,
  tvd_prim: list(string),
  [@sexp_drop_if sexp_locs_disabled]
  tvd_loc: Location.t,
};

type toplevel_stmt_desc =
  | TTopForeign(value_description)
  | TTopImport(import_declaration)
  | TTopExport(list(export_declaration))
  | TTopData(list(data_declaration))
  | TTopLet(export_flag, rec_flag, mut_flag, list(value_binding))
  | TTopExpr(expression);

[@deriving sexp]
type toplevel_stmt = {
  ttop_desc: toplevel_stmt_desc,
  ttop_loc: Location.t,
  ttop_env: Env.t,
};

[@deriving sexp]
type typed_program = {
  statements: list(toplevel_stmt),
  env: Env.t,
  signature: Cmi_format.cmi_infos,
};

/* Auxiliary functions over the AST */

let iter_pattern_desc: (pattern => unit, pattern_desc) => unit;
let map_pattern_desc: (pattern => pattern, pattern_desc) => pattern_desc;

let let_bound_idents: list(value_binding) => list(Ident.t);
let rev_let_bound_idents: list(value_binding) => list(Ident.t);

let let_bound_idents_with_loc:
  list(value_binding) => list((Ident.t, loc(string)));

/** Alpha conversion of patterns */

let alpha_pat: (list((Ident.t, Ident.t)), pattern) => pattern;

let mknoloc: 'a => Asttypes.loc('a);
let mkloc: ('a, Location.t) => Asttypes.loc('a);

let pattern_bound_idents: pattern => list(Ident.t);
