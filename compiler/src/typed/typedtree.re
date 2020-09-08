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

open Sexplib.Conv;
open Grain_parsing;
open Types;

let sexp_locs_disabled = _ => ! Grain_utils.Config.sexp_locs_enabled^;

type loc('a) = Location.loc('a);
[@deriving sexp]
type partial =
  | Partial
  | Total;

type export_flag = Asttypes.export_flag = | Nonexported | Exported;
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

let (prim1_of_sexp, sexp_of_prim1) = (
  Parsetree.prim1_of_sexp,
  Parsetree.sexp_of_prim1,
);
let (prim2_of_sexp, sexp_of_prim2) = (
  Parsetree.prim2_of_sexp,
  Parsetree.sexp_of_prim2,
);

[@deriving sexp]
type core_type = {
  ctyp_desc: core_type_desc,
  ctyp_type: type_expr,
  ctyp_env: [@sexp.opaque] Env.t,
  [@sexp_drop_if sexp_locs_disabled]
  ctyp_loc: Location.t,
}

[@deriving sexp]
and core_type_desc =
  | TTyAny
  | TTyVar(string)
  | TTyArrow(list(core_type), core_type)
  | TTyTuple(list(core_type))
  | TTyRecord(list((loc(Identifier.t), core_type)))
  | TTyConstr(Path.t, loc(Identifier.t), list(core_type))
  | TTyPoly(list(string), core_type);

[@deriving sexp]
type constructor_arguments =
  | TConstrTuple(list(core_type))
  | TConstrSingleton;

[@deriving sexp]
type constructor_declaration = {
  cd_id: Ident.t,
  cd_name: loc(string),
  cd_args: constructor_arguments,
  cd_res: option(core_type),
  [@sexp_drop_if sexp_locs_disabled]
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

[@deriving sexp]
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
  [@sexp_drop_if sexp_locs_disabled]
  data_loc: Location.t,
};

[@deriving sexp]
type pattern = {
  pat_desc: pattern_desc,
  [@sexp_drop_if sexp_locs_disabled]
  pat_loc: Location.t,
  [@default []] [@sexp_drop_default (==)]
  pat_extra: list((pat_extra, Location.t)),
  pat_type: type_expr,
  mutable pat_env: [@sexp.opaque] Env.t,
}

[@deriving sexp]
and pat_extra =
  | TPatConstraint(core_type)

[@deriving sexp]
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
  [@sexp_drop_if sexp_locs_disabled]
  exp_loc: Location.t,
  [@default []] [@sexp_drop_default (==)]
  exp_extra: list((exp_extra, Location.t)),
  exp_type: type_expr,
  exp_env: [@sexp.opaque] Env.t,
}

[@deriving sexp]
and exp_extra =
  | TExpConstraint(core_type)

[@deriving sexp]
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
    )
  | TExpBlock(list(expression))
  | TExpNull

and record_label_definition =
  | Kept(Types.type_expr)
  | Overridden(loc(Identifier.t), expression)

[@deriving sexp]
and value_binding = {
  vb_pat: pattern,
  vb_expr: expression,
  [@sexp_drop_if sexp_locs_disabled]
  vb_loc: Location.t,
}

[@deriving sexp]
and match_branch = {
  mb_pat: pattern,
  mb_body: expression,
  [@sexp_drop_if sexp_locs_disabled]
  mb_loc: Location.t,
};

[@deriving sexp]
type import_declaration = {
  timp_path: Path.t,
  [@sexp_drop_if sexp_locs_disabled]
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

[@deriving sexp]
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
  [@sexp_drop_if sexp_locs_disabled]
  ttop_loc: Location.t,
  ttop_env: [@sexp.opaque] Env.t,
};

[@deriving sexp]
type typed_program = {
  statements: list(toplevel_stmt),
  env: [@sexp.opaque] Env.t,
  signature: Cmi_format.cmi_infos,
};

let iter_pattern_desc = (f, patt) =>
  switch (patt) {
  | TPatTuple(patts)
  | [@implicit_arity] TPatConstruct(_, _, patts) => List.iter(f, patts)
  | [@implicit_arity] TPatRecord(fields, _) =>
    List.iter(((_, _, p)) => f(p), fields)
  | TPatAny
  | TPatVar(_)
  | TPatConstant(_) => ()
  | [@implicit_arity] TPatAlias(p, _, _) => f(p)
  | [@implicit_arity] TPatOr(p1, p2) =>
    f(p1);
    f(p2);
  };

let map_pattern_desc = (f, patt) =>
  switch (patt) {
  | TPatTuple(patts) => TPatTuple(List.map(f, patts))
  | [@implicit_arity] TPatRecord(fields, c) =>
    [@implicit_arity]
    TPatRecord(List.map(((id, ld, pat)) => (id, ld, f(pat)), fields), c)
  | [@implicit_arity] TPatAlias(p1, id, s) =>
    [@implicit_arity] TPatAlias(f(p1), id, s)
  | [@implicit_arity] TPatConstruct(lid, c, pats) =>
    [@implicit_arity] TPatConstruct(lid, c, List.map(f, pats))
  | [@implicit_arity] TPatOr(p1, p2) =>
    [@implicit_arity] TPatOr(f(p1), f(p2))
  | _ => patt
  };

let pattern_bound_idents_and_locs = patt => {
  let ret = ref([]);
  let rec help = ({pat_desc: desc, _}) =>
    switch (desc) {
    | [@implicit_arity] TPatVar(id, s) => ret := [(id, s), ...ret^]
    | [@implicit_arity] TPatAlias(p, id, s) =>
      help(p);
      ret := [(id, s), ...ret^];
    | [@implicit_arity] TPatOr(p1, _) =>
      /* Invariant: both arguments bind the same variables */
      help(p1)
    | _ => iter_pattern_desc(help, desc)
    };
  help(patt);
  ret^;
};

let pattern_bound_idents = patt =>
  pattern_bound_idents_and_locs(patt) |> List.map(fst);

let rev_let_bound_idents_with_loc = bindings =>
  List.map(({vb_pat, _}) => vb_pat, bindings)
  |> List.map(pattern_bound_idents_and_locs)
  |> List.fold_left((acc, cur) => cur @ acc, []);

let let_bound_idents_with_loc = bindings =>
  rev_let_bound_idents_with_loc(bindings) |> List.rev;

let rev_let_bound_idents = pat =>
  List.map(fst, rev_let_bound_idents_with_loc(pat));
let let_bound_idents = pat => List.map(fst, let_bound_idents_with_loc(pat));

let alpha_var = (env, id) => List.assoc(id, env);

let rec alpha_pat = (env, {pat_desc: desc, _} as p) =>
  switch (desc) {
  | [@implicit_arity] TPatVar(id, s) =>
    let new_desc =
      try([@implicit_arity] TPatVar(alpha_var(env, id), s)) {
      | Not_found => TPatAny
      };
    {...p, pat_desc: new_desc};
  | [@implicit_arity] TPatAlias(p1, id, s) =>
    let new_p = alpha_pat(env, p1);
    try({
      ...p,
      pat_desc: [@implicit_arity] TPatAlias(new_p, alpha_var(env, id), s),
    }) {
    | Not_found => new_p
    };
  | _ => {...p, pat_desc: map_pattern_desc(alpha_pat(env), desc)}
  };

let mkloc = Location.mkloc;
let mknoloc = Location.mknoloc;
