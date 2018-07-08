(* Stripped-down version of OCaml's typedtree. Original copyright: *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Typed variant of the AST. *)
open Grain_parsing
open Types

type 'a loc = 'a Location.loc
type partial = Partial | Total

type rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive

type prim1 = Parsetree.prim1 =
  | Add1
  | Sub1
  | Not
  | Box
  | Unbox
  | IsNum
  | IsBool
  | IsTuple

type prim2 = Parsetree.prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | LessEq
  | GreaterEq
  | Eq
  | And
  | Or

type core_type = {
  ctyp_desc : core_type_desc;
  ctyp_type : type_expr;
  ctyp_env : Env.t;
  ctyp_loc: Location.t;
}

and core_type_desc =
  | TTyAny
  | TTyVar of string
  | TTyArrow of core_type list * core_type
  | TTyTuple of core_type list
  | TTyConstr of Path.t * Identifier.t loc * core_type list
  | TTyPoly of string list * core_type

type constructor_arguments =
  | TConstrTuple of core_type list
  | TConstrSingleton

type constructor_declaration = {
  cd_id: Ident.t;
  cd_name: string loc;
  cd_args: constructor_arguments;
  cd_res: core_type option;
  cd_loc: Location.t;
} [@@deriving sexp]

type data_kind =
  | TDataVariant of constructor_declaration list

type data_declaration = {
  data_id: Ident.t;
  data_name: string loc;
  data_params: core_type list;
  data_type: Types.type_declaration;
  data_kind: data_kind;
  data_loc: Location.t;
} [@@deriving sexp]

type pattern = {
  pat_desc: pattern_desc;
  pat_loc: Location.t;
  pat_extra: (pat_extra * Location.t) list;
  pat_type: type_expr;
  mutable pat_env: Env.t;
} [@@deriving sexp]

and pat_extra =
  | TPatConstraint of core_type

and pattern_desc =
  | TPatAny
  | TPatVar of Ident.t * string loc
  | TPatConstant of constant
  | TPatTuple of pattern list
  | TPatConstruct of Identifier.t loc * constructor_description * pattern list
  | TPatAlias of pattern * Ident.t * string loc
  | TPatOr of pattern * pattern

type expression = {
  exp_desc: expression_desc;
  exp_loc: Location.t;
  exp_extra: (exp_extra * Location.t) list;
  exp_type: type_expr;
  exp_env: Env.t;
} [@@deriving sexp]

and exp_extra =
  | TExpConstraint of core_type

and expression_desc =
  | TExpIdent of Path.t * Identifier.t loc * Types.value_description
  | TExpConstant of constant
  | TExpTuple of expression list
  | TExpLet of rec_flag * value_binding list * expression
  | TExpMatch of expression * match_branch list * partial
  | TExpPrim1 of prim1 * expression
  | TExpPrim2 of prim2 * expression * expression
  | TExpAssign of expression * expression
  | TExpIf of expression * expression * expression
  | TExpWhile of expression * expression
  | TExpLambda of match_branch list * partial
  | TExpApp of expression * expression list
  | TExpConstruct of Identifier.t loc * constructor_description * expression list (* TODO: Decide if needed *)
  | TExpBlock of expression list
  | TExpNull

and value_binding = {
  vb_pat: pattern;
  vb_expr: expression;
  vb_loc: Location.t;
}

and match_branch = {
  mb_pat: pattern;
  mb_body: expression;
  mb_loc: Location.t;
}

type import_declaration = {
  timp_path: Path.t;
  timp_mod: Identifier.t Location.loc;
  timp_loc: Location.t;
} [@@deriving sexp]

type value_description = {
  tvd_id: Ident.t;
  tvd_mod: string loc;
  tvd_name: string loc;
  tvd_desc: core_type;
  tvd_val: Types.value_description;
  tvd_prim: string list;
  tvd_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

type toplevel_stmt_desc =
  | TTopForeign of value_description
  | TTopImport of import_declaration
  | TTopData of data_declaration
  | TTopLet of rec_flag * value_binding list

type toplevel_stmt = {
  ttop_desc: toplevel_stmt_desc;
  ttop_loc: Location.t;
  ttop_env: Env.t;
} [@@deriving sexp]

type typed_program = {
  statements: toplevel_stmt list;
  body: expression;
  env: Env.t;
  signature: Cmi_format.cmi_infos;
} [@@deriving sexp]

(* Auxiliary functions over the AST *)

val iter_pattern_desc: (pattern -> unit) -> pattern_desc -> unit
val map_pattern_desc: (pattern -> pattern) -> pattern_desc -> pattern_desc

val let_bound_idents: value_binding list -> Ident.t list
val rev_let_bound_idents: value_binding list -> Ident.t list

val let_bound_idents_with_loc:
    value_binding list -> (Ident.t * string loc) list

(** Alpha conversion of patterns *)
val alpha_pat: (Ident.t * Ident.t) list -> pattern -> pattern

val mknoloc: 'a -> 'a Asttypes.loc
val mkloc: 'a -> Location.t -> 'a Asttypes.loc

val pattern_bound_idents: pattern -> Ident.t list
