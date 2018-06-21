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

open Sexplib.Conv
open Grain_parsing
open Types

type 'a loc = 'a Location.loc
type partial = Partial | Total [@@deriving sexp]

type rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive

type prim1 = Parsetree.prim1 =
  | Add1
  | Sub1
  | Not
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

let prim1_of_sexp, sexp_of_prim1 = Parsetree.prim1_of_sexp, Parsetree.sexp_of_prim1
let prim2_of_sexp, sexp_of_prim2 = Parsetree.prim2_of_sexp, Parsetree.sexp_of_prim2


type core_type = {
  ctyp_desc : core_type_desc;
  ctyp_type : type_expr;
  ctyp_env : Env.t sexp_opaque;
  ctyp_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

and core_type_desc =
  | TTyAny
  | TTyVar of string
  | TTyArrow of core_type list * core_type
  | TTyTuple of core_type list
  | TTyConstr of Path.t * Identifier.t loc * core_type list
  | TTyPoly of string list * core_type
[@@deriving sexp]

type constructor_arguments =
  | TConstrTuple of core_type list
  | TConstrSingleton
[@@deriving sexp]

type constructor_declaration = {
  cd_id: Ident.t;
  cd_name: string loc;
  cd_args: constructor_arguments;
  cd_res: core_type option;
  cd_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

type data_kind =
  | TDataVariant of constructor_declaration list
[@@deriving sexp]

type data_declaration = {
  data_id: Ident.t;
  data_name: string loc;
  data_params: core_type list;
  data_type: Types.type_declaration;
  data_kind: data_kind;
  data_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

type pattern = {
  pat_desc: pattern_desc;
  pat_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
  pat_extra: (pat_extra * Location.t) list [@default []] [@sexp_drop_default];
  pat_type: type_expr;
  mutable pat_env: Env.t sexp_opaque;
} [@@deriving sexp]

and pat_extra =
  | TPatConstraint of core_type
[@@deriving sexp]

and pattern_desc =
  | TPatAny
  | TPatVar of Ident.t * string loc
  | TPatConstant of constant
  | TPatTuple of pattern list
  | TPatConstruct of Identifier.t loc * constructor_description * pattern list
  | TPatAlias of pattern * Ident.t * string loc
  | TPatOr of pattern * pattern
[@@deriving sexp]

type expression = {
  exp_desc: expression_desc;
  exp_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
  exp_extra: (exp_extra * Location.t) list [@default []] [@sexp_drop_default];
  exp_type: type_expr;
  exp_env: Env.t sexp_opaque;
} [@@deriving sexp]

and exp_extra =
  | TExpConstraint of core_type
[@@deriving sexp]

and expression_desc =
  | TExpIdent of Path.t * Identifier.t loc * Types.value_description
  | TExpConstant of constant
  | TExpTuple of expression list
  | TExpLet of rec_flag * value_binding list * expression
  | TExpMatch of expression * match_branch list * partial
  | TExpPrim1 of prim1 * expression
  | TExpPrim2 of prim2 * expression * expression
  | TExpIf of expression * expression * expression
  | TExpLambda of match_branch list * partial
  | TExpApp of expression * expression list
  | TExpConstruct of Identifier.t loc * constructor_description * expression list
  | TExpBlock of expression list
  | TExpNull
[@@deriving sexp]

and value_binding = {
  vb_pat: pattern;
  vb_expr: expression;
  vb_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

and match_branch = {
  mb_pat: pattern;
  mb_body: expression;
  mb_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

type import_declaration = {
  timp_path: Path.t;
  timp_mod: Identifier.t loc;
  timp_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
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
[@@deriving sexp]

type toplevel_stmt = {
  ttop_desc: toplevel_stmt_desc;
  ttop_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
  ttop_env: Env.t sexp_opaque;
} [@@deriving sexp]

type typed_program = {
  statements: toplevel_stmt list;
  body: expression;
  env: Env.t sexp_opaque;
  signature: Cmi_format.cmi_infos;
} [@@deriving sexp]

let iter_pattern_desc f patt =
  match patt with
  | TPatTuple patts
  | TPatConstruct(_, _, patts) -> List.iter f patts
  | TPatAny
  | TPatVar _
  | TPatConstant _ -> ()
  | TPatAlias(p, _, _) -> f p
  | TPatOr(p1, p2) -> f p1; f p2

let map_pattern_desc f patt =
  match patt with
  | TPatTuple patts -> TPatTuple (List.map f patts)
  | TPatAlias(p1, id, s) -> TPatAlias(f p1, id, s)
  | TPatConstruct(lid, c, pats) -> TPatConstruct(lid, c, List.map f pats)
  | TPatOr(p1, p2) -> TPatOr(f p1, f p2)
  | _ -> patt

let pattern_bound_idents_and_locs patt =
  let ret = ref [] in
  let rec help {pat_desc=desc; _} =
    match desc with
    | TPatVar(id, s) -> ret := (id, s)::!ret
    | TPatAlias(p, id, s) ->
      help p; ret := (id, s)::!ret
    | TPatOr(p1, _) ->
      (* Invariant: both arguments bind the same variables *)
      help p1
    | _ -> iter_pattern_desc help desc in
  help patt;
  !ret

let pattern_bound_idents patt =
  pattern_bound_idents_and_locs patt
  |> List.map fst

let rev_let_bound_idents_with_loc bindings =
  List.map (fun {vb_pat; _} -> vb_pat) bindings
  |> List.map pattern_bound_idents_and_locs
  |> List.fold_left (fun acc cur -> cur @ acc) []

let let_bound_idents_with_loc bindings =
  rev_let_bound_idents_with_loc bindings
  |> List.rev

let rev_let_bound_idents pat = List.map fst (rev_let_bound_idents_with_loc pat)
let let_bound_idents pat = List.map fst (let_bound_idents_with_loc pat)

let alpha_var env id = List.assoc id env

let rec alpha_pat env ({pat_desc=desc; _} as p) =
  match desc with
  | TPatVar(id, s) ->
    let new_desc = try
        TPatVar(alpha_var env id, s)
      with
      | Not_found -> TPatAny in
    {p with pat_desc=new_desc}
  | TPatAlias(p1, id, s) ->
    let new_p = alpha_pat env p1 in
    begin try
        {p with pat_desc=TPatAlias(new_p, alpha_var env id, s)}
      with
      | Not_found -> new_p
    end
  | _ -> {p with pat_desc = map_pattern_desc (alpha_pat env) desc}

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc
