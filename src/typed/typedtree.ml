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

open Grain_parsing
open Types

type 'a loc = 'a Location.loc
type partial = Partial | Total

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

type constructor_arguments =
  | TConstrTuple of core_type list
  | TConstrSingleton

type constructor_declaration = {
  cd_id: Ident.t;
  cd_name: string loc;
  cd_args: constructor_arguments;
  cd_res: core_type option;
  cd_loc: Location.t;
}

type data_kind =
  | TDataVariant of constructor_declaration list

type data_declaration = {
  data_id: Ident.t;
  data_name: string loc;
  data_params: core_type list;
  data_type: Types.type_declaration;
  data_kind: data_kind;
  data_loc: Location.t;
}

type pattern = {
  pat_desc: pattern_desc;
  pat_loc: Location.t;
  pat_type: type_expr;
}

and pat_extra =
  | TPatConstraint of core_type

and pattern_desc =
  | TPatAny
  | TPatVar of Ident.t * string loc
  | TPatTuple of pattern list

type expression = {
  exp_desc: expression_desc;
  exp_loc: Location.t;
  exp_extra: (exp_extra * Location.t) list;
  exp_type: type_expr;
  exp_env: Env.t;
}

and exp_extra =
  | TExpConstraint of core_type

and expression_desc =
  | TExpIdent of Path.t * Identifier.t loc * Env.t
  | TExpConstant of constant
  | TExpTuple of expression list
  | TExpLet of rec_flag * value_binding list * expression
  | TExpMatch of expression * match_branch list * partial
  | TExpPrim1 of prim1 * expression
  | TExpPrim2 of prim2 * expression * expression
  | TExpIf of expression * expression * expression
  | TExpLambda of pattern list * expression * partial
  | TExpApp of expression * expression list
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
  timp_mod: Identifier.t Location.loc;
  timp_loc: Location.t;
}

type toplevel_stmt_desc =
  | TTopImport of import_declaration
  | TTopData of data_declaration
  | TTopLet of rec_flag * value_binding list

type toplevel_stmt = {
  ttop_desc: toplevel_stmt_desc;
  ttop_loc: Location.t;
}

type typed_program = {
  statements: toplevel_stmt list;
  body: expression;
}

let iter_pattern_desc f patt =
  match patt with
  | TPatTuple patts -> List.iter f patts
  | TPatAny
  | TPatVar _ -> ()

let map_pattern_desc f patt =
  match patt with
  | TPatTuple patts -> TPatTuple (List.map f patts)
  | _ -> patt

let pattern_bound_idents_and_locs patt =
  let ret = ref [] in
  let rec help {pat_desc=desc; _} =
    match desc with
    | TPatVar(id, s) -> ret := (id, s)::!ret
    | _ -> iter_pattern_desc help desc in
  help patt;
  !ret

let pattern_bound_idents patt =
  pattern_bound_idents_and_locs patt
  |> List.map fst

let rev_let_bound_idents_with_loc bindings =
  List.map pattern_bound_idents_and_locs bindings
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
  | _ -> {p with pat_desc = map_pattern_desc (alpha_pat env) desc}

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc
