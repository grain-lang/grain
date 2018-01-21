(* Stripped-down version of OCaml's typedtree. *)
open Grain_parsing
open Types

type 'a loc = 'a Location.loc
type partial = Partial | Total

type rec_flag = Parsetree.rec_flag = Nonrecursive | Recursive

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
  | TPatConstraint

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
  pimp_mod: Identifier.t Location.loc;
  pimp_loc: Location.t;
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
