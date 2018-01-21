(** Compile-time type and binding information *)
(* Inspired by OCaml's typing module *)
open Grain_parsing

type type_expr = {
  mutable desc: type_desc;
  id: int
}

and type_desc =
  | TTyVar of string option
  | TTyArrow of type_expr list * type_expr
  | TTyTuple of type_expr list
    (* Ident is from OCaml. *)
  | TTyConstr of Ident.t * type_expr list

type value_description = {
  val_type: type_expr;
  val_kind: value_kind;
  val_loc: Location.t
}

and value_kind =
  | TValReg

type constructor_declaration = {
  cd_id: Ident.t;
  cd_args: constructor_arguments;
  cd_loc: Location.t;
}

and constructor_arguments =
  | TConstrTuple of type_expr list
  | TConstrSingleton


type type_declaration = {
  type_params: type_expr list;
  type_arity: int;
  type_kind: type_kind;
  type_loc: Location.t;
}

and type_kind =
  | TDataVariant of constructor_declaration list

type module_type =
  | TModIdent of Path.t

type constructor_tag =
  | CstrConstant of int
  | CstrBlock of int
  | CstrUnboxed

type constructor_description = {
  cstr_name : string;
  cstr_res: type_expr;
  cstr_existentials: type_expr list;
  cstr_args: type_expr list;
  cstr_arity: int;
  cstr_tag: constructor_tag;
  cstr_loc: Location.t;
}

