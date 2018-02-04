open Printf
open Expr
open Legacy_types
open Pretty

type simple_expr =
  | Id of string
  | Num of int
  | Bool of bool
  | Prim1 of prim1 * simple_expr
  | Prim2 of prim2 * simple_expr * simple_expr
  | App of simple_expr * simple_expr list

let rec string_of_simple s =
  match s with
  | Id s -> s
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Prim1(op, arg) -> sprintf "%s(%s)" (string_of_op1 op) (string_of_simple arg)
  | Prim2(op, left, right) -> sprintf "%s(%s, %s)" (string_of_op2 op) (string_of_simple left) (string_of_simple right)
  | App(f, args) -> sprintf "%s(%s)" (string_of_simple f) (ExtString.String.join ", " (List.map string_of_simple args))
;;

let simple_to_imm s =
  match s with
  | Id n -> ImmId(n, ())
  | Num n -> ImmNum(n, ())
  | Bool b -> ImmBool(b, ())
  | _ -> failwith "Not an immediate"
;;

let rec simple_to_cexpr simple =
  match simple with
  | Prim1(op, arg) -> CPrim1(op, simple_to_imm arg, ())
  | Prim2(op, left, right) -> CPrim2(op, simple_to_imm left, simple_to_imm right, ())
  | App(f, args) -> CApp(simple_to_imm f, List.map simple_to_imm args, ())
  | _ -> CImmExpr (simple_to_imm simple)
;;
let imm_to_simple i =
  match i with
  | ImmId(n, _) -> Id n
  | ImmNum(n, _) -> Num n
  | ImmBool(b, _) -> Bool b
;;
let cexpr_to_simple_opt cexp =
  match cexp with
  | CPrim1(op, arg, _) -> Some (Prim1(op, imm_to_simple arg))
  | CPrim2(op, left, right, _) -> Some (Prim2(op, imm_to_simple left, imm_to_simple right))
  | CApp(f, args, _) -> Some (App(imm_to_simple f, List.map imm_to_simple args))
  | CImmExpr i -> Some (imm_to_simple i)
  | _ -> None
;;
