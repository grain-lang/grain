(* Abstract syntax of (a small subset of) x86 assembly instructions *)
let word_size = 4
;;

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

               
type sourcespan = (Lexing.position * Lexing.position)
exception UnboundId of string * sourcespan (* name, where used *)
exception UnboundFun of string * sourcespan (* name of fun, where used *)
exception ShadowId of string * sourcespan * sourcespan (* name, where used, where defined *)
exception DuplicateId of string * sourcespan * sourcespan (* name, where used, where defined *)
exception DuplicateFun of string * sourcespan * sourcespan (* name, where used, where defined *)
exception Overflow of int * sourcespan (* value, where used *)
exception LetRecNonFunction of string * sourcespan (* name binding, where defined *)



type prim1 =
  | Add1
  | Sub1
  | Not
  | Print
  | PrintStack
  | IsNum
  | IsBool
  | IsTuple

type prim2 =
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

type 'a bind = (string * 'a expr * 'a)

and 'a expr =
  | ELet of 'a bind list * 'a expr * 'a
  | ELetRec of 'a bind list * 'a expr * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a
  | ETuple of 'a expr list * 'a
  | EGetItem of 'a expr * 'a expr * 'a
  | ESetItem of 'a expr * 'a expr * 'a expr * 'a
  | ENumber of int * 'a
  | EBool of bool * 'a
  | EId of string * 'a
  | EApp of 'a expr * 'a expr list * 'a
  | ELambda of (string * 'a) list * 'a expr * 'a
  | ESeq of 'a expr list * 'a

type 'a program = 'a expr

type 'a immexpr = (* immediate expressions *)
  | ImmNum of int * 'a
  | ImmBool of bool * 'a
  | ImmId of string * 'a
and 'a cexpr = (* compound expressions *)
  | CIf of 'a immexpr * 'a aexpr * 'a aexpr * 'a
  | CPrim1 of prim1 * 'a immexpr * 'a
  | CPrim2 of prim2 * 'a immexpr * 'a immexpr * 'a
  | CApp of 'a immexpr * 'a immexpr list * 'a
  | CTuple of 'a immexpr list * 'a
  | CGetItem of 'a immexpr * 'a immexpr * 'a
  | CSetItem of 'a immexpr * 'a immexpr * 'a immexpr * 'a
  | CLambda of string list * 'a aexpr * 'a                                            
  | CImmExpr of 'a immexpr (* for when you just need an immediate value *)
and 'a aexpr = (* anf expressions *)
  | ALet of string * 'a cexpr * 'a aexpr * 'a
  | ALetRec of (string * 'a cexpr) list * 'a aexpr * 'a
  | ASeq of 'a cexpr * 'a aexpr * 'a
  | ACExpr of 'a cexpr

and 'a aprogram = 'a aexpr
