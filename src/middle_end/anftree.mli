(** Linearized (ANF) AST. *)
open Sexplib.Conv

open Grain_parsing
open Grain_typed
open Types

type rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive
type global_flag = Global | Nonglobal [@@deriving sexp]

type 'a loc = 'a Location.loc

type analysis = ..

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

(** Immediate expressions (requiring no computation) *)
type imm_expression = {
  imm_desc: imm_expression_desc;
  imm_loc: Location.t;
  imm_env: Env.t;
  imm_analyses: (analysis list) ref;
} [@@deriving sexp]

and imm_expression_desc =
  | ImmId of Ident.t
  | ImmConst of constant
[@@deriving sexp]


(** Compound expressions (non-let-bound) *)
type comp_expression = {
  comp_desc: comp_expression_desc;
  comp_loc: Location.t;
  comp_env: Env.t;
  comp_analyses: (analysis list) ref;
}
[@@deriving sexp]

and comp_expression_desc =
  | CImmExpr of imm_expression
  | CPrim1 of prim1 * imm_expression
  | CPrim2 of prim2 * imm_expression * imm_expression
  | CAssign of imm_expression * imm_expression
  | CTuple of imm_expression list
  | CGetTupleItem of int32 * imm_expression
  | CSetTupleItem of int32 * imm_expression * imm_expression
  | CIf of imm_expression * anf_expression * anf_expression
  | CWhile of anf_expression * anf_expression (* FIXME [philip]: I find it hard to believe that the condition is an [anf_expression] here *)
  | CSwitch of imm_expression * (int * anf_expression) list
  | CApp of imm_expression * imm_expression list
  | CAppBuiltin of string * string * imm_expression list (* Unwrapped function call (to WASM functions) *)
  | CLambda of Ident.t list * anf_expression
  | CString of string
[@@deriving sexp]

(** Compound expressions (possibly let-bound)
    TODO: better name *)
and anf_expression = {
  anf_desc: anf_expression_desc;
  anf_loc: Location.t;
  anf_env: Env.t;
  anf_analyses: (analysis list) ref;
}
[@@deriving sexp]

and anf_expression_desc =
  | AELet of global_flag * rec_flag * (Ident.t * comp_expression) list * anf_expression
  | AESeq of comp_expression * anf_expression
  | AEComp of comp_expression
[@@deriving sexp]

type import_shape =
  | FunctionShape of int * int
  | GlobalShape
[@@deriving sexp]

type import_desc =
  | GrainValue of string * string
  | WasmFunction of string * string
  | JSFunction of string * string
[@@deriving sexp]

type import_spec = {
  imp_use_id: Ident.t; (* <- internal references to the name will use this *)
  imp_desc: import_desc;
  imp_shape: import_shape;
  imp_analyses: (analysis list) ref;
}
[@@deriving sexp]

type anf_program = {
  body: anf_expression;
  env: Env.t;
  imports: import_spec list;
  signature: Cmi_format.cmi_infos;
  analyses: (analysis list) ref;
} [@@deriving sexp]
