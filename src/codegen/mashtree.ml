(** Low-level IR, suitable for direct translation into WASM *)
open Sexplib.Conv

open Grain_parsing
open Grain_typed
open Value_tags
open Runtime_errors

(* OCaml floats are 64-bit
   (see section 2.3: https://github.com/janestreet/janestreet.github.com/blob/009358427533b46ba2c66200779ea05a73ef0783/ocaml-perf-notes.md)*)
type float32 = float
type float64 = float

type tag_type = Value_tags.tag_type
type heap_tag_type = Value_tags.heap_tag_type

type grain_error = Runtime_errors.grain_error
let prim1_of_sexp, sexp_of_prim1 = Parsetree.prim1_of_sexp, Parsetree.sexp_of_prim1
let prim2_of_sexp, sexp_of_prim2 = Parsetree.prim2_of_sexp, Parsetree.sexp_of_prim2

type prim1 = Parsetree.prim1 =
  | Incr
  | Decr
  | Not
  | Box
  | Unbox
  | Ignore
  | ArrayLength
  | Assert
  | FailWith

type prim2 = Parsetree.prim2 =
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

(* Types within the WASM output *)
type asmtype =
  | I32Type
  | I64Type
  | F32Type
  | F64Type
[@@deriving sexp]

type constant =
  | MConstI32 of int32
  | MConstI64 of int64
  | MConstF32 of float
  | MConstF64 of float
  | MConstLiteral of constant (* Special case for things which should not be encoded *)
[@@deriving sexp]

type binding =
  | MArgBind of int32
  | MLocalBind of int32
  | MGlobalBind of int32
  | MClosureBind of int32
  | MSwapBind of int32 (* Used like a register would be *)
  | MImport of int32 (* Index into list of imports *)
[@@deriving sexp]

type immediate =
  | MImmConst of constant
  | MImmBinding of binding
[@@deriving sexp]

type closure_data = {
  func_idx: int32;
  arity: int32;
  variables: immediate list;
} [@@deriving sexp]

type allocation_type =
  | MClosure of closure_data
  | MTuple of immediate list
  | MArray of immediate list
  | MRecord of immediate * (string * immediate) list
  | MADT of immediate * immediate * immediate list (* Type Tag, Variant Tag, Elements *)
  | MString of string
[@@deriving sexp]

type tag_op =
  | MCheckTag
  | MAssertTag
  | MAddTag
  | MRemoveTag
[@@deriving sexp]

type arity_operand =
  | MLambdaArity
  | MTupleArity
[@@deriving sexp]

type arity_op =
  | MGetArity
  | MAssertArity of int32
[@@deriving sexp]

type tuple_op =
  | MTupleGet of int32
  | MTupleSet of int32 * immediate
[@@deriving sexp]

type array_op =
  | MArrayGet of immediate
  | MArraySet of immediate * immediate
  | MArrayLength
[@@deriving sexp]

type adt_op =
  | MAdtGet of int32
  | MAdtGetModule
  | MAdtGetTag
[@@deriving sexp]

type record_op =
  | MRecordGet of int32
[@@deriving sexp]

type instr =
  | MImmediate of immediate
  | MCallKnown of int32 * immediate list (* Optimized path for statically-known function names *)
  | MCallIndirect of immediate * immediate list
  | MError of grain_error * immediate list
  | MAllocate of allocation_type
  | MTagOp of tag_op * tag_type * immediate
  | MArityOp of arity_operand * arity_op * immediate
  | MIf of immediate * block * block
  | MWhile of block * block
  | MSwitch of immediate * (int32 * block) list * block (* value, branches, default *)
  | MPrim1 of prim1 * immediate
  | MPrim2 of prim2 * immediate * immediate
  | MTupleOp of tuple_op * immediate
  | MArrayOp of array_op * immediate
  | MAdtOp of adt_op * immediate
  | MRecordOp of record_op * immediate
  | MStore of (binding * instr) list (* Items in the same list have their backpatching delayed until the end of that list *)
  | MDrop (* Ignore the result of the last expression. Used for sequences. *)
[@@deriving sexp]

and block = instr list [@@deriving sexp]

type import_type =
  | MFuncImport of asmtype list * asmtype list
  | MGlobalImport of asmtype
[@@deriving sexp]

type import_kind =
  | MImportWasm
  | MImportGrain
[@@deriving sexp]

type import_setup =
  | MCallGetter
  | MWrap of int32
  | MSetupNone
[@@deriving sexp]

type import = {
  mimp_mod: Ident.t;
  mimp_name: Ident.t;
  mimp_type: import_type;
  mimp_kind: import_kind;
  mimp_setup: import_setup;
} [@@deriving sexp]

type export = {
  ex_name: Ident.t;
  ex_global_index: int32;
  ex_getter_index: int32;
} [@@deriving sexp]

type mash_function = {
  index: int32;
  arity: int32; (* TODO: Proper typing of arguments *)
  body: block;
  stack_size: int;
} [@@deriving sexp]

type mash_program = {
  functions: mash_function list;
  imports: import list;
  exports: export list;
  main_body: block;
  main_body_stack_size: int;
  num_globals: int;
  signature: Cmi_format.cmi_infos;
} [@@deriving sexp]

let const_true =  MConstLiteral (MConstI32 (Int32.of_int 0xFFFFFFFF))
let const_false = MConstLiteral (MConstI32 (Int32.of_int 0x7FFFFFFF))
let const_void = MConstLiteral (MConstI32 (Int32.of_int 0x6FFFFFFF))
