/** Low-level IR, suitable for direct translation into WASM */
open Sexplib.Conv;

open Grain_parsing;
open Grain_typed;
open Value_tags;
open Runtime_errors;

/* OCaml floats are 64-bit
   (see section 2.3: https://github.com/janestreet/janestreet.github.com/blob/009358427533b46ba2c66200779ea05a73ef0783/ocaml-perf-notes.md)*/
type float32 = float;
type float64 = float;

type tag_type = Value_tags.tag_type;
type heap_tag_type = Value_tags.heap_tag_type;

type grain_error = Runtime_errors.grain_error;
let (prim1_of_sexp, sexp_of_prim1) = (
  Parsetree.prim1_of_sexp,
  Parsetree.sexp_of_prim1,
);
let (prim2_of_sexp, sexp_of_prim2) = (
  Parsetree.prim2_of_sexp,
  Parsetree.sexp_of_prim2,
);

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

/* Types within the WASM output */
[@deriving sexp]
type asmtype =
  | I32Type
  | I64Type
  | F32Type
  | F64Type;

[@deriving sexp]
type constant =
  | MConstI32(int32)
  | MConstI64(int64)
  | MConstF32(float)
  | MConstF64(float)
  | MConstLiteral(constant); /* Special case for things which should not be encoded */

[@deriving sexp]
type binding =
  | MArgBind(int32)
  | MLocalBind(int32)
  | MGlobalBind(int32)
  | MClosureBind(int32)
  | MSwapBind(int32) /* Used like a register would be */
  | MImport(int32); /* Index into list of imports */

[@deriving sexp]
type immediate =
  | MImmConst(constant)
  | MImmBinding(binding);

[@deriving sexp]
type closure_data = {
  func_idx: int32,
  arity: int32,
  variables: list(immediate),
};

[@deriving sexp]
type allocation_type =
  | MClosure(closure_data)
  | MTuple(list(immediate))
  | MBox(immediate)
  | MArray(list(immediate))
  | MRecord(immediate, list((string, immediate)))
  | MADT(immediate, immediate, list(immediate)) /* Type Tag, Variant Tag, Elements */
  | MString(string)
  | MInt32(int32)
  | MInt64(int64);

[@deriving sexp]
type tag_op =
  | MCheckTag
  | MAssertTag
  | MAddTag
  | MRemoveTag;

[@deriving sexp]
type arity_operand =
  | MLambdaArity
  | MTupleArity;

[@deriving sexp]
type arity_op =
  | MGetArity
  | MAssertArity(int32);

[@deriving sexp]
type tuple_op =
  | MTupleGet(int32)
  | MTupleSet(int32, immediate);

[@deriving sexp]
type box_op =
  | MBoxUnbox
  | MBoxUpdate(immediate);

[@deriving sexp]
type array_op =
  | MArrayGet(immediate)
  | MArraySet(immediate, immediate)
  | MArrayLength;

[@deriving sexp]
type adt_op =
  | MAdtGet(int32)
  | MAdtGetModule
  | MAdtGetTag;

[@deriving sexp]
type record_op =
  | MRecordGet(int32)
  | MRecordSet(int32, immediate);

[@deriving sexp]
type instr = {
  instr_desc,
  instr_loc: Location.t,
} /* Optimized path for statically-known function names */ /* value, branches, default */ /* Items in the same list have their backpatching delayed until the end of that list */ /* Ignore the result of an expression. Used for sequences. */ /* Prints a message to the console; for compiler debugging */
[@deriving sexp]
and instr_desc =
  | MImmediate(immediate)
  | MCallKnown(string, list(immediate))
  | MCallIndirect(immediate, list(immediate))
  | MError(grain_error, list(immediate))
  | MAllocate(allocation_type)
  | MTagOp(tag_op, tag_type, immediate)
  | MArityOp(arity_operand, arity_op, immediate)
  | MIf(immediate, block, block)
  | MWhile(block, block)
  | MSwitch(immediate, list((int32, block)), block) /* value, branches, default */
  | MPrim1(prim1, immediate)
  | MPrim2(prim2, immediate, immediate)
  | MTupleOp(tuple_op, immediate)
  | MBoxOp(box_op, immediate)
  | MArrayOp(array_op, immediate)
  | MAdtOp(adt_op, immediate)
  | MRecordOp(record_op, immediate)
  | MStore(list((binding, instr))) /* Items in the same list have their backpatching delayed until the end of that list */
  | MDrop(instr) /* Ignore the result of an expression. Used for sequences. */
  | MTracepoint(int) /* Prints a message to the console; for compiler debugging */

[@deriving sexp]
and block = list(instr);

[@deriving sexp]
type import_type =
  | MFuncImport(list(asmtype), list(asmtype))
  | MGlobalImport(asmtype);

[@deriving sexp]
type import_kind =
  | MImportWasm
  | MImportGrain;

[@deriving sexp]
type import_setup =
  | MCallGetter
  | MWrap(int32)
  | MSetupNone;

[@deriving sexp]
type import = {
  mimp_mod: Ident.t,
  mimp_name: Ident.t,
  mimp_type: import_type,
  mimp_kind: import_kind,
  mimp_setup: import_setup,
};

[@deriving sexp]
type export = {
  ex_name: Ident.t,
  ex_global_index: int32,
  ex_getter_index: int32,
};

[@deriving sexp]
type mash_function = {
  index: int32,
  arity: int32, /* TODO: Proper typing of arguments */
  body: block,
  stack_size: int,
  func_loc: Location.t,
};

[@deriving sexp]
type mash_program = {
  functions: list(mash_function),
  imports: list(import),
  exports: list(export),
  main_body: block,
  main_body_stack_size: int,
  num_globals: int,
  signature: Cmi_format.cmi_infos,
};

let const_true = MConstLiteral(MConstI32(Int32.of_int(0xFFFFFFFF)));
let const_false = MConstLiteral(MConstI32(Int32.of_int(0x7FFFFFFF)));
let const_void = MConstLiteral(MConstI32(Int32.of_int(0x6FFFFFFF)));
