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

[@deriving sexp]
type attributes = list(attribute)

[@deriving sexp]
and attribute =
  | Disable_gc;

type grain_error = Runtime_errors.grain_error;
let (prim1_of_sexp, sexp_of_prim1) = (
  Parsetree.prim1_of_sexp,
  Parsetree.sexp_of_prim1,
);
let (prim2_of_sexp, sexp_of_prim2) = (
  Parsetree.prim2_of_sexp,
  Parsetree.sexp_of_prim2,
);
let (primn_of_sexp, sexp_of_primn) = (
  Parsetree.primn_of_sexp,
  Parsetree.sexp_of_primn,
);

type wasm_prim_type =
  Parsetree.wasm_prim_type =
    | Wasm_int32 | Wasm_int64 | Wasm_float32 | Wasm_float64 | Grain_bool;

type wasm_op =
  Parsetree.wasm_op =
    | Op_clz_int32
    | Op_ctz_int32
    | Op_popcnt_int32
    | Op_neg_float32
    | Op_abs_float32
    | Op_ceil_float32
    | Op_floor_float32
    | Op_trunc_float32
    | Op_nearest_float32
    | Op_sqrt_float32
    | Op_eq_z_int32
    | Op_clz_int64
    | Op_ctz_int64
    | Op_popcnt_int64
    | Op_neg_float64
    | Op_abs_float64
    | Op_ceil_float64
    | Op_floor_float64
    | Op_trunc_float64
    | Op_nearest_float64
    | Op_sqrt_float64
    | Op_eq_z_int64
    | Op_extend_s_int32
    | Op_extend_u_int32
    | Op_wrap_int64
    | Op_trunc_s_float32_to_int32
    | Op_trunc_s_float32_to_int64
    | Op_trunc_u_float32_to_int32
    | Op_trunc_u_float32_to_int64
    | Op_trunc_s_float64_to_int32
    | Op_trunc_s_float64_to_int64
    | Op_trunc_u_float64_to_int32
    | Op_trunc_u_float64_to_int64
    | Op_reinterpret_float32
    | Op_reinterpret_float64
    | Op_convert_s_int32_to_float32
    | Op_convert_s_int32_to_float64
    | Op_convert_u_int32_to_float32
    | Op_convert_u_int32_to_float64
    | Op_convert_s_int64_to_float32
    | Op_convert_s_int64_to_float64
    | Op_convert_u_int64_to_float32
    | Op_convert_u_int64_to_float64
    | Op_promote_float32
    | Op_demote_float64
    | Op_reinterpret_int32
    | Op_reinterpret_int64
    | Op_extend_s8_int32
    | Op_extend_s16_int32
    | Op_extend_s8_int64
    | Op_extend_s16_int64
    | Op_extend_s32_int64
    | Op_add_int32
    | Op_sub_int32
    | Op_mul_int32
    | Op_div_s_int32
    | Op_div_u_int32
    | Op_rem_s_int32
    | Op_rem_u_int32
    | Op_and_int32
    | Op_or_int32
    | Op_xor_int32
    | Op_shl_int32
    | Op_shr_u_int32
    | Op_shr_s_int32
    | Op_rot_l_int32
    | Op_rot_r_int32
    | Op_eq_int32
    | Op_ne_int32
    | Op_lt_s_int32
    | Op_lt_u_int32
    | Op_le_s_int32
    | Op_le_u_int32
    | Op_gt_s_int32
    | Op_gt_u_int32
    | Op_ge_s_int32
    | Op_ge_u_int32
    | Op_add_int64
    | Op_sub_int64
    | Op_mul_int64
    | Op_div_s_int64
    | Op_div_u_int64
    | Op_rem_s_int64
    | Op_rem_u_int64
    | Op_and_int64
    | Op_or_int64
    | Op_xor_int64
    | Op_shl_int64
    | Op_shr_u_int64
    | Op_shr_s_int64
    | Op_rot_l_int64
    | Op_rot_r_int64
    | Op_eq_int64
    | Op_ne_int64
    | Op_lt_s_int64
    | Op_lt_u_int64
    | Op_le_s_int64
    | Op_le_u_int64
    | Op_gt_s_int64
    | Op_gt_u_int64
    | Op_ge_s_int64
    | Op_ge_u_int64
    | Op_add_float32
    | Op_sub_float32
    | Op_mul_float32
    | Op_div_float32
    | Op_copy_sign_float32
    | Op_min_float32
    | Op_max_float32
    | Op_eq_float32
    | Op_ne_float32
    | Op_lt_float32
    | Op_le_float32
    | Op_gt_float32
    | Op_ge_float32
    | Op_add_float64
    | Op_sub_float64
    | Op_mul_float64
    | Op_div_float64
    | Op_copy_sign_float64
    | Op_min_float64
    | Op_max_float64
    | Op_eq_float64
    | Op_ne_float64
    | Op_lt_float64
    | Op_le_float64
    | Op_gt_float64
    | Op_ge_float64
    | Op_memory_size
    | Op_memory_grow;

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
    | Int32ToNumber
    | Float64ToNumber
    | Float32ToNumber
    | Int64Lnot
    | WasmFromGrain
    | WasmToGrain
    | WasmUnaryI32({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryI64({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryF32({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      })
    | WasmUnaryF64({
        wasm_op,
        arg_type: wasm_prim_type,
        ret_type: wasm_prim_type,
      });

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
    | Is
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
    | Int64Lte
    | WasmLoadI32({
        sz: int,
        signed: bool,
      })
    | WasmLoadI64({
        sz: int,
        signed: bool,
      })
    | WasmLoadF32
    | WasmLoadF64
    | WasmBinaryI32({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryI64({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryF32({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      })
    | WasmBinaryF64({
        wasm_op,
        arg_types: (wasm_prim_type, wasm_prim_type),
        ret_type: wasm_prim_type,
      });

type primn =
  Parsetree.primn =
    | WasmStoreI32({sz: int})
    | WasmStoreI64({sz: int})
    | WasmStoreF32
    | WasmStoreF64
    | WasmMemoryCopy
    | WasmMemoryFill;

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
  | MArgBind(int32, asmtype)
  | MLocalBind(int32, asmtype)
  | MGlobalBind(string, asmtype, bool)
  | MClosureBind(int32)
  | MSwapBind(int32, asmtype) /* Used like a register would be */
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
  | MChar(string)
  | MInt32(int32)
  | MInt64(int64)
  | MFloat32(float)
  | MFloat64(float)
  | MRational(int32, int32);

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
  | MCallKnown({
      func: string,
      func_type: (list(asmtype), asmtype),
      args: list(immediate),
    })
  | MCallIndirect({
      func: immediate,
      func_type: (list(asmtype), asmtype),
      args: list(immediate),
    })
  | MReturnCallIndirect({
      func: immediate,
      func_type: (list(asmtype), asmtype),
      args: list(immediate),
    })
  | MError(grain_error, list(immediate))
  | MAllocate(allocation_type)
  | MTagOp(tag_op, tag_type, immediate)
  | MArityOp(arity_operand, arity_op, immediate)
  | MIf(immediate, block, block)
  | MFor(option(block), option(block), block)
  | MContinue
  | MBreak
  | MSwitch(immediate, list((int32, block)), block) /* value, branches, default */
  | MPrim1(prim1, immediate)
  | MPrim2(prim2, immediate, immediate)
  | MPrimN(primn, list(immediate))
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
};

[@deriving sexp]
type mash_function = {
  index: int32,
  args: list(asmtype),
  return_type: asmtype,
  body: block,
  stack_size,
  attrs: attributes,
  func_loc: Location.t,
}
and stack_size = {
  stack_size_i32: int,
  stack_size_i64: int,
  stack_size_f32: int,
  stack_size_f64: int,
};

[@deriving sexp]
type mash_program = {
  functions: list(mash_function),
  imports: list(import),
  exports: list(export),
  main_body: block,
  main_body_stack_size: stack_size,
  globals: list((int32, asmtype)),
  signature: Cmi_format.cmi_infos,
};

let const_true = MConstLiteral(MConstI32(Int32.of_int(0xFFFFFFFE)));
let const_false = MConstLiteral(MConstI32(Int32.of_int(0x7FFFFFFE)));
let const_void = MConstLiteral(MConstI32(Int32.of_int(0x6FFFFFFE)));
