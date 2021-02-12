/** Linearized (ANF) AST. */

open Sexplib.Conv;

open Grain_parsing;
open Grain_typed;
open Types;

type rec_flag = Asttypes.rec_flag = | Nonrecursive | Recursive;
[@deriving sexp]
type global_flag =
  | Global
  | Nonglobal;

type loc('a) = Location.loc('a);

[@deriving sexp]
type attributes = Asttypes.attributes;

type analysis = ..;

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

/** Immediate expressions (requiring no computation) */

[@deriving sexp]
type imm_expression = {
  imm_desc: imm_expression_desc,
  imm_loc: Location.t,
  imm_env: Env.t,
  imm_analyses: ref(list(analysis)),
}

[@deriving sexp]
and imm_expression_desc =
  | ImmId(Ident.t)
  | ImmConst(constant);

/** Compound expressions (non-let-bound) */

[@deriving sexp]
type comp_expression = {
  comp_desc: comp_expression_desc,
  comp_loc: Location.t,
  comp_env: Env.t,
  comp_attributes: attributes,
  comp_analyses: ref(list(analysis)),
  comp_allocation_type: allocation_type,
}

[@deriving sexp]
and comp_expression_desc =
  | CImmExpr(imm_expression)
  | CPrim1(prim1, imm_expression)
  | CPrim2(prim2, imm_expression, imm_expression)
  | CPrimN(primn, list(imm_expression))
  | CBoxAssign(imm_expression, imm_expression)
  | CAssign(imm_expression, imm_expression)
  | CTuple(list(imm_expression))
  | CArray(list(imm_expression))
  | CArrayGet(imm_expression, imm_expression)
  | CArraySet(imm_expression, imm_expression, imm_expression)
  | CRecord(imm_expression, list((loc(string), imm_expression)))
  | CAdt(imm_expression, imm_expression, list(imm_expression))
  | CGetTupleItem(int32, imm_expression)
  | CSetTupleItem(int32, imm_expression, imm_expression)
  | CGetAdtItem(int32, imm_expression)
  | CGetAdtTag(imm_expression)
  | CGetRecordItem(int32, imm_expression)
  | CSetRecordItem(int32, imm_expression, imm_expression)
  | CIf(imm_expression, anf_expression, anf_expression)
  | CFor(option(anf_expression), option(anf_expression), anf_expression)
  | CContinue
  | CBreak
  | CSwitch(imm_expression, list((int, anf_expression)))
  | CApp(
      (imm_expression, (list(allocation_type), allocation_type)),
      list(imm_expression),
      bool,
    )
  | CAppBuiltin(string, string, list(imm_expression))
  | CLambda(
      list((Ident.t, allocation_type)),
      (anf_expression, allocation_type),
    )
  | CString(string)
  | CChar(string)
  | CNumber(Asttypes.number_type)
  | CInt32(int32)
  | CInt64(int64)
  | CFloat32(float)
  | CFloat64(float)

/** Compound expressions (possibly let-bound)
    TODO: better name */

[@deriving sexp]
and anf_expression = {
  anf_desc: anf_expression_desc,
  anf_loc: Location.t,
  anf_env: Env.t,
  anf_analyses: ref(list(analysis)),
}

[@deriving sexp]
and anf_expression_desc =
  | AELet(
      global_flag,
      rec_flag,
      list((Ident.t, comp_expression)),
      anf_expression,
    )
  | AESeq(comp_expression, anf_expression)
  | AEComp(comp_expression);

[@deriving sexp]
type import_shape =
  | FunctionShape(list(allocation_type), list(allocation_type))
  | GlobalShape(allocation_type);

[@deriving sexp]
type import_desc =
  | GrainValue(string, string)
  | WasmFunction(string, string)
  | JSFunction(string, string);

[@deriving sexp]
type import_spec = {
  imp_use_id: Ident.t, /* <- internal references to the name will use this */
  imp_desc: import_desc,
  imp_shape: import_shape,
  imp_exported: global_flag,
  imp_analyses: ref(list(analysis)),
};

[@deriving sexp]
type anf_program = {
  body: anf_expression,
  env: Env.t,
  imports: list(import_spec),
  signature: Cmi_format.cmi_infos,
  analyses: ref(list(analysis)),
};

type anf_bind =
  | BSeq(comp_expression)
  | BLet(Ident.t, comp_expression)
  | BLetRec(list((Ident.t, comp_expression)))
  | BLetExport(rec_flag, list((Ident.t, comp_expression)));
