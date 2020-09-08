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

type analysis = ..;

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

let (prim1_of_sexp, sexp_of_prim1) = (
  Parsetree.prim1_of_sexp,
  Parsetree.sexp_of_prim1,
);
let (prim2_of_sexp, sexp_of_prim2) = (
  Parsetree.prim2_of_sexp,
  Parsetree.sexp_of_prim2,
);
let sexp_locs_disabled = _ => ! Grain_utils.Config.sexp_locs_enabled^;

/** Immediate expressions (requiring no computation) */

[@deriving sexp]
type imm_expression = {
  imm_desc: imm_expression_desc,
  [@sexp_drop_if sexp_locs_disabled]
  imm_loc: Location.t,
  imm_env: [@sexp.opaque] Env.t,
  imm_analyses: [@sexp.opaque] ref(list(analysis)),
}

[@deriving sexp]
and imm_expression_desc =
  | ImmId(Ident.t)
  | ImmConst(constant);

/** Compound expressions (non-let-bound) */

[@deriving sexp]
type comp_expression = {
  comp_desc: comp_expression_desc,
  [@sexp_drop_if sexp_locs_disabled]
  comp_loc: Location.t,
  comp_env: [@sexp.opaque] Env.t,
  comp_analyses: [@sexp.opaque] ref(list(analysis)),
}

[@deriving sexp]
and comp_expression_desc =
  | CImmExpr(imm_expression)
  | CPrim1(prim1, imm_expression)
  | CPrim2(prim2, imm_expression, imm_expression)
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
  | CWhile(anf_expression, anf_expression)
  | CSwitch(imm_expression, list((int, anf_expression)))
  | CApp(imm_expression, list(imm_expression))
  | CAppBuiltin(string, string, list(imm_expression))
  | CLambda(list(Ident.t), anf_expression)
  | CString(string)
  | CInt32(int32)
  | CInt64(int64)

/** Compound expressions (possibly let-bound)
    TODO: better name */

[@deriving sexp]
and anf_expression = {
  anf_desc: anf_expression_desc,
  [@sexp_drop_if sexp_locs_disabled]
  anf_loc: Location.t,
  anf_env: [@sexp.opaque] Env.t,
  anf_analyses: [@sexp.opaque] ref(list(analysis)),
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
  | FunctionShape(int, int)
  | GlobalShape;

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
  imp_analyses: [@sexp.opaque] ref(list(analysis)),
};

[@deriving sexp]
type anf_program = {
  body: anf_expression,
  env: [@sexp.opaque] Env.t,
  imports: list(import_spec),
  signature: Cmi_format.cmi_infos,
  analyses: [@sexp.opaque] ref(list(analysis)),
};
