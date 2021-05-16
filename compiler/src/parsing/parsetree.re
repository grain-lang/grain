/** Parse tree type definitions. This is a reformulation
    of our original parse tree which better aligns with the design
    of the OCaml parse tree. Credit for the module's architecture goes to
    the OCaml team. */
open Sexplib.Conv;
open Asttypes;

let sexp_locs_disabled = _ => ! Grain_utils.Config.sexp_locs_enabled^;

[@deriving yojson]
type loc('a) =
  Asttypes.loc('a) = {
    txt: 'a,
    loc: Location.t,
  };

type export_flag = Asttypes.export_flag = | Nonexported | Exported;
type rec_flag = Asttypes.rec_flag = | Nonrecursive | Recursive;
type mut_flag = Asttypes.mut_flag = | Mutable | Immutable;

/** Type for syntax-level types */

[@deriving (sexp, yojson)]
type parsed_type_desc =
  | PTyAny
  | PTyVar(string)
  | PTyArrow(list(parsed_type), parsed_type)
  | PTyTuple(list(parsed_type))
  | PTyConstr(loc(Identifier.t), list(parsed_type))
  | PTyPoly(list(loc(string)), parsed_type)

and parsed_type = {
  ptyp_desc: parsed_type_desc,
  [@sexp_drop_if sexp_locs_disabled]
  ptyp_loc: Location.t,
};

/** Type for arguments to a constructor */

[@deriving (sexp, yojson)]
type constructor_arguments =
  | PConstrTuple(list(parsed_type))
  | PConstrSingleton

[@deriving (sexp, yojson)]
and type_extension = {
  ptyext_path: loc(Identifier.t),
  ptyext_params: list(parsed_type),
  ptyext_constructors: list(extension_constructor),
  ptyext_loc: Location.t,
}

[@deriving (sexp, yojson)]
and extension_constructor = {
  pext_name: loc(string),
  pext_kind: extension_constructor_kind,
  pext_loc: Location.t,
}

[@deriving (sexp, yojson)]
and type_exception = {
  ptyexn_constructor: extension_constructor,
  ptyexn_loc: Location.t,
}

[@deriving (sexp, yojson)]
and extension_constructor_kind =
  | PExtDecl(constructor_arguments)
  | PExtRebind(loc(Identifier.t));

/** Type for branches within data declarations */

[@deriving (sexp, yojson)]
type constructor_declaration = {
  pcd_name: loc(string),
  pcd_args: constructor_arguments,
  [@sexp_drop_if sexp_locs_disabled]
  pcd_loc: Location.t,
};

/** Type for fields within a record */

[@deriving (sexp, yojson)]
type label_declaration = {
  pld_name: loc(Identifier.t),
  pld_type: parsed_type,
  pld_mutable: mut_flag,
  [@sexp_drop_if sexp_locs_disabled]
  pld_loc: Location.t,
};

/** Different types of data which can be declared. Currently only one. */

[@deriving (sexp, yojson)]
type data_kind =
  | PDataVariant(list(constructor_declaration))
  | PDataRecord(list(label_declaration));

/** Type for data declarations. */

[@deriving (sexp, yojson)]
type data_declaration = {
  pdata_name: loc(string),
  pdata_params: list(parsed_type),
  pdata_kind: data_kind,
  [@sexp_drop_if sexp_locs_disabled]
  pdata_loc: Location.t,
};

/** Constants supported by Grain */

[@deriving (sexp, yojson)]
type constant =
  | PConstNumber(number_type)
  | PConstInt32(string)
  | PConstInt64(string)
  | PConstFloat32(string)
  | PConstFloat64(string)
  | PConstWasmI32(string)
  | PConstWasmI64(string)
  | PConstWasmF32(string)
  | PConstWasmF64(string)
  | PConstBool(bool)
  | PConstVoid
  | PConstBytes(string)
  | PConstString(string)
  | PConstChar(string)

[@deriving (sexp, yojson)]
and number_type =
  | PConstNumberInt(string)
  | PConstNumberFloat(string)
  | PConstNumberRational(string, string);

/** Various binding forms */

[@deriving (sexp, yojson)]
type pattern_desc =
  | PPatAny
  | PPatVar(loc(string))
  | PPatTuple(list(pattern))
  | PPatArray(list(pattern))
  | PPatRecord(list((loc(Identifier.t), pattern)), closed_flag)
  | PPatConstant(constant)
  | PPatConstraint(pattern, parsed_type)
  | PPatConstruct(loc(Identifier.t), list(pattern))
  | PPatOr(pattern, pattern)
  | PPatAlias(pattern, loc(string))

[@deriving (sexp, yojson)]
and pattern = {
  ppat_desc: pattern_desc,
  [@sexp_drop_if sexp_locs_disabled]
  ppat_loc: Location.t,
};

[@deriving (sexp, yojson)]
type wasm_prim_type =
  | Wasm_int32
  | Wasm_int64
  | Wasm_float32
  | Wasm_float64
  | Grain_bool;

/* If adding new wasm ops, be sure to add them in comp_wasm_prim.re and in the inline_wasm analysis and optimization. */

[@deriving (sexp, yojson)]
type wasm_op =
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
  | Op_ge_float64;

/** Single-argument operators */
[@deriving (sexp, yojson)]
type prim1 =
  | Not
  | Box
  | Unbox
  | BoxBind
  | UnboxBind
  | Ignore
  | ArrayLength
  | Assert
  | Throw
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
    })
  | WasmMemoryGrow;

/** Two-argument operators */

[@deriving (sexp, yojson)]
type prim2 =
  | Is
  | Eq
  | And
  | Or
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

[@deriving (sexp, yojson)]
type primn =
  | WasmStoreI32({sz: int})
  | WasmStoreI64({sz: int})
  | WasmStoreF32
  | WasmStoreF64
  | WasmMemoryCopy
  | WasmMemoryFill
  | WasmMemorySize
  | WasmMemoryCompare;

[@deriving (sexp, yojson)]
type attributes = Asttypes.attributes;

/** Type for expressions (i.e. things which evaluate to something) */

[@deriving (sexp, yojson)]
type expression = {
  pexp_desc: expression_desc,
  pexp_attributes: attributes,
  [@sexp_drop_if sexp_locs_disabled]
  pexp_loc: Location.t,
}

[@deriving (sexp, yojson)]
and expression_desc =
  | PExpId(loc(Identifier.t))
  | PExpConstant(constant)
  | PExpTuple(list(expression))
  | PExpArray(list(expression))
  | PExpArrayGet(expression, expression)
  | PExpArraySet(expression, expression, expression)
  | PExpRecord(list((loc(Identifier.t), expression)))
  | PExpRecordGet(expression, loc(Identifier.t))
  | PExpRecordSet(expression, loc(Identifier.t), expression)
  | PExpLet(rec_flag, mut_flag, list(value_binding))
  | PExpMatch(expression, list(match_branch))
  | PExpPrim1(prim1, expression)
  | PExpPrim2(prim2, expression, expression)
  | PExpPrimN(primn, list(expression))
  | PExpIf(expression, expression, expression)
  | PExpWhile(expression, expression)
  | PExpFor(
      option(expression),
      option(expression),
      option(expression),
      expression,
    )
  | PExpContinue
  | PExpBreak
  | PExpConstraint(expression, parsed_type)
  | PExpLambda(list(pattern), expression)
  | PExpApp(expression, list(expression))
  | PExpBlock(list(expression))
  | PExpBoxAssign(expression, expression)
  | PExpAssign(expression, expression)
  | /** Used for modules without body expressions */
    PExpNull

/** let-binding form */

[@deriving (sexp, yojson)]
and value_binding = {
  pvb_pat: pattern,
  pvb_expr: expression,
  [@sexp_drop_if sexp_locs_disabled]
  pvb_loc: Location.t,
}

[@deriving (sexp, yojson)]
and match_branch = {
  pmb_pat: pattern,
  pmb_body: expression,
  pmb_guard: option(expression),
  [@sexp_drop_if sexp_locs_disabled]
  pmb_loc: Location.t,
};

[@deriving (sexp, yojson)]
type import_value =
  | PImportModule(loc(Identifier.t))
  | PImportAllExcept(list(loc(Identifier.t)))
  | PImportValues(list((loc(Identifier.t), option(loc(Identifier.t)))));

/** Type for import statements */

[@deriving (sexp, yojson)]
type import_declaration = {
  pimp_path: loc(string),
  pimp_val: list(import_value),
  [@sexp_drop_if sexp_locs_disabled]
  pimp_loc: Location.t,
};

[@deriving (sexp, yojson)]
type value_description = {
  pval_mod: loc(string),
  pval_name: loc(string),
  pval_name_alias: option(loc(string)),
  pval_type: parsed_type,
  pval_prim: list(string),
  [@sexp_drop_if sexp_locs_disabled]
  pval_loc: Location.t,
};

[@deriving (sexp, yojson)]
type export_declaration_desc = {
  pex_name: loc(string),
  pex_alias: option(loc(string)),
  [@sexp_drop_if sexp_locs_disabled]
  pex_loc: Location.t,
}

[@deriving (sexp, yojson)]
and export_declaration =
  | ExportData(export_declaration_desc)
  | ExportValue(export_declaration_desc);

[@deriving (sexp, yojson)]
type export_except =
  | ExportExceptData(loc(string))
  | ExportExceptValue(loc(string));

/** Statements which can exist at the top level */

[@deriving (sexp, yojson)]
type toplevel_stmt_desc =
  | PTopImport(import_declaration)
  | PTopForeign(export_flag, value_description)
  | PTopPrimitive(export_flag, value_description)
  | PTopData(list((export_flag, data_declaration)))
  | PTopLet(export_flag, rec_flag, mut_flag, list(value_binding))
  | PTopExpr(expression)
  | PTopException(export_flag, type_exception)
  | PTopExport(list(export_declaration))
  | PTopExportAll(list(export_except));

[@deriving (sexp, yojson)]
type toplevel_stmt = {
  ptop_desc: toplevel_stmt_desc,
  ptop_attributes: attributes,
  [@sexp_drop_if sexp_locs_disabled]
  ptop_loc: Location.t,
};

[@deriving (sexp, yojson)]
type comment_desc = {
  cmt_content: string,
  cmt_source: string,
  cmt_loc: Location.t,
};

[@deriving (sexp, yojson)]
type comment =
  | Line(comment_desc)
  | Shebang(comment_desc)
  | Block(comment_desc)
  | Doc(comment_desc);

/** The type for parsed programs */

[@deriving (sexp, yojson)]
type parsed_program = {
  statements: list(toplevel_stmt),
  comments: list(comment),
  [@sexp_drop_if sexp_locs_disabled]
  prog_loc: Location.t,
};
