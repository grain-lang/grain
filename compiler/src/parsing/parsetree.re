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

type provide_flag =
  Asttypes.provide_flag = | NotProvided | Provided | Abstract;
type rec_flag = Asttypes.rec_flag = | Nonrecursive | Recursive;
type mut_flag = Asttypes.mut_flag = | Mutable | Immutable;

/** Type for syntax-level types */

[@deriving (sexp, yojson)]
type parsed_type_desc =
  | PTyAny
  | PTyVar(string)
  | PTyArrow(list(parsed_type_argument), parsed_type)
  | PTyTuple(list(parsed_type))
  | PTyConstr(loc(Identifier.t), list(parsed_type))
  | PTyPoly(list(loc(string)), parsed_type)

and parsed_type = {
  ptyp_desc: parsed_type_desc,
  [@sexp_drop_if sexp_locs_disabled]
  ptyp_loc: Location.t,
}

and parsed_type_argument = {
  ptyp_arg_label: argument_label,
  ptyp_arg_type: parsed_type,
  ptyp_arg_loc: Location.t,
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

/** Type for arguments to a constructor */

[@deriving (sexp, yojson)]
type constructor_arguments =
  | PConstrTuple(loc(list(parsed_type)))
  | PConstrRecord(loc(list(label_declaration)))
  | PConstrSingleton

[@deriving (sexp, yojson)]
and type_extension = {
  ptyext_path: loc(Identifier.t),
  ptyext_params: list(parsed_type),
  ptyext_constructors: list(extension_constructor),
  [@sexp_drop_if sexp_locs_disabled]
  ptyext_loc: Location.t,
}

[@deriving (sexp, yojson)]
and extension_constructor = {
  pext_name: loc(string),
  pext_kind: extension_constructor_kind,
  [@sexp_drop_if sexp_locs_disabled]
  pext_loc: Location.t,
}

[@deriving (sexp, yojson)]
and type_exception = {
  ptyexn_constructor: extension_constructor,
  [@sexp_drop_if sexp_locs_disabled]
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

/** Different types of data which can be declared. Currently only one. */

[@deriving (sexp, yojson)]
type data_kind =
  | PDataAbstract
  | PDataVariant(list(constructor_declaration))
  | PDataRecord(list(label_declaration));

/** Type for data declarations. */

[@deriving (sexp, yojson)]
type data_declaration = {
  pdata_name: loc(string),
  pdata_params: list(parsed_type),
  pdata_kind: data_kind,
  pdata_manifest: option(parsed_type),
  pdata_rec: rec_flag,
  [@sexp_drop_if sexp_locs_disabled]
  pdata_loc: Location.t,
};

/** Constants supported by Grain */

[@deriving (sexp, yojson)]
type constant =
  | PConstNumber(number_type)
  | PConstInt8(loc(string))
  | PConstInt16(loc(string))
  | PConstInt32(loc(string))
  | PConstInt64(loc(string))
  | PConstUint8(loc(string))
  | PConstUint16(loc(string))
  | PConstUint32(loc(string))
  | PConstUint64(loc(string))
  | PConstFloat32(loc(string))
  | PConstFloat64(loc(string))
  | PConstWasmI32(loc(string))
  | PConstWasmI64(loc(string))
  | PConstWasmF32(loc(string))
  | PConstWasmF64(loc(string))
  | PConstBigInt(loc(string))
  | PConstRational(loc(string))
  | PConstBool(bool)
  | PConstVoid
  | PConstBytes(loc(string))
  | PConstString(loc(string))
  | PConstChar(loc(string))

[@deriving (sexp, yojson)]
and number_type =
  | PConstNumberInt(loc(string))
  | PConstNumberFloat(loc(string))
  | PConstNumberRational({
      numerator: loc(string),
      slash: Location.t,
      denominator: loc(string),
    });

[@deriving (sexp, yojson)]
type list_item('a) =
  | ListItem('a)
  | ListSpread('a, Location.t);

[@deriving (sexp, yojson)]
type record_item('a) =
  | RecordItem(loc(Identifier.t), 'a)
  | RecordSpread('a, Location.t);

/** Various binding forms */

[@deriving (sexp, yojson)]
type pattern_desc =
  | PPatAny
  | PPatVar(loc(string))
  | PPatTuple(list(pattern))
  | PPatList(list(list_item(pattern)))
  | PPatArray(list(pattern))
  | PPatRecord(list((loc(Identifier.t), pattern)), closed_flag)
  | PPatConstant(constant)
  | PPatConstraint(pattern, parsed_type)
  | PPatConstruct(loc(Identifier.t), constructor_pattern)
  | PPatOr(pattern, pattern)
  | PPatAlias(pattern, loc(string))

[@deriving (sexp, yojson)]
and constructor_pattern =
  | PPatConstrRecord(list((loc(Identifier.t), pattern)), closed_flag)
  | PPatConstrTuple(list(pattern))
  | PPatConstrSingleton

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

/** Zero-argument operators */
[@deriving (sexp, yojson)]
type prim0 =
  | AllocateInt32
  | AllocateInt64
  | AllocateUint32
  | AllocateUint64
  | AllocateFloat32
  | AllocateFloat64
  | AllocateRational
  | WasmMemorySize
  | Unreachable
  | HeapStart
  | HeapTypeMetadata;

/** Single-argument operators */
[@deriving (sexp, yojson)]
type prim1 =
  | AllocateArray
  | AllocateTuple
  | AllocateBytes
  | AllocateString
  | AllocateBigInt
  | NewInt32
  | NewInt64
  | NewUint32
  | NewUint64
  | NewFloat32
  | NewFloat64
  | BuiltinId
  | LoadAdtVariant
  | StringSize
  | BytesSize
  | TagSimpleNumber
  | UntagSimpleNumber
  | TagChar
  | UntagChar
  | TagInt8
  | UntagInt8
  | TagInt16
  | UntagInt16
  | TagUint8
  | UntagUint8
  | TagUint16
  | UntagUint16
  | Not
  | Box
  | Unbox
  | BoxBind
  | UnboxBind
  | Ignore
  | ArrayLength
  | Assert
  | Throw
  | Magic
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
  | NewRational
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
  | WasmMemoryCompare;

[@deriving (sexp, yojson)]
type use_items =
  | PUseAll
  | PUseItems(list(use_item))

[@deriving (sexp, yojson)]
and use_item =
  | PUseType({
      name: loc(Identifier.t),
      alias: option(loc(Identifier.t)),
      loc: Location.t,
    })
  | PUseException({
      name: loc(Identifier.t),
      alias: option(loc(Identifier.t)),
      loc: Location.t,
    })
  | PUseModule({
      name: loc(Identifier.t),
      alias: option(loc(Identifier.t)),
      loc: Location.t,
    })
  | PUseValue({
      name: loc(Identifier.t),
      alias: option(loc(Identifier.t)),
      loc: Location.t,
    });

[@deriving (sexp, yojson)]
type attribute = Asttypes.attribute;

[@deriving (sexp, yojson)]
type attributes = Asttypes.attributes;

/** Type for expressions (i.e. things which evaluate to something) */

[@deriving (sexp, yojson)]
type expression = {
  pexp_desc: expression_desc,
  pexp_attributes: attributes,
  [@sexp_drop_if sexp_locs_disabled]
  pexp_loc: Location.t, // The full location, including attributes
  [@sexp_drop_if sexp_locs_disabled]
  pexp_core_loc: Location.t // The core expression location, without attributes
}

[@deriving (sexp, yojson)]
and expression_desc =
  | PExpId(loc(Identifier.t))
  | PExpConstant(constant)
  | PExpTuple(list(expression))
  | PExpList(list(list_item(expression)))
  | PExpArray(list(expression))
  | PExpArrayGet(expression, expression)
  | PExpArraySet({
      lhs_loc: Location.t,
      array: expression,
      index: expression,
      value: expression,
      infix_op: option(expression),
    })
  | PExpRecord(option(expression), list((loc(Identifier.t), expression)))
  | PExpRecordGet(expression, loc(Identifier.t))
  | PExpRecordSet(expression, loc(Identifier.t), expression)
  | PExpLet(rec_flag, mut_flag, list(value_binding))
  | PExpMatch(expression, loc(list(match_branch)))
  | PExpPrim0(prim0)
  | PExpPrim1(prim1, expression)
  | PExpPrim2(prim2, expression, expression)
  | PExpPrimN(primn, list(expression))
  | PExpIf(expression, expression, option(expression))
  | PExpWhile(expression, expression)
  | PExpFor(
      option(expression),
      option(expression),
      option(expression),
      expression,
    )
  | PExpContinue
  | PExpBreak
  | PExpReturn(option(expression))
  | PExpConstraint(expression, parsed_type)
  | PExpUse(loc(Identifier.t), use_items)
  | PExpLambda(list(lambda_argument), expression)
  | PExpApp(expression, list(application_argument))
  | PExpConstruct(loc(Identifier.t), constructor_expression)
  | PExpBlock(list(expression))
  | PExpBoxAssign(expression, expression)
  | PExpAssign(expression, expression)

[@deriving (sexp, yojson)]
and constructor_expression =
  | PExpConstrTuple(list(expression))
  | PExpConstrRecord(list((loc(Identifier.t), expression)))
  | PExpConstrSingleton

[@deriving (sexp, yojson)]
and lambda_argument = {
  pla_label: argument_label,
  pla_pattern: pattern,
  pla_default: option(expression),
  pla_loc: Location.t,
}

[@deriving (sexp, yojson)]
and application_argument = {
  paa_label: argument_label,
  paa_expr: expression,
  paa_loc: Location.t,
}

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

/** Type for include statements */

[@deriving (sexp, yojson)]
type include_declaration = {
  pinc_path: loc(string),
  pinc_module: loc(string),
  pinc_alias: option(loc(string)),
  [@sexp_drop_if sexp_locs_disabled]
  pinc_loc: Location.t,
};

[@deriving (sexp, yojson)]
type value_description = {
  pval_mod: loc(string),
  pval_name: loc(string),
  pval_name_alias: option(loc(string)),
  pval_type: parsed_type,
  [@sexp_drop_if sexp_locs_disabled]
  pval_loc: Location.t,
};

[@deriving (sexp, yojson)]
type provide_item =
  | PProvideType({
      name: loc(Identifier.t),
      alias: option(loc(Identifier.t)),
      loc: Location.t,
    })
  | PProvideException({
      name: loc(Identifier.t),
      alias: option(loc(Identifier.t)),
      loc: Location.t,
    })
  | PProvideModule({
      name: loc(Identifier.t),
      alias: option(loc(Identifier.t)),
      loc: Location.t,
    })
  | PProvideValue({
      name: loc(Identifier.t),
      alias: option(loc(Identifier.t)),
      loc: Location.t,
    });

[@deriving (sexp, yojson)]
type module_declaration = {
  pmod_name: loc(string),
  pmod_stmts: list(toplevel_stmt),
  pmod_loc: Location.t,
}

[@deriving (sexp, yojson)]
and primitive_description = {
  pprim_ident: loc(string),
  pprim_name: loc(string),
  [@sexp_drop_if sexp_locs_disabled]
  pprim_loc: Location.t,
}

/** Statements which can exist at the top level */

[@deriving (sexp, yojson)]
and toplevel_stmt_desc =
  | PTopInclude(include_declaration)
  | PTopForeign(provide_flag, value_description)
  | PTopPrimitive(provide_flag, primitive_description)
  | PTopModule(provide_flag, module_declaration)
  | PTopData(list((provide_flag, data_declaration, Location.t)))
  | PTopLet(provide_flag, rec_flag, mut_flag, list(value_binding))
  | PTopExpr(expression)
  | PTopException(provide_flag, type_exception)
  | PTopProvide(list(provide_item))

[@deriving (sexp, yojson)]
and toplevel_stmt = {
  ptop_desc: toplevel_stmt_desc,
  ptop_attributes: attributes,
  [@sexp_drop_if sexp_locs_disabled]
  ptop_loc: Location.t, // The full location, including attributes
  [@sexp_drop_if sexp_locs_disabled]
  ptop_core_loc: Location.t // The core location, without attributes
};

[@deriving (sexp, yojson)]
type comment_desc = {
  cmt_content: string,
  cmt_source: string,
  [@sexp_drop_if sexp_locs_disabled]
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
  attributes,
  module_name: loc(string),
  statements: list(toplevel_stmt),
  comments: list(comment),
  [@sexp_drop_if sexp_locs_disabled]
  prog_loc: Location.t, // The full location of the program
  [@sexp_drop_if sexp_locs_disabled]
  prog_core_loc: Location.t // The core location, without attributes
};
