/** Parse tree type definitions. This is a reformulation
    of our original parse tree which better aligns with the design
    of the OCaml parse tree. Credit for the module's architecture goes to
    the OCaml team. */
open Sexplib.Conv;
open Asttypes;

let sexp_locs_disabled = _ => ! Grain_utils.Config.sexp_locs_enabled^;

type loc('a) =
  Asttypes.loc('a) = {
    txt: 'a,
    loc: Location.t,
  };

type export_flag = Asttypes.export_flag = | Nonexported | Exported;
type rec_flag = Asttypes.rec_flag = | Nonrecursive | Recursive;
type mut_flag = Asttypes.mut_flag = | Mutable | Immutable;

/** Type for syntax-level types */

[@deriving sexp]
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

[@deriving sexp]
type constructor_arguments =
  | PConstrTuple(list(parsed_type))
  | PConstrSingleton;

/** Type for branches within data declarations */

[@deriving sexp]
type constructor_declaration = {
  pcd_name: loc(string),
  pcd_args: constructor_arguments,
  [@sexp_drop_if sexp_locs_disabled]
  pcd_loc: Location.t,
};

/** Type for fields within a record */

[@deriving sexp]
type label_declaration = {
  pld_name: loc(Identifier.t),
  pld_type: parsed_type,
  pld_mutable: mut_flag,
  [@sexp_drop_if sexp_locs_disabled]
  pld_loc: Location.t,
};

/** Different types of data which can be declared. Currently only one. */

[@deriving sexp]
type data_kind =
  | PDataVariant(list(constructor_declaration))
  | PDataRecord(list(label_declaration));

/** Type for data declarations. */

[@deriving sexp]
type data_declaration = {
  pdata_name: loc(string),
  pdata_params: list(parsed_type),
  pdata_kind: data_kind,
  [@sexp_drop_if sexp_locs_disabled]
  pdata_loc: Location.t,
};

/** Constants supported by Grain */

[@deriving sexp]
type constant =
  | PConstNumber(int)
  | PConstInt32(int32)
  | PConstInt64(int64)
  | PConstBool(bool)
  | PConstVoid
  | PConstString(string);

/** Various binding forms */

[@deriving sexp]
type pattern_desc =
  | PPatAny
  | PPatVar(loc(string))
  | PPatTuple(list(pattern))
  | PPatRecord(list((loc(Identifier.t), pattern)), closed_flag)
  | PPatConstant(constant)
  | PPatConstraint(pattern, parsed_type)
  | PPatConstruct(loc(Identifier.t), list(pattern))
  | PPatOr(pattern, pattern)
  | PPatAlias(pattern, loc(string))

[@deriving sexp]
and pattern = {
  ppat_desc: pattern_desc,
  [@sexp_drop_if sexp_locs_disabled]
  ppat_loc: Location.t,
};

/** Single-argument operators */

[@deriving sexp]
type prim1 =
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

/** Two-argument operators */

[@deriving sexp]
type prim2 =
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

/** Type for expressions (i.e. things which evaluate to something) */

[@deriving sexp]
type expression = {
  pexp_desc: expression_desc,
  [@sexp_drop_if sexp_locs_disabled]
  pexp_loc: Location.t,
}

[@deriving sexp]
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
  | PExpLet(rec_flag, mut_flag, list(value_binding), expression)
  | PExpMatch(expression, list(match_branch))
  | PExpPrim1(prim1, expression)
  | PExpPrim2(prim2, expression, expression)
  | PExpIf(expression, expression, expression)
  | PExpWhile(expression, expression)
  | PExpConstraint(expression, parsed_type)
  | PExpLambda(list(pattern), expression)
  | PExpApp(expression, list(expression))
  | PExpBlock(list(expression))
  | PExpBoxAssign(expression, expression)
  | PExpAssign(expression, expression)
  | /** Used for modules without body expressions */
    PExpNull

/** let-binding form */

[@deriving sexp]
and value_binding = {
  pvb_pat: pattern,
  pvb_expr: expression,
  [@sexp_drop_if sexp_locs_disabled]
  pvb_loc: Location.t,
}

[@deriving sexp]
and match_branch = {
  pmb_pat: pattern,
  pmb_body: expression,
  [@sexp_drop_if sexp_locs_disabled]
  pmb_loc: Location.t,
};

[@deriving sexp]
type import_value =
  | PImportModule
  | PImportAllExcept(list(loc(Identifier.t)))
  | PImportValues(list((loc(Identifier.t), option(loc(Identifier.t)))));

/** Type for import statements */

[@deriving sexp]
type import_declaration = {
  pimp_mod_alias: option(loc(Identifier.t)),
  pimp_path: loc(string),
  pimp_val: import_value,
  [@sexp_drop_if sexp_locs_disabled]
  pimp_loc: Location.t,
};

[@deriving sexp]
type value_description = {
  pval_mod: loc(string),
  pval_name: loc(string),
  pval_name_alias: option(loc(string)),
  pval_type: parsed_type,
  pval_prim: list(string),
  [@sexp_drop_if sexp_locs_disabled]
  pval_loc: Location.t,
};

[@deriving sexp]
type export_declaration_desc = {
  pex_name: loc(string),
  pex_alias: option(loc(string)),
  [@sexp_drop_if sexp_locs_disabled]
  pex_loc: Location.t,
}

[@deriving sexp]
and export_declaration =
  | ExportData(export_declaration_desc)
  | ExportValue(export_declaration_desc);

[@deriving sexp]
type export_except =
  | ExportExceptData(loc(string))
  | ExportExceptValue(loc(string));

/** Statements which can exist at the top level */

[@deriving sexp]
type toplevel_stmt_desc =
  | PTopImport(list(import_declaration))
  | PTopForeign(export_flag, value_description)
  | PTopPrimitive(export_flag, value_description)
  | PTopData(export_flag, data_declaration)
  | PTopLet(export_flag, rec_flag, mut_flag, list(value_binding))
  | PTopExpr(expression)
  | PTopExport(list(export_declaration))
  | PTopExportAll(list(export_except));

[@deriving sexp]
type toplevel_stmt = {
  ptop_desc: toplevel_stmt_desc,
  [@sexp_drop_if sexp_locs_disabled]
  ptop_loc: Location.t,
};

/** The type for parsed programs */

[@deriving sexp]
type parsed_program = {
  statements: list(toplevel_stmt),
  [@sexp_drop_if sexp_locs_disabled]
  prog_loc: Location.t,
};
