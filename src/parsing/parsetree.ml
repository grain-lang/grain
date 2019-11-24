(** Parse tree type definitions. This is a reformulation
    of our original parse tree which better aligns with the design
    of the OCaml parse tree. Credit for the module's architecture goes to
    the OCaml team. *)
open Sexplib.Conv
open Asttypes

type 'a loc = 'a Asttypes.loc = {
  txt: 'a;
  loc: Location.t;
}

type export_flag = Asttypes.export_flag = Nonexported | Exported
type rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive

(** Type for syntax-level types *)
type parsed_type_desc =
  | PTyAny
  | PTyVar of string
  | PTyArrow of parsed_type list * parsed_type
  | PTyTuple of parsed_type list
  | PTyConstr of (Identifier.t loc) * parsed_type list
  | PTyPoly of string loc list * parsed_type
[@@deriving sexp]

and parsed_type = {
  ptyp_desc: parsed_type_desc;
  ptyp_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
}

(** Type for arguments to a constructor *)
type constructor_arguments =
  | PConstrTuple of parsed_type list
  | PConstrSingleton
[@@deriving sexp]

(** Type for branches within data declarations *)
type constructor_declaration = {
  pcd_name: string loc;
  pcd_args: constructor_arguments;
  pcd_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

(** Type for fields within a record *)
type label_declaration = {
  pld_name: Identifier.t loc;
  pld_type: parsed_type;
  pld_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

(** Different types of data which can be declared. Currently only one. *)
type data_kind =
  | PDataVariant of constructor_declaration list
  | PDataRecord of label_declaration list
[@@deriving sexp]

(** Type for data declarations. *)
type data_declaration = {
  pdata_name: string loc;
  pdata_params: parsed_type list;
  pdata_kind: data_kind;
  pdata_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

(** Constants supported by Grain *)
type constant =
  | PConstNumber of int
  | PConstBool of bool
  | PConstString of string
[@@deriving sexp]

(** Various binding forms *)
type pattern_desc =
  | PPatAny
  | PPatVar of string loc
  | PPatTuple of pattern list
  | PPatRecord of (Identifier.t loc * pattern) list * closed_flag
  | PPatConstant of constant
  | PPatConstraint of pattern * parsed_type
  | PPatConstruct of Identifier.t loc * pattern list
  | PPatOr of pattern * pattern
  | PPatAlias of pattern * string loc
[@@deriving sexp]

and pattern = {
  ppat_desc: pattern_desc;
  ppat_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

(** Single-argument operators *)
type prim1 =
  | Add1
  | Sub1
  | Not
  | Box
  | Unbox
  | IsNum
  | IsBool
  | IsTuple
[@@deriving sexp]

(** Two-argument operators *)
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
[@@deriving sexp]

type ignored = TypeKept | TypeIgnored [@@deriving sexp]

(** Type for expressions (i.e. things which evaluate to something) *)
type expression = {
  pexp_desc: expression_desc;
  pexp_ignored: ignored;
  pexp_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

and expression_desc =
  | PExpId of Identifier.t loc
  | PExpConstant of constant
  | PExpTuple of expression list
  | PExpRecord of (Identifier.t loc * expression) list
  | PExpRecordGet of expression * Identifier.t loc
  | PExpLet of rec_flag * value_binding list * expression
  | PExpMatch of expression * match_branch list
  | PExpPrim1 of prim1 * expression
  | PExpPrim2 of prim2 * expression * expression
  | PExpIf of expression * expression * expression
  | PExpWhile of expression * expression
  | PExpConstraint of expression * parsed_type
  | PExpLambda of pattern list * expression
  | PExpApp of expression * expression list
  | PExpBlock of expression list
  | PExpAssign of expression * expression
  | PExpNull (** Used for modules without body expressions *)
[@@deriving sexp]

(** let-binding form *)
and value_binding = {
  pvb_pat: pattern;
  pvb_expr: expression;
  pvb_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

and match_branch = {
  pmb_pat: pattern;
  pmb_body: expression;
  pmb_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

type import_value = 
  | PImportModule
  | PImportAllExcept of (Identifier.t loc) list
  | PImportValues of (Identifier.t loc * Identifier.t loc option) list
[@@deriving sexp]

(** Type for import statements *)
type import_declaration = {
  pimp_mod_alias: Identifier.t loc option;
  pimp_path: string loc;
  pimp_val: import_value;
  pimp_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

type value_description = {
  pval_mod: string loc;
  pval_name: string loc;
  pval_type: parsed_type;
  pval_prim: string list;
  pval_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

type export_declaration_desc = {
  pex_name: string loc;
  pex_alias: string loc option;
  pex_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

and export_declaration = 
  | ExportData of export_declaration_desc 
  | ExportValue of export_declaration_desc
[@@deriving sexp]

type export_except = 
  | ExportExceptData of string loc 
  | ExportExceptValue of string loc 
[@@deriving sexp]

(** Statements which can exist at the top level *)
type toplevel_stmt_desc =
  | PTopImport of import_declaration list
  | PTopForeign of export_flag * value_description
  | PTopData of export_flag * data_declaration
  | PTopLet of export_flag * rec_flag * value_binding list
  | PTopExport of export_declaration list
  | PTopExportAll of export_except list
[@@deriving sexp]

type toplevel_stmt = {
  ptop_desc: toplevel_stmt_desc;
  ptop_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

(** The type for parsed programs *)
type parsed_program = {
  statements: toplevel_stmt list;
  body: expression;
  prog_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]

