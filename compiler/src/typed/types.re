/* Inspired by OCaml's typing module */
/** Compile-time type and binding information */
open Grain_parsing;
include Asttypes;
open Sexplib.Conv;

let sexp_locs_disabled = _ => ! Grain_utils.Config.sexp_locs_enabled^;

module OrderedString = {
  type t = string;
  let compare = (x: t, y) => compare(x, y);
};

module Vars = Map.Make(OrderedString);

module Concr = Set.Make(OrderedString);

/**
  [commutable] is a flag appended to every arrow type.
  When typing an application, if the type of the functional is
  known, its type is instantiated with [TComOk] arrows, otherwise as
  [TComLink (ref TComUnknown)].
  When the type is not known, the application will be used to infer
  the actual type.
*/
[@deriving (sexp, yojson)]
type commutable =
  | TComOk
  | TComUnknown
  | TComLink(ref(commutable));

[@deriving (sexp, yojson)]
type type_expr = {
  mutable desc: type_desc,
  mutable level: int,
  id: int,
}

and type_desc =
  | TTyVar(option(string)) // A type variable (None == "_")
  | TTyArrow(list((argument_label, type_expr)), type_expr, commutable) // A function type.
  | TTyTuple(list(type_expr)) // A tuple type.
  | TTyRecord(list((string, type_expr))) // A record type.
  | TTyConstr(Path.t, list(type_expr), ref(abbrev_memo)) // A parameterized type.
  | TTyUniVar(option(string)) // This is a special version of type variables which were introduced by a forall.
  | TTyPoly(type_expr, list(type_expr)) // This is a forall quantifier with the list type variables over the type.
  | TTyLink(type_expr) // Used internally by the unification engine.
  | TTySubst(type_expr) // Used internally by the unification engine.

/**
  [abbrev_memo] allows one to keep track of different expansions of a type
  alias. This is done for performance purposes.
  For instance, when defining [type Pair<a> = (a, a)], when one refers to an
  [Pair<a>], it is just a shortcut for the [(a, a)] type.
  This expansion will be stored in the [abbrev_memo] of the corresponding
  [TTyConstr] node.
  In practice, [abbrev_memo] behaves like list of expansions with a mutable
  tail.
  Note on marshalling: [abbrev_memo] must not appear in saved types.
  [Btype], with [cleanup_abbrev] and [memo], takes care of tracking and
  removing abbreviations.
*/
and abbrev_memo =
  // No known abbrevation
  | TMemNil
  // Found one abbreviation.
  // A valid abbreviation should be at least as visible and reachable by
  // the same path. The first expression is the abbreviation and the
  // second the expansion.
  | TMemCons(Path.t, type_expr, type_expr, abbrev_memo)
  // Abbreviations can be found after this indirection
  | TMemLink(ref(abbrev_memo));

[@deriving (sexp, yojson)]
type allocation_type =
  | Unmanaged(wasm_repr)
  | Managed

[@deriving (sexp, yojson)]
and wasm_repr =
  | WasmI32
  | WasmI64
  | WasmF32
  | WasmF64;

[@deriving (sexp, yojson)]
type val_repr =
  | ReprFunction(list(wasm_repr), list(wasm_repr), func_direct)
  | ReprValue(wasm_repr)

[@deriving (sexp, yojson)]
and func_direct =
  | Direct({
      name: string,
      closure: bool,
    })
  | Indirect
  | Unknown;

[@deriving (sexp, yojson)]
type record_field = {
  rf_name: Ident.t,
  rf_type: type_expr,
  rf_mutable: bool,
  [@sexp_drop_if sexp_locs_disabled]
  rf_loc: Location.t,
};

[@deriving (sexp, yojson)]
type type_declaration = {
  type_params: list(type_expr),
  type_arity: int,
  type_kind,
  type_manifest: option(type_expr),
  type_newtype_level: option((int, int)),
  [@sexp_drop_if sexp_locs_disabled] [@default Location.dummy_loc]
  type_loc: Location.t,
  type_path: Path.t,
  type_allocation: allocation_type,
}

and type_kind =
  | TDataVariant(list(constructor_declaration))
  | TDataAbstract
  | TDataRecord(list(record_field))
  | TDataOpen

[@deriving (sexp, yojson)]
and constructor_declaration = {
  cd_id: Ident.t,
  cd_args: constructor_arguments,
  cd_res: option(type_expr),
  cd_repr: val_repr,
  [@sexp_drop_if sexp_locs_disabled]
  cd_loc: Location.t,
}

and constructor_arguments =
  | TConstrTuple(list(type_expr))
  | TConstrRecord(list(record_field))
  | TConstrSingleton;

[@deriving (sexp, yojson)]
type extension_constructor_type =
  | CstrExtensionConstant
  | CstrExtensionBlock;

[@deriving (sexp, yojson)]
type extension_constructor = {
  ext_type_path: Path.t,
  ext_type_params: list(type_expr),
  ext_args: constructor_arguments,
  ext_repr: val_repr,
  ext_name: Ident.t,
  [@sexp_drop_if sexp_locs_disabled]
  ext_loc: Location.t,
};

[@deriving (sexp, yojson)]
type constructor_tag =
  | CstrConstant(int)
  | CstrBlock(int)
  | CstrExtension(
      int,
      Path.t,
      extension_constructor_type,
      extension_constructor,
    )
  | CstrUnboxed;

[@deriving (sexp, yojson)]
type constructor_description = {
  cstr_name: string, // Constructor name
  cstr_res: type_expr, // Type of the result
  cstr_existentials: list(type_expr), // list of existentials
  cstr_args: list(type_expr), // Type of the arguments
  cstr_arity: int, // Number of arguments
  cstr_tag: constructor_tag, // Tag for heap blocks
  cstr_consts: int, // Number of constant constructors
  cstr_nonconsts: int, // Number of non-constant constructors
  cstr_inlined: option(type_declaration), // For inlined record constructors
  [@sexp_drop_if sexp_locs_disabled]
  cstr_loc: Location.t,
}

and value_unbound_reason =
  | ValUnboundGhostRecursive;

[@deriving (sexp, yojson)]
type adt_constructor_type =
  | TupleConstructor
  | RecordConstructor(list(string));

[@deriving (sexp, yojson)]
type type_metadata =
  | ADTMetadata(int, list((int, string, adt_constructor_type)))
  | RecordMetadata(int, list(string))
  | ExceptionMetadata(int, int, string, adt_constructor_type);

[@deriving (sexp, yojson)]
type value_kind =
  | TValReg
  | TValPrim(string)
  | TValUnbound(value_unbound_reason)
  | TValConstructor(constructor_description);

[@deriving (sexp, yojson)]
type value_description = {
  val_type: type_expr,
  val_repr,
  val_kind: value_kind,
  val_internalpath: Path.t,
  val_fullpath: Path.t,
  val_mutable: bool,
  val_global: bool,
  [@sexp_drop_if sexp_locs_disabled] [@default Location.dummy_loc]
  val_loc: Location.t,
};

[@deriving (sexp, yojson)]
type rec_status =
  | TRecNot
  | TRecFirst
  | TRecNext;

[@deriving (sexp, yojson)]
type ext_status =
  | TExtFirst
  | TExtNext
  | TExtException;

[@deriving (sexp, yojson)]
type signature_item =
  | TSigValue(Ident.t, value_description)
  | TSigType(Ident.t, type_declaration, rec_status)
  | TSigTypeExt(Ident.t, extension_constructor, ext_status)
  | TSigModule(Ident.t, module_declaration, rec_status)
  | TSigModType(Ident.t, modtype_declaration)

/** The interface of a module, which includes all [signature_item] exported by the module */
and signature = list(signature_item)

and module_type =
  | TModIdent(Path.t)
  | TModAlias(Path.t)
  | TModSignature(signature)

and module_declaration = {
  md_type: module_type,
  md_filepath: option(string),
  [@sexp_drop_if sexp_locs_disabled] [@default Location.dummy_loc]
  md_loc: Location.t,
}

and modtype_declaration = {
  mtd_type: option(module_type),
  [@sexp_drop_if sexp_locs_disabled] [@default Location.dummy_loc]
  mtd_loc: Location.t,
};

[@deriving sexp]
type use_items =
  | TUseAll
  | TUseItems(list(use_item))

[@deriving sexp]
and use_item =
  | TUseType({
      name: string,
      declaration: type_declaration,
      loc: Location.t,
    })
  | TUseException({
      name: string,
      ext: extension_constructor,
      loc: Location.t,
    })
  | TUseModule({
      name: string,
      declaration: module_declaration,
      loc: Location.t,
    })
  | TUseValue({
      name: string,
      value: value_description,
      loc: Location.t,
    });

module TypeOps = {
  type t = type_expr;
  let compare = (t1, t2) => t1.id - t2.id;
  let hash = t => t.id;
  let equal = (t1, t2) => t1 === t2;
};

let equal_tag = (t1, t2) =>
  switch (t1, t2) {
  | (CstrBlock(i1), CstrBlock(i2))
  | (CstrConstant(i1), CstrConstant(i2)) => i1 == i2
  | (CstrExtension(i1, p1, b1, e1), CstrExtension(i2, p2, b2, e2)) =>
    i1 == i2 && Path.same(p1, p2) && b1 == b2 && e1 == e2
  | (CstrUnboxed, CstrUnboxed) => true
  | _ => false
  };

let may_equal_constr = (c1, c2) =>
  switch (c1.cstr_tag, c2.cstr_tag) {
  | (tag1, tag2) => equal_tag(tag1, tag2)
  };

[@deriving (sexp, yojson)]
type label_description = {
  lbl_name: string, // Short name
  lbl_res: type_expr, // Type of the result
  lbl_arg: type_expr, // Type of the argument
  lbl_pos: int, // Position in block
  lbl_mut: bool, // If this label is mutable
  lbl_all: array(label_description), // All the labels in this type
  [@sexp_drop_if sexp_locs_disabled] [@default Location.dummy_loc]
  lbl_loc: Location.t,
};
