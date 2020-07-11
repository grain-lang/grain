/* Inspired by OCaml's typing module */
/** Compile-time type and binding information */
open Grain_parsing;
include Asttypes;
open Sexplib.Conv;

module OrderedString = {
  type t = string;
  let compare = (x: t, y) => compare(x, y);
};

module Vars = Map.Make(OrderedString);

module Concr = Set.Make(OrderedString);

/** [commutable] is a flag appended to every arrow type.
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
  | /** A type variable (None == "_")*/
    TTyVar(option(string))
  | /** A function type. */
    TTyArrow(list(type_expr), type_expr, commutable)
  | /** A tuple type. */
    TTyTuple(list(type_expr))
  | /** A record type. */
    TTyRecord(list((string, type_expr)))
  | /** A parameterized type. */
    TTyConstr(
      Path.t,
      list(type_expr),
      ref(abbrev_memo),
    )
  | /** This is a special version of type variables which were introduced by a forall.*/
    TTyUniVar(
      option(string),
    )
  | /** This is a forall quantifier with the list type variables over the type. */
    TTyPoly(
      type_expr,
      list(type_expr),
    )
  | /** Used internally by the unification engine. */
    TTyLink(type_expr)
  | /** Used internally by the unification engine. */
    TTySubst(type_expr)

/** [abbrev_memo] allows one to keep track of different expansions of a type
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
  | /** No known abbrevation */
    TMemNil
  | /** Found one abbreviation.
        A valid abbreviation should be at least as visible and reachable by
        the same path. The first expression is the abbreviation and the
        second the expansion. */
    TMemCons(
      Path.t,
      type_expr,
      type_expr,
      abbrev_memo,
    )
  | /** Abbreviations can be found after this indirection */
    TMemLink(
      ref(abbrev_memo),
    );

[@deriving (sexp, yojson)]
type constructor_tag =
  | CstrConstant(int)
  | CstrBlock(int)
  | CstrUnboxed

[@deriving (sexp, yojson)]
and constructor_description = {
  /** Constructor name */
  cstr_name: string,
  /** Type of the result */
  cstr_res: type_expr,
  /** list of existentials */
  cstr_existentials: list(type_expr),
  /** Type of the arguments */
  cstr_args: list(type_expr),
  /** Number of arguments */
  cstr_arity: int,
  /** Tag for heap blocks */
  cstr_tag: constructor_tag,
  /** Number of constant constructors */
  cstr_consts: int,
  /** Number of non-constant constructors */
  cstr_nonconsts: int,
  [@sexp_drop_if _ => ! Grain_utils.Config.sexp_locs_enabled^]
  cstr_loc: Location.t,
}

and value_unbound_reason =
  | ValUnboundGhostRecursive;

[@deriving sexp]
type value_kind =
  | TValReg
  | TValPrim(string)
  | TValUnbound(value_unbound_reason)
  | TValConstructor(constructor_description);

/* See: https://github.com/janestreet/ppx_sexp_conv/issues/26 */
let rec value_kind_to_yojson =
  fun
  | TValReg => `String("TValReg")
  | TValPrim(n) => `List([`String("TValPrim"), `String(n)])
  | TValUnbound(r) =>
    `List([`String("TValUnbound"), value_unbound_reason_to_yojson(r)])
  | TValConstructor(d) =>
    `List([
      `String("TValConstructor"),
      constructor_description_to_yojson(d),
    ])

and value_kind_of_yojson = {
  let res_map = f =>
    fun
    | Result.Ok(v) => Result.Ok(f(v))
    | Result.Error(e) => Result.Error(e);

  fun
  | `String("TValReg") => Result.Ok(TValReg)
  | `List([`String("TValPrim"), `String(n)]) => Result.Ok(TValPrim(n))
  | `List([`String("TValUnbound"), r]) =>
    res_map(r => TValUnbound(r), value_unbound_reason_of_yojson(r))
  | `List([`String("TValConstructor"), d]) =>
    res_map(d => TValConstructor(d), constructor_description_of_yojson(d))
  | other =>
    Result.Error(
      "value_kind_of_yojson: Invalid JSON: " ++ Yojson.Safe.to_string(other),
    );
};

[@deriving (sexp, yojson)]
type value_description = {
  val_type: type_expr,
  val_kind: value_kind,
  val_fullpath: Path.t,
  val_mutable: bool,
  [@sexp_drop_if _ => ! Grain_utils.Config.sexp_locs_enabled^] [@default
                                                                 Location.dummy_loc
                                                               ]
  val_loc: Location.t,
};

[@deriving (sexp, yojson)]
type record_field = {
  rf_name: Ident.t,
  rf_type: type_expr,
  [@sexp_drop_if _ => ! Grain_utils.Config.sexp_locs_enabled^]
  rf_loc: Location.t,
};

[@deriving (sexp, yojson)]
type constructor_declaration = {
  cd_id: Ident.t,
  cd_args: constructor_arguments,
  cd_res: option(type_expr),
  [@sexp_drop_if _ => ! Grain_utils.Config.sexp_locs_enabled^]
  cd_loc: Location.t,
}

and constructor_arguments =
  | TConstrTuple(list(type_expr))
  | TConstrSingleton;

/* Whether the type should not be a pointer */

[@deriving (sexp, yojson)]
type type_declaration = {
  type_params: list(type_expr),
  type_arity: int,
  type_kind,
  type_manifest: option(type_expr),
  type_newtype_level: option((int, int)),
  [@sexp_drop_if _ => ! Grain_utils.Config.sexp_locs_enabled^] [@default
                                                                 Location.dummy_loc
                                                               ]
  type_loc: Location.t,
  type_path: Path.t,
  type_immediate: bool,
}

and type_kind =
  | TDataVariant(list(constructor_declaration))
  | TDataAbstract
  | TDataRecord(list(record_field));

[@deriving (sexp, yojson)]
type rec_status =
  | TRecNot
  | TRecFirst
  | TRecNext;

[@deriving (sexp, yojson)]
type signature_item =
  | TSigValue(Ident.t, value_description)
  | TSigType(Ident.t, type_declaration, rec_status)
  | TSigModule(Ident.t, module_declaration, rec_status)
  | TSigModType(Ident.t, modtype_declaration)

and signature = list(signature_item)

and module_type =
  | TModIdent(Path.t)
  | TModAlias(Path.t)
  | TModSignature(signature)

and module_declaration = {
  md_type: module_type,
  md_filepath: option(string),
  [@sexp_drop_if _ => ! Grain_utils.Config.sexp_locs_enabled^] [@default
                                                                 Location.dummy_loc
                                                               ]
  md_loc: Location.t,
}

and modtype_declaration = {
  mtd_type: option(module_type),
  [@sexp_drop_if _ => ! Grain_utils.Config.sexp_locs_enabled^] [@default
                                                                 Location.dummy_loc
                                                               ]
  mtd_loc: Location.t,
};

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
  | (CstrUnboxed, CstrUnboxed) => true
  | _ => false
  };

let may_equal_constr = (c1, c2) =>
  switch (c1.cstr_tag, c2.cstr_tag) {
  | (tag1, tag2) => equal_tag(tag1, tag2)
  };

[@deriving (sexp, yojson)]
type label_description = {
  lbl_name: string, /* Short name */
  lbl_res: type_expr, /* Type of the result */
  lbl_arg: type_expr, /* Type of the argument */
  lbl_pos: int, /* Position in block */
  lbl_all: array(label_description), /* All the labels in this type */
  [@sexp_drop_if _ => ! Grain_utils.Config.sexp_locs_enabled^] [@default
                                                                 Location.dummy_loc
                                                               ]
  lbl_loc: Location.t,
};
