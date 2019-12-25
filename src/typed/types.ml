(** Compile-time type and binding information *)
(* Inspired by OCaml's typing module *)
open Grain_parsing
include Asttypes
open Sexplib.Conv

module OrderedString = struct
  type t = string
  let compare (x:t) y = compare x y
end

module Vars = Map.Make(OrderedString)

module Concr = Set.Make(OrderedString)

(** [commutable] is a flag appended to every arrow type.
    When typing an application, if the type of the functional is
    known, its type is instantiated with [TComOk] arrows, otherwise as
    [TComLink (ref TComUnknown)].
    When the type is not known, the application will be used to infer
    the actual type.  
*)
type commutable =
  | TComOk
  | TComUnknown
  | TComLink of commutable ref
[@@deriving sexp, yojson]

type type_expr = {
  mutable desc: type_desc;
  mutable level: int;
  id: int;
} [@@deriving sexp, yojson]

and type_desc =
  | TTyVar of string option
  (** A type variable (None == "_")*)
  | TTyArrow of type_expr list * type_expr * commutable
  (** A function type. *)
  | TTyTuple of type_expr list
  (** A tuple type. *)
  | TTyRecord of (string * type_expr) list
  (** A record type. *)
  | TTyConstr of Path.t * type_expr list * abbrev_memo ref
  (** A parameterized type. *)
  | TTyUniVar of string option
  (** This is a special version of type variables which were introduced by a forall.*)
  | TTyPoly of type_expr * type_expr list
  (** This is a forall quantifier with the list type variables over the type. *)
  | TTyLink of type_expr
  (** Used internally by the unification engine. *)
  | TTySubst of type_expr
  (** Used internally by the unification engine. *)

(** [abbrev_memo] allows one to keep track of different expansions of a type
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
*)
and abbrev_memo =
  | TMemNil (** No known abbrevation *)
  | TMemCons of Path.t * type_expr * type_expr * abbrev_memo
    (** Found one abbreviation.
        A valid abbreviation should be at least as visible and reachable by
        the same path. The first expression is the abbreviation and the
        second the expansion. *)
  | TMemLink of abbrev_memo ref (** Abbreviations can be found after this indirection *)

type constructor_tag =
  | CstrConstant of int
  | CstrBlock of int
  | CstrUnboxed
[@@deriving sexp, yojson]

and constructor_description = {
  cstr_name : string;                (** Constructor name *)
  cstr_res: type_expr;               (** Type of the result *)
  cstr_existentials: type_expr list; (** list of existentials *)
  cstr_args: type_expr list;         (** Type of the arguments *)
  cstr_arity: int;                   (** Number of arguments *)
  cstr_tag: constructor_tag;         (** Tag for heap blocks *)
  cstr_consts: int;                  (** Number of constant constructors *)
  cstr_nonconsts: int;               (** Number of non-constant constructors *)
  cstr_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp, yojson]

and value_unbound_reason =
  | ValUnboundGhostRecursive

type value_kind =
  | TValReg
  | TValPrim of string
  | TValUnbound of value_unbound_reason
  | TValConstructor of constructor_description
[@@deriving sexp]

(* See: https://github.com/janestreet/ppx_sexp_conv/issues/26 *)
let rec value_kind_to_yojson = function
  | TValReg -> `String "TValReg"
  | TValPrim n -> `List [`String "TValPrim"; `String n]
  | TValUnbound r -> `List [`String "TValUnbound"; value_unbound_reason_to_yojson r]
  | TValConstructor d -> `List [`String "TValConstructor"; constructor_description_to_yojson d]

and value_kind_of_yojson =
  let res_map f = function
    | Result.Ok v -> Result.Ok (f v)
    | Result.Error e -> Result.Error e
  in
  function
  | `String "TValReg" -> Result.Ok TValReg
  | `List [`String "TValPrim"; `String n] -> Result.Ok (TValPrim n)
  | `List [`String "TValUnbound"; r] -> res_map (fun r -> TValUnbound r) (value_unbound_reason_of_yojson r)
  | `List [`String "TValConstructor"; d] -> res_map (fun d -> TValConstructor d) (constructor_description_of_yojson d)
  | other -> Result.Error ("value_kind_of_yojson: Invalid JSON: " ^ (Yojson.Safe.to_string other))


type value_description = {
  val_type: type_expr;
  val_kind: value_kind;
  val_fullpath: Path.t;
  val_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled] [@default Location.dummy_loc];
} [@@deriving sexp, yojson]

type record_field = {
  rf_name: Ident.t;
  rf_type: type_expr;
  rf_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp, yojson]

type constructor_declaration = {
  cd_id: Ident.t;
  cd_args: constructor_arguments;
  cd_res: type_expr option;
  cd_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp, yojson]

and constructor_arguments =
  | TConstrTuple of type_expr list
  | TConstrSingleton


type type_declaration = {
  type_params: type_expr list;
  type_arity: int;
  type_kind: type_kind;
  type_manifest: type_expr option;
  type_newtype_level: (int * int) option;
  type_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled] [@default Location.dummy_loc];
  type_immediate: bool; (* Whether the type should not be a pointer *)
} [@@deriving sexp, yojson]

and type_kind =
  | TDataVariant of constructor_declaration list
  | TDataAbstract
  | TDataRecord of record_field list

type rec_status =
  | TRecNot
  | TRecFirst
  | TRecNext
[@@deriving sexp, yojson]

type signature_item =
  | TSigValue of Ident.t * value_description
  | TSigType of Ident.t * type_declaration * rec_status
  | TSigModule of Ident.t * module_declaration * rec_status
  | TSigModType of Ident.t * modtype_declaration
[@@deriving sexp, yojson]

and signature = signature_item list

and module_type =
  | TModIdent of Path.t
  | TModAlias of Path.t
  | TModSignature of signature

and module_declaration = {
  md_type: module_type;
  md_filepath: string option;
  md_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled] [@default Location.dummy_loc];
}

and modtype_declaration = {
  mtd_type: module_type option;
  mtd_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled] [@default Location.dummy_loc];
}

module TypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

let equal_tag t1 t2 =
  match t1, t2 with
  | CstrBlock i1, CstrBlock i2
  | CstrConstant i1, CstrConstant i2 -> i1 = i2
  | CstrUnboxed, CstrUnboxed -> true
  | _ -> false

let may_equal_constr c1 c2 =
  match c1.cstr_tag, c2.cstr_tag with
  | tag1, tag2 -> equal_tag tag1 tag2

type label_description = { 
  lbl_name: string;                   (* Short name *)
  lbl_res: type_expr;                 (* Type of the result *)
  lbl_arg: type_expr;                 (* Type of the argument *)
  lbl_pos: int;                       (* Position in block *)
  lbl_all: label_description array;   (* All the labels in this type *)
  lbl_loc: Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled] [@default Location.dummy_loc];
} [@@deriving sexp, yojson]