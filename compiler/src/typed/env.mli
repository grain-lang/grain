open Grain_parsing
open Parsetree
open Types

module PathMap : Map.S with type key = Path.t
                        and type 'a t = 'a Map.Make(Path).t

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_module of summary * Ident.t * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_open of summary * Path.t
  | Env_constraints of summary * type_declaration PathMap.t
  | Env_copy_types of summary * string list
[@@deriving sexp]

type type_descriptions = constructor_description list * label_description list
type t

val empty : t
val initial_safe_string : t
val initial_unsafe_string : t

type dependency_chain = (string Location.loc) list

type error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string
  | Depend_on_unsafe_string_unit of string * string
  | Missing_module of Location.t * Path.t * Path.t
  | Unbound_module of Location.t * string
  | Unbound_label of Location.t * string
  | No_module_file of string * string option
  | Value_not_found_in_module of Location.t * string * string
  | Illegal_value_name of Location.t * string
  | Cyclic_dependencies of string * dependency_chain

exception Error of error
val error : error -> 'a

(* For short-paths *)
type iter_cont
val iter_types:
    (Path.t -> Path.t * (type_declaration * type_descriptions) -> unit) ->
    t -> iter_cont
val run_iter_cont: iter_cont list -> (Path.t * iter_cont) list
val same_types: t -> t -> bool
val used_persistent: unit -> Concr.t
val find_shadowed_types: Path.t -> t -> Path.t list
val without_cmis: ('a -> 'b) -> 'a -> 'b
(** [without_cmis f arg] applies [f] to [arg], but does not
    allow opening cmis during its execution *)

(* By-path lookups *)
val find_value: Path.t -> t -> value_description
val find_type: Path.t -> t -> type_declaration
val find_type_descrs: Path.t -> t -> type_descriptions
val find_module: Path.t -> string option -> t -> module_declaration
val find_modtype: Path.t -> t -> modtype_declaration

val find_type_expansion:
  Path.t -> t -> type_expr list * type_expr * int option
val find_type_expansion_opt:
  Path.t -> t -> type_expr list * type_expr * int option
(* Find the manifest type information associated to a type for the sake
   of the compiler's type-based optimisations. *)
val find_modtype_expansion: Path.t -> t -> module_type
val normalize_path: Location.t option -> t -> Path.t -> Path.t
val normalize_path_prefix: Location.t option -> t -> Path.t -> Path.t
(** Normalize the path to a concrete value or module.
    If the option is None, allow returning dangling paths.
    Otherwise raise a Missing_module error, and may add forgotten
    head as required global. *)

val has_local_constraints: t -> bool

(* By-identifier lookups *)
val lookup_value: ?mark:bool -> Identifier.t -> t -> Path.t * value_description
(** Looks up the value associated with the given identifier. *)
val lookup_type: ?mark:bool ->  Identifier.t -> t -> Path.t
(** Looks up the type associated with the given identifier. *)
val lookup_constructor: ?mark:bool -> Identifier.t -> t -> constructor_description
(** Looks up the constructor associated with the given identifier. *)
val lookup_all_constructors:
  ?mark:bool -> Identifier.t -> t -> (constructor_description * (unit -> unit)) list
val lookup_all_labels: 
  ?mark:bool -> Identifier.t -> t -> (label_description * (unit -> unit)) list
val lookup_module:
  load:bool -> ?loc:Location.t -> ?mark:bool -> Identifier.t -> string option -> t -> Path.t
val lookup_modtype:
  ?loc:Location.t -> ?mark:bool ->
  Identifier.t -> t -> Path.t * modtype_declaration

val copy_types: string list -> t -> t
  (* Used only in Typecore.duplicate_ident_types. *)

(* By-identifier insertions *)
val add_value: ?check:(string -> Warnings.t) -> Ident.t -> value_description -> t -> t
(** Adds a value identifier with the given name and description. *)
val add_type: check:bool -> Ident.t -> type_declaration -> t -> t
(** Adds a type identifier with the given name and declaration. *)
val add_constructor: Ident.t -> constructor_description -> t -> t
(** Adds a constructor with the given name and description. *)
val add_module: ?arg:bool -> Ident.t -> module_type -> string option -> t -> t
val add_module_declaration: ?arg:bool -> check:bool -> Ident.t ->
  module_declaration -> t -> t
val add_modtype: Ident.t -> modtype_declaration -> t -> t
val add_local_constraint: Path.t -> type_declaration -> int -> t -> t
val add_local_type: Path.t -> type_declaration -> t -> t

(* Insertion of all fields of a signature. *)

val add_item: signature_item -> t -> t
val add_signature: signature -> t -> t

(* Remember the current compilation unit: modname * filename. *)
val set_unit: string * string -> unit
val get_unit: unit -> string * string

(* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. Returns None if the path refers to a functor,
   not a structure. *)
val open_signature:
    ?used_slot:bool ref -> ?toplevel:bool -> Path.t -> Identifier.t -> import_declaration ->
      t -> t option
(* Similar to [open_signature], except that modules from the load path
   have precedence over sub-modules of the opened module.
   For instance, if opening a module [M] with a sub-module [X]:
   - if the load path contains a [x.cmi] file, then resolving [X] in the
     new environment yields the same result as resolving [X] in the
     old environment
   - otherwise, in the new environment [X] resolves to [M.X]
*)
val open_signature_of_initially_opened_module:
    ?loc:Location.t -> Path.t -> string option -> t -> t option


(* Read, save a signature to/from a file *)

val read_signature: string -> string -> signature
        (* Arguments: module name, file name. Results: signature. *)
val build_signature:
  ?deprecated:string -> signature -> string -> string -> Cmi_format.cmi_infos
        (* Arguments: signature, module name, file name. *)
val build_signature_with_imports:
  ?deprecated:string ->
  signature -> string -> string -> (string * Digest.t option) list
  -> Cmi_format.cmi_infos
        (* Arguments: signature, module name, file name,
           imported units with their CRCs. *)

(* Return the CRC of the interface of the given compilation unit *)

val crc_of_unit: string -> string option -> Digest.t

(* Return the set of compilation units imported, with their CRC *)

val imports: unit -> (string * Digest.t option) list

(* [is_imported_opaque md] returns true if [md] is an opaque imported module  *)
val is_imported_opaque: string -> bool

(* Direct access to the table of imported compilation units with their CRC *)

module Consistbl : module type of struct
  include Grain_utils.Consistbl.Make (Misc.Stdlib.String)
end

val crc_units: Consistbl.t
val add_import: string -> unit
val clear_imports: unit -> unit

(* By-name insertions *)
val enter_value: string -> value_description -> t -> Ident.t * t
(** Adds a value identifier with the given name and description.
    The new environment and a generated identifier are returned. *)
val enter_type: string -> type_declaration -> t -> Ident.t * t
(** Adds a type identifier with the given name and declaration.
    The new environment and a generated identifier are returned. *)

(* Forward declaration to break mutual recursion with Includemod. *)
val check_modtype_inclusion:
  (loc:Location.t -> t -> module_type -> Path.t -> module_type -> unit) ref
(* Forward declaration to break mutual recursion with Mtype. *)
val strengthen:
  (aliasable:bool -> t -> module_type -> Path.t -> module_type) ref
(* Forward declaration to break mutual recursion with Typecore. *)
val add_delayed_check_forward: ((unit -> unit) -> unit) ref
(* Forward declaration to break mutual recursion with Ctype. *)
val same_constr: (t -> type_expr -> type_expr -> bool) ref
(* Forward declaration to break mutual recursion with Compile.
   Compiles the given input file to the given output file location. *)
val compile_module_dependency: (string -> string -> unit) ref

(* Analysis functions *)

val fold_values:
  (string -> Path.t -> value_description -> 'a -> 'a) ->
  Identifier.t option -> t -> 'a -> 'a
val fold_types:
  (string -> Path.t -> type_declaration * type_descriptions -> 'a -> 'a) ->
  Identifier.t option -> t -> 'a -> 'a
val fold_constructors:
  (constructor_description -> 'a -> 'a) ->
  Identifier.t option -> t -> 'a -> 'a
(** Persistent structures are only traversed if they are already loaded. *)
val fold_modules:
  (string -> Path.t -> module_declaration -> 'a -> 'a) ->
  Identifier.t option -> t -> 'a -> 'a
val fold_modtypes:
  (string -> Path.t -> modtype_declaration -> 'a -> 'a) ->
  Identifier.t option -> t -> 'a -> 'a

val scrape_alias: t -> module_type -> module_type
val check_value_name: string -> Location.t -> unit

module Persistent_signature : sig
  type t =
    { filename : string; (** Name of the file containing the signature. *)
      cmi : Cmi_format.cmi_infos }

  (** Function used to load a persistent signature. The default is to look for
      the .cmi file in the load path. This function can be overridden to load
      it from memory, for instance to build a self-contained toplevel. *)
  val load : (?loc:Location.t -> unit_name:string -> t option) ref
end

(* Summaries -- compact representation of an environment, to be
   exported in debugging information. *)

val summary: t -> summary
