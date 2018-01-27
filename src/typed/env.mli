open Grain_parsing
open Types

module PathMap : Map.S with type key = Path.t
                        and type 'a t = 'a Map.Make(Path).t

type type_descriptions = constructor_description list
type t

val empty : t
val initial_safe_string : t
val initial_unsafe_string : t

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
val find_module: Path.t -> t -> module_declaration
val find_modtype: Path.t -> t -> modtype_declaration

val find_type_expansion:
  Path.t -> t -> type_expr list * type_expr * int option
val find_type_expansion_opt:
  Path.t -> t -> type_expr list * type_expr * int option
val normalize_path: Location.t option -> t -> Path.t -> Path.t
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
  ?mark:bool ->
  Identifier.t -> t -> (constructor_description * (unit -> unit)) list
val lookup_module:
  load:bool -> ?loc:Location.t -> ?mark:bool -> Identifier.t -> t -> Path.t
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
val add_module: ?arg:bool -> Ident.t -> module_type -> t -> t
val add_module_declaration: ?arg:bool -> check:bool -> Ident.t ->
  module_declaration -> t -> t
val add_modtype: Ident.t -> modtype_declaration -> t -> t
val add_local_constraint: Path.t -> type_declaration -> int -> t -> t
val add_local_type: Path.t -> type_declaration -> t -> t

(* Insertion of all fields of a signature. *)

val add_item: signature_item -> t -> t
val add_signature: signature -> t -> t

(* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. Returns None if the path refers to a functor,
   not a structure. *)
val open_signature:
    ?used_slot:bool ref ->
    ?loc:Location.t -> ?toplevel:bool -> Path.t ->
      t -> t option

(* By-name insertions *)
val enter_value: string -> value_description -> t -> Ident.t * t
(** Adds a value identifier with the given name and description.
    The new environment and a generated identifier are returned. *)
val enter_type: string -> type_declaration -> t -> Ident.t * t
(** Adds a type identifier with the given name and declaration.
    The new environment and a generated identifier are returned. *)
val enter_constructor: string -> constructor_description -> t -> Ident.t * t
(** Adds a constructor with the given name and description.
    The new environment and a generated identifier are returned. *)

(* Forward declaration to break mutual recursion with Ctype. *)
val same_constr: (t -> type_expr -> type_expr -> bool) ref

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
  val load : (unit_name:string -> t option) ref
end
