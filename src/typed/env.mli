open Grain_parsing
open Types

module PathMap : Map.S with type key = Path.t
                        and type 'a t = 'a Map.Make(Path).t

type type_descriptions = constructor_description list
type t

val empty : t

(* By-path lookups *)
val find_value: Path.t -> t -> value_description
val find_type: Path.t -> t -> type_declaration
val find_type_descrs: Path.t -> t -> type_descriptions

(* By-identifier lookups *)
val lookup_value: Identifier.t -> t -> Path.t * value_description
(** Looks up the value associated with the given identifier. *)
val lookup_type: Identifier.t -> t -> Path.t
(** Looks up the type associated with the given identifier. *)
val lookup_constructor: Identifier.t -> t -> Path.t * constructor_description
(** Looks up the constructor associated with the given identifier. *)

(* By-identifier insertions *)
val add_value: Ident.t -> value_description -> t -> t
(** Adds a value identifier with the given name and description. *)
val add_type: Ident.t -> type_declaration -> t -> t
(** Adds a type identifier with the given name and declaration. *)
val add_constructor: Ident.t -> constructor_description -> t -> t
(** Adds a constructor with the given name and description. *)

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

(* Analysis functions *)
(*val fold_values: (string -> Path.t -> value_description -> 'a -> 'a) -> Identifier.t option -> t -> 'a -> 'a
val fold_types: (string -> Path.t -> type_declaration * type_descriptions -> 'a -> 'a) -> Identifier.t option -> t -> 'a -> 'a
*)
