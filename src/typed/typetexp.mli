(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Typechecking of type expressions for the core language *)

open Grain_parsing
open Types

val transl_simple_type:
        Env.t -> bool -> Parsetree.parsed_type -> Typedtree.core_type
val transl_simple_type_univars:
        Env.t -> Parsetree.parsed_type -> Typedtree.core_type
val transl_simple_type_delayed:
        Env.t -> Parsetree.parsed_type -> Typedtree.core_type * (unit -> unit)
        (* Translate a type, but leave type variables unbound. Returns
           the type and a function that binds the type variable. *)
val transl_type_scheme:
        Env.t -> Parsetree.parsed_type -> Typedtree.core_type
val reset_type_variables: unit -> unit
val type_variable: Location.t -> string -> type_expr
val transl_type_param:
  Env.t -> Parsetree.parsed_type -> Typedtree.core_type

type variable_context
val narrow: unit -> variable_context
val widen: variable_context -> unit

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Identifier.t
  | Unbound_type_constructor_2 of Path.t
  | Type_arity_mismatch of Identifier.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Identifier.t
  | Type_mismatch of (type_expr * type_expr) list
  | Alias_type_mismatch of (type_expr * type_expr) list
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * type_expr
  | Multiple_constraints_on_type of Identifier.t
  | Method_mismatch of string * type_expr * type_expr
  | Unbound_value of Identifier.t
  | Unbound_constructor of Identifier.t
  | Unbound_label of Identifier.t
  | Unbound_module of Identifier.t
  | Unbound_class of Identifier.t
  | Unbound_modtype of Identifier.t
  | Unbound_cltype of Identifier.t
  | Ill_typed_functor_application
      of Identifier.t * Identifier.t * Includemod.error list option
  | Illegal_reference_to_recursive_module
  | Wrong_use_of_module of Identifier.t * [ `Structure_used_as_functor
                                         | `Abstract_used_as_functor
                                         | `Functor_used_as_structure
                                         | `Abstract_used_as_structure
                                         | `Generative_used_as_applicative
                                         ]
  | Cannot_scrape_alias of Identifier.t * Path.t
  | Opened_object of Path.t option
  | Not_an_object of type_expr

exception Error of Location.t * Env.t * error

val report_error: Env.t -> Format.formatter -> error -> unit

(* Support for first-class modules. *)
val transl_modtype_identifier:  (* from Typemod *)
    (Location.t -> Env.t -> Identifier.t -> Path.t) ref

val find_type:
    Env.t -> Location.t -> Identifier.t -> Path.t * type_declaration
val find_constructor:
    Env.t -> Location.t -> Identifier.t -> constructor_description
val find_all_constructors:
    Env.t -> Location.t -> Identifier.t ->
    (constructor_description * (unit -> unit)) list
val find_value:
    Env.t -> Location.t -> Identifier.t -> Path.t * value_description
val find_module:
    Env.t -> Location.t -> Identifier.t -> Path.t * module_declaration
val lookup_module:
    ?load:bool -> Env.t -> Location.t -> Identifier.t -> Path.t
val find_modtype:
    Env.t -> Location.t -> Identifier.t -> Path.t * modtype_declaration

val unbound_constructor_error: Env.t -> Identifier.t Location.loc -> 'a
