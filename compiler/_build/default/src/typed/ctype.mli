(* Modified version of OCaml's typing/ctype.mli module *)
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

(* Operations on core types *)

open Grain_parsing
open Asttypes
open Types

exception Unify of (type_expr * type_expr) list
exception Subtype of
    (type_expr * type_expr) list * (type_expr * type_expr) list
exception Cannot_expand
exception Cannot_apply
exception Recursive_abbrev
exception Unification_recursive_abbrev of (type_expr * type_expr) list

val init_def: int -> unit
(* Set the initial variable level *)
val begin_def: unit -> unit
(* Raise the variable level by one at the beginning of a definition. *)
val end_def: unit -> unit
(* Lower the variable level by one at the end of a definition *)
val begin_class_def: unit -> unit
val raise_nongen_level: unit -> unit
val reset_global_level: unit -> unit
(* Reset the global level before typing an expression *)
val increase_global_level: unit -> int
val restore_global_level: int -> unit
(* This pair of functions is only used in Typetexp *)
type levels =
  { current_level: int; nongen_level: int; global_level: int;
    saved_level: (int * int) list; }
val save_levels: unit -> levels
val set_levels: levels -> unit

val newty: type_desc -> type_expr
val newvar: ?name:string -> unit -> type_expr
val newvar2: ?name:string -> int -> type_expr
(* Return a fresh variable *)
val new_global_var: ?name:string -> unit -> type_expr
(* Return a fresh variable, bound at toplevel
   (as type variables ['a] in type constraints). *)
(*val newobj: type_expr -> type_expr*)
val newconstr: Path.t -> type_expr list -> type_expr
val none: type_expr
(* A dummy type expression *)

val repr: type_expr -> type_expr
(* Return the canonical representative of a type. *)

(*val object_fields: type_expr -> type_expr*)
(*val opened_object: type_expr -> bool*)
(*val close_object: type_expr -> unit*)
(*val row_variable: type_expr -> type_expr*)
(* Return the row variable of an open object type *)
(*val set_object_name:
  Ident.t -> type_expr -> type_expr list -> type_expr -> unit
val remove_object_name: type_expr -> unit
val hide_private_methods: type_expr -> unit
val find_cltype_for_path: Env.t -> Path.t -> type_declaration * type_expr*)
val lid_of_path: ?hash:string -> Path.t -> Identifier.t

val generalize: type_expr -> unit
(* Generalize in-place the given type *)
val generalize_expansive: Env.t -> type_expr -> unit
(* Generalize the covariant part of a type, making
   contravariant branches non-generalizable *)
val generalize_global: type_expr -> unit
(* Generalize the structure of a type, lowering variables
   to !global_level *)
val generalize_structure: type_expr -> unit
(* Same, but variables are only lowered to !current_level *)
val generalize_spine: type_expr -> unit
(* Special function to generalize a method during inference *)
val correct_levels: type_expr -> type_expr
(* Returns a copy with decreasing levels *)
val limited_generalize: type_expr -> type_expr -> unit
(* Only generalize some part of the type
   Make the remaining of the type non-generalizable *)

val instance: ?partial:bool -> Env.t -> type_expr -> type_expr
(* Take an instance of a type scheme *)
(* partial=None  -> normal
   partial=false -> newvar() for non generic subterms
   partial=true  -> newty2 ty.level Tvar for non generic subterms *)
val instance_def: type_expr -> type_expr
(* use defaults *)
val generic_instance: Env.t -> type_expr -> type_expr
(* Same as instance, but new nodes at generic_level *)
val instance_list: Env.t -> type_expr list -> type_expr list
(* Take an instance of a list of type schemes *)
val instance_constructor:
  ?in_pattern:Env.t ref * int ->
  constructor_description -> type_expr list * type_expr
(* Same, for a constructor *)
val instance_parameterized_type:
  ?keep_names:bool ->
  type_expr list -> type_expr -> type_expr list * type_expr
val instance_parameterized_type_2:
  type_expr list -> type_expr list -> type_expr ->
  type_expr list * type_expr list * type_expr
val instance_declaration: type_declaration -> type_declaration

val instance_poly:
  ?keep_names:bool ->
  bool -> type_expr list -> type_expr -> type_expr list * type_expr
(* Take an instance of a type scheme containing free univars *)

val apply:
  Env.t -> type_expr list -> type_expr -> type_expr list -> type_expr
(* [apply [p1...pN] t [a1...aN]] match the arguments [ai] to
   the parameters [pi] and returns the corresponding instance of
   [t]. Exception [Cannot_apply] is raised in case of failure. *)

val expand_head_once: Env.t -> type_expr -> type_expr
val expand_head: Env.t -> type_expr -> type_expr
val try_expand_once_opt: Env.t -> type_expr -> type_expr
val expand_head_opt: Env.t -> type_expr -> type_expr
(** The compiler's own version of [expand_head] necessary for type-based
    optimisations. *)

val full_expand: Env.t -> type_expr -> type_expr
val extract_concrete_typedecl:
  Env.t -> type_expr -> Path.t * Path.t * type_declaration
(* Return the original path of the types, and the first concrete
   type declaration found expanding it.
   Raise [Not_found] if none appears or not a type constructor. *)

val enforce_constraints: Env.t -> type_expr -> unit
val instance_label: bool -> label_description -> type_expr list * type_expr * type_expr

val unify: Env.t -> type_expr -> type_expr -> unit
(* Unify the two types given. Raise [Unify] if not possible. *)
val unify_gadt: newtype_level:int -> Env.t ref -> type_expr -> type_expr -> unit
(* Unify the two types given and update the environment with the
   local constraints. Raise [Unify] if not possible. *)
val unify_var: Env.t -> type_expr -> type_expr -> unit
(* Same as [unify], but allow free univars when first type
   is a variable. *)
val with_passive_variants: ('a -> 'b) -> ('a -> 'b)
(* Call [f] in passive_variants mode, for exhaustiveness check. *)
val filter_arrow: int -> Env.t -> type_expr -> type_expr list * type_expr
(* A special case of unification (with l:'a -> 'b). *)
val occur_in: Env.t -> type_expr -> type_expr -> bool
val deep_occur: type_expr -> type_expr -> bool
val moregeneral: Env.t -> bool -> type_expr -> type_expr -> bool
(* Check if the first type scheme is more general than the second. *)

val rigidify: type_expr -> type_expr list
(* "Rigidify" a type and return its type variable *)
val all_distinct_vars: Env.t -> type_expr list -> bool
(* Check those types are all distinct type variables *)
val matches: Env.t -> type_expr -> type_expr -> bool
(* Same as [moregeneral false], implemented using the two above
   functions and backtracking. Ignore levels *)
val reify_univars : Types.type_expr -> Types.type_expr
(* Replaces all the variables of a type by a univar. *)

val equal: Env.t -> bool -> type_expr list -> type_expr list -> bool
(* [equal env [x1...xn] tau [y1...yn] sigma]
   checks whether the parameterized types
   [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)

(*val enlarge_type: Env.t -> type_expr -> type_expr * bool*)
(* Make a type larger, flag is true if some pruning had to be done *)
(*val subtype: Env.t -> type_expr -> type_expr -> unit -> unit*)
(* [subtype env t1 t2] checks that [t1] is a subtype of [t2].
   It accumulates the constraints the type variables must
   enforce and returns a function that enforces this
   constraints. *)

val nondep_type: Env.t -> Ident.t -> type_expr -> type_expr
(* Return a type equivalent to the given type but without
   references to the given module identifier. Raise [Not_found]
   if no such type exists. *)
val nondep_type_decl:
  Env.t -> Ident.t -> Ident.t -> bool -> type_declaration ->
  type_declaration
(* Same for type declarations. *)
(*val nondep_extension_constructor:
  Env.t -> Ident.t -> extension_constructor ->
  extension_constructor*)
(* Same for extension constructor *)
val cyclic_abbrev: Env.t -> Ident.t -> type_expr -> bool
val is_contractive: Env.t -> Path.t -> bool
val normalize_type: Env.t -> type_expr -> unit

val closed_schema: Env.t -> type_expr -> bool
(* Check whether the given type scheme contains no non-generic
   type variables *)

val free_variables: ?env:Env.t -> type_expr -> type_expr list
(* If env present, then check for incomplete definitions too *)
val closed_type_decl: type_declaration -> type_expr option
(*val closed_extension_constructor: extension_constructor -> type_expr option*)

(* Check whether all type variables are bound *)

val unalias: type_expr -> type_expr
val arity: type_expr -> int
(* Return the arity (as for curried functions) of the given type. *)

val collapse_conj_params: Env.t -> type_expr list -> unit
(* Collapse conjunctive types in class parameters *)

val get_current_level: unit -> int
(*val wrap_trace_gadt_instances: Env.t -> ('a -> 'b) -> 'a -> 'b*)
val reset_reified_var_counter: unit -> unit

val maybe_pointer_type : Env.t -> type_expr -> bool
(* True if type is possibly pointer, false if definitely not a pointer *)

(* Stubs *)
val package_subtype :
  (Env.t -> Path.t -> Identifier.t list -> type_expr list ->
   Path.t -> Identifier.t list -> type_expr list -> bool) ref

val mcomp : Env.t -> type_expr -> type_expr -> unit
