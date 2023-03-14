/* Modified version of OCaml's typing/ctype.mli module */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operations on core types */

open Grain_parsing;
open Asttypes;
open Types;

exception Unify(list((type_expr, type_expr)));
exception
  Subtype(list((type_expr, type_expr)), list((type_expr, type_expr)));
exception Cannot_expand;
exception Cannot_apply;
exception Recursive_abbrev;
exception Unification_recursive_abbrev(list((type_expr, type_expr)));

let init_def: int => unit;
/* Set the initial variable level */
let begin_def: unit => unit;
/* Raise the variable level by one at the beginning of a definition. */
let end_def: unit => unit;
/* Lower the variable level by one at the end of a definition */
let begin_class_def: unit => unit;
let raise_nongen_level: unit => unit;
let reset_global_level: unit => unit;
/* Reset the global level before typing an expression */
let increase_global_level: unit => int;
let restore_global_level: int => unit;
/* This pair of functions is only used in Typetexp */
type levels = {
  current_level: int,
  nongen_level: int,
  global_level: int,
  saved_level: list((int, int)),
};
let save_levels: unit => levels;
let set_levels: levels => unit;
let reset_levels: unit => unit;

let newty: type_desc => type_expr;
let newvar: (~name: string=?, unit) => type_expr;
let newvar2: (~name: string=?, int) => type_expr;
/* Return a fresh variable */
let new_global_var: (~name: string=?, unit) => type_expr;
/* Return a fresh variable, bound at toplevel
   (as type variables ['a] in type constraints). */
/*val newobj: type_expr -> type_expr*/
let newconstr: (Path.t, list(type_expr)) => type_expr;
let none: type_expr;
/* A dummy type expression */

let repr: type_expr => type_expr;
/* Return the canonical representative of a type. */

/*val object_fields: type_expr -> type_expr*/
/*val opened_object: type_expr -> bool*/
/*val close_object: type_expr -> unit*/
/*val row_variable: type_expr -> type_expr*/
/* Return the row variable of an open object type */
/*val set_object_name:
    Ident.t -> type_expr -> type_expr list -> type_expr -> unit
  val remove_object_name: type_expr -> unit
  val hide_private_methods: type_expr -> unit
  val find_cltype_for_path: Env.t -> Path.t -> type_declaration * type_expr*/
let lid_of_path: (~hash: string=?, Path.t) => Identifier.t;

let generalize: type_expr => unit;
/* Generalize in-place the given type */
let generalize_expansive: (Env.t, type_expr) => unit;
/* Generalize the covariant part of a type, making
   contravariant branches non-generalizable */
let generalize_global: type_expr => unit;
/* Generalize the structure of a type, lowering variables
   to !global_level */
let generalize_structure: type_expr => unit;
/* Same, but variables are only lowered to !current_level */
let generalize_spine: type_expr => unit;
/* Special function to generalize a method during inference */
let correct_levels: type_expr => type_expr;
/* Returns a copy with decreasing levels */
let limited_generalize: (type_expr, type_expr) => unit;
/* Only generalize some part of the type
   Make the remaining of the type non-generalizable */

let instance: (~partial: bool=?, Env.t, type_expr) => type_expr;
/* Take an instance of a type scheme */
/* partial=None  -> normal
   partial=false -> newvar() for non generic subterms
   partial=true  -> newty2 ty.level Tvar for non generic subterms */
let instance_def: type_expr => type_expr;
/* use defaults */
let generic_instance: (Env.t, type_expr) => type_expr;
/* Same as instance, but new nodes at generic_level */
let instance_list: (Env.t, list(type_expr)) => list(type_expr);
/* Take an instance of a list of type schemes */
let instance_constructor:
  (~in_pattern: (ref(Env.t), int)=?, constructor_description) =>
  (list(type_expr), type_expr);
/* Same, for a constructor */
let instance_parameterized_type:
  (~keep_names: bool=?, list(type_expr), type_expr) =>
  (list(type_expr), type_expr);
let instance_parameterized_type_2:
  (list(type_expr), list(type_expr), type_expr) =>
  (list(type_expr), list(type_expr), type_expr);
let instance_declaration: type_declaration => type_declaration;

let instance_poly:
  (~keep_names: bool=?, bool, list(type_expr), type_expr) =>
  (list(type_expr), type_expr);
/* Take an instance of a type scheme containing free univars */

let apply: (Env.t, list(type_expr), type_expr, list(type_expr)) => type_expr;
/* [apply [p1...pN] t [a1...aN]] match the arguments [ai] to
   the parameters [pi] and returns the corresponding instance of
   [t]. Exception [Cannot_apply] is raised in case of failure. */

let expand_head_once: (Env.t, type_expr) => type_expr;
let expand_head: (Env.t, type_expr) => type_expr;
let try_expand_once_opt: (Env.t, type_expr) => type_expr;
/** The compiler's own version of [expand_head] necessary for type-based
    optimisations. */

let expand_head_opt: (Env.t, type_expr) => type_expr;

let full_expand: (Env.t, type_expr) => type_expr;
let extract_concrete_typedecl:
  (Env.t, type_expr) => (Path.t, Path.t, type_declaration);
/* Return the original path of the types, and the first concrete
   type declaration found expanding it.
   Raise [Not_found] if none appears or not a type constructor. */

let enforce_constraints: (Env.t, type_expr) => unit;
let instance_label:
  (bool, label_description) => (list(type_expr), type_expr, type_expr);

let unify: (Env.t, type_expr, type_expr) => unit;
/* Unify the two types given. Raise [Unify] if not possible. */
let unify_gadt:
  (~newtype_level: int, ref(Env.t), type_expr, type_expr) => unit;
/* Unify the two types given and update the environment with the
   local constraints. Raise [Unify] if not possible. */
let unify_var: (Env.t, type_expr, type_expr) => unit;
/* Same as [unify], but allow free univars when first type
   is a variable. */
let with_passive_variants: ('a => 'b, 'a) => 'b;
/* Call [f] in passive_variants mode, for exhaustiveness check. */

type filter_arrow_failure =
  | Unification_error(list((type_expr, type_expr)))
  | Label_mismatch({
      got: argument_label,
      expected: argument_label,
      expected_type: type_expr,
    })
  | Arity_mismatch
  | Not_a_function;

exception Filter_arrow_failed(filter_arrow_failure);

let filter_arrow:
  (Env.t, type_expr, list(argument_label)) => (list(type_expr), type_expr);
/* A special case of unification (with l:'a -> 'b). */
let occur_in: (Env.t, type_expr, type_expr) => bool;
let deep_occur: (type_expr, type_expr) => bool;
let moregeneral: (Env.t, bool, type_expr, type_expr) => bool;
/* Check if the first type scheme is more general than the second. */

let rigidify: type_expr => list(type_expr);
/* "Rigidify" a type and return its type variable */
let all_distinct_vars: (Env.t, list(type_expr)) => bool;
/* Check those types are all distinct type variables */
let matches: (Env.t, type_expr, type_expr) => bool;
/* Same as [moregeneral false], implemented using the two above
   functions and backtracking. Ignore levels */
let reify_univars: Types.type_expr => Types.type_expr;
/* Replaces all the variables of a type by a univar. */

let equal: (Env.t, bool, list(type_expr), list(type_expr)) => bool;
/* [equal env [x1...xn] tau [y1...yn] sigma]
   checks whether the parameterized types
   [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. */

/*val enlarge_type: Env.t -> type_expr -> type_expr * bool*/
/* Make a type larger, flag is true if some pruning had to be done */
/*val subtype: Env.t -> type_expr -> type_expr -> unit -> unit*/
/* [subtype env t1 t2] checks that [t1] is a subtype of [t2].
   It accumulates the constraints the type variables must
   enforce and returns a function that enforces this
   constraints. */

let nondep_type: (Env.t, Ident.t, type_expr) => type_expr;
/* Return a type equivalent to the given type but without
   references to the given module identifier. Raise [Not_found]
   if no such type exists. */
let nondep_type_decl:
  (Env.t, Ident.t, Ident.t, bool, type_declaration) => type_declaration;
/* Same for type declarations. */
let nondep_extension_constructor:
  (Env.t, Ident.t, extension_constructor) => extension_constructor;
/* Same for extension constructor */
let cyclic_abbrev: (Env.t, Ident.t, type_expr) => bool;
let is_contractive: (Env.t, Path.t) => bool;
let normalize_type: (Env.t, type_expr) => unit;

let closed_schema: (Env.t, type_expr) => bool;
/* Check whether the given type scheme contains no non-generic
   type variables */

let free_variables: (~env: Env.t=?, type_expr) => list(type_expr);
/* If env present, then check for incomplete definitions too */
let closed_type_decl: type_declaration => option(type_expr);
/*val closed_extension_constructor: extension_constructor -> type_expr option*/

/* Check whether all type variables are bound */

let unalias: type_expr => type_expr;
let arity: type_expr => int;
/* Return the arity (as for curried functions) of the given type. */

let collapse_conj_params: (Env.t, list(type_expr)) => unit;
/* Collapse conjunctive types in class parameters */

let get_current_level: unit => int;
/*val wrap_trace_gadt_instances: Env.t -> ('a -> 'b) -> 'a -> 'b*/
let reset_reified_var_counter: unit => unit;

let maybe_pointer_type: (Env.t, type_expr) => bool;
/* True if type is possibly pointer, false if definitely not a pointer */

/* Stubs */
let package_subtype:
  ref(
    (
      Env.t,
      Path.t,
      list(Identifier.t),
      list(type_expr),
      Path.t,
      list(Identifier.t),
      list(type_expr)
    ) =>
    bool,
  );

let mcomp: (Env.t, type_expr, type_expr) => unit;
