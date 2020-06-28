/* Modified version of typing/btype.mli from the OCaml compiler. The original copyright notice is reproduced below. */
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

/* Basic operations on core types */

open Asttypes;
open Types;

/**** Sets, maps and hashtables of types ****/

module TypeSet: Set.S with type elt = type_expr;
module TypeMap: Map.S with type key = type_expr;
module TypeHash: Hashtbl.S with type key = type_expr;

/**** Levels ****/

let generic_level: int;

let newty2: (int, type_desc) => type_expr;
/* Create a type */
let newgenty: type_desc => type_expr;
/* Create a generic type */
let newgenvar: (~name: string=?, unit) => type_expr;
/* Return a fresh generic variable */

/**** Types ****/

let is_Tvar: type_expr => bool;
let is_Tunivar: type_expr => bool;
let is_Tconstr: type_expr => bool;

let repr: type_expr => type_expr;
/* Return the canonical representative of a type. */

let commu_repr: commutable => commutable;
/* Return the canonical representative of a commutation lock */

/**** Utilities for type traversal ****/

let iter_type_expr: (type_expr => unit, type_expr) => unit;
/* Iteration on types */
let iter_abbrev: (type_expr => unit, abbrev_memo) => unit;
/* Iteration on types in an abbreviation list */

type type_iterators = {
  it_signature: (type_iterators, signature) => unit,
  it_signature_item: (type_iterators, signature_item) => unit,
  it_value_description: (type_iterators, value_description) => unit,
  it_type_declaration: (type_iterators, type_declaration) => unit,
  it_module_declaration: (type_iterators, module_declaration) => unit,
  it_modtype_declaration: (type_iterators, modtype_declaration) => unit,
  it_module_type: (type_iterators, module_type) => unit,
  it_type_kind: (type_iterators, type_kind) => unit,
  it_do_type_expr: (type_iterators, type_expr) => unit,
  it_type_expr: (type_iterators, type_expr) => unit,
  it_path: Path.t => unit,
};
let type_iterators: type_iterators;
/* Iteration on arbitrary type information.
   [it_type_expr] calls [mark_type_node] to avoid loops. */
let unmark_iterators: type_iterators;
/* Unmark any structure containing types. See [unmark_type] below. */

let copy_type_desc:
  (~keep_names: bool=?, type_expr => type_expr, type_desc) => type_desc;
/* Copy on types */
let save_desc: (type_expr, type_desc) => unit;
/* Save a type description */
let cleanup_types: unit => unit;
/* Restore type descriptions */

let lowest_level: int;
/* Marked type: ty.level < lowest_level */
let pivot_level: int;
/* Type marking: ty.level <- pivot_level - ty.level */
let mark_type: type_expr => unit;
/* Mark a type */
let mark_type_node: type_expr => unit;
/* Mark a type node (but not its sons) */
let mark_type_params: type_expr => unit;
/* Mark the sons of a type node */
let unmark_type: type_expr => unit;
let unmark_type_decl: type_declaration => unit;

/**** Memorization of abbreviation expansion ****/

let find_expans: (private_flag, Path.t, abbrev_memo) => option(type_expr);
/* Look up a memorized abbreviation */
let cleanup_abbrev: unit => unit;
/* Flush the cache of abbreviation expansions.
   When some types are saved (using [output_value]), this
   function MUST be called just before. */
let memorize_abbrev:
  (ref(abbrev_memo), private_flag, Path.t, type_expr, type_expr) => unit;
/* Add an expansion in the cache */
let forget_abbrev: (ref(abbrev_memo), Path.t) => unit;
/* Remove an abbreviation from the cache */

/**** Utilities for backtracking ****/

type snapshot;
/* A snapshot for backtracking */
let snapshot: unit => snapshot;
/* Make a snapshot for later backtracking. Costs nothing */
let backtrack: snapshot => unit;
/* Backtrack to a given snapshot. Only possible if you have
   not already backtracked to a previous snapshot.
   Calls [cleanup_abbrev] internally */
let undo_compress: snapshot => unit;
/* Backtrack only path compression. Only meaningful if you have
   not already backtracked to a previous snapshot.
   Does not call [cleanup_abbrev] */

/* Functions to use when modifying a type (only Ctype?) */
let link_type: (type_expr, type_expr) => unit;
/* Set the desc field of [t1] to [Tlink t2], logging the old
   value if there is an active snapshot */
let set_level: (type_expr, int) => unit;
let set_name:
  (
    ref(option((Path.t, list(type_expr)))),
    option((Path.t, list(type_expr)))
  ) =>
  unit;
let set_univar: (ref(option(type_expr)), type_expr) => unit;
let set_commu: (ref(commutable), commutable) => unit;
let set_typeset: (ref(TypeSet.t), TypeSet.t) => unit;
/* Set references, logging the old value */
let log_type: type_expr => unit;
/* Log the old value of a type, before modifying it by hand */

/**** Forward declarations ****/
let print_raw: ref((Format.formatter, type_expr) => unit);

let iter_type_expr_kind: (type_expr => unit, type_kind) => unit;

let iter_type_expr_cstr_args:
  (type_expr => unit, constructor_arguments) => unit;
let map_type_expr_cstr_args:
  (type_expr => type_expr, constructor_arguments) => constructor_arguments;
