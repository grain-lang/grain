(* Modified version of typing/btype.mli from the OCaml compiler. The original copyright notice is reproduced below. *)
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

(* Basic operations on core types *)

open Asttypes
open Types

(**** Sets, maps and hashtables of types ****)

module TypeSet  : Set.S with type elt = type_expr
module TypeMap  : Map.S with type key = type_expr
module TypeHash : Hashtbl.S with type key = type_expr

(**** Levels ****)

val generic_level: int

val newty2: int -> type_desc -> type_expr
        (* Create a type *)
val newgenty: type_desc -> type_expr
        (* Create a generic type *)
val newgenvar: ?name:string -> unit -> type_expr
        (* Return a fresh generic variable *)

(**** Types ****)

val is_Tvar: type_expr -> bool
val is_Tunivar: type_expr -> bool
val is_Tconstr: type_expr -> bool

val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)

val commu_repr: commutable -> commutable
        (* Return the canonical representative of a commutation lock *)


(**** Utilities for type traversal ****)

val iter_type_expr: (type_expr -> unit) -> type_expr -> unit
        (* Iteration on types *)
val iter_abbrev: (type_expr -> unit) -> abbrev_memo -> unit
        (* Iteration on types in an abbreviation list *)

type type_iterators =
  { it_signature: type_iterators -> signature -> unit;
    it_signature_item: type_iterators -> signature_item -> unit;
    it_value_description: type_iterators -> value_description -> unit;
    it_type_declaration: type_iterators -> type_declaration -> unit;
    it_module_declaration: type_iterators -> module_declaration -> unit;
    it_modtype_declaration: type_iterators -> modtype_declaration -> unit;
    it_module_type: type_iterators -> module_type -> unit;
    it_type_kind: type_iterators -> type_kind -> unit;
    it_do_type_expr: type_iterators -> type_expr -> unit;
    it_type_expr: type_iterators -> type_expr -> unit;
    it_path: Path.t -> unit; }
val type_iterators: type_iterators
        (* Iteration on arbitrary type information.
           [it_type_expr] calls [mark_type_node] to avoid loops. *)
val unmark_iterators: type_iterators
        (* Unmark any structure containing types. See [unmark_type] below. *)

val copy_type_desc:
    ?keep_names:bool -> (type_expr -> type_expr) -> type_desc -> type_desc
        (* Copy on types *)
val save_desc: type_expr -> type_desc -> unit
        (* Save a type description *)
val cleanup_types: unit -> unit
        (* Restore type descriptions *)

val lowest_level: int
        (* Marked type: ty.level < lowest_level *)
val pivot_level: int
        (* Type marking: ty.level <- pivot_level - ty.level *)
val mark_type: type_expr -> unit
        (* Mark a type *)
val mark_type_node: type_expr -> unit
        (* Mark a type node (but not its sons) *)
val mark_type_params: type_expr -> unit
        (* Mark the sons of a type node *)
val unmark_type: type_expr -> unit
val unmark_type_decl: type_declaration -> unit

(**** Memorization of abbreviation expansion ****)

val find_expans: private_flag -> Path.t -> abbrev_memo -> type_expr option
        (* Look up a memorized abbreviation *)
val cleanup_abbrev: unit -> unit
        (* Flush the cache of abbreviation expansions.
           When some types are saved (using [output_value]), this
           function MUST be called just before. *)
val memorize_abbrev:
        abbrev_memo ref ->
        private_flag -> Path.t -> type_expr -> type_expr -> unit
        (* Add an expansion in the cache *)
val forget_abbrev:
        abbrev_memo ref -> Path.t -> unit
        (* Remove an abbreviation from the cache *)

(**** Utilities for backtracking ****)

type snapshot
        (* A snapshot for backtracking *)
val snapshot: unit -> snapshot
        (* Make a snapshot for later backtracking. Costs nothing *)
val backtrack: snapshot -> unit
        (* Backtrack to a given snapshot. Only possible if you have
           not already backtracked to a previous snapshot.
           Calls [cleanup_abbrev] internally *)
val undo_compress: snapshot -> unit
        (* Backtrack only path compression. Only meaningful if you have
           not already backtracked to a previous snapshot.
           Does not call [cleanup_abbrev] *)

(* Functions to use when modifying a type (only Ctype?) *)
val link_type: type_expr -> type_expr -> unit
        (* Set the desc field of [t1] to [Tlink t2], logging the old
           value if there is an active snapshot *)
val set_level: type_expr -> int -> unit
val set_name:
    (Path.t * type_expr list) option ref ->
    (Path.t * type_expr list) option -> unit
val set_univar: type_expr option ref -> type_expr -> unit
val set_commu: commutable ref -> commutable -> unit
val set_typeset: TypeSet.t ref -> TypeSet.t -> unit
        (* Set references, logging the old value *)
val log_type: type_expr -> unit
        (* Log the old value of a type, before modifying it by hand *)

(**** Forward declarations ****)
val print_raw: (Format.formatter -> type_expr -> unit) ref

val iter_type_expr_kind: (type_expr -> unit) -> (type_kind -> unit)

val iter_type_expr_cstr_args: (type_expr -> unit) ->
  (constructor_arguments -> unit)
val map_type_expr_cstr_args: (type_expr -> type_expr) ->
  (constructor_arguments -> constructor_arguments)
