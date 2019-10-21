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

(* Type inference for the core language *)

open Grain_parsing
open Asttypes
open Types
open Format

val is_nonexpansive: Typedtree.expression -> bool

val type_binding:
        Env.t -> rec_flag ->
          Parsetree.value_binding list ->
          'a option ->
          Typedtree.value_binding list * Env.t
val type_let:
        Env.t -> rec_flag ->
          Parsetree.value_binding list ->
          'a option ->
          Typedtree.value_binding list * Env.t
val type_expression:
  Env.t -> Parsetree.expression -> Typedtree.expression
val type_statement_expr:
  ?explanation:Checkertypes.type_forcing_context -> Env.t -> Parsetree.expression -> Typedtree.expression
(*val check_partial:
        ?lev:int -> Env.t -> type_expr ->
        Location.t -> Typedtree.match_branch list -> Typedtree.partial*)
val type_expect:
        ?in_function:(Location.t * type_expr) ->
        Env.t -> Parsetree.expression -> Checkertypes.type_expected -> Typedtree.expression
val type_exp:
        Env.t -> Parsetree.expression -> Typedtree.expression
val type_approx:
        Env.t -> Parsetree.expression -> type_expr
val type_arguments:
        Env.t -> Parsetree.expression list ->
        type_expr list -> type_expr list -> Typedtree.expression list

val extract_concrete_record:
        Env.t -> type_expr ->
        Path.t * Path.t * record_field list

val generalizable: int -> type_expr -> bool

val name_pattern : string -> Typedtree.match_branch list -> Ident.t

type error =
  | Arity_mismatch of type_expr * int
  | Polymorphic_label of Identifier.t
  | Constructor_arity_mismatch of Identifier.t * int * int
  | Label_mismatch of Identifier.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Or_pattern_type_clash of Ident.t * (type_expr * type_expr) list
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of (type_expr * type_expr) list * Checkertypes.type_forcing_context option
  | Apply_non_function of type_expr
  | Label_multiply_defined of string
  | Label_missing of Ident.t list
  | Label_not_mutable of Identifier.t
  | Wrong_name of string * Checkertypes.type_expected * string * Path.t * string * string list
  | Name_type_mismatch of
      string * Identifier.t * (Path.t * Path.t) * (Path.t * Path.t) list
  | Invalid_format of string
  | Undefined_method of type_expr * string * string list option
  | Undefined_inherited_method of string * string list
  | Virtual_class of Identifier.t
  | Private_type of type_expr
  | Private_label of Identifier.t * type_expr
  | Unbound_instance_variable of string * string list
  | Instance_variable_not_mutable of bool * string
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of
      type_expr * type_expr * (type_expr * type_expr) list * bool
  | Too_many_arguments of bool * type_expr * Checkertypes.type_forcing_context option
  | Scoping_let_module of string * type_expr
  | Masked_instance_variable of Identifier.t
  | Not_a_variant_type of Identifier.t
  | Incoherent_label_order
  | Less_general of string * (type_expr * type_expr) list
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module of type_expr
  | Recursive_local_constraint of (type_expr * type_expr) list
  | Unexpected_existential
  | Unqualified_gadt_pattern of Path.t * string
  | Invalid_interval
  | Invalid_for_loop_index
  | No_value_clauses
  | Exception_pattern_below_toplevel
  | Inlined_record_escape
  | Inlined_record_expected
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Literal_overflow of string
  | Unknown_literal of string * char
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_class_expr
  | Unbound_value_missing_rec of Identifier.t * Location.t

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

(* Forward declaration, to be filled in by Typemod.type_open *)
(*val type_open:
  (?used_slot:bool ref -> Env.t -> Location.t ->
   Identifier.t loc -> Path.t * Env.t)
    ref*)

val constant: Parsetree.constant -> (Asttypes.constant, Checkertypes.error) result

val check_recursive_bindings : Env.t -> Typedtree.value_binding list -> unit
