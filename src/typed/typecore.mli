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

(* This variant is used to print improved error messages, and does not affect
   the behavior of the typechecker itself.

   It describes possible explanation for types enforced by a keyword of the
   language; e.g. "if" requires the condition to be of type bool, and the
   then-branch to be of type unit if there is no else branch; "for" requires
   indices to be of type int, and the body to be of type unit.
*)
type type_forcing_context =
  | If_conditional
  | If_no_else_branch
  | While_loop_conditional
  | While_loop_body
  | For_loop_start_index
  | For_loop_stop_index
  | For_loop_body
  | Assert_condition
  | Sequence_left_hand_side

(* The combination of a type and a "type forcing context". The intent is that it
   describes a type that is "expected" (required) by the context. If unifying
   with such a type fails, then the "explanation" field explains why it was
   required, in order to display a more enlightening error message.
*)
type type_expected = private {
  ty: type_expr;
  explanation: type_forcing_context option;
}

val mk_expected:
  ?explanation:type_forcing_context ->
  type_expr ->
  type_expected

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
val type_statement:
  Env.t -> Parsetree.toplevel_stmt -> Typedtree.toplevel_stmt
val check_partial:
        ?lev:int -> Env.t -> type_expr ->
        Location.t -> Typedtree.match_branch list -> Typedtree.partial
val type_expect:
        ?in_function:(Location.t * type_expr) ->
        Env.t -> Parsetree.expression -> type_expected -> Typedtree.expression
val type_exp:
        Env.t -> Parsetree.expression -> Typedtree.expression
val type_approx:
        Env.t -> Parsetree.expression -> type_expr
val type_argument:
        Env.t -> Parsetree.expression ->
        type_expr -> type_expr -> Typedtree.expression

val option_some: Typedtree.expression -> Typedtree.expression
val option_none: type_expr -> Location.t -> Typedtree.expression
val extract_option_type: Env.t -> type_expr -> type_expr
val iter_pattern: (Typedtree.pattern -> unit) -> Typedtree.pattern -> unit
val generalizable: int -> type_expr -> bool
val reset_delayed_checks: unit -> unit
val force_delayed_checks: unit -> unit

val name_pattern : string -> Typedtree.match_branch list -> Ident.t

val self_coercion : (Path.t * Location.t list ref) list ref

type error =
    Polymorphic_label of Identifier.t
  | Constructor_arity_mismatch of Identifier.t * int * int
  | Label_mismatch of Identifier.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Or_pattern_type_clash of Ident.t * (type_expr * type_expr) list
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of (type_expr * type_expr) list * type_forcing_context option
  | Apply_non_function of type_expr
  | Label_multiply_defined of string
  | Label_missing of Ident.t list
  | Label_not_mutable of Identifier.t
  | Wrong_name of string * type_expected * string * Path.t * string * string list
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
  | Too_many_arguments of bool * type_expr * type_forcing_context option
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
  | Unrefuted_pattern of Typedtree.pattern
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

val report_error: Env.t -> formatter -> error -> unit
 (* Deprecated.  Use Location.{error_of_exn, report_error}. *)

(* Forward declaration, to be filled in by Typemod.type_open *)
val type_open:
  (?used_slot:bool ref -> Env.t -> Location.t ->
   Identifier.t loc -> Path.t * Env.t)
    ref

val constant: Parsetree.constant -> (Asttypes.constant, error) result

val check_recursive_bindings : Env.t -> Typedtree.value_binding list -> unit
