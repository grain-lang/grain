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

/* Type inference for the core language */

open Grain_parsing;
open Asttypes;
open Types;
open Format;

let is_nonexpansive: Typedtree.expression => bool;

let type_binding:
  (Env.t, rec_flag, mut_flag, list(Parsetree.value_binding), option('a)) =>
  (list(Typedtree.value_binding), Env.t);
let type_let:
  (Env.t, rec_flag, mut_flag, list(Parsetree.value_binding), option('a)) =>
  (list(Typedtree.value_binding), Env.t);
let type_expression: (Env.t, Parsetree.expression) => Typedtree.expression;
let type_statement_expr:
  (
    ~explanation: Checkertypes.type_forcing_context=?,
    Env.t,
    Parsetree.expression
  ) =>
  Typedtree.expression;
/*val check_partial:
  ?lev:int -> Env.t -> type_expr ->
  Location.t -> Typedtree.match_branch list -> Typedtree.partial*/
let type_expect:
  (
    ~in_function: (Location.t, type_expr)=?,
    Env.t,
    Parsetree.expression,
    Checkertypes.type_expected
  ) =>
  Typedtree.expression;
let type_exp: (Env.t, Parsetree.expression) => Typedtree.expression;
let type_approx: (Env.t, Parsetree.expression) => type_expr;
let type_arguments:
  (Env.t, list(Parsetree.expression), list(type_expr), list(type_expr)) =>
  list(Typedtree.expression);

let generalizable: (int, type_expr) => bool;

let name_pattern: (string, list(Typedtree.match_branch)) => Ident.t;

type error =
  | Arity_mismatch(type_expr, int)
  | Polymorphic_label(Identifier.t)
  | Constructor_arity_mismatch(Identifier.t, int, int)
  | Label_mismatch(Identifier.t, list((type_expr, type_expr)))
  | Pattern_type_clash(list((type_expr, type_expr)))
  | Or_pattern_type_clash(Ident.t, list((type_expr, type_expr)))
  | Multiply_bound_variable(string)
  | Orpat_vars(Ident.t, list(Ident.t))
  | Expr_type_clash(
      list((type_expr, type_expr)),
      option(Checkertypes.type_forcing_context),
    )
  | Apply_non_function(type_expr)
  | Label_multiply_defined(string)
  | Label_missing(list(Ident.t))
  | Label_not_mutable(Identifier.t)
  | Assign_not_mutable(Identifier.t)
  | Wrong_name(
      string,
      Checkertypes.type_expected,
      string,
      Path.t,
      string,
      list(string),
    )
  | Name_type_mismatch(
      string,
      Identifier.t,
      (Path.t, Path.t),
      list((Path.t, Path.t)),
    )
  | Invalid_format(string)
  | Undefined_method(type_expr, string, option(list(string)))
  | Undefined_inherited_method(string, list(string))
  | Virtual_class(Identifier.t)
  | Private_type(type_expr)
  | Private_label(Identifier.t, type_expr)
  | Unbound_instance_variable(string, list(string))
  | Instance_variable_not_mutable(bool, string)
  | Not_subtype(
      list((type_expr, type_expr)),
      list((type_expr, type_expr)),
    )
  | Outside_class
  | Value_multiply_overridden(string)
  | Coercion_failure(
      type_expr,
      type_expr,
      list((type_expr, type_expr)),
      bool,
    )
  | Too_many_arguments(
      bool,
      type_expr,
      option(Checkertypes.type_forcing_context),
    )
  | Scoping_let_module(string, type_expr)
  | Masked_instance_variable(Identifier.t)
  | Not_a_variant_type(Identifier.t)
  | Incoherent_label_order
  | Less_general(string, list((type_expr, type_expr)))
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module(type_expr)
  | Recursive_local_constraint(list((type_expr, type_expr)))
  | Unexpected_existential
  | Unqualified_gadt_pattern(Path.t, string)
  | Invalid_interval
  | Invalid_for_loop_index
  | No_value_clauses
  | Exception_pattern_below_toplevel
  | Inlined_record_escape
  | Inlined_record_expected
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Literal_overflow(string)
  | Unknown_literal(string, char)
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_class_expr
  | Unbound_value_missing_rec(Identifier.t, Location.t);

exception Error(Location.t, Env.t, error);
exception Error_forward(Location.error);

/* Forward declaration, to be filled in by Typemod.type_open */
/*val type_open:
  (?used_slot:bool ref -> Env.t -> Location.t ->
   Identifier.t loc -> Path.t * Env.t)
    ref*/

let constant:
  Parsetree.constant => result(Asttypes.constant, Checkertypes.error);

let check_recursive_bindings: (Env.t, list(Typedtree.value_binding)) => unit;
