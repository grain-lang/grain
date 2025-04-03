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

let prim0_type: Parsetree.prim0 => type_expr;
let prim1_type: Parsetree.prim1 => type_expr;
let prim2_type: Parsetree.prim2 => type_expr;
let primn_type: Parsetree.primn => type_expr;

let type_binding:
  (Env.t, rec_flag, mut_flag, list(Parsetree.value_binding), option('a)) =>
  (list(Typedtree.value_binding), Env.t);
let type_let:
  (
    Env.t,
    rec_flag,
    mut_flag,
    bool,
    list(Parsetree.value_binding),
    option('a)
  ) =>
  (list(Typedtree.value_binding), Env.t);
let type_expression: (Env.t, Parsetree.expression) => Typedtree.expression;
let type_statement_expr:
  (
    ~explanation: Checkertypes.type_forcing_context=?,
    ~in_function: (Location.t, list(type_expr), type_expr)=?,
    Env.t,
    Parsetree.expression
  ) =>
  Typedtree.expression;
/*val check_partial:
  ?lev:int -> Env.t -> type_expr ->
  Location.t -> Typedtree.match_branch list -> Typedtree.partial*/
let type_expect:
  (
    ~in_function: (Location.t, list(type_expr), type_expr)=?,
    Env.t,
    Parsetree.expression,
    Checkertypes.type_expected
  ) =>
  Typedtree.expression;
let type_exp: (Env.t, Parsetree.expression) => Typedtree.expression;
let type_approx: (Env.t, Parsetree.expression) => type_expr;

let generalizable: (int, type_expr) => bool;

let name_pattern: (string, list(Typedtree.match_branch)) => Ident.t;

type error =
  | Arity_mismatch(type_expr, option(Checkertypes.type_forcing_context))
  | Constructor_arity_mismatch(Identifier.t, int, int)
  | Label_mismatch(Identifier.t, list((type_expr, type_expr)))
  | Pattern_type_clash(list((type_expr, type_expr)))
  | Or_pattern_type_clash(Ident.t, list((type_expr, type_expr)))
  | Expr_type_clash(
      list((type_expr, type_expr)),
      option(Checkertypes.type_forcing_context),
    )
  | Apply_non_function(type_expr)
  | Apply_too_many_arguments(type_expr, list((argument_label, type_expr)))
  | Apply_too_few_arguments(list((argument_label, type_expr)))
  | Apply_unknown_label(string, list(string))
  | Label_multiply_defined(string)
  | Label_missing(list(Ident.t))
  | Label_not_mutable(Identifier.t)
  | Assign_not_mutable(Identifier.t)
  | Not_a_function(type_expr, option(Checkertypes.type_forcing_context))
  | Function_label_mismatch({
      got: argument_label,
      expected: argument_label,
      expected_type: type_expr,
      explanation: option(Checkertypes.type_forcing_context),
    })
  | Less_general(string, list((type_expr, type_expr)))
  | Unqualified_gadt_pattern(Path.t, string)
  | Inlined_record_escape
  | Inlined_record_misuse(Identifier.t, string, string)
  | Illegal_letrec_pat
  | Unbound_value_missing_rec(Identifier.t, Location.t);

exception Error(Location.t, Env.t, error);
exception Error_forward(Location.error);

/* Forward declaration, to be filled in by Typemod.type_open */
/*val type_open:
  (?used_slot:bool ref -> Env.t -> Location.t ->
   Identifier.t loc -> Path.t * Env.t)
    ref*/

let constant:
  (Location.t, Parsetree.constant) =>
  result(Asttypes.constant, Location.error);

let check_recursive_bindings: (Env.t, list(Typedtree.value_binding)) => unit;
