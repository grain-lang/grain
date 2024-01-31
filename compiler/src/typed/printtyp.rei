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

/* Printing functions */

open Grain_parsing;
open Format;
open Types;
open Outcometree;

let identifier: (formatter, Identifier.t) => unit;
let ident: (formatter, Ident.t) => unit;
let tree_of_path: Path.t => out_ident;
let path: (formatter, Path.t) => unit;
let string_of_path: Path.t => string;
let raw_type_expr: (formatter, type_expr) => unit;

let wrap_printing_env: (~error: bool, Env.t, unit => 'a) => 'a;
/* Call the function using the environment for type path shortening */
/* This affects all the printing functions below */

let reset: unit => unit;
let mark_loops: type_expr => unit;
let reset_and_mark_loops: type_expr => unit;
let reset_and_mark_loops_list: list(type_expr) => unit;
let type_expr: (formatter, type_expr) => unit;
let constructor_arguments: (formatter, constructor_arguments) => unit;
let tree_of_type_scheme: type_expr => out_type;
// The `type_sch` functions avoid resetting the marked variables
let type_sch: (formatter, type_expr) => unit;
let string_of_type_sch: type_expr => string;
// The `type_scheme` functions reset the marked variables
let type_scheme: (formatter, type_expr) => unit;
let string_of_type_scheme: type_expr => string;
/* Maxence */
let reset_names: unit => unit;
let type_scheme_max: (~b_reset_names: bool=?, formatter, type_expr) => unit;
/* End Maxence */
let tree_of_value_description: (Ident.t, value_description) => out_sig_item;
let value_description: (Ident.t, formatter, value_description) => unit;
let string_of_value_description:
  (~ident: Ident.t, value_description) => string;
let tree_of_type_declaration:
  (Ident.t, type_declaration, rec_status) => out_sig_item;
let type_declaration: (Ident.t, formatter, type_declaration) => unit;
let string_of_type_declaration: (~ident: Ident.t, type_declaration) => string;
let string_of_constructor: constructor_declaration => string;
let extension_constructor: (Ident.t, formatter, extension_constructor) => unit;
let string_of_extension_constructor:
  (~ident: Ident.t, extension_constructor) => string;
let tree_of_module:
  (Ident.t, ~ellipsis: bool=?, module_type, rec_status) => out_sig_item;
let modtype: (formatter, module_type) => unit;
let signature: (formatter, signature) => unit;
let tree_of_modtype_declaration:
  (Ident.t, modtype_declaration) => out_sig_item;
let tree_of_signature: Types.signature => list(out_sig_item);
let tree_of_typexp: (bool, type_expr) => out_type;
let modtype_declaration: (Ident.t, formatter, modtype_declaration) => unit;
let type_expansion: (type_expr, Format.formatter, type_expr) => unit;
let prepare_expansion: ((type_expr, type_expr)) => (type_expr, type_expr);
let trace:
  (bool, bool, string, formatter, list((type_expr, type_expr))) => unit;
let report_unification_error:
  (
    formatter,
    Env.t,
    ~unif: bool=?,
    list((type_expr, type_expr)),
    ~type_expected_explanation: formatter => unit=?,
    formatter => unit,
    formatter => unit
  ) =>
  unit;
let report_ambiguous_type_error:
  (
    formatter,
    Env.t,
    (Path.t, Path.t),
    list((Path.t, Path.t)),
    formatter => unit,
    formatter => unit,
    formatter => unit
  ) =>
  unit;

/* for toploop */
let print_items:
  ((Env.t, signature_item) => option('a), Env.t, list(signature_item)) =>
  list((out_sig_item, option('a)));
