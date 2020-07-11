/* Modified from OCaml's source. Original copyright below. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Grain_parsing;
open Asttypes;
open Typedtree;

module type IteratorArgument = {
  let enter_typed_program: typed_program => unit;

  let enter_pattern: pattern => unit;
  let enter_expression: expression => unit;
  let enter_core_type: core_type => unit;
  let enter_toplevel_stmt: toplevel_stmt => unit;

  let leave_typed_program: typed_program => unit;
  let leave_pattern: pattern => unit;
  let leave_expression: expression => unit;
  let leave_core_type: core_type => unit;
  let leave_toplevel_stmt: toplevel_stmt => unit;

  let enter_bindings: (export_flag, rec_flag, mut_flag) => unit;
  let enter_binding: value_binding => unit;
  let leave_binding: value_binding => unit;
  let leave_bindings: (export_flag, rec_flag, mut_flag) => unit;

  let enter_data_declarations: unit => unit;
  let enter_data_declaration: data_declaration => unit;
  let leave_data_declaration: data_declaration => unit;
  let leave_data_declarations: unit => unit;
};

module MakeIterator:
  (Iter: IteratorArgument) =>
   {
    let iter_typed_program: typed_program => unit;
    let iter_toplevel_stmt: toplevel_stmt => unit;
    let iter_expression: expression => unit;
    let iter_pattern: pattern => unit;
  };

module DefaultIteratorArgument: IteratorArgument;
