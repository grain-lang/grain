/* This file is largely copied from OCaml's parsing/ast_iterator.mli.
   The original copyright notice is reproduced below. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Nicolas Ojeda Bar, LexiFi                         */
/*                                                                        */
/*   Copyright 2012 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Parsetree;

module type IteratorArgument = {
  let enter_location: Location.t => unit;
  let leave_location: Location.t => unit;

  let enter_attribute: attribute => unit;
  let leave_attribute: attribute => unit;

  let enter_parsed_program: parsed_program => unit;
  let leave_parsed_program: parsed_program => unit;

  let enter_include: include_declaration => unit;
  let leave_include: include_declaration => unit;

  let enter_provide: list(provide_item) => unit;
  let leave_provide: list(provide_item) => unit;

  let enter_foreign: (provide_flag, value_description) => unit;
  let leave_foreign: (provide_flag, value_description) => unit;

  let enter_primitive: (provide_flag, value_description) => unit;
  let leave_primitive: (provide_flag, value_description) => unit;

  let enter_top_let:
    (provide_flag, rec_flag, mut_flag, list(value_binding)) => unit;
  let leave_top_let:
    (provide_flag, rec_flag, mut_flag, list(value_binding)) => unit;

  let enter_module: (provide_flag, module_declaration) => unit;
  let leave_module: (provide_flag, module_declaration) => unit;

  let enter_pattern: pattern => unit;
  let leave_pattern: pattern => unit;

  let enter_expression: expression => unit;
  let leave_expression: expression => unit;

  let enter_type: parsed_type => unit;
  let leave_type: parsed_type => unit;

  let enter_toplevel_stmt: toplevel_stmt => unit;
  let leave_toplevel_stmt: toplevel_stmt => unit;

  let enter_constant: constant => unit;
  let leave_constant: constant => unit;

  let enter_let: (rec_flag, mut_flag, list(value_binding)) => unit;
  let leave_let: (rec_flag, mut_flag, list(value_binding)) => unit;

  let enter_value_binding: value_binding => unit;
  let leave_value_binding: value_binding => unit;

  let enter_data_declarations:
    list((provide_flag, data_declaration)) => unit;
  let leave_data_declarations:
    list((provide_flag, data_declaration)) => unit;

  let enter_data_declaration: data_declaration => unit;
  let leave_data_declaration: data_declaration => unit;
};

module type Iterator = {
  let iter_parsed_program: parsed_program => unit;
  let iter_toplevel_stmt: toplevel_stmt => unit;
};

module MakeIterator: (Iter: IteratorArgument) => Iterator;

module DefaultIteratorArgument: IteratorArgument;
