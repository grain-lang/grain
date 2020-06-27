/* Modified from OCaml's source. Original copyright below. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                   Fabrice Le Fessant, INRIA Saclay                     */
/*                                                                        */
/*   Copyright 2012 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Typedtree;

module type MapArgument = {
  let enter_typed_program: typed_program => typed_program;
  let enter_data_declaration: data_declaration => data_declaration;

  let enter_pattern: pattern => pattern;
  let enter_expression: expression => expression;

  let enter_core_type: core_type => core_type;
  let enter_toplevel_stmt: toplevel_stmt => toplevel_stmt;

  let leave_typed_program: typed_program => typed_program;
  let leave_data_declaration: data_declaration => data_declaration;
  let leave_pattern: pattern => pattern;
  let leave_expression: expression => expression;

  let leave_core_type: core_type => core_type;
  let leave_toplevel_stmt: toplevel_stmt => toplevel_stmt;
};

module MakeMap:
  (Iter: MapArgument) =>
   {
    let map_typed_program: typed_program => typed_program;
    let map_pattern: pattern => pattern;
    let map_toplevel_stmt: toplevel_stmt => toplevel_stmt;
    let map_expression: expression => expression;
  };

module DefaultMapArgument: MapArgument;
