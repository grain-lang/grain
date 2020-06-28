/* Modified version of typing/datarepr.ml from OCaml. The original copyright is reproduced below. */
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

/* Compute constructor and label descriptions from type declarations,
   determining their representation. */

open Types;

let constructors_of_type:
  (Path.t, type_declaration) => list((Ident.t, constructor_description));

let labels_of_type:
  (Path.t, type_declaration) => list((Ident.t, label_description));

exception Constr_not_found;

let find_constr_by_tag:
  (constructor_tag, list(constructor_declaration)) => constructor_declaration;

/** Takes [cd_args] and [cd_res] from a [constructor_declaration] and
    returns:
    - the types of the constructor's arguments
    - the existential variables introduced by the constructor
 */

let constructor_existentials:
  (constructor_arguments, option(type_expr)) =>
  (list(type_expr), list(type_expr));
