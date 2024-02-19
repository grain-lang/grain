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

/* Typing of type definitions and primitive definitions */

open Grain_parsing;
open Types;
open Format;

let transl_data_decl:
  (
    Env.t,
    Asttypes.rec_flag,
    list((Asttypes.provide_flag, Parsetree.data_declaration, Location.t))
  ) =>
  (list(Typedtree.data_declaration), Env.t);

let transl_value_decl:
  (Env.t, Location.t, Parsetree.value_description) =>
  (Typedtree.value_description, Env.t);

let transl_exception:
  (Env.t, Parsetree.extension_constructor) =>
  (Typedtree.extension_constructor, Env.t);
