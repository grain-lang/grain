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

open Grain_parsing;

let pretty_const: Asttypes.constant => string;
let top_pretty: (Format.formatter, Typedtree.pattern) => unit;
let pretty_pat: Typedtree.pattern => unit;
let pretty_line: (Format.formatter, list(Typedtree.pattern)) => unit;
let pretty_matrix: (Format.formatter, list(list(Typedtree.pattern))) => unit;
