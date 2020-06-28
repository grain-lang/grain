/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                   Projet Cristal, INRIA Rocquencourt                   */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Format;
open Outcometree;

let out_value: ref((formatter, out_value) => unit);
let out_type: ref((formatter, out_type) => unit);
let out_module_type: ref((formatter, out_module_type) => unit);
let out_sig_item: ref((formatter, out_sig_item) => unit);
let out_signature: ref((formatter, list(out_sig_item)) => unit);
let out_phrase: ref((formatter, out_phrase) => unit);

let parenthesized_ident: string => bool;
