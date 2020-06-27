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

/* Association tables from any ordered type to any type.
   We use the generic ordering to compare keys. */

type t('k, 'v);

let empty: t('k, 'v);
let add: ('k, 'v, t('k, 'v)) => t('k, 'v);
let find: ('k, t('k, 'v)) => 'v;
let find_str: (string, t(string, 'v)) => 'v;
let mem: ('k, t('k, 'v)) => bool;
let remove: ('k, t('k, 'v)) => t('k, 'v);
let iter: (('k, 'v) => unit, t('k, 'v)) => unit;
let map: (('k, 'v1) => 'v2, t('k, 'v1)) => t('k, 'v2);
let fold: (('k, 'v, 'acc) => 'acc, t('k, 'v), 'acc) => 'acc;

open Format;

let print:
  ((formatter, 'k) => unit, (formatter, 'v) => unit, formatter, t('k, 'v)) =>
  unit;
