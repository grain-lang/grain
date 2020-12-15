/* Modified from OCaml. The original copyright notice is reproduced below. */
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
open Sexplib.Conv;

let sexp_locs_disabled = _ => ! Grain_utils.Config.sexp_locs_enabled^;

/** Auxiliary AST types used by parsetree and typedtree. */;

[@deriving (sexp, yojson)]
type constant =
  | Const_number(number_type)
  | Const_string(string)
  | Const_char(string)
  | Const_int32(int32)
  | Const_int64(int64)
  | Const_float32(float)
  | Const_float64(float)
  | Const_bool(bool)
  | Const_void

[@deriving (sexp, yojson)]
and number_type =
  | Const_number_int(int64)
  | Const_number_float(float)
  | Const_number_rational(int32, int32);

/** Marker for exported/nonexported let bindings */

[@deriving (sexp, yojson)]
type export_flag =
  | Nonexported
  | Exported;

/** Marker for recursive/nonrecursive let bindings */

[@deriving (sexp, yojson)]
type rec_flag =
  | Nonrecursive
  | Recursive;

/** Marker for mutable/immutable let bindings */

[@deriving (sexp, yojson)]
type mut_flag =
  | Mutable
  | Immutable;

/** Marker for closed/open records */

[@deriving (sexp, yojson)]
type closed_flag =
  | Closed
  | Open;

/** A location-tagged value. */

[@deriving (sexp, yojson)]
type loc('a) =
  Location.loc('a) = {
    txt: 'a,
    [@sexp_drop_if sexp_locs_disabled]
    loc: Location.t,
  };
