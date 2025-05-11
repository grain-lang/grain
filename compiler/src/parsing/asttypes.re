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
type bigint_data = {
  bigint_negative: bool,
  bigint_limbs: array(int64),
  bigint_rep: string,
};

[@deriving (sexp, yojson)]
type rational_data = {
  rational_negative: bool,
  rational_num_limbs: array(int64),
  rational_den_limbs: array(int64),
  rational_num_rep: string,
  rational_den_rep: string,
};

[@deriving (sexp, yojson)]
type constant =
  | Const_number(number_type)
  | Const_bytes(bytes)
  | Const_string(string)
  | Const_char(string)
  | Const_int8(int32) // no built-in int8/16 types; just store in 32-bit integers
  | Const_int16(int32)
  | Const_int32(int32)
  | Const_int64(int64)
  | Const_uint8(int32)
  | Const_uint16(int32)
  | Const_uint32(int32)
  | Const_uint64(int64)
  | Const_float32(float)
  | Const_float64(float)
  | Const_wasmi32(int32)
  | Const_wasmi64(int64)
  | Const_wasmf32(float)
  | Const_wasmf64(float)
  | Const_bigint(bigint_data)
  | Const_rational(rational_data)
  | Const_bool(bool)
  | Const_void

[@deriving (sexp, yojson)]
and number_type =
  | Const_number_int(int64)
  | Const_number_float(float)
  | Const_number_rational(rational_data)
  | Const_number_bigint(bigint_data);

/** Marker for exported/nonexported let bindings */

[@deriving (sexp, yojson)]
type provide_flag =
  | NotProvided
  | Provided
  | Abstract;

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

let mkloc = Location.mkloc;
let mknoloc = Location.mknoloc;

/** Addtional expression information that may affect compilation. */
[@deriving (sexp, yojson)]
type attribute = {
  attr_name: loc(string),
  attr_args: list(loc(string)),
  attr_loc: Location.t,
};

[@deriving (sexp, yojson)]
type attributes = list(attribute);

[@deriving (sexp, yojson)]
type argument_label =
  | Unlabeled
  | Labeled(loc(string))
  | Default(loc(string));
