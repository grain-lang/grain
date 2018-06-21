(* Modified from OCaml. The original copyright notice is reproduced below. *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open Sexplib.Conv

(** Auxiliary AST types used by parsetree and typedtree. *)

(* These are taken from OCaml. While not all fully supported,
   we will likely want to support them. *)
type constant =
  | Const_int of int
  | Const_string of string
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_bool of bool
[@@deriving sexp]

(** Marker for recursive/nonrecursive let bindings *)
type rec_flag = Nonrecursive | Recursive [@@deriving sexp]

(** A location-tagged value. *)
type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t [@sexp_drop_if fun _ -> not !Grain_utils.Config.sexp_locs_enabled];
} [@@deriving sexp]
