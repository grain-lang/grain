(* This file largely borrows from OCaml's utils/warnings.mli file. The original copyright is reproduced below: *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type t =
  | LetRecNonFunction of string

val is_active : t -> bool;;
val is_error : t -> bool;;


type reporting_information = {
  number : int;
  message : string;
  is_error : bool;
  sub_locs : (loc * string) list;
}

val report : t -> [ `Active of reporting_information | `Inactive ]

exception Errors;;

val check_fatal : unit -> unit;;
val reset_fatal: unit -> unit

type state
val backup: unit -> state
val restore: state -> unit
