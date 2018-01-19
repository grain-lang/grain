(* This file is mostly copied from OCaml's parsing/location.ml.
   The original copyright notice is reproduced below. *)
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

open Lexing
(* NOTE: A lot of this file is taken from OCaml's parsing/location.ml.
   Perhaps we should just go ahead and copy the whole thing. *)
let absname = ref false

type t = {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool;
}

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let dummy_loc = {
  loc_start=dummy_pos;
  loc_end=dummy_pos;
  loc_ghost=true
}

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
}

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
}

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
}

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
}

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

open Format

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) =
  ("File \"", "\", line ", ", characters ", "-", ":")

(** Returns (file, line, char) *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

(* Currently a no-op *)
let setup_colors() = ()

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if is_relative s then concat (Sys.getcwd ()) s else s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file = if !absname then absolute_path file else file
let print_filename ppf file = fprintf ppf "%s" (show_filename file)

let print_loc ppf loc =
  setup_colors();
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  fprintf ppf "%s@{<loc>%a%s%i" msg_file print_filename file msg_line line;
  if startchar >= 0 then
    fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar;
  fprintf ppf "@}"

let default_printer ppf loc =
  setup_colors();
  fprintf ppf "@{<loc>%a@}%s@," print_loc loc msg_colon

let printer = ref default_printer
let print ppf loc = !printer ppf loc

let error_prefix = "Error"
let warning_prefix = "Warning"

let print_error_prefix ppf =
  setup_colors ();
  fprintf ppf "@{<error>%s@}" error_prefix

let print_compact ppf loc =
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  fprintf ppf "%a:%i" print_filename file line;
  if startchar >= 0 then fprintf ppf ",%i--%i" startchar endchar

let print_error ppf loc =
  fprintf ppf "%a%t:" print loc print_error_prefix

let print_error_cur_file ppf () = print_error ppf (in_file !input_name)

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt dummy_loc
