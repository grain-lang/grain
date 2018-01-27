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

(* Values as patterns pretty printer *)
open Grain_parsing
open Asttypes
open Typedtree
open Types
open Format

let is_cons = function
| {cstr_name = "::"} -> true
| _ -> false

let pretty_const c = match c with
| Const_int i -> Printf.sprintf "%d" i
| Const_char c -> Printf.sprintf "%C" c
| Const_string (s, _) -> Printf.sprintf "%S" s
| Const_float f -> Printf.sprintf "%s" f
| Const_int32 i -> Printf.sprintf "%ldl" i
| Const_int64 i -> Printf.sprintf "%LdL" i
| Const_nativeint i -> Printf.sprintf "%ndn" i

let rec pretty_val ppf v =
  match v.pat_extra with
      (cstr, _loc) :: rem ->
        begin match cstr with
          | TPatConstraint _ ->
            fprintf ppf "@[(%a : _)@]" pretty_val { v with pat_extra = rem }
        end
    | [] ->
  match v.pat_desc with
  | TPatAny -> fprintf ppf "_"
  | TPatVar (x,_) -> fprintf ppf "%s" (Ident.name x)
  | TPatTuple vs ->
      fprintf ppf "@[(%a)@]" (pretty_vals ",") vs

and pretty_car ppf v = match v.pat_desc with
  | _ -> pretty_val ppf v

and pretty_cdr ppf v = match v.pat_desc with
  | _ -> pretty_val ppf v

and pretty_arg ppf v = match v.pat_desc with
  |  _ -> pretty_val ppf v

and pretty_or ppf v = match v.pat_desc with
  | _ -> pretty_val ppf v

and pretty_vals sep ppf = function
  | [] -> ()
  | [v] -> pretty_val ppf v
  | v::vs ->
      fprintf ppf "%a%s@ %a" pretty_val v sep (pretty_vals sep) vs

let top_pretty ppf v =
  fprintf ppf "@[%a@]@?" pretty_val v


let pretty_pat p =
  top_pretty Format.str_formatter p ;
  prerr_string (Format.flush_str_formatter ())

type matrix = pattern list list

let pretty_line fmt =
  List.iter (fun p ->
    Format.fprintf fmt " <";
    top_pretty fmt p;
    Format.fprintf fmt ">";
  )

let pretty_matrix fmt (pss : matrix) =
  Format.fprintf fmt "begin matrix\n" ;
  List.iter (fun ps ->
    pretty_line fmt ps ;
    Format.fprintf fmt "\n"
  ) pss;
  Format.fprintf fmt "end matrix\n%!"
