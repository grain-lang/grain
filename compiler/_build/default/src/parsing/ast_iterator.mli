(* This file is largely copied from OCaml's parsing/ast_iterator.mli.
   The original copyright notice is reproduced below. *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Nicolas Ojeda Bar, LexiFi                         *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Parsetree

type iterator = {
  constant: iterator -> constant -> unit;
  expr: iterator -> expression -> unit;
  pat: iterator -> pattern -> unit;
  typ: iterator -> parsed_type -> unit;
  data: iterator -> data_declaration -> unit;
  constructor: iterator -> constructor_declaration -> unit;
  label: iterator -> label_declaration -> unit;
  location: iterator -> Location.t -> unit;
  import: iterator -> import_declaration list -> unit;
  export: iterator -> export_declaration list -> unit;
  export_all: iterator -> export_except list -> unit;
  value_binding: iterator -> value_binding -> unit;
  match_branch: iterator -> match_branch -> unit;
  value_description: iterator -> value_description -> unit;
  toplevel: iterator -> toplevel_stmt -> unit;
}
(** A [iterator] record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the iterator to be applied to children in the syntax
    tree. *)

val default_iterator: iterator
(** A default iterator, which implements a "do not do anything" mapping. *)
