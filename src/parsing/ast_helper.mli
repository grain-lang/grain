(* This file is largely copied from OCaml's parsing/ast_helper.mli.
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

open Parsetree

type id = Identifier.t loc
type str = string loc
type loc = Location.t

val default_loc_src: (unit -> loc) ref
    (** Default value for all optional location arguments. *)

val with_default_loc: loc -> (unit -> 'a) -> 'a
    (** Set the [default_loc] within the scope of the execution
        of the provided function. *)

val with_default_loc_src: (unit -> loc) -> (unit -> 'a) -> 'a

module Const : sig
  val string : string -> constant
  val int : int -> constant
  val bool : bool -> constant
end

module Typ : sig
  val mk: ?loc:loc -> parsed_type_desc -> parsed_type
  val any: ?loc:loc -> unit -> parsed_type
  val var: ?loc:loc -> string -> parsed_type
  val arrow: ?loc:loc -> parsed_type list -> parsed_type -> parsed_type
  val tuple: ?loc:loc -> parsed_type list -> parsed_type
  val constr: ?loc:loc -> id -> parsed_type list -> parsed_type
  val poly: ?loc:loc -> str list -> parsed_type -> parsed_type
  val force_poly: parsed_type -> parsed_type
end

module CDecl : sig
  val mk: ?loc:loc -> str -> constructor_arguments -> constructor_declaration
  val singleton: ?loc:loc -> str -> constructor_declaration
  val tuple: ?loc:loc -> str -> parsed_type list -> constructor_declaration
end

module LDecl : sig
  val mk: ?loc:loc -> id -> parsed_type -> label_declaration
end

module Dat : sig
  val mk: ?loc:loc -> str -> parsed_type list -> data_kind -> data_declaration
  val variant: ?loc:loc -> str -> parsed_type list -> constructor_declaration list -> data_declaration
  val record: ?loc:loc -> str -> parsed_type list -> label_declaration list -> data_declaration
end

module Pat : sig
  val mk: ?loc:loc -> pattern_desc -> pattern
  val any: ?loc:loc -> unit -> pattern
  val var: ?loc:loc -> str -> pattern
  val tuple: ?loc:loc -> pattern list -> pattern
  val record: ?loc:loc -> ((id * pattern) option * Asttypes.closed_flag) list -> pattern
  val list: ?loc: loc -> pattern list -> pattern option -> pattern
  val constant: ?loc:loc -> constant -> pattern
  val constraint_: ?loc:loc -> pattern -> parsed_type -> pattern
  val construct: ?loc:loc -> id -> pattern list -> pattern
  val or_: ?loc:loc -> pattern -> pattern -> pattern
  val alias: ?loc:loc -> pattern -> str -> pattern
end

module Exp: sig
  val mk: ?loc:loc -> expression_desc -> expression
  val ident: ?loc:loc -> id -> expression
  val constant: ?loc:loc -> constant -> expression
  val tuple: ?loc:loc -> expression list -> expression
  val record: ?loc:loc -> (id * expression) list -> expression
  val record_get: ?loc:loc -> expression -> id -> expression
  val let_: ?loc:loc -> rec_flag -> value_binding list -> expression -> expression
  val match_: ?loc:loc -> expression -> match_branch list -> expression
  val prim1: ?loc:loc -> prim1 -> expression -> expression
  val prim2: ?loc:loc -> prim2 -> expression -> expression -> expression
  val if_: ?loc:loc -> expression -> expression -> expression -> expression
  val while_: ?loc:loc -> expression -> expression -> expression
  val constraint_: ?loc:loc -> expression -> parsed_type -> expression
  val assign: ?loc:loc -> expression -> expression -> expression
  val lambda: ?loc:loc -> pattern list -> expression -> expression
  val apply: ?loc:loc -> expression -> expression list -> expression
  val block: ?loc:loc -> expression list -> expression
  val null: ?loc:loc -> unit -> expression
  val list: ?loc:loc -> expression list -> expression option -> expression
  val ignore: expression -> expression
end

module Top: sig
  val mk: ?loc:loc -> toplevel_stmt_desc -> toplevel_stmt
  val import: ?loc:loc -> import_declaration list -> toplevel_stmt
  val foreign: ?loc:loc -> export_flag -> value_description -> toplevel_stmt
  val data: ?loc:loc -> export_flag -> data_declaration -> toplevel_stmt
  val let_: ?loc:loc -> export_flag -> rec_flag -> value_binding list -> toplevel_stmt
  val expr: ?loc:loc -> expression -> toplevel_stmt
  val export: ?loc:loc -> export_declaration list -> toplevel_stmt
  val export_all: ?loc:loc -> export_except list -> toplevel_stmt
end

module Val: sig
  val mk: ?loc:loc -> mod_:str -> name:str -> typ:parsed_type -> prim:string list -> value_description
end

module Vb: sig
  val mk: ?loc:loc -> pattern -> expression -> value_binding
end

module Mb: sig
  val mk: ?loc:loc -> pattern -> expression -> match_branch
end

module Imp: sig
  val mk: ?loc:loc -> (import_value * id option) list -> str -> import_declaration list
end

module Ex: sig
  val mk: ?loc:loc -> (str * str option) list -> export_declaration list
end
