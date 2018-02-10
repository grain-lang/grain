(* Modified from OCaml's source. Original copyright below. *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree

module type MapArgument = sig
  val enter_typed_program : typed_program -> typed_program
  val enter_data_declaration : data_declaration -> data_declaration

  val enter_pattern : pattern -> pattern
  val enter_expression : expression -> expression

  val enter_core_type : core_type -> core_type
  val enter_toplevel_stmt : toplevel_stmt -> toplevel_stmt

  val leave_typed_program : typed_program -> typed_program
  val leave_data_declaration : data_declaration -> data_declaration
  val leave_pattern : pattern -> pattern
  val leave_expression : expression -> expression

  val leave_core_type : core_type -> core_type
  val leave_toplevel_stmt : toplevel_stmt -> toplevel_stmt

end

module MakeMap :
  functor
    (Iter : MapArgument) ->
sig
  val map_typed_program : typed_program -> typed_program
  val map_pattern : pattern -> pattern
  val map_toplevel_stmt : toplevel_stmt -> toplevel_stmt
  val map_expression : expression -> expression
end

module DefaultMapArgument : MapArgument
