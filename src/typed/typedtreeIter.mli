(* Modified from OCaml's source. Original copyright below. *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Grain_parsing
open Asttypes
open Typedtree

module type IteratorArgument = sig
    val enter_typed_program : typed_program -> unit

    val enter_pattern : pattern -> unit
    val enter_expression : expression -> unit
    val enter_core_type : core_type -> unit
    val enter_toplevel_stmt : toplevel_stmt -> unit


    val leave_typed_program : typed_program -> unit
    val leave_pattern : pattern -> unit
    val leave_expression : expression -> unit
    val leave_core_type : core_type -> unit
    val leave_toplevel_stmt : toplevel_stmt -> unit

    val enter_bindings : rec_flag -> unit
    val enter_binding : value_binding -> unit
    val leave_binding : value_binding -> unit
    val leave_bindings : rec_flag -> unit

    val enter_data_declarations : unit -> unit
    val enter_data_declaration : data_declaration -> unit
    val leave_data_declaration : data_declaration -> unit
    val leave_data_declarations : unit -> unit

end

module MakeIterator :
  functor (Iter : IteratorArgument) ->
    sig
      val iter_typed_program : typed_program -> unit
      val iter_toplevel_stmt : toplevel_stmt -> unit
      val iter_expression : expression -> unit
      val iter_pattern : pattern -> unit
    end

module DefaultIteratorArgument : IteratorArgument
