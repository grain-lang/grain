(* Taken from OCaml's typing/typecore.ml module. Original copyright: *)
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
open Grain_parsing
open Parsetree
open Types
open Ctype
open Builtin_types

type type_forcing_context =
  | If_conditional
  | If_no_else_branch
  | While_loop_conditional
  | While_loop_body
  | For_loop_start_index
  | For_loop_stop_index
  | For_loop_body
  | Assert_condition
  | Sequence_left_hand_side
  | Assign_not_box
  | Assign_not_array
  | Assign_not_array_index

type type_expected = {
  ty: type_expr;
  explanation: type_forcing_context option;
}

type error = string
exception Error of Location.t * Env.t * error

(*
  Saving and outputting type information.
  We keep these function names short, because they have to be
  called each time we create a record of type [Typedtree.expression]
  or [Typedtree.pattern] that will end up in the typed AST.
*)
let re node =
  (*Cmt_format.add_saved_type (Cmt_format.Partial_expression node);
  Stypes.record (Stypes.Ti_expr node);*)
  node
;;
let rp node =
  (*Cmt_format.add_saved_type (Cmt_format.Partial_pattern node);
  Stypes.record (Stypes.Ti_pat node);*)
  node
;;


let mk_expected ?explanation ty = { ty; explanation; }

let type_constant = function
  | Const_int _ -> instance_def Builtin_types.type_number
  | Const_bool _ -> instance_def Builtin_types.type_bool
  | Const_string _ -> instance_def Builtin_types.type_string
  | _ -> failwith "NYI: type_constant"

let constant : Parsetree.constant -> (Asttypes.constant, error) result = function
  | PConstNumber(n) -> Ok(Const_int n)
  | PConstBool(b) -> Ok(Const_bool b)
  | PConstString(s) -> Ok(Const_string s)

let constant_or_raise env loc cst =
  match constant cst with
  | Ok c -> c
  | Error err -> raise (Error (loc, env, err))
