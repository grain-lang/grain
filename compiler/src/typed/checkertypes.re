/* Taken from OCaml's typing/typecore.ml module. Original copyright: */
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
open Grain_parsing;
open Grain_utils;
open Parsetree;
open Types;
open Ctype;
open Builtin_types;

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
  | Assign_not_array_index;

type type_expected = {
  ty: type_expr,
  explanation: option(type_forcing_context),
};

type error = string;
exception Error(Location.error);

/*
   Saving and outputting type information.
   We keep these function names short, because they have to be
   called each time we create a record of type [Typedtree.expression]
   or [Typedtree.pattern] that will end up in the typed AST.
 */
let re = node =>
  /*Cmt_format.add_saved_type (Cmt_format.Partial_expression node);
    Stypes.record (Stypes.Ti_expr node);*/
  node;

let rp = node =>
  /*Cmt_format.add_saved_type (Cmt_format.Partial_pattern node);
    Stypes.record (Stypes.Ti_pat node);*/
  node;

let mk_expected = (~explanation=?, ty) => {ty, explanation};

let type_constant =
  fun
  | Const_number(_) => instance_def(Builtin_types.type_number)
  | Const_int32(_) => instance_def(Builtin_types.type_int32)
  | Const_int64(_) => instance_def(Builtin_types.type_int64)
  | Const_float32(_) => instance_def(Builtin_types.type_float32)
  | Const_float64(_) => instance_def(Builtin_types.type_float64)
  | Const_bool(_) => instance_def(Builtin_types.type_bool)
  | Const_void => instance_def(Builtin_types.type_void)
  | Const_string(_) => instance_def(Builtin_types.type_string);

let constant:
  (Location.t, Parsetree.constant) =>
  result(Asttypes.constant, Location.error) =
  (loc, c) =>
    switch (c) {
    | PConstNumber(PConstNumberInt(n)) =>
      switch (Literals.conv_number_int(n)) {
      | Some(n) => Ok(Const_number(Const_number_int(n)))
      | None =>
        Error(
          Location.errorf(
            ~loc,
            "Number literal %s is outside of the integer range of the Number type.",
            n,
          ),
        )
      }
    | PConstNumber(PConstNumberFloat(n)) =>
      switch (Literals.conv_number_float(n)) {
      | Some(n) => Ok(Const_number(Const_number_float(n)))
      | None =>
        Error(
          Location.errorf(
            ~loc,
            "Number literal %s is outside of the floating-point range of the Number type.",
            n,
          ),
        )
      }
    | PConstNumber(PConstNumberRational(_)) =>
      failwith("NYI: checkertypes constant PConstNumberRational")
    | PConstInt32(n) =>
      switch (Literals.conv_int32(n)) {
      | Some(n) => Ok(Const_int32(n))
      | None =>
        Error(
          Location.errorf(
            ~loc,
            "Int32 literal %sl exceeds the range of representable 32-bit integers.",
            n,
          ),
        )
      }
    | PConstInt64(n) =>
      switch (Literals.conv_int64(n)) {
      | Some(n) => Ok(Const_int64(n))
      | None =>
        Error(
          Location.errorf(
            ~loc,
            "Int64 literal %sL exceeds the range of representable 64-bit integers.",
            n,
          ),
        )
      }
    | PConstFloat32(n) =>
      switch (Literals.conv_float32(n)) {
      | Some(n) => Ok(Const_float32(n))
      | None =>
        Error(
          Location.errorf(
            ~loc,
            "Float32 literal %sf exceeds the range of representable 32-bit floats.",
            n,
          ),
        )
      }
    | PConstFloat64(n) =>
      switch (Literals.conv_float64(n)) {
      | Some(n) => Ok(Const_float64(n))
      | None =>
        Error(
          Location.errorf(
            ~loc,
            "Float64 literal %sd exceeds the range of representable 64-bit floats.",
            n,
          ),
        )
      }
    | PConstBool(b) => Ok(Const_bool(b))
    | PConstVoid => Ok(Const_void)
    | PConstString(s) => Ok(Const_string(s))
    };

let constant_or_raise = (env, loc, cst) =>
  switch (constant(loc, cst)) {
  | Ok(c) => c
  | Error(err) => raise(Error(err))
  };

let () =
  Location.register_error_of_exn(
    fun
    | Error(err) => Some(err)
    | _ => None,
  );
