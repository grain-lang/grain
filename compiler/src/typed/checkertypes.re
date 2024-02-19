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
  | Loop_conditional
  | Loop_body
  | Assert_condition
  | Sequence_left_hand_side
  | Assign_not_box
  | Assign_not_array
  | Assign_not_array_index;

type type_expected = {
  ty: type_expr,
  explanation: option(type_forcing_context),
};

exception InvalidConstant(Location.error);

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
  | Const_int8(_) => instance_def(Builtin_types.type_int8)
  | Const_int16(_) => instance_def(Builtin_types.type_int16)
  | Const_int32(_) => instance_def(Builtin_types.type_int32)
  | Const_int64(_) => instance_def(Builtin_types.type_int64)
  | Const_uint8(_) => instance_def(Builtin_types.type_uint8)
  | Const_uint16(_) => instance_def(Builtin_types.type_uint16)
  | Const_uint32(_) => instance_def(Builtin_types.type_uint32)
  | Const_uint64(_) => instance_def(Builtin_types.type_uint64)
  | Const_float32(_) => instance_def(Builtin_types.type_float32)
  | Const_float64(_) => instance_def(Builtin_types.type_float64)
  | Const_wasmi32(_) => instance_def(Builtin_types.type_wasmi32)
  | Const_wasmi64(_) => instance_def(Builtin_types.type_wasmi64)
  | Const_wasmf32(_) => instance_def(Builtin_types.type_wasmf32)
  | Const_wasmf64(_) => instance_def(Builtin_types.type_wasmf64)
  | Const_bigint(_) => instance_def(Builtin_types.type_bigint)
  | Const_rational(_) => instance_def(Builtin_types.type_rational)
  | Const_bool(_) => instance_def(Builtin_types.type_bool)
  | Const_void => instance_def(Builtin_types.type_void)
  | Const_bytes(_) => instance_def(Builtin_types.type_bytes)
  | Const_string(_) => instance_def(Builtin_types.type_string)
  | Const_char(_) => instance_def(Builtin_types.type_char);

let process_signed_int_literal = (loc, num_bits, conv, get_variant, s) => {
  let n = String_utils.slice(~first=0, ~last=-1, s);
  switch (conv(n)) {
  | Some(n) => Ok(get_variant(n))
  | None =>
    Error(
      Location.errorf(
        ~loc,
        "Int%s literal %s exceeds the range of representable %s-bit signed integers.",
        num_bits,
        s,
        num_bits,
      ),
    )
  };
};

let process_unsigned_int_literal =
    (loc, num_bits, literal_suffix, conv, get_neg_hex, get_variant, s) => {
  let is_neg = String.starts_with(~prefix="-", s);
  let n = String_utils.slice(~first=is_neg ? 1 : 0, ~last=-2, s);
  switch (is_neg, conv(n)) {
  | (false, Some(num)) => Ok(get_variant(num))
  | (false, None) =>
    Error(
      Location.errorf(
        ~loc,
        "Uint%s literal %s exceeds the range of representable %s-bit unsigned integers.",
        num_bits,
        s,
        num_bits,
      ),
    )
  | (true, Some(num)) =>
    Error(
      Location.errorf(
        ~loc,
        "Uint%s literal %s contains a sign but should be unsigned; consider using 0x%s%s instead.",
        num_bits,
        s,
        get_neg_hex(num),
        literal_suffix,
      ),
    )
  | (true, None) =>
    Error(
      Location.errorf(
        ~loc,
        "Uint%s literal %s contains a sign but should be unsigned.",
        num_bits,
        s,
      ),
    )
  };
};

let process_wasm_literal = (~loc, ~prefix, ~bits, ~conv, ~create, s) => {
  let n = String_utils.slice(~first=0, ~last=-1, s);
  switch (conv(n)) {
  | Some(n) => Ok(create(n))
  | None =>
    Error(
      Location.errorf(
        ~loc,
        "Wasm%s%s literal %s exceeds the range of representable %s-bit integers.",
        prefix,
        bits,
        s,
        bits,
      ),
    )
  };
};

let process_bigint_literal = (~loc, s) => {
  let n = String_utils.slice(~first=0, ~last=-1, s);
  switch (Literals.conv_bigint(n)) {
  | Some((bigint_negative, bigint_limbs)) =>
    Ok(Const_bigint({bigint_negative, bigint_limbs, bigint_rep: n}))
  // Should not happen, since `None` is only returned for the empty string,
  // and that is disallowed by the lexer
  | None =>
    Error(
      Location.errorf(~loc, "Unable to parse big-integer literal %s.", s),
    )
  };
};

let process_float_literal = (~loc, ~bits, ~conv, ~create, s) => {
  let n = String_utils.slice(~first=0, ~last=-1, s);
  switch (conv(n)) {
  | Some(n) => Ok(create(n))
  | None =>
    Error(
      Location.errorf(
        ~loc,
        "Float%s literal %s exceeds the range of representable %s-bit floats.",
        bits,
        s,
        bits,
      ),
    )
  };
};

let process_rational_literal = (~loc, s) => {
  let (n, d) =
    switch (
      String.split_on_char('/', String_utils.slice(~first=0, ~last=-1, s))
    ) {
    | [n, d] => (n, d)
    | _ => failwith("Impossible: Invalid rational literal")
    };

  // TODO(#1168): allow arbitrary-length arguments in rational constants
  switch (Literals.conv_number_rational(n, d)) {
  | Some((n, d)) =>
    // (until above TODO is done, we keep existing behavior and limit to 32-bits (see #1168))
    Ok(
      Const_rational({
        rational_negative: Int32.compare(n, 0l) < 0, // true if rational is less than 0
        rational_num_limbs: [|Int64.abs(Int64.of_int32(n))|],
        rational_den_limbs: [|Int64.abs(Int64.of_int32(d))|],
        rational_num_rep: Int32.to_string(n),
        rational_den_rep: Int32.to_string(Int32.abs(d)),
      }),
    )
  | None =>
    Error(
      Location.errorf(
        ~loc,
        "Rational literal %s is outside of the rational number range of the Number type.",
        s,
      ),
    )
  };
};

let process_bytes_literal = (~loc, s) => {
  switch (Literals.conv_bytes(s)) {
  | Ok(bytes) => Ok(Const_bytes(bytes))
  | Error(msg) => Error(Location.error(~loc, msg))
  };
};

let process_string_literal = (~loc, s) => {
  switch (Literals.conv_string(s)) {
  | Ok(str) => Ok(Const_string(str))
  | Error(msg) => Error(Location.error(~loc, msg))
  };
};

let process_char_literal = (~loc, s) => {
  switch (Literals.conv_char(s)) {
  | Ok(c) => Ok(Const_char(c))
  | Error(msg) => Error(Location.error(~loc, msg))
  };
};

let constant:
  (Location.t, Parsetree.constant) =>
  result(Asttypes.constant, Location.error) =
  (loc, c) =>
    switch (c) {
    | PConstNumber(PConstNumberInt({txt: n})) =>
      switch (Literals.conv_number_int(n)) {
      | Some(n) => Ok(Const_number(Const_number_int(n)))
      | None =>
        switch (Literals.conv_bigint(n)) {
        | Some((bigint_negative, bigint_limbs)) =>
          Ok(
            Const_number(
              Const_number_bigint({
                bigint_negative,
                bigint_limbs,
                bigint_rep: n,
              }),
            ),
          )
        // Should not happen, since `None` is only returned for the empty string,
        // and that is disallowed by the lexer
        | None =>
          Error(
            Location.errorf(
              ~loc,
              "Unable to parse big-integer literal %st.",
              n,
            ),
          )
        }
      }
    | PConstNumber(PConstNumberFloat({txt: n})) =>
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
    | PConstNumber(PConstNumberRational({numerator, denominator})) =>
      // TODO(#1168): allow arbitrary-length arguments in rational constants
      switch (Literals.conv_number_rational(numerator.txt, denominator.txt)) {
      | Some((n, d)) when d == 1l =>
        Ok(Const_number(Const_number_int(Int64.of_int32(n))))
      | Some((n, d)) =>
        // (until above TODO is done, we keep existing behavior and limit to 32-bits (see #1168))
        Ok(
          Const_number(
            Const_number_rational({
              rational_negative: Int32.compare(n, 0l) < 0, // true if rational is less than 0
              rational_num_limbs: [|Int64.abs(Int64.of_int32(n))|],
              rational_den_limbs: [|Int64.abs(Int64.of_int32(d))|],
              rational_num_rep: Int32.to_string(n),
              rational_den_rep: Int32.to_string(Int32.abs(d)),
            }),
          ),
        )
      | None =>
        Error(
          Location.errorf(
            ~loc,
            "Number literal %s/%s is outside of the rational number range of the Number type.",
            numerator.txt,
            denominator.txt,
          ),
        )
      }
    | PConstInt8({txt: n}) =>
      process_signed_int_literal(
        loc,
        "8",
        Literals.conv_int8,
        n => Const_int8(n),
        n,
      )
    | PConstInt16({txt: n}) =>
      process_signed_int_literal(
        loc,
        "16",
        Literals.conv_int16,
        n => Const_int16(n),
        n,
      )
    | PConstInt32({txt: n}) =>
      process_signed_int_literal(
        loc,
        "32",
        Literals.conv_int32,
        n => Const_int32(n),
        n,
      )
    | PConstInt64({txt: n}) =>
      process_signed_int_literal(
        loc,
        "64",
        Literals.conv_int64,
        n => Const_int64(n),
        n,
      )
    | PConstUint8({txt: n}) =>
      process_unsigned_int_literal(
        loc,
        "8",
        "us",
        Literals.conv_uint8,
        Literals.get_neg_uint8_hex,
        n => Const_uint8(n),
        n,
      )
    | PConstUint16({txt: n}) =>
      process_unsigned_int_literal(
        loc,
        "16",
        "uS",
        Literals.conv_uint16,
        Literals.get_neg_uint16_hex,
        n => Const_uint16(n),
        n,
      )
    | PConstUint32({txt: n}) =>
      process_unsigned_int_literal(
        loc,
        "32",
        "ul",
        Literals.conv_uint32,
        Literals.get_neg_uint32_hex,
        n => Const_uint32(n),
        n,
      )
    | PConstUint64({txt: n}) =>
      process_unsigned_int_literal(
        loc,
        "64",
        "uL",
        Literals.conv_uint64,
        Literals.get_neg_uint64_hex,
        n => Const_uint64(n),
        n,
      )
    | PConstFloat32({txt: s}) =>
      process_float_literal(
        ~loc,
        ~bits="32",
        ~conv=Literals.conv_float32,
        ~create=n => Const_float32(n),
        s,
      )
    | PConstFloat64({txt: s}) =>
      process_float_literal(
        ~loc,
        ~bits="64",
        ~conv=Literals.conv_float64,
        ~create=n => Const_float64(n),
        s,
      )
    | PConstBigInt({txt: s}) => process_bigint_literal(~loc, s)
    | PConstRational({txt: s}) => process_rational_literal(~loc, s)
    | PConstWasmI32({txt: s}) =>
      process_wasm_literal(
        ~loc,
        ~prefix="I",
        ~bits="32",
        ~conv=Literals.conv_wasmi32,
        ~create=n => Const_wasmi32(n),
        s,
      )
    | PConstWasmI64({txt: s}) =>
      process_wasm_literal(
        ~loc,
        ~prefix="I",
        ~bits="64",
        ~conv=Literals.conv_wasmi64,
        ~create=n => Const_wasmi64(n),
        s,
      )
    | PConstWasmF32({txt: s}) =>
      process_wasm_literal(
        ~loc,
        ~prefix="F",
        ~bits="32",
        ~conv=Literals.conv_wasmf32,
        ~create=n => Const_wasmf32(n),
        s,
      )
    | PConstWasmF64({txt: s}) =>
      process_wasm_literal(
        ~loc,
        ~prefix="F",
        ~bits="64",
        ~conv=Literals.conv_wasmf64,
        ~create=n => Const_wasmf64(n),
        s,
      )
    | PConstBool(b) => Ok(Const_bool(b))
    | PConstVoid => Ok(Const_void)
    | PConstBytes({txt: s}) => process_bytes_literal(~loc, s)
    | PConstString({txt: s}) => process_string_literal(~loc, s)
    | PConstChar({txt: s}) => process_char_literal(~loc, s)
    };

let constant_or_raise = (env, loc, cst) =>
  switch (constant(loc, cst)) {
  | Ok(c) => c
  | Error(err) => raise(InvalidConstant(err))
  };

let () =
  Location.register_error_of_exn(
    fun
    | InvalidConstant(err) => Some(err)
    | _ => None,
  );
