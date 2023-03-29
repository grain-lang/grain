/* Modified version of predef.ml from OCaml. Original copyright notice is below. */
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

/* Predefined type constructors (with special typing rules in typecore) */

open Grain_parsing;
open Path;
open Types;
open Btype;

let builtin_idents = Hashtbl.create(64);

let wrap = (create, s) => {
  let id = create(s);
  Hashtbl.add(builtin_idents, s, id);
  id;
};

let ident_create = wrap(Ident.create);
let ident_create_predef_exn = wrap(Ident.create_predef_exn);

let ident_number = ident_create("Number")
and ident_exception = ident_create("Exception")
and ident_option = ident_create("Option")
and ident_result = ident_create("Result")
and ident_list = ident_create("List");
let ident_range = ident_create("Range")
and ident_range_start = ident_create("rangeStart")
and ident_range_end = ident_create("rangeEnd")
and ident_int8 = ident_create("Int8")
and ident_int16 = ident_create("Int16")
and ident_int32 = ident_create("Int32")
and ident_int64 = ident_create("Int64")
and ident_uint8 = ident_create("Uint8")
and ident_uint16 = ident_create("Uint16")
and ident_uint32 = ident_create("Uint32")
and ident_uint64 = ident_create("Uint64")
and ident_wasmi32 = ident_create("WasmI32")
and ident_wasmi64 = ident_create("WasmI64")
and ident_wasmf32 = ident_create("WasmF32")
and ident_wasmf64 = ident_create("WasmF64")
and ident_rational = ident_create("Rational")
and ident_float32 = ident_create("Float32")
and ident_float64 = ident_create("Float64")
and ident_bigint = ident_create("BigInt")
and ident_bool = ident_create("Bool")
and ident_string = ident_create("String")
and ident_bytes = ident_create("Bytes")
and ident_char = ident_create("Char")
and ident_void = ident_create("Void")
and ident_box = ident_create("Box")
and ident_array = ident_create("Array")
and ident_assertion_error = ident_create_predef_exn("AssertionError")
and ident_index_out_of_bounds = ident_create_predef_exn("IndexOutOfBounds")
and ident_index_non_integer = ident_create_predef_exn("IndexNonInteger")
and ident_match_failure = ident_create_predef_exn("MatchFailure");

let path_number = PIdent(ident_number)
and path_exception = PIdent(ident_exception)
and path_option = PIdent(ident_option)
and path_result = PIdent(ident_result)
and path_list = PIdent(ident_list)
and path_range = PIdent(ident_range)
and path_int8 = PIdent(ident_int8)
and path_int16 = PIdent(ident_int16)
and path_int32 = PIdent(ident_int32)
and path_int64 = PIdent(ident_int64)
and path_uint8 = PIdent(ident_uint8)
and path_uint16 = PIdent(ident_uint16)
and path_uint32 = PIdent(ident_uint32)
and path_uint64 = PIdent(ident_uint64)
and path_wasmi32 = PIdent(ident_wasmi32)
and path_wasmi64 = PIdent(ident_wasmi64)
and path_wasmf32 = PIdent(ident_wasmf32)
and path_wasmf64 = PIdent(ident_wasmf64)
and path_rational = PIdent(ident_rational)
and path_float32 = PIdent(ident_float32)
and path_float64 = PIdent(ident_float64)
and path_bigint = PIdent(ident_bigint)
and path_bool = PIdent(ident_bool)
and path_string = PIdent(ident_string)
and path_bytes = PIdent(ident_bytes)
and path_char = PIdent(ident_char)
and path_void = PIdent(ident_void)
and path_box = PIdent(ident_box)
and path_array = PIdent(ident_array);

let type_number = newgenty(TTyConstr(path_number, [], ref(TMemNil)))
and type_exception = newgenty(TTyConstr(path_exception, [], ref(TMemNil)))
and type_option = var =>
  newgenty(TTyConstr(path_option, [var], ref(TMemNil)))
and type_result = (ok, err) =>
  newgenty(TTyConstr(path_result, [ok, err], ref(TMemNil)))
and type_list = var => newgenty(TTyConstr(path_list, [var], ref(TMemNil)))
and type_range = var =>
  newgenty(
    TTyRecord([
      (Ident.name(ident_range_start), var),
      (Ident.name(ident_range_end), var),
    ]),
  )
and type_int8 = newgenty(TTyConstr(path_int8, [], ref(TMemNil)))
and type_int16 = newgenty(TTyConstr(path_int16, [], ref(TMemNil)))
and type_int32 = newgenty(TTyConstr(path_int32, [], ref(TMemNil)))
and type_int64 = newgenty(TTyConstr(path_int64, [], ref(TMemNil)))
and type_uint8 = newgenty(TTyConstr(path_uint8, [], ref(TMemNil)))
and type_uint16 = newgenty(TTyConstr(path_uint16, [], ref(TMemNil)))
and type_uint32 = newgenty(TTyConstr(path_uint32, [], ref(TMemNil)))
and type_uint64 = newgenty(TTyConstr(path_uint64, [], ref(TMemNil)))
and type_rational = newgenty(TTyConstr(path_rational, [], ref(TMemNil)))
and type_float32 = newgenty(TTyConstr(path_float32, [], ref(TMemNil)))
and type_float64 = newgenty(TTyConstr(path_float64, [], ref(TMemNil)))
and type_bigint = newgenty(TTyConstr(path_bigint, [], ref(TMemNil)))
and type_wasmi32 = newgenty(TTyConstr(path_wasmi32, [], ref(TMemNil)))
and type_wasmi64 = newgenty(TTyConstr(path_wasmi64, [], ref(TMemNil)))
and type_wasmf32 = newgenty(TTyConstr(path_wasmf32, [], ref(TMemNil)))
and type_wasmf64 = newgenty(TTyConstr(path_wasmf64, [], ref(TMemNil)))
and type_bool = newgenty(TTyConstr(path_bool, [], ref(TMemNil)))
and type_string = newgenty(TTyConstr(path_string, [], ref(TMemNil)))
and type_bytes = newgenty(TTyConstr(path_bytes, [], ref(TMemNil)))
and type_char = newgenty(TTyConstr(path_char, [], ref(TMemNil)))
and type_void = newgenty(TTyConstr(path_void, [], ref(TMemNil)))
and type_box = var => newgenty(TTyConstr(path_box, [var], ref(TMemNil)))
and type_array = var =>
  newgenty(TTyConstr(path_array, [var], ref(TMemNil)))
and type_lambda = (args, res) => newgenty(TTyArrow(args, res, TComOk));

let decl_abstr = path => {
  type_params: [],
  type_arity: 0,
  type_kind: TDataAbstract,
  type_loc: Location.dummy_loc,
  type_path: path,
  type_manifest: None,
  type_newtype_level: Some((0, 0)),
  type_allocation: Managed,
};

let decl_abstr_imm = (repr, path) => {
  ...decl_abstr(path),
  type_allocation: Unmanaged(repr),
};

let cstr = (id, args) => {
  cd_id: id,
  cd_args: TConstrTuple(args),
  cd_res: None,
  cd_repr: ReprFunction(List.map(_ => WasmI32, args), [WasmI32], Indirect),
  cd_loc: Location.dummy_loc,
};

let ident_false = ident_create("false")
and ident_true = ident_create("true")
and ident_void_cstr = ident_create("()")
and ident_some_cstr = ident_create("Some")
and ident_none_cstr = ident_create("None")
and ident_ok_cstr = ident_create("Ok")
and ident_err_cstr = ident_create("Err")
and ident_cons_cstr = ident_create("[...]")
and ident_empty_cstr = ident_create("[]");

let decl_exception = {...decl_abstr(path_exception), type_kind: TDataOpen};
let decl_bool = {
  ...decl_abstr_imm(WasmI32, path_bool),
  type_kind: TDataVariant([cstr(ident_false, []), cstr(ident_true, [])]),
}
and decl_void = {
  ...decl_abstr_imm(WasmI32, path_void),
  type_kind: TDataVariant([cstr(ident_void_cstr, [])]),
}
and decl_option = {
  let tvar = newgenvar();
  {
    ...decl_abstr(path_option),
    type_params: [tvar],
    type_arity: 1,
    type_kind:
      TDataVariant([
        cstr(ident_some_cstr, [tvar]),
        cstr(ident_none_cstr, []),
      ]),
  };
}
and decl_result = {
  let ok = newgenvar();
  let err = newgenvar();
  {
    ...decl_abstr(path_result),
    type_params: [ok, err],
    type_arity: 2,
    type_kind:
      TDataVariant([
        cstr(ident_ok_cstr, [ok]),
        cstr(ident_err_cstr, [err]),
      ]),
  };
}
and decl_list = {
  let tvar = newgenvar();
  {
    ...decl_abstr(path_list),
    type_params: [tvar],
    type_arity: 1,
    type_kind:
      TDataVariant([
        cstr(ident_cons_cstr, [tvar, type_list(tvar)]),
        cstr(ident_empty_cstr, []),
      ]),
  };
}
and decl_range = {
  let tvar = newgenvar();
  {
    ...decl_abstr(path_range),
    type_params: [tvar],
    type_arity: 1,
    type_kind:
      TDataRecord([
        {
          rf_name: ident_range_start,
          rf_type: tvar,
          rf_mutable: false,
          rf_loc: Location.dummy_loc,
        },
        {
          rf_name: ident_range_end,
          rf_type: tvar,
          rf_mutable: false,
          rf_loc: Location.dummy_loc,
        },
      ]),
  };
}
and decl_box = {
  let tvar = newgenvar();
  {...decl_abstr(path_box), type_params: [tvar], type_arity: 1};
}
and decl_array = {
  let tvar = newgenvar();
  {...decl_abstr(path_array), type_params: [tvar], type_arity: 1};
};

let exception_create = (name, ty_args, args) => {
  {
    ext_type_path: path_exception,
    ext_type_params: ty_args,
    ext_args: args,
    ext_repr: ReprValue(WasmI32),
    ext_name: name,
    Types.ext_loc: Location.dummy_loc,
  };
};

let decl_assertion_error =
  exception_create(ident_assertion_error, [], TConstrTuple([type_string]));
let decl_index_out_of_bounds =
  exception_create(ident_index_out_of_bounds, [], TConstrSingleton);
let decl_index_non_integer =
  exception_create(ident_index_non_integer, [], TConstrSingleton);
let decl_match_failure =
  exception_create(ident_match_failure, [], TConstrSingleton);

let initial_env = (add_type, add_extension, empty_env) =>
  empty_env
  |> add_type(ident_number, decl_abstr(path_number))
  |> add_type(ident_exception, decl_exception)
  |> add_type(ident_option, decl_option)
  |> add_type(ident_result, decl_result)
  |> add_type(ident_list, decl_list)
  |> add_type(ident_range, decl_range)
  |> add_type(ident_int8, decl_abstr_imm(WasmI32, path_int8))
  |> add_type(ident_int16, decl_abstr_imm(WasmI32, path_int16))
  |> add_type(ident_int32, decl_abstr(path_int32))
  |> add_type(ident_int64, decl_abstr(path_int64))
  |> add_type(ident_uint8, decl_abstr_imm(WasmI32, path_uint8))
  |> add_type(ident_uint16, decl_abstr_imm(WasmI32, path_uint16))
  |> add_type(ident_uint32, decl_abstr(path_uint32))
  |> add_type(ident_uint64, decl_abstr(path_uint64))
  |> add_type(ident_float32, decl_abstr(path_float32))
  |> add_type(ident_float64, decl_abstr(path_float64))
  |> add_type(ident_bigint, decl_abstr(path_bigint))
  |> add_type(ident_wasmi32, decl_abstr_imm(WasmI32, path_wasmi32))
  |> add_type(ident_wasmi64, decl_abstr_imm(WasmI64, path_wasmi64))
  |> add_type(ident_wasmf32, decl_abstr_imm(WasmF32, path_wasmf32))
  |> add_type(ident_wasmf64, decl_abstr_imm(WasmF64, path_wasmf64))
  |> add_type(ident_rational, decl_abstr(path_rational))
  |> add_type(ident_bool, decl_bool)
  |> add_type(ident_box, decl_box)
  |> add_type(ident_string, decl_abstr(path_string))
  |> add_type(ident_char, decl_abstr_imm(WasmI32, path_char))
  |> add_type(ident_void, decl_void)
  |> add_type(ident_array, decl_array)
  |> add_type(ident_bytes, decl_abstr(path_bytes))
  |> add_extension(ident_assertion_error, decl_assertion_error)
  |> add_extension(ident_index_out_of_bounds, decl_index_out_of_bounds)
  |> add_extension(ident_index_non_integer, decl_index_non_integer)
  |> add_extension(ident_match_failure, decl_match_failure);

let builtin_values =
  List.map(
    id => {
      Ident.make_global(id);
      (Ident.name(id), id);
    },
    [],
  );

/* Start non-predef identifiers at 1000.  This way, more predefs can
   be defined in this file (above!) without breaking .cmi
   compatibility. */

let _ = Ident.setup();
