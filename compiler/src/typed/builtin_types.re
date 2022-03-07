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

let builtin_idents = ref([]);
let builtin_decls = ref([]);

let wrap = (create, s) => {
  let id = create(s);
  builtin_idents := [(s, id), ...builtin_idents^];
  id;
};

let ident_create = wrap(Ident.create);
let ident_create_predef_exn = wrap(Ident.create_predef_exn);

let ident_number = ident_create("Number")
and ident_exception = ident_create("Exception")
and ident_int32 = ident_create("Int32")
and ident_int64 = ident_create("Int64")
and ident_wasmi32 = ident_create("WasmI32")
and ident_wasmi64 = ident_create("WasmI64")
and ident_wasmf32 = ident_create("WasmF32")
and ident_wasmf64 = ident_create("WasmF64")
and ident_rational = ident_create("Rational")
and ident_float32 = ident_create("Float32")
and ident_float64 = ident_create("Float64")
and ident_bool = ident_create("Bool")
and ident_string = ident_create("String")
and ident_bytes = ident_create("Bytes")
and ident_char = ident_create("Char")
and ident_void = ident_create("Void")
and ident_box = ident_create("Box")
and ident_array = ident_create("Array")
and ident_fd = ident_create("FileDescriptor");

let path_number = PIdent(ident_number)
and path_exception = PIdent(ident_exception)
and path_int32 = PIdent(ident_int32)
and path_int64 = PIdent(ident_int64)
and path_wasmi32 = PIdent(ident_wasmi32)
and path_wasmi64 = PIdent(ident_wasmi64)
and path_wasmf32 = PIdent(ident_wasmf32)
and path_wasmf64 = PIdent(ident_wasmf64)
and path_rational = PIdent(ident_rational)
and path_float32 = PIdent(ident_float32)
and path_float64 = PIdent(ident_float64)
and path_bool = PIdent(ident_bool)
and path_string = PIdent(ident_string)
and path_bytes = PIdent(ident_bytes)
and path_char = PIdent(ident_char)
and path_void = PIdent(ident_void)
and path_box = PIdent(ident_box)
and path_array = PIdent(ident_array)
and path_fd = PIdent(ident_fd);

let type_number = newgenty(TTyConstr(path_number, [], ref(TMemNil)))
and type_exception = newgenty(TTyConstr(path_exception, [], ref(TMemNil)))
and type_int32 = newgenty(TTyConstr(path_int32, [], ref(TMemNil)))
and type_int64 = newgenty(TTyConstr(path_int64, [], ref(TMemNil)))
and type_rational = newgenty(TTyConstr(path_rational, [], ref(TMemNil)))
and type_float32 = newgenty(TTyConstr(path_float32, [], ref(TMemNil)))
and type_float64 = newgenty(TTyConstr(path_float64, [], ref(TMemNil)))
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
and type_fd = newgenty(TTyConstr(path_fd, [], ref(TMemNil)))
and type_lambda = (args, res) => newgenty(TTyArrow(args, res, TComOk));

let all_predef_exns = [];

let decl_abstr = path => {
  type_params: [],
  type_arity: 0,
  type_kind: TDataAbstract,
  type_loc: Location.dummy_loc,
  type_path: path,
  type_manifest: None,
  type_newtype_level: Some((0, 0)),
  type_allocation: HeapAllocated,
};

let decl_abstr_imm = (repr, path) => {
  ...decl_abstr(path),
  type_allocation: StackAllocated(repr),
};

let cstr = (id, args) => {
  cd_id: id,
  cd_args: TConstrTuple(args),
  cd_res: None,
  cd_loc: Location.dummy_loc,
};

let ident_false = ident_create("false")
and ident_true = ident_create("true")
and ident_void_cstr = ident_create("()");

let decl_create = decl => {
  builtin_decls := [decl, ...builtin_decls^];
  decl;
};

let decl_exception =
  decl_create({...decl_abstr(path_exception), type_kind: TDataOpen});
let decl_bool =
  decl_create({
    ...decl_abstr_imm(WasmI32, path_bool),
    type_kind: TDataVariant([cstr(ident_false, []), cstr(ident_true, [])]),
  })
and decl_void =
  decl_create({
    ...decl_abstr_imm(WasmI32, path_void),
    type_kind: TDataVariant([cstr(ident_void_cstr, [])]),
  })
and decl_box = {
  let tvar = newgenvar();
  decl_create({
    ...decl_abstr(path_box),
    type_params: [tvar],
    type_arity: 1,
  });
}
and decl_array = {
  let tvar = newgenvar();
  decl_create({
    ...decl_abstr(path_array),
    type_params: [tvar],
    type_arity: 1,
  });
};

let initial_env = (add_type, empty_env) =>
  empty_env
  |> add_type(ident_number, decl_abstr(path_number))
  |> add_type(ident_exception, decl_exception)
  |> add_type(ident_int32, decl_abstr(path_int32))
  |> add_type(ident_int64, decl_abstr(path_int64))
  |> add_type(ident_float32, decl_abstr(path_float32))
  |> add_type(ident_float64, decl_abstr(path_float64))
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
  |> add_type(ident_fd, decl_abstr(path_fd))
  |> add_type(ident_bytes, decl_abstr(path_bytes));

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

let _ = Ident.set_current_time(999);
let builtin_idents = List.rev(builtin_idents^);
let builtin_decls = List.rev(builtin_decls^);
