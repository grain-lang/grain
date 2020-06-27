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
and ident_int32 = ident_create("Int32")
and ident_int64 = ident_create("Int64")
and ident_bool = ident_create("Bool")
and ident_string = ident_create("String")
and ident_void = ident_create("Void")
and ident_box = ident_create("Box")
and ident_array = ident_create("Array")
and ident_fd = ident_create("FileDescriptor");

let path_number = PIdent(ident_number)
and path_int32 = PIdent(ident_int32)
and path_int64 = PIdent(ident_int64)
and path_bool = PIdent(ident_bool)
and path_string = PIdent(ident_string)
and path_void = PIdent(ident_void)
and path_box = PIdent(ident_box)
and path_array = PIdent(ident_array)
and path_fd = PIdent(ident_fd);

let type_number =
  newgenty([@implicit_arity] TTyConstr(path_number, [], ref(TMemNil)))
and type_int32 =
  newgenty([@implicit_arity] TTyConstr(path_int32, [], ref(TMemNil)))
and type_int64 =
  newgenty([@implicit_arity] TTyConstr(path_int64, [], ref(TMemNil)))
and type_bool =
  newgenty([@implicit_arity] TTyConstr(path_bool, [], ref(TMemNil)))
and type_string =
  newgenty([@implicit_arity] TTyConstr(path_string, [], ref(TMemNil)))
and type_void =
  newgenty([@implicit_arity] TTyConstr(path_void, [], ref(TMemNil)))
and type_box = var =>
  newgenty([@implicit_arity] TTyConstr(path_box, [var], ref(TMemNil)))
and type_array = var =>
  newgenty([@implicit_arity] TTyConstr(path_array, [var], ref(TMemNil)))
and type_fd =
  newgenty([@implicit_arity] TTyConstr(path_fd, [], ref(TMemNil)))
and type_lambda = (args, res) =>
  newgenty([@implicit_arity] TTyArrow(args, res, TComOk));

let all_predef_exns = [];

let decl_abstr = path => {
  type_params: [],
  type_arity: 0,
  type_kind: TDataAbstract,
  type_loc: Location.dummy_loc,
  type_path: path,
  type_manifest: None,
  type_newtype_level: Some((0, 0)),
  type_immediate: false,
};

let decl_abstr_imm = path => {...decl_abstr(path), type_immediate: true};

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

let decl_bool =
  decl_create({
    ...decl_abstr(path_bool),
    type_kind: TDataVariant([cstr(ident_false, []), cstr(ident_true, [])]),
    type_immediate: true,
  })
and decl_void =
  decl_create({
    ...decl_abstr(path_void),
    type_kind: TDataVariant([cstr(ident_void_cstr, [])]),
    type_immediate: true,
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

let common_initial_env = (add_type, empty_env) =>
  empty_env
  |> add_type(ident_number, decl_abstr_imm(path_number))
  |> add_type(ident_int32, decl_abstr(path_int32))
  |> add_type(ident_int64, decl_abstr(path_int64))
  |> add_type(ident_bool, decl_bool)
  |> add_type(ident_box, decl_box)
  |> add_type(ident_string, decl_abstr(path_string))
  |> add_type(ident_void, decl_void)
  |> add_type(ident_array, decl_array)
  |> add_type(ident_fd, decl_abstr(path_fd));

let build_initial_env = (add_type, empty_env) => {
  let common = common_initial_env(add_type, empty_env);
  /*let safe_string = add_type ident_bytes decl_abstr common in
    let decl_bytes_unsafe = {decl_abstr with type_manifest = Some type_string} in
    let unsafe_string = add_type ident_bytes decl_bytes_unsafe common in
      (safe_string, unsafe_string)*/
  (common, common);
};

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
