/* Modified version of typing/datarepr.ml from OCaml. The original copyright is reproduced below. */
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

open Asttypes;
open Types;
open Btype;
open Grain_parsing;

/* Simplified version of Ctype.free_vars */
let free_vars = (~param=false, ty) => {
  let ret = ref(TypeSet.empty);
  let rec loop = ty => {
    let ty = repr(ty);
    if (ty.level >= lowest_level) {
      ty.level = pivot_level - ty.level;
      switch (ty.desc) {
      | TTyVar(_) => ret := TypeSet.add(ty, ret^)
      | _ => iter_type_expr(loop, ty)
      };
    };
  };

  loop(ty);
  unmark_type(ty);
  ret^;
};

let newgenconstr = (path, tyl) =>
  newgenty(TTyConstr(path, tyl, ref(TMemNil)));

let constructor_existentials = (cd_args, cd_res) => {
  let tyl =
    switch (cd_args) {
    | TConstrSingleton => []
    | TConstrTuple(l) => l
    | TConstrRecord(l) => List.map(l => l.rf_type, l)
    };

  let existentials =
    switch (cd_res) {
    | None => []
    | Some(type_ret) =>
      let arg_vars_set = free_vars(newgenty(TTyTuple(tyl)));
      let res_vars = free_vars(type_ret);
      TypeSet.elements(TypeSet.diff(arg_vars_set, res_vars));
    };

  (tyl, existentials);
};

let constructor_args = (cd_args, cd_res, path) => {
  let (tyl, existentials) = constructor_existentials(cd_args, cd_res);
  switch (cd_args) {
  | TConstrSingleton => (existentials, [], None)
  | TConstrTuple(l) => (existentials, l, None)
  | TConstrRecord(rfs) =>
    let arg_vars_set = free_vars(~param=true, newgenty(TTyTuple(tyl)));
    let type_params = TypeSet.elements(arg_vars_set);
    let arity = List.length(type_params);
    let tdecl = {
      type_params,
      type_arity: arity,
      type_kind: TDataRecord(rfs),
      type_manifest: None,
      type_loc: Location.dummy_loc,
      type_newtype_level: None,
      type_allocation: Managed,
      type_path: path,
    };
    (existentials, [newgenconstr(path, type_params)], Some(tdecl));
  };
};

let constructor_descrs = (ty_path, decl, cstrs) => {
  let ty_res = newgenconstr(ty_path, decl.type_params);
  let num_consts = ref(0)
  and num_nonconsts = ref(0)
  and num_normal = ref(0);
  List.iter(
    ({cd_args, _}) => {
      if (cd_args == TConstrSingleton) {
        incr(num_consts);
      } else {
        incr(num_nonconsts);
      };
      incr(num_normal);
    },
    cstrs,
  );
  let describe_constructor = (idx, {cd_id, cd_args, cd_res, cd_loc}) => {
    let ty_res =
      switch (cd_res) {
      | Some(ty_res') => ty_res'
      | None => ty_res
      };

    let tag =
      switch (cd_args) {
      | TConstrSingleton => CstrConstant(idx)
      | _ => CstrBlock(idx)
      };
    let cstr_name = Ident.name(cd_id);
    let (existentials, cstr_args, cstr_inlined) =
      constructor_args(cd_args, cd_res, Path.PExternal(ty_path, cstr_name));

    let cstr = {
      cstr_name,
      cstr_res: ty_res,
      cstr_existentials: existentials,
      cstr_args,
      cstr_arity: List.length(cstr_args),
      cstr_tag: tag,
      cstr_consts: num_consts^,
      cstr_nonconsts: num_nonconsts^,
      cstr_loc: cd_loc,
      cstr_inlined,
    };
    (cd_id, cstr);
  };
  List.mapi(describe_constructor, cstrs);
};

let extension_descr = (path_ext, ext) => {
  let ty_res = newgenconstr(ext.ext_type_path, ext.ext_type_params);

  let (existentials, cstr_args, cstr_inlined) =
    constructor_args(
      ext.ext_args,
      Some(ty_res),
      Path.PExternal(path_ext, "#extension#"),
    );

  let cstr_ext_type =
    if (cstr_args == []) {
      CstrExtensionConstant;
    } else {
      CstrExtensionBlock;
    };

  {
    cstr_name: Path.last(path_ext),
    cstr_res: ty_res,
    cstr_existentials: existentials,
    cstr_args,
    cstr_arity: List.length(cstr_args),
    cstr_tag: CstrExtension(ext.ext_name.stamp, path_ext, cstr_ext_type, ext),
    cstr_consts: (-1),
    cstr_nonconsts: (-1),
    cstr_loc: ext.ext_loc,
    cstr_inlined,
  };
};

let none = {desc: TTyTuple([]), level: (-1), id: (-1)};
/* Clearly ill-formed type */
let dummy_label = {
  lbl_name: "",
  lbl_res: none,
  lbl_arg: none,
  lbl_pos: (-1),
  lbl_mut: false,
  lbl_all: [||],
  lbl_loc: Location.dummy_loc,
};

let label_descrs = (ty_res, lbls) => {
  let all_labels = Array.make(List.length(lbls), dummy_label);
  let rec describe_labels = num =>
    fun
    | [] => []
    | [l, ...rest] => {
        let lbl = {
          lbl_name: Ident.name(l.rf_name),
          lbl_res: ty_res,
          lbl_arg: l.rf_type,
          lbl_mut: l.rf_mutable,
          lbl_pos: num,
          lbl_all: all_labels,
          lbl_loc: l.rf_loc,
        };
        all_labels[num] = lbl;
        [(l.rf_name, lbl), ...describe_labels(num + 1, rest)];
      };
  describe_labels(0, lbls);
};

exception Constr_not_found;

let rec find_constr = (tag, num_const, num_nonconst) =>
  fun
  | [] => raise(Constr_not_found)
  | [{cd_args: TConstrSingleton, _} as c, ...rem] =>
    if (tag == CstrConstant(num_const)) {
      c;
    } else {
      find_constr(tag, num_const + 1, num_nonconst, rem);
    }
  | [c, ...rem] =>
    if (tag == CstrBlock(num_nonconst) || tag == CstrUnboxed) {
      c;
    } else {
      find_constr(tag, num_const, num_nonconst + 1, rem);
    };

let find_constr_by_tag = (tag, cstrlist) => find_constr(tag, 0, 0, cstrlist);

let constructors_of_type = (ty_path, decl) =>
  switch (decl.type_kind) {
  | TDataVariant(cstrs) => constructor_descrs(ty_path, decl, cstrs)
  | TDataRecord(_)
  | TDataOpen
  | TDataAbstract => []
  };

let labels_of_type = (ty_path, decl) =>
  switch (decl.type_kind) {
  | TDataRecord(labels) =>
    label_descrs(newgenconstr(ty_path, decl.type_params), labels)
  | TDataVariant(_)
  | TDataOpen
  | TDataAbstract => []
  };
