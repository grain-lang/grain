(* Modified version of typing/datarepr.ml from OCaml. The original copyright is reproduced below. *)
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

open Asttypes
open Types
open Btype
open Grain_parsing

(* Simplified version of Ctype.free_vars *)
let free_vars ?(param=false) ty =
  let ret = ref TypeSet.empty in
  let rec loop ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      ty.level <- pivot_level - ty.level;
      match ty.desc with
      | TTyVar _ ->
        ret := TypeSet.add ty !ret
      | _ ->
        iter_type_expr loop ty
    end
  in
  loop ty;
  unmark_type ty;
  !ret

let newgenconstr path tyl = newgenty (TTyConstr (path, tyl, ref TMemNil))

let constructor_existentials cd_args cd_res =
  let tyl =
    match cd_args with
    | TConstrSingleton -> []
    | TConstrTuple l -> l
  in
  let existentials =
    match cd_res with
    | None -> []
    | Some type_ret ->
      let arg_vars_set = free_vars (newgenty (TTyTuple tyl)) in
      let res_vars = free_vars type_ret in
      TypeSet.elements (TypeSet.diff arg_vars_set res_vars)
  in
  (tyl, existentials)

let constructor_args cd_args cd_res path =
  let tyl, existentials = constructor_existentials cd_args cd_res in
  match cd_args with
  | TConstrSingleton -> existentials, [], None
  | TConstrTuple l -> existentials, l, None

let constructor_descrs ty_path decl cstrs =
  let ty_res = newgenconstr ty_path decl.type_params in
  let num_consts = ref 0 and num_nonconsts = ref 0  and num_normal = ref 0 in
  List.iter
    (fun {cd_args; _} ->
       if cd_args = TConstrSingleton then incr num_consts else incr num_nonconsts;
       incr num_normal)
    cstrs;
  let describe_constructor idx {cd_id; cd_args; cd_res; cd_loc} =
    let ty_res =
      match cd_res with
      | Some ty_res' -> ty_res'
      | None -> ty_res
    in
    let tag =
      match cd_args with
      | TConstrSingleton -> CstrConstant idx
      | _  -> CstrBlock idx in
    let cstr_name = Ident.name cd_id in
    let existentials, cstr_args, cstr_inlined =
      constructor_args cd_args cd_res
        (Path.PExternal (ty_path, cstr_name, Path.nopos))
    in
    let cstr =
      {
        cstr_name;
        cstr_res = ty_res;
        cstr_existentials = existentials;
        cstr_args;
        cstr_arity = List.length cstr_args;
        cstr_tag = tag;
        cstr_consts = !num_consts;
        cstr_nonconsts = !num_nonconsts;
        cstr_loc = cd_loc;
      } in
    (cd_id, cstr) in
  List.mapi describe_constructor cstrs

let none = {desc = TTyTuple []; level = -1; id = -1}
                                        (* Clearly ill-formed type *)
let dummy_label =
  { 
    lbl_name = ""; lbl_res = none; lbl_arg = none;
    lbl_pos = (-1); lbl_all = [||];
    lbl_loc = Location.dummy_loc;
  }

let label_descrs ty_res lbls =
  let all_labels = Array.make (List.length lbls) dummy_label in
  let rec describe_labels num = function
      [] -> []
    | l :: rest ->
        let lbl =
          { 
            lbl_name = Ident.name l.rf_name;
            lbl_res = ty_res;
            lbl_arg = l.rf_type;
            lbl_pos = num;
            lbl_all = all_labels;
            lbl_loc = l.rf_loc;
          } in
        all_labels.(num) <- lbl;
        (l.rf_name, lbl) :: describe_labels (num+1) rest in
  describe_labels 0 lbls

exception Constr_not_found

let rec find_constr tag num_const num_nonconst = function
    [] ->
      raise Constr_not_found
  | {cd_args = TConstrSingleton; _} as c  :: rem ->
      if tag = CstrConstant num_const
      then c
      else find_constr tag (num_const + 1) num_nonconst rem
  | c :: rem ->
      if tag = CstrBlock num_nonconst || tag = CstrUnboxed
      then c
      else find_constr tag num_const (num_nonconst + 1) rem

let find_constr_by_tag tag cstrlist =
  find_constr tag 0 0 cstrlist

let constructors_of_type ty_path decl =
  match decl.type_kind with
  | TDataVariant cstrs -> constructor_descrs ty_path decl cstrs
  | TDataRecord _
  | TDataAbstract -> []

let labels_of_type ty_path decl =
  match decl.type_kind with
  | TDataRecord(labels) ->
      label_descrs (newgenconstr ty_path decl.type_params)
        labels
  | TDataVariant _ | TDataAbstract -> []

