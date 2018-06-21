(* Modified from OCaml. *)
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

(* Substitutions *)
open Grain_parsing
open Misc
open Path
open Types
open Btype

type type_replacement =
  | Path of Path.t
  | Type_function of { params : type_expr list; body : type_expr }

module PathMap = Map.Make(Path)

type t =
  { types: type_replacement PathMap.t;
    modules: Path.t PathMap.t;
    modtypes: (Ident.t, module_type) Tbl.t;
    for_saving: bool;
  }

let identity =
  { types = PathMap.empty;
    modules = PathMap.empty;
    modtypes = Tbl.empty;
    for_saving = false;
  }

let add_type_path id p s = { s with types = PathMap.add id (Path p) s.types }
let add_type id p s = add_type_path (PIdent id) p s

let add_type_function id ~params ~body s =
  { s with types = PathMap.add id (Type_function { params; body }) s.types }

let add_module_path id p s = { s with modules = PathMap.add id p s.modules }
let add_module id p s = add_module_path (PIdent id) p s

let add_modtype id ty s = { s with modtypes = Tbl.add id ty s.modtypes }

let for_saving s = { s with for_saving = true }

let loc s x =
  if s.for_saving then Location.dummy_loc else x

let remove_loc =
  let open Ast_mapper in
  {default_mapper with location = (fun _this _loc -> Location.dummy_loc)}

(* FIXME: Remove*)
let attrs s x =
  x

let rec module_path s path =
  try PathMap.find path s.modules
  with Not_found ->
    match path with
    | PIdent _ -> path
    | PExternal(p, n, pos) ->
       PExternal(module_path s p, n, pos)

let modtype_path s = function
  | PIdent id as p ->
      begin try
        match Tbl.find id s.modtypes with
          | TModIdent p -> p
          | _ -> fatal_error "Subst.modtype_path"
      with Not_found -> p end
  | PExternal(p, n, pos) ->
      PExternal(module_path s p, n, pos)

let type_path s path =
  match PathMap.find path s.types with
  | Path p -> p
  | Type_function _ -> assert false
  | exception Not_found ->
     match path with
     | PIdent _ -> path
     | PExternal(p, n, pos) ->
        PExternal(module_path s p, n, pos)

(* FIXME: Decide if this is needed *)
(*let type_path s p =
  match Path.constructor_typath p with
  | Regular p -> type_path s p
  | Cstr (ty_path, cstr) -> Pdot(type_path s ty_path, cstr, nopos)
  | LocalExt _ -> type_path s p
  | Ext (p, cstr) -> Pdot(module_path s p, cstr, nopos)*)

let to_subst_by_type_function s p =
  match PathMap.find p s.types with
  | Path _ -> false
  | Type_function _ -> true
  | exception Not_found -> false

(* Special type ids for saved signatures *)

let new_id = ref (-1)
let reset_for_saving () = new_id := -1

let newpersty desc =
  decr new_id;
  { desc = desc; level = generic_level; id = !new_id }

(* ensure that all occurrences of 'Tvar None' are physically shared *)
let tvar_none = TTyVar None
let tunivar_none = TTyUniVar None
let norm = function
  | TTyVar None -> tvar_none
  | TTyUniVar None -> tunivar_none
  | d -> d

let ctype_apply_env_empty = ref (fun _ -> assert false)

(* Similar to [Ctype.nondep_type_rec]. *)
let rec typexp s ty =
  let ty = repr ty in
  match ty.desc with
    TTyVar _ | TTyUniVar _ as desc ->
      if s.for_saving || ty.id < 0 then
        let ty' =
          if s.for_saving then newpersty (norm desc)
          else newty2 ty.level desc
        in
        save_desc ty desc; ty.desc <- TTySubst ty'; ty'
      else ty
  | TTySubst ty ->
      ty
  | _ ->
    let desc = ty.desc in
    save_desc ty desc;
    (* Make a stub *)
    let ty' = if s.for_saving then newpersty (TTyVar None) else newgenvar () in
    ty.desc <- TTySubst ty';
    ty'.desc <-
      begin
        match desc with
        | TTyConstr (p, args, _abbrev) ->
          let args = List.map (typexp s) args in
          begin match PathMap.find p s.types with
            | exception Not_found -> TTyConstr(type_path s p, args, ref TMemNil)
            | Path _ -> TTyConstr(type_path s p, args, ref TMemNil)
            | Type_function { params; body } ->
              (!ctype_apply_env_empty params body args).desc
          end
        | _ -> copy_type_desc (typexp s) desc
      end;
    ty'

(*
   Always make a copy of the type. If this is not done, type levels
   might not be correct.
*)
let type_expr s ty =
  let ty' = typexp s ty in
  cleanup_types ();
  ty'

let constructor_arguments s = function
  | TConstrTuple l ->
      TConstrTuple (List.map (typexp s) l)
  | TConstrSingleton -> TConstrSingleton

let constructor_declaration s c =
  {
    cd_id = c.cd_id;
    cd_args = constructor_arguments s c.cd_args;
    cd_res = may_map (typexp s) c.cd_res;
    cd_loc = loc s c.cd_loc;
  }

let type_declaration s decl =
  let decl =
    { type_params = List.map (typexp s) decl.type_params;
      type_arity = decl.type_arity;
      type_kind =
        begin match decl.type_kind with
          | TDataAbstract -> TDataAbstract
          | TDataVariant cstrs ->
            TDataVariant (List.map (constructor_declaration s) cstrs)
        end;
      type_manifest =
        begin
          match decl.type_manifest with
            None -> None
          | Some ty -> Some(typexp s ty)
        end;
      type_newtype_level = None;
      type_loc = loc s decl.type_loc;
      type_immediate = decl.type_immediate;
    }
  in
  cleanup_types ();
  decl

let value_description s descr =
  { val_type = type_expr s descr.val_type;
    val_kind = descr.val_kind;
    val_fullpath = Path.PIdent (Ident.create "<unknown>");
    val_loc = loc s descr.val_loc;
   }

let rec rename_bound_idents s idents = function
    [] -> (List.rev idents, s)
  | TSigType(id, _, _) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_type id (PIdent id') s) (id' :: idents) sg
  | TSigModule(id, _, _) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_module id (PIdent id') s) (id' :: idents) sg
  | TSigModType(id, _) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_modtype id (TModIdent(PIdent id')) s) (id' :: idents) sg
  | (TSigValue(id, _)) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents s (id' :: idents) sg

let rec modtype s = function
    TModIdent p as mty ->
      begin match p with
        PIdent id ->
          begin try Tbl.find id s.modtypes with Not_found -> mty end
      | PExternal(p, n, pos) ->
          TModIdent(PExternal(module_path s p, n, pos))
      end
  | TModSignature sg ->
      TModSignature(signature s sg)

and signature s sg =
  (* Components of signature may be mutually recursive (e.g. type declarations
     or class and type declarations), so first build global renaming
     substitution... *)
  let (new_idents, s') = rename_bound_idents s [] sg in
  (* ... then apply it to each signature component in turn *)
  List.map2 (signature_component s') sg new_idents

and signature_component s comp newid =
  match comp with
  | TSigValue(_id, d) ->
      TSigValue(newid, {(value_description s d) with val_fullpath = Path.PIdent(_id)})
  | TSigType(_id, d, rs) ->
      TSigType(newid, type_declaration s d, rs)
  | TSigModule(_id, d, rs) ->
      TSigModule(newid, module_declaration s d, rs)
  | TSigModType(_id, d) ->
      TSigModType(newid, modtype_declaration s d)

and module_declaration s decl =
  {
    md_type = modtype s decl.md_type;
    md_loc = loc s decl.md_loc;
  }

and modtype_declaration s decl  =
  {
    mtd_type = may_map (modtype s) decl.mtd_type;
    mtd_loc = loc s decl.mtd_loc;
  }

(* For every binding k |-> d of m1, add k |-> f d to m2
   and return resulting merged map. *)

let merge_tbls f m1 m2 =
  Tbl.fold (fun k d accu -> Tbl.add k (f d) accu) m1 m2

let merge_path_maps f m1 m2 =
  PathMap.fold (fun k d accu -> PathMap.add k (f d) accu) m1 m2

let type_replacement s = function
  | Path p -> Path (type_path s p)
  | Type_function { params; body } ->
     let params = List.map (typexp s) params in
     let body = typexp s body in
     Type_function { params; body }

let compose s1 s2 =
  { types = merge_path_maps (type_replacement s2) s1.types s2.types;
    modules = merge_path_maps (module_path s2) s1.modules s2.modules;
    modtypes = merge_tbls (modtype s2) s1.modtypes s2.modtypes;
    for_saving = s1.for_saving || s2.for_saving;
  }
