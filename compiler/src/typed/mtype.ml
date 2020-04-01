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

(* Operations on module types *)

open Grain_parsing
open Asttypes
open Path
open Types


let rec scrape env mty =
  match mty with
  | TModIdent p ->
      begin try
        scrape env (Env.find_modtype_expansion p env)
      with Not_found ->
        mty
      end
  | _ -> mty

let freshen mty =
  Subst.modtype Subst.identity mty

let rec strengthen ~aliasable env mty p =
  match scrape env mty with
  (*| TModSignature sg ->
      TModSignature(strengthen_sig ~aliasable env sg p 0)
  | Mty_functor(param, arg, res)
    when !Clflags.applicative_functors && Ident.name param <> "*" ->
      Mty_functor(param, arg,
        strengthen ~aliasable:false env res (Papply(p, PIdent param)))*)
  | mty ->
      mty

and strengthen_sig ~aliasable env sg p pos =
  match sg with
    [] -> []
  | (TSigValue(_, desc) as sigelt) :: rem ->
      let nextpos =
        match desc.val_kind with
        | TValPrim _ -> pos
        | _ -> pos + 1
      in
      sigelt :: strengthen_sig ~aliasable env rem p nextpos
  (*| TSigType(id, {type_kind=TDataAbstract}, _) ::
    (TSigType(id', {type_private=Private}, _) :: _ as rem)
    when Ident.name id = Ident.name id' ^ "#row" ->
      strengthen_sig ~aliasable env rem p pos*)
  | TSigType(id, decl, rs) :: rem ->
      let newdecl =
        match decl.type_manifest, (*decl.type_private,*) decl.type_kind with
          Some _, (*Public,*) _ -> decl
        (*| Some _, Private, (Type_record _ | Type_variant _) -> decl*)
        | _ ->
            let manif =
              Some(Btype.newgenty(TTyConstr(PExternal(p, Ident.name id, nopos),
                                            decl.type_params, ref TMemNil))) in
            if decl.type_kind = TDataAbstract then
              { decl with (*type_private = Public;*) type_manifest = manif }
            else
              { decl with type_manifest = manif }
      in
      TSigType(id, newdecl, rs) :: strengthen_sig ~aliasable env rem p pos
  (*| (Sig_typext _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p (pos+1)*)
  | TSigModule(id, md, rs) :: rem ->
      let str =
        strengthen_decl ~aliasable env md (PExternal(p, Ident.name id, pos))
      in
      TSigModule(id, str, rs)
      :: strengthen_sig ~aliasable
        (Env.add_module_declaration ~check:false id md env) rem p (pos+1)
      (* Need to add the module in case it defines manifest module types *)
  | TSigModType(id, decl) :: rem ->
      let newdecl =
        match decl.mtd_type with
          None ->
            {decl with mtd_type = Some(TModIdent(PExternal(p,Ident.name id,nopos)))}
        | Some _ ->
            decl
      in
      TSigModType(id, newdecl) ::
      strengthen_sig ~aliasable (Env.add_modtype id decl env) rem p pos
      (* Need to add the module type in case it is manifest *)
  (*| (Sig_class _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p (pos+1)
  | (Sig_class_type _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p pos*)

and strengthen_decl ~aliasable env md p =
  match md.md_type with
  | TModAlias _ -> md
  | _ when aliasable -> {md with md_type = TModAlias(p)}
  | mty -> {md with md_type = strengthen ~aliasable env mty p}

let () = Env.strengthen := strengthen

let scrape_for_type_of env mty =
  let rec loop env path mty =
    match mty, path with
    | TModAlias path, _ -> begin
        try
          let md = Env.find_module path None env in
          loop env (Some path) md.md_type
        with Not_found -> mty
      end
    | mty, Some path ->
        strengthen ~aliasable:false env mty path
    | _ -> mty
  in
  loop env None mty

(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

type variance = Co | Contra | Strict

let nondep_supertype env mid mty =

  let rec nondep_mty env va mty =
    match mty with
    | TModIdent p ->
        if Path.isfree mid p then
          nondep_mty env va (Env.find_modtype_expansion p env)
        else mty
    | TModAlias p ->
        if Path.isfree mid p then
          nondep_mty env va (Env.find_module p None env).md_type
        else mty
    | TModSignature sg ->
        TModSignature(nondep_sig env va sg)
    (*| Mty_functor(param, arg, res) ->
        let var_inv =
          match va with Co -> Contra | Contra -> Co | Strict -> Strict in
        Mty_functor(param, Misc.may_map (nondep_mty env var_inv) arg,
                    nondep_mty
                      (Env.add_module ~arg:true param
                         (Btype.default_mty arg) env) va res)*)

  and nondep_sig env va = function
    [] -> []
  | item :: rem ->
      let rem' = nondep_sig env va rem in
      match item with
        TSigValue(id, d) ->
          TSigValue(id,
                    {d with val_type = Ctype.nondep_type env mid d.val_type})
          :: rem'
      | TSigType(id, d, rs) ->
          TSigType(id, Ctype.nondep_type_decl env mid id (va = Co) d, rs)
          :: rem'
      (*| Sig_typext(id, ext, es) ->
          Sig_typext(id, Ctype.nondep_extension_constructor env mid ext, es)
          :: rem'*)
      | TSigModule(id, md, rs) ->
          TSigModule(id, {md with md_type=nondep_mty env va md.md_type}, rs)
          :: rem'
      | TSigModType(id, d) ->
          begin try
            TSigModType(id, nondep_modtype_decl env d) :: rem'
          with Not_found ->
            match va with
              Co -> TSigModType(id, {mtd_type=None; mtd_loc=Location.dummy_loc;
                                     (*mtd_attributes=[]*)}) :: rem'
            | _  -> raise Not_found
          end
      (*| Sig_class(id, d, rs) ->
          Sig_class(id, Ctype.nondep_class_declaration env mid d, rs)
          :: rem'
      | Sig_class_type(id, d, rs) ->
          Sig_class_type(id, Ctype.nondep_cltype_declaration env mid d, rs)
          :: rem'*)

  and nondep_modtype_decl env mtd =
    {mtd with mtd_type = Misc.may_map (nondep_mty env Strict) mtd.mtd_type}

  in
    nondep_mty env Co mty

let enrich_typedecl env p id decl =
  match decl.type_manifest with
    Some _ -> decl
  | None ->
      try
        let orig_decl = Env.find_type p env in
        if decl.type_arity <> orig_decl.type_arity then
          decl
        else
          let orig_ty =
            Ctype.reify_univars
              (Btype.newgenty(TTyConstr(p, orig_decl.type_params, ref TMemNil)))
          in
          let new_ty =
            Ctype.reify_univars
              (Btype.newgenty(TTyConstr(PIdent id, decl.type_params, ref TMemNil)))
          in
          let env = Env.add_type ~check:false id decl env in
          Ctype.mcomp env orig_ty new_ty;
          let orig_ty =
            Btype.newgenty(TTyConstr(p, decl.type_params, ref TMemNil))
          in
          {decl with type_manifest = Some orig_ty}
      with Not_found | Ctype.Unify _ ->
        (* - Not_found: type which was not present in the signature, so we don't
           have anything to do.
           - Unify: the current declaration is not compatible with the one we
           got from the signature. We should just fail now, but then, we could
           also have failed if the arities of the two decls were different,
           which we didn't. *)
        decl

let rec enrich_modtype env p mty =
  match mty with
  | TModSignature sg ->
      TModSignature(List.map (enrich_item env p) sg)
  | _ ->
      mty

and enrich_item env p = function
    TSigType(id, decl, rs) ->
      TSigType(id,
                enrich_typedecl env (PExternal(p, Ident.name id, nopos)) id decl, rs)
  | TSigModule(id, md, rs) ->
      TSigModule(id,
                  {md with
                   md_type = enrich_modtype env
                       (PExternal(p, Ident.name id, nopos)) md.md_type},
                 rs)
  | item -> item

let rec type_paths env p mty =
  match scrape env mty with
  | TModIdent _ -> []
  | TModAlias _ -> []
  | TModSignature sg -> type_paths_sig env p 0 sg
  (*| Mty_functor _ -> []*)

and type_paths_sig env p pos sg =
  match sg with
    [] -> []
  | TSigValue(_id, decl) :: rem ->
      let pos' = match decl.val_kind with TValPrim _ -> pos | _ -> pos + 1 in
      type_paths_sig env p pos' rem
  | TSigType(id, _decl, _) :: rem ->
      PExternal(p, Ident.name id, nopos) :: type_paths_sig env p pos rem
  | TSigModule(id, md, _) :: rem ->
      type_paths env (PExternal(p, Ident.name id, pos)) md.md_type @
      type_paths_sig (Env.add_module_declaration ~check:false id md env)
        p (pos+1) rem
  | TSigModType(id, decl) :: rem ->
      type_paths_sig (Env.add_modtype id decl env) p pos rem
  (*| (Sig_typext _ | Sig_class _) :: rem ->
      type_paths_sig env p (pos+1) rem*)
  (*| (Sig_class_type _) :: rem ->
      type_paths_sig env p pos rem*)

let rec no_code_needed env mty =
  match scrape env mty with
    TModIdent _ -> false
  | TModSignature sg -> no_code_needed_sig env sg
  (*| Mty_functor(_, _, _) -> false*)
  (*| Mty_alias(Mta_absent, _) -> true*)
  | TModAlias _ -> false

and no_code_needed_sig env sg =
  match sg with
    [] -> true
  | TSigValue(_id, decl) :: rem ->
      begin match decl.val_kind with
      | TValPrim _ -> no_code_needed_sig env rem
      | _ -> false
      end
  | TSigModule(id, md, _) :: rem ->
      no_code_needed env md.md_type &&
      no_code_needed_sig
        (Env.add_module_declaration ~check:false id md env) rem
  | (TSigType _ | TSigModType _ (*| Sig_class_type _*)) :: rem ->
      no_code_needed_sig env rem
  (*| (Sig_typext _ | Sig_class _) :: _ ->
      false*)


(* Check whether a module type may return types *)

let rec contains_type env = function
    TModIdent path ->
      begin try match (Env.find_modtype path env).mtd_type with
      | None -> raise Exit (* PR#6427 *)
      | Some mty -> contains_type env mty
      with Not_found -> raise Exit
      end
  | TModSignature sg ->
      contains_type_sig env sg
  (*| Mty_functor (_, _, body) ->
      contains_type env body*)
  | TModAlias _ ->
      ()

and contains_type_sig env = List.iter (contains_type_item env)

and contains_type_item env = function
    (*TSigType (_,({type_manifest = None} |
                 {type_kind = TDataAbstract; type_private = Private}),_)*)
  | TSigModType _
  (*| Sig_typext (_, {ext_args = Cstr_record _}, _)*) ->
      (* We consider that extension constructors with an inlined
         record create a type (the inlined record), even though
         it would be technically safe to ignore that considering
         the current constraints which guarantee that this type
         is kept local to expressions.  *)
      raise Exit
  | TSigModule (_, {md_type = mty}, _) ->
      contains_type env mty
  | TSigValue _
  | TSigType _
  (*| Sig_typext _*)
  (*| Sig_class _*)
  (*| Sig_class_type _*) ->
      ()

let contains_type env mty =
  try contains_type env mty; false with Exit -> true


(* Remove module aliases from a signature *)

module PathSet = Set.Make (Path)
module PathMap = Map.Make (Path)

let rec get_prefixes = function
  | PIdent _ -> PathSet.empty
  | PExternal (p, _, _)
  (*| Papply (p, _)*) -> PathSet.add p (get_prefixes p)

let rec get_arg_paths = function
    PIdent _ -> PathSet.empty
  | PExternal (p, _, _) -> get_arg_paths p
  (*| Papply (p1, p2) ->
      PathSet.add p2
        (PathSet.union (get_prefixes p2)
           (PathSet.union (get_arg_paths p1) (get_arg_paths p2)))*)

let rec rollback_path subst p =
  try PIdent (PathMap.find p subst)
  with Not_found ->
    match p with
      PIdent _ (*| Papply _*) -> p
    | PExternal (p1, s, n) ->
        let p1' = rollback_path subst p1 in
        if Path.same p1 p1' then p else rollback_path subst (PExternal (p1', s, n))

let rec collect_ids subst bindings p =
    begin match rollback_path subst p with
      PIdent id ->
        let ids =
          try collect_ids subst bindings (Ident.find_same id bindings)
          with Not_found -> Ident.Set.empty
        in
        Ident.Set.add id ids
    | _ -> Ident.Set.empty
    end

let collect_arg_paths mty =
  let open Btype in
  let paths = ref PathSet.empty
  and subst = ref PathMap.empty
  and bindings = ref Ident.empty in
  (* let rt = Ident.create "Root" in
     and prefix = ref (Path.PIdent rt) in *)
  let it_path p = paths := PathSet.union (get_arg_paths p) !paths
  and it_signature_item it si =
    type_iterators.it_signature_item it si;
    match si with
      TSigModule (id, {md_type=TModAlias p}, _) ->
        bindings := Ident.add id p !bindings
    | TSigModule (id, {md_type=TModSignature sg}, _) ->
        List.iter
          (function TSigModule (id', _, _) ->
              subst :=
                PathMap.add (PExternal (PIdent id, Ident.name id', -1)) id' !subst
            | _ -> ())
          sg
    | _ -> ()
  in
  let it = {type_iterators with it_path; it_signature_item} in
  it.it_module_type it mty;
  it.it_module_type unmark_iterators mty;
  PathSet.fold (fun p -> Ident.Set.union (collect_ids !subst !bindings p))
    !paths Ident.Set.empty

let rec remove_aliases_mty env excl mty =
  match mty with
    TModSignature sg ->
      TModSignature (remove_aliases_sig env excl sg)
  | TModAlias _ ->
      let mty' = Env.scrape_alias env mty in
      if mty' = mty then mty else
      remove_aliases_mty env excl mty'
  | mty ->
      mty

and remove_aliases_sig env excl sg =
  match sg with
    [] -> []
  | TSigModule(id, md, rs) :: rem  ->
      let mty =
        match md.md_type with
          TModAlias _ when Ident.Set.mem id excl ->
            md.md_type
        | mty ->
            remove_aliases_mty env excl mty
      in
      TSigModule(id, {md with md_type = mty} , rs) ::
      remove_aliases_sig (Env.add_module id mty None env) excl rem
  | TSigModType(id, mtd) :: rem ->
      TSigModType(id, mtd) ::
      remove_aliases_sig (Env.add_modtype id mtd env) excl rem
  | it :: rem ->
      it :: remove_aliases_sig env excl rem


let scrape_for_type_of ~remove_aliases env mty =
  if remove_aliases then begin
    let excl = collect_arg_paths mty in
    remove_aliases_mty env excl mty
  end else begin
    scrape_for_type_of env mty
  end

(* Lower non-generalizable type variables *)

let lower_nongen nglev mty =
  let open Btype in
  let it_type_expr it ty =
    let ty = repr ty in
    match ty with
      {desc=TTyVar _; level} ->
        if level < generic_level && level > nglev then set_level ty nglev
    | _ ->
        type_iterators.it_type_expr it ty
  in
  let it = {type_iterators with it_type_expr} in
  it.it_module_type it mty;
  it.it_module_type unmark_iterators mty
