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
open Misc
open Longident
open Path
open Asttypes
open Parsetree
open Types
open Format

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Identifier.t
  | With_mismatch of Identifier.t * Includemod.error list
  | With_makes_applicative_functor_ill_typed of
      Identifier.t * Path.t * Includemod.error list
  | With_changes_module_alias of Identifier.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_module of module_type
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Identifier.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

open Typedtree


(* Add recursion flags on declarations arising from a mutually recursive
   block. *)

let map_rec fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl -> fn TRecFirst d1 :: map_end (fn TRecNext) dl rem

let map_rec_type ~rec_flag fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl ->
      let first =
        match rec_flag with
        | Recursive -> TRecFirst
        | Nonrecursive -> TRecNot
      in
      fn first d1 :: map_end (fn TRecNext) dl rem

let rec map_rec_type_with_row_types ~rec_flag fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl ->
      (*if Btype.is_row_name (Ident.name d1.typ_id) then
        fn Trec_not d1 :: map_rec_type_with_row_types ~rec_flag fn dl rem
      else*)
        map_rec_type ~rec_flag fn decls rem



(* Auxiliaries for checking uniqueness of names in signatures and structures *)

module StringSet =
  Set.Make(struct type t = string let compare (x:t) y = String.compare x y end)

let check cl loc set_ref name =
  if StringSet.mem name !set_ref
  then raise(Error(loc, Env.empty, Repeated_name(cl, name)))
  else set_ref := StringSet.add name !set_ref

type names =
  {
    types: StringSet.t ref;
    modules: StringSet.t ref;
    modtypes: StringSet.t ref;
    typexts: StringSet.t ref;
  }

let new_names () =
  {
    types = ref StringSet.empty;
    modules = ref StringSet.empty;
    modtypes = ref StringSet.empty;
    typexts = ref StringSet.empty;
  }


let check_name check names name = check names name.loc name.txt
let check_type names loc s = check "type" loc names.types s
let check_module names loc s = check "module" loc names.modules s
let check_modtype names loc s = check "module type" loc names.modtypes s


let check_sig_item names loc = function
  | TSigType(id, _, _) -> check_type names loc (Ident.name id)
  | TSigModule(id, _, _) -> check_module names loc (Ident.name id)
  | TSigModType(id, _) -> check_modtype names loc (Ident.name id)
  | _ -> ()


let enrich_type_decls anchor decls oldenv newenv =
  match anchor with
    None -> newenv
  | Some p ->
      List.fold_left
        (fun e info ->
          let id = info.data_id in
          let info' =
            let p = PExternal(p, Ident.name id, nopos) in
            let decl = info.data_type in
            match decl.type_manifest with
            | Some _ -> decl
            | None ->
              try
                let orig_decl = Env.find_type p oldenv in
                if orig_decl.type_arity <> decl.type_arity
                then decl
                else {decl with type_manifest = Some(Btype.newgenty(TTyConstr(p, decl.type_params, ref TMemNil)))}
              with Not_found -> decl
          in
            Env.add_type ~check:true id info' e)
        oldenv decls


let type_module ?(toplevel=false) funct_body anchor env sstr (*scope*) =

  let (imports, datas, lets) = List.fold_right (fun {ptop_desc; ptop_loc=loc} (imports, datas, lets) ->
      match ptop_desc with
      | PTopImport i -> ((i, loc)::imports, datas, lets)
      | PTopData d -> (imports, (d, loc)::datas, lets)
      | PTopLet(r, vb) -> (imports, datas, ((r, vb), loc)::lets)) sstr.Parsetree.statements ([], [], []) in

  (* TODO: imports*)
  let decls, newenv = Typedecl.transl_data_decl env Recursive (List.map fst datas) in
  let ty_decl = map_rec_type_with_row_types ~rec_flag:Recursive
      (fun rs info -> TSigType(info.data_id, info.data_type, rs))
      decls [] in
  let newenv = enrich_type_decls anchor decls env newenv in

  let process_let env (rec_flag, binds) =
    let scope = None in
    let defs, newenv = Typecore.type_binding env rec_flag binds scope in
    let () = if rec_flag = Recursive then
        Typecore.check_recursive_bindings env defs
    in
    (TTopLet(rec_flag, defs)),
    List.map (fun id -> TSigValue(id, Env.find_value (PIdent id) newenv))
      (let_bound_idents defs), newenv in

  let init_stmts = List.map2 (fun d (_, loc) -> {ttop_desc=TTopData(d); ttop_loc=loc; ttop_env=newenv}) decls datas in

  let rec process_all_lets env lets =
    Ctype.init_def(Ident.current_time());
    match lets with
    | [] -> ([], [], env)
    | (lt, loc) :: rem ->
      let desc, sg, new_env = process_let env lt in
      let stmt = { ttop_desc = desc; ttop_loc = loc; ttop_env = env } in
      let (tt_rem, sig_rem, final_env) = process_all_lets new_env rem in
      (stmt::tt_rem, sg @ sig_rem, final_env) in

  let run() =
    let (items, sg, final_env) = process_all_lets env lets in
    let str = { statements=init_stmts @ items; env = final_env; body = Typecore.type_expression final_env sstr.body } in
    str, (ty_decl @ sg), final_env in

  run()

let type_module = type_module false None
