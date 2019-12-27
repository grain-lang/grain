(* Stripped down version of typing/btype.ml from the OCaml compiler. *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Basic operations on core types *)

open Misc
open Asttypes
open Types

(**** Sets, maps and hashtables of types ****)

module TypeSet = Set.Make(TypeOps)
module TypeMap = Map.Make (TypeOps)
module TypeHash = Hashtbl.Make(TypeOps)

(**** Forward declarations ****)

let print_raw =
  ref (fun _ -> assert false : Format.formatter -> type_expr -> unit)

(**** Type level management ****)

let generic_level = 100000000

(* Used to mark a type during a traversal. *)
let lowest_level = 0
let pivot_level = 2 * lowest_level - 1
    (* pivot_level - lowest_level < lowest_level *)

(**** Some type creators ****)

let new_id = ref (-1)

let newty2 level desc  =
  incr new_id; { desc; level; id = !new_id }
let newgenty desc      = newty2 generic_level desc
let newgenvar ?name () = newgenty (TTyVar name)

(**** Check some types ****)

let is_Tvar = function {desc=TTyVar _} -> true | _ -> false
let is_Tunivar = function {desc=TTyUniVar _} -> true | _ -> false
let is_Tconstr = function {desc=TTyConstr _} -> true | _ -> false

let dummy_method = "*dummy method*"

(**** Definitions for backtracking ****)

type change =
    Ctype of type_expr * type_desc
  | Ccompress of type_expr * type_desc * type_desc
  | Clevel of type_expr * int
  | Cname of
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option
  | Ccommu of commutable ref * commutable
  | Cuniv of type_expr option ref * type_expr option
  | Ctypeset of TypeSet.t ref * TypeSet.t

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

let trail = Weak.create 1

let log_change ch =
  match Weak.get trail 0 with None -> ()
  | Some r ->
      let r' = ref Unchanged in
      r := Change (ch, r');
      Weak.set trail 0 (Some r')


(**** Representative of a type ****)

let rec repr_link compress t d =
  function
  | {desc = TTyLink t' as d'} ->
    repr_link true t d' t'
  | t' ->
    if compress then begin
      log_change (Ccompress(t, t.desc, d)); t.desc <- d
    end;
    t'

let repr t =
  match t.desc with
  | TTyLink t' as d -> repr_link false t d t'
  | _ -> t


let rec commu_repr = function
  | TComLink r when !r <> TComUnknown -> commu_repr !r
  | c -> c

                  (**********************************)
                  (*  Utilities for type traversal  *)
                  (**********************************)

let iter_type_expr f ty =
  match ty.desc with
  | TTyVar _ -> ()
  | TTyArrow(args, ret, _) -> List.iter f args; f ret
  | TTyTuple ts -> List.iter f ts
  | TTyRecord ts -> List.iter (fun (_, t) -> f t) ts
  | TTyConstr(_, args, _) -> List.iter f args;
  | TTyLink ty -> f ty
  | TTySubst ty -> f ty
  | TTyUniVar _ -> ()
  | TTyPoly(t, l) -> f t; List.iter f l

let rec iter_abbrev f = function
  | TMemNil                   -> ()
  | TMemCons(_, ty, ty', rem) -> f ty; f ty'; iter_abbrev f rem
  | TMemLink rem              -> iter_abbrev f !rem

type type_iterators =
  { it_signature: type_iterators -> signature -> unit;
    it_signature_item: type_iterators -> signature_item -> unit;
    it_value_description: type_iterators -> value_description -> unit;
    it_type_declaration: type_iterators -> type_declaration -> unit;
    it_module_declaration: type_iterators -> module_declaration -> unit;
    it_modtype_declaration: type_iterators -> modtype_declaration -> unit;
    it_module_type: type_iterators -> module_type -> unit;
    it_type_kind: type_iterators -> type_kind -> unit;
    it_do_type_expr: type_iterators -> type_expr -> unit;
    it_type_expr: type_iterators -> type_expr -> unit;
    it_path: Path.t -> unit; }

let iter_type_expr_cstr_args f = function
  | TConstrTuple tl -> List.iter f tl
  | TConstrSingleton -> ()

let map_type_expr_cstr_args f = function
  | TConstrSingleton -> TConstrSingleton
  | TConstrTuple tl -> TConstrTuple (List.map f tl)

let iter_type_expr_kind f = function
  | TDataAbstract -> ()
  | TDataVariant cstrs -> List.iter (fun cd ->
      iter_type_expr_cstr_args f cd.cd_args)
      cstrs
  | TDataRecord fields -> List.iter (fun {rf_type} -> f rf_type) fields


let type_iterators =
  let it_signature it =
    List.iter (it.it_signature_item it)
  and it_signature_item it = function
    | TSigValue (_, vd)     -> it.it_value_description it vd
    | TSigType (_, td, _)   -> it.it_type_declaration it td
    | TSigModule (_, md, _) -> it.it_module_declaration it md
    | TSigModType (_, mtd)  -> it.it_modtype_declaration it mtd
  and it_value_description it vd =
    it.it_type_expr it vd.val_type
  and it_type_declaration it td =
    List.iter (it.it_type_expr it) td.type_params;
    it.it_type_kind it td.type_kind
  and it_module_type it = function
    | TModIdent p -> it.it_path p
    | TModAlias p -> it.it_path p
    | TModSignature sg -> it.it_signature it sg
  and it_type_kind it kind =
    iter_type_expr_kind (it.it_type_expr it) kind
  and it_module_declaration it md =
    it.it_module_type it md.md_type
  and it_modtype_declaration it mtd =
    may (it.it_module_type it) mtd.mtd_type
  and it_do_type_expr it ty =
    iter_type_expr (it.it_type_expr it) ty;
    match ty.desc with
    | TTyConstr(p, _, _) ->
        it.it_path p
    | _ -> ()
  and it_path _p = ()
  in
  { it_signature; it_signature_item;
    it_path; it_type_expr = it_do_type_expr; it_do_type_expr;
    it_type_kind; it_module_type;
    it_type_declaration; it_value_description;
    it_module_declaration; it_modtype_declaration;
  }

let copy_commu c =
  if commu_repr c = TComOk then TComOk else TComLink (ref TComUnknown)

(* Since univars may be used as row variables, we need to do some
   encoding during substitution *)
let rec norm_univar ty =
  match ty.desc with
  | TTyLink ty -> norm_univar ty
  | TTyUniVar _
  | TTySubst _ -> ty
  | TTyTuple (ty :: _) -> norm_univar ty
  | TTyRecord _
  | TTyVar _
  | TTyArrow _
  | TTyTuple _
  | TTyConstr _
  | TTyPoly _ -> assert false

let rec copy_type_desc ?(keep_names=false) f = function
  | TTyVar _ as ty -> if keep_names then ty else TTyVar None
  | TTyArrow(tyl, ret, c) -> TTyArrow(List.map f tyl, f ret, copy_commu c)
  | TTyTuple l -> TTyTuple (List.map f l)
  | TTyRecord l -> TTyRecord (List.map (fun (name, arg) -> (name, f arg)) l)
  | TTyConstr(p, l, _) -> TTyConstr(p, List.map f l, ref TMemNil)
  | TTyUniVar _ as ty -> ty
  | TTySubst _ -> assert false
  | TTyLink ty -> copy_type_desc f ty.desc
  | TTyPoly(ty, tyl) ->
    let tyl = List.map (fun x -> norm_univar (f x)) tyl in
    TTyPoly(f ty, tyl)

(* Utilities for copying *)

let saved_desc = ref []
  (* Saved association of generic nodes with their description. *)

let save_desc ty desc =
  saved_desc := (ty, desc)::!saved_desc

let saved_kinds = ref [] (* duplicated kind variables *)
let new_kinds = ref []   (* new kind variables *)

(* Restored type descriptions. *)
let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  List.iter (fun r -> r := None) !saved_kinds;
  saved_desc := []; saved_kinds := []; new_kinds := []

(* Mark a type. *)
let rec mark_type ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr mark_type ty
  end

let mark_type_node ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
  end

let mark_type_params ty =
  iter_type_expr mark_type ty

let type_iterators =
  let it_type_expr it ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      mark_type_node ty;
      it.it_do_type_expr it ty;
    end
  in
  {type_iterators with it_type_expr}


(* Remove marks from a type. *)
let rec unmark_type ty =
  let ty = repr ty in
  if ty.level < lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr unmark_type ty
  end

let unmark_iterators =
  let it_type_expr _it ty = unmark_type ty in
  {type_iterators with it_type_expr}

let unmark_type_decl decl =
  unmark_iterators.it_type_declaration unmark_iterators decl


                  (*******************************************)
                  (*  Memorization of abbreviation expansion *)
                  (*******************************************)

(* Search whether the expansion has been memorized. *)

let rec find_expans priv p1 = function
  | TMemNil -> None
  | TMemCons(p2, _ty0, ty, _) when Path.same p1 p2 -> Some ty
  | TMemCons(_, _, _, rem)   -> find_expans priv p1 rem
  | TMemLink {contents = rem} -> find_expans priv p1 rem

let memo = ref []
        (* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
        (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := TMemNil) !memo;
  memo := []

let memorize_abbrev mem priv path v v' =
        (* Memorize the expansion of an abbreviation. *)
  mem := TMemCons(path, v, v', !mem);
  (* check_expans [] v; *)
  memo := mem :: !memo

let rec forget_abbrev_rec mem path =
  match mem with
  | TMemNil -> assert false
  | TMemCons(path', _, _, rem) when Path.same path path' -> rem
  | TMemCons(path', v, v', rem) -> TMemCons(path', v, v', forget_abbrev_rec rem path)
  | TMemLink mem' ->
    mem' := forget_abbrev_rec !mem' path;
    raise Exit

let forget_abbrev mem path =
  try mem := forget_abbrev_rec !mem path with Exit -> ()

                  (**********************************)
                  (*  Utilities for labels          *)
                  (**********************************)

let is_optional = function Optional _ -> true | _ -> false

let label_name = function
    Nolabel -> ""
  | Labelled s
  | Optional s -> s

let prefixed_label_name = function
    Nolabel -> ""
  | Labelled s -> "~" ^ s
  | Optional s -> "?" ^ s

let rec extract_label_aux hd l = function
    [] -> raise Not_found
  | (l',t as p) :: ls ->
      if label_name l' = l then (l', t, List.rev hd, ls)
      else extract_label_aux (p::hd) l ls

let extract_label l ls = extract_label_aux [] l ls


                  (**********************************)
                  (*  Utilities for backtracking    *)
                  (**********************************)

let undo_change = function
    Ctype  (ty, desc) -> ty.desc <- desc
  | Ccompress  (ty, desc, _) -> ty.desc <- desc
  | Clevel (ty, level) -> ty.level <- level
  | Cname  (r, v) -> r := v
  | Ccommu (r, v) -> r := v
  | Cuniv  (r, v) -> r := v
  | Ctypeset (r, v) -> r := v

type snapshot = changes ref * int
let last_snapshot = ref 0

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  log_type ty;
  let desc = ty.desc in
  ty.desc <- TTyLink ty';
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match desc, ty'.desc with
    TTyVar name, TTyVar name' ->
      begin match name, name' with
      | Some _, None ->  log_type ty'; ty'.desc <- TTyVar name
      | None, Some _ ->  ()
      | Some _, Some _ ->
          if ty.level < ty'.level then (log_type ty'; ty'.desc <- TTyVar name)
      | None, None   ->  ()
      end
  | _ -> ()
  (* ; assert (check_memorized_abbrevs ()) *)
  (*  ; check_expans [] ty' *)
let set_level ty level =
  if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
  ty.level <- level
let set_univar rty ty =
  log_change (Cuniv (rty, !rty)); rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm)); nm := v
let set_commu rc c =
  log_change (Ccommu (rc, !rc)); rc := c
let set_typeset rs s =
  log_change (Ctypeset (rs, !rs)); rs := s

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  match Weak.get trail 0 with Some r -> (r, old)
  | None ->
      let r = ref Unchanged in
      Weak.set trail 0 (Some r);
      (r, old)

let rec rev_log accu = function
    Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
      let d = !next in
      next := Invalid;
      rev_log (ch::accu) d

let backtrack (changes, old) =
  match !changes with
    Unchanged -> last_snapshot := old
  | Invalid -> failwith "Btype.backtrack"
  | Change _ as change ->
      cleanup_abbrev ();
      let backlog = rev_log [] change in
      List.iter undo_change backlog;
      changes := Unchanged;
      last_snapshot := old;
      Weak.set trail 0 (Some changes)

let rec rev_compress_log log r =
  match !r with
    Unchanged | Invalid ->
      log
  | Change (Ccompress _, next) ->
      rev_compress_log (r::log) next
  | Change (_, next) ->
      rev_compress_log log next

let undo_compress (changes, _old) =
  match !changes with
    Unchanged
  | Invalid -> ()
  | Change _ ->
      let log = rev_compress_log [] changes in
      List.iter
        (fun r -> match !r with
          Change (Ccompress (ty, desc, d), next) when ty.desc == d ->
            ty.desc <- desc; r := !next
        | _ -> ())
        log
