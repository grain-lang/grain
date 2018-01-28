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

(* Detection of partial matches and unused match cases. *)

open Grain_parsing
open Misc
open Asttypes
open Types
open Typedtree


(*************************************)
(* Utilities for building patterns   *)
(*************************************)

let make_pat desc ty tenv =
  {pat_desc = desc; pat_loc = Location.dummy_loc; pat_extra = [];
   pat_type = ty ; pat_env = tenv;
  }

let omega = make_pat TPatAny Ctype.none Env.empty

let extra_pat =
  make_pat
    (TPatVar (Ident.create "+", mknoloc "+"))
    Ctype.none Env.empty

let rec omegas i =
  if i <= 0 then [] else omega :: omegas (i-1)

let omega_list l = List.map (fun _ -> omega) l

let zero = make_pat (TPatConstant (Const_int 0)) Ctype.none Env.empty


exception Empty (* Empty pattern *)



let pat_of_constr ex_pat cstr =
  {ex_pat with pat_desc =
   TPatConstruct(mknoloc (Identifier.IdentName "?pat_of_constr?"),
                 cstr, omegas cstr.cstr_arity)}

(*let orify x y = make_pat (Tpat_or (x, y, None)) x.pat_type x.pat_env*)

let rec orify_many = function
| [] -> assert false
| [x] -> x
| x :: xs -> failwith "NYI: parmatch > orify_many" (*orify x (orify_many xs)*)

(* build an or-pattern from a constructor list *)
let pat_of_constrs ex_pat cstrs =
  if cstrs = [] then raise Empty else
  orify_many (List.map (pat_of_constr ex_pat) cstrs)

let pats_of_type ?(always=false) env ty =
  let ty' = Ctype.expand_head env ty in
  match ty'.desc with
  | TTyConstr (path, _, _) ->
      begin try match (Env.find_type path env).type_kind with
      | TDataVariant cl when always || List.length cl = 1 ||
        List.for_all (fun cd -> cd.Types.cd_res <> None) cl ->
          let cstrs = (Env.find_type_descrs path env) in
          List.map (pat_of_constr (make_pat TPatAny ty env)) cstrs
      | _ -> [omega]
      with Not_found -> [omega]
      end
  | TTyTuple tl ->
      [make_pat (TPatTuple (omegas (List.length tl))) ty env]
  | _ -> [omega]


(************************)
(* Exhaustiveness check *)
(************************)

(* FIXME: If we port over untypeast, then use its function instead *)
let untype_constant = function
  | Const_int i -> Parsetree.PConstNumber i
  | Const_string s -> Parsetree.PConstString s
  | Const_bool b -> Parsetree.PConstBool b
  | _ -> failwith "NYI untype_constant"

(* conversion from Typedtree.pattern to Parsetree.pattern list *)
module Conv = struct
  open Parsetree
  let mkpat desc = Ast_helper.Pat.mk desc

  let name_counter = ref 0
  let fresh name =
    let current = !name_counter in
    name_counter := !name_counter + 1;
    "#$" ^ name ^ string_of_int current

  let conv typed =
    let constrs = Hashtbl.create 7 in
    let labels = Hashtbl.create 7 in
    let rec loop pat =
      match pat.pat_desc with
      | TPatAny
      | TPatVar _ ->
          mkpat PPatAny
      | TPatConstant c ->
          mkpat (PPatConstant (untype_constant c))
      | TPatTuple lst ->
          mkpat (PPatTuple (List.map loop lst))
      | TPatConstruct (cstr_lid, cstr, lst) ->
          let id = fresh cstr.cstr_name in
          let lid = { cstr_lid with txt = Identifier.IdentName id } in
          Hashtbl.add constrs id cstr;
          mkpat (PPatConstruct(lid, List.map loop lst))
    in
    let ps = loop typed in
    (ps, constrs, labels)
end


(* Whether the counter-example contains an extension pattern *)
let contains_extension pat =
  false

(* Build an untyped or-pattern from its expected type *)
let ppat_of_type env ty =
  match pats_of_type env ty with
    [{pat_desc = TPatAny}] ->
      (Conv.mkpat Parsetree.PPatAny, Hashtbl.create 0, Hashtbl.create 0)
  | pats ->
      Conv.conv (orify_many pats)
