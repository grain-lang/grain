(** Typing of patterns *)
(* Taken from OCaml's typing/typecore.ml module. Original copyright: *)
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
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype
open Checkertypes
open Disambiguation

type error =
  | ConstructorArityMismatch of Identifier.t * int * int
  | PatternTypeClash of (type_expr * type_expr) list
  | ExprTypeClash of (type_expr * type_expr) list * type_forcing_context option
  | RecursiveLocalConstraint of (type_expr * type_expr) list
  | MultiplyBoundVariable of string
  | ModulesNotAllowed
  | UnexpectedExistential

exception Error of Location.t * Env.t * error

let iter_ppat f p =
  match p.ppat_desc with
  | PPatAny
  | PPatVar _
  | PPatConstant _ -> ()
  | PPatTuple lst -> List.iter f lst
  | PPatConstraint (p,_) -> f p
  | PPatConstruct (_, lst) -> List.iter f lst


let map_fold_cont f xs k =
  List.fold_right (fun x k ys -> f x (fun y -> k (y :: ys)))
    xs (fun ys -> k (List.rev ys)) []

(* unification inside type_pat*)
let unify_pat_types loc env ty ty' =
  try
    unify env ty ty'
  with
    Unify trace ->
      raise(Error(loc, env, PatternTypeClash(trace)))

(* unification inside type_exp and type_expect *)
let unify_exp_types loc env ty expected_ty =
  (* Format.eprintf "@[%a@ %a@]@." Printtyp.raw_type_expr exp.exp_type
    Printtyp.raw_type_expr expected_ty; *)
  try
    unify env ty expected_ty
  with
    Unify trace ->
      raise(Error(loc, env, ExprTypeClash(trace, None)))

(* level at which to create the local type declarations *)
let newtype_level = ref None
let get_newtype_level () =
  match !newtype_level with
    Some y -> y
  | None -> assert false

let unify_pat_types_gadt loc env ty ty' =
  let newtype_level =
    match !newtype_level with
    | None -> assert false
    | Some x -> x
  in
  try
    unify_gadt ~newtype_level env ty ty'
  with
    Unify trace ->
      raise(Error(loc, !env, PatternTypeClash(trace)))
  | Unification_recursive_abbrev trace ->
      raise(Error(loc, !env, RecursiveLocalConstraint trace))


(* Creating new conjunctive types is not allowed when typing patterns *)

let unify_pat env pat expected_ty =
  unify_pat_types pat.pat_loc env pat.pat_type expected_ty

(* make all Reither present in open variants *)
let finalize_variant pat =
  match pat.pat_desc with
  | _ -> ()

let rec iter_pattern f p =
  f p;
  iter_pattern_desc (iter_pattern f) p.pat_desc

let has_variants p =
  false


(* pattern environment *)
let pattern_variables = ref ([] :
 (Ident.t * type_expr * string loc * Location.t * bool (* as-variable *)) list)
let pattern_force = ref ([] : (unit -> unit) list)
let pattern_scope = ref (None : Annot.ident option);;
let allow_modules = ref false
let module_variables = ref ([] : (string loc * Location.t) list)
let reset_pattern scope allow =
  pattern_variables := [];
  pattern_force := [];
  pattern_scope := scope;
  allow_modules := allow;
  module_variables := [];
;;

let enter_variable ?(is_module=false) ?(is_as_variable=false) loc name ty =
  if List.exists (fun (id, _, _, _, _) -> Ident.name id = name.txt)
      !pattern_variables
  then raise(Error(loc, Env.empty, MultiplyBoundVariable name.txt));
  let id = Ident.create name.txt in
  pattern_variables :=
    (id, ty, name, loc, is_as_variable) :: !pattern_variables;
  if is_module then begin
    (* Note: unpack patterns enter a variable of the same name *)
    if not !allow_modules then
      raise (Error (loc, Env.empty, ModulesNotAllowed));
    module_variables := (name, loc) :: !module_variables
  end else
    (* moved to genannot *)
    (*may (fun s -> Stypes.record (Stypes.An_ident (name.loc, name.txt, s)))
        !pattern_scope;*)
    ();
  id

let sort_pattern_variables vs =
  List.sort
    (fun (x,_,_,_,_) (y,_,_,_,_) ->
      Pervasives.compare (Ident.name x) (Ident.name y))
    vs

(*let enter_orpat_variables loc env  p1_vs p2_vs =
  (* unify_vars operate on sorted lists *)

  let p1_vs = sort_pattern_variables p1_vs
  and p2_vs = sort_pattern_variables p2_vs in

  let rec unify_vars p1_vs p2_vs =
    let vars vs = List.map (fun (x,_t,_,_l,_a) -> x) vs in
    match p1_vs, p2_vs with
      | (x1,t1,_,_l1,_a1)::rem1, (x2,t2,_,_l2,_a2)::rem2
        when Ident.equal x1 x2 ->
          if x1==x2 then
            unify_vars rem1 rem2
          else begin
            begin try
              unify env t1 t2
            with
            | Unify trace ->
                raise(Error(loc, env, Or_pattern_type_clash(x1, trace)))
            end;
          (x2,x1)::unify_vars rem1 rem2
          end
      | [],[] -> []
      | (x,_,_,_,_)::_, [] -> raise (Error (loc, env, Orpat_vars (x, [])))
      | [],(y,_,_,_,_)::_  -> raise (Error (loc, env, Orpat_vars (y, [])))
      | (x,_,_,_,_)::_, (y,_,_,_,_)::_ ->
          let err =
            if Ident.name x < Ident.name y
            then Orpat_vars (x, vars p2_vs)
            else Orpat_vars (y, vars p1_vs) in
          raise (Error (loc, env, err)) in
  unify_vars p1_vs p2_vs
*)

(* Remember current state for backtracking.
   No variable information, as we only backtrack on
   patterns without variables (cf. assert statements). *)
type state =
 { snapshot: Btype.snapshot;
   levels: Ctype.levels;
   env: Env.t; }
let save_state env =
  { snapshot = Btype.snapshot ();
    levels = Ctype.save_levels ();
    env = !env; }
let set_state s env =
  Btype.backtrack s.snapshot;
  Ctype.set_levels s.levels;
  env := s.env

(* type_pat does not generate local constraints inside or patterns *)
type type_pat_mode =
  | Normal
  | Splitting_or   (* splitting an or-pattern *)
  | Inside_or      (* inside a non-split or-pattern *)
  | Split_or       (* always split or-patterns *)

exception Need_backtrack

(* type_pat propagates the expected type as well as maps for
   constructors and labels.
   Unification may update the typing environment. *)
(* constrs <> None => called from parmatch: backtrack on or-patterns
   explode > 0 => explode Ppat_any for gadts *)
let rec type_pat ~constrs ~labels ~no_existentials ~mode ~explode ~env
    sp expected_ty k =
  type_pat_aux ~constrs ~labels ~no_existentials ~mode ~explode ~env
    sp expected_ty k
  (*Builtin_attributes.warning_scope sp.ppat_attributes
    (fun () ->
       type_pat_aux ~constrs ~labels ~no_existentials ~mode ~explode ~env
         sp expected_ty k
    )*)

and type_pat_aux ~constrs ~labels ~no_existentials ~mode ~explode ~env
    sp expected_ty k =
  let mode' = if mode = Splitting_or then Normal else mode in
  let type_pat ?(constrs=constrs) ?(labels=labels) ?(mode=mode')
      ?(explode=explode) ?(env=env) =
    type_pat ~constrs ~labels ~no_existentials ~mode ~explode ~env in
  let loc = sp.ppat_loc in
  let rp k x : pattern = if constrs = None then k (rp x) else k x in
  match sp.ppat_desc with
  | PPatAny ->
      let k' d = rp k {
        pat_desc = d;
        pat_loc = loc; pat_extra=[];
        pat_type = expected_ty;
        pat_env = !env }
      in
      if explode > 0 then
        let (sp, constrs, labels) = Parmatch.ppat_of_type !env expected_ty in
        if sp.ppat_desc = Parsetree.PPatAny then k' TPatAny else
        if mode = Inside_or then raise Need_backtrack else
        let explode =
          match sp.ppat_desc with
            (*Parsetree.Ppat_or _ -> explode - 5*)
          | _ -> explode - 1
        in
        type_pat ~constrs:(Some constrs) ~labels:(Some labels)
          ~explode sp expected_ty k
      else k' TPatAny
  | PPatVar name ->
      let id = (* PR#7330 *)
        if name.txt = "*extension*" then Ident.create name.txt else
        enter_variable loc name expected_ty
      in
      rp k {
        pat_desc = TPatVar (id, name);
        pat_loc = loc; pat_extra=[];
        pat_type = expected_ty;
        pat_env = !env }
  (*| PPatConstraint({ppat_desc=PPatVar name; ppat_loc=lloc},
                   ({ptyp_desc=PTypPoly _} as sty)) ->
      (* explicitly polymorphic type *)
      assert (constrs = None);
      let cty, force = Typetexp.transl_simple_type_delayed !env sty in
      let ty = cty.ctyp_type in
      unify_pat_types lloc !env ty expected_ty;
      pattern_force := force :: !pattern_force;
      begin match ty.desc with
      | Tpoly (body, tyl) ->
          begin_def ();
          let _, ty' = instance_poly ~keep_names:true false tyl body in
          end_def ();
          generalize ty';
          let id = enter_variable lloc name ty' in
          rp k {
            pat_desc = Tpat_var (id, name);
            pat_loc = lloc;
            pat_extra = [Tpat_constraint cty, loc, sp.ppat_attributes];
            pat_type = ty;
            pat_attributes = [];
            pat_env = !env
          }
      | _ -> assert false
      end*)
  | PPatConstant cst ->
      let cst = constant_or_raise !env loc cst in
      unify_pat_types loc !env (type_constant cst) expected_ty;
      rp k {
        pat_desc = TPatConstant cst;
        pat_loc = loc; pat_extra=[];
        pat_type = expected_ty;
        pat_env = !env }
  | PPatTuple spl ->
      assert (List.length spl >= 2);
      let spl_ann = List.map (fun p -> (p,newvar ())) spl in
      let ty = newty (TTyTuple(List.map snd spl_ann)) in
      unify_pat_types loc !env ty expected_ty;
      map_fold_cont (fun (p,t) -> type_pat p t) spl_ann (fun pl ->
        rp k {
        pat_desc = TPatTuple pl;
        pat_loc = loc; pat_extra=[];
        pat_type = expected_ty;
        pat_env = !env })
  | PPatConstruct(lid, sargs) ->
      let opath =
        (*try
          let (p0, p, _) = extract_concrete_variant !env expected_ty in
            Some (p0, p, true)
        with Not_found ->*) None
      in
      let candidates =
        match lid.txt, constrs with
          Identifier.IdentName s, Some constrs when Hashtbl.mem constrs s ->
            [Hashtbl.find constrs s, (fun () -> ())]
        | _ ->  Typetexp.find_all_constructors !env lid.loc lid.txt
      in
      (*let check_lk tpath constr =
        if constr.cstr_generalized then
          raise (Error (lid.loc, !env,
                        Unqualified_gadt_pattern (tpath, constr.cstr_name)))
      in*)
      let constr =
        wrap_disambiguate "This variant pattern is expected to have"
          (mk_expected expected_ty)
          (Constructor.disambiguate lid !env opath) candidates
      in
      (*if constr.cstr_generalized && constrs <> None && mode = Inside_or
      then raise Need_backtrack;*)
      (*Env.mark_constructor Env.Pattern !env (Identifier.last lid.txt) constr;*)
      (*Builtin_attributes.check_deprecated loc constr.cstr_attributes
        constr.cstr_name;*)
      if no_existentials && constr.cstr_existentials <> [] then
        raise (Error (loc, !env, UnexpectedExistential));
      (* if constructor is gadt, we must verify that the expected type has the
         correct head *)
      (*if constr.cstr_generalized then
        unify_head_only loc !env expected_ty constr;*)
      (*begin match sargs with
      | [{ppat_desc = Ppat_constant _} as sp]
        when Builtin_attributes.warn_on_literal_pattern
            constr.cstr_attributes ->
          Location.prerr_warning sp.ppat_loc
            Warnings.Fragile_literal_pattern
      | _ -> ()
      end;*)
      if List.length sargs <> constr.cstr_arity then
        raise(Error(loc, !env, ConstructorArityMismatch(lid.txt,
                                     constr.cstr_arity, List.length sargs)));
      let (ty_args, ty_res) =
        instance_constructor ~in_pattern:(env, get_newtype_level ()) constr
      in
      (* PR#7214: do not use gadt unification for toplevel lets *)
      (*if not constr.cstr_generalized || mode = Inside_or || no_existentials
      then unify_pat_types loc !env ty_res expected_ty
      else unify_pat_types_gadt loc env ty_res expected_ty;

      let rec check_non_escaping p =
        match p.ppat_desc with
        | Ppat_or (p1, p2) ->
            check_non_escaping p1;
            check_non_escaping p2
        | Ppat_alias (p, _) ->
            check_non_escaping p
        | PPatConstraint _ ->
            raise (Error (p.ppat_loc, !env, Inlined_record_escape))
        | _ ->
            ()
      in
      if constr.cstr_inlined <> None then List.iter check_non_escaping sargs;*)

      map_fold_cont (fun (p,t) -> type_pat p t) (List.combine sargs ty_args)
      (fun args ->
        rp k {
          pat_desc=TPatConstruct(lid, constr, args);
          pat_loc = loc; pat_extra=[];
          pat_type = expected_ty;
          pat_env = !env })
  | PPatConstraint(sp, sty) ->
    (* Separate when not already separated by !principal *)
    failwith "NYI: typepat > type_pat_aux > PPatConstraint (needs TyAlias)"
      (*let separate = true in
      if separate then begin_def();
      let cty, force = Typetexp.transl_simple_type_delayed !env sty in
      let ty = cty.ctyp_type in
      let ty, expected_ty' =
        if separate then begin
          end_def();
          generalize_structure ty;
          instance !env ty, instance !env ty
        end else ty, ty
      in
      unify_pat_types loc !env ty expected_ty;
      type_pat sp expected_ty' (fun p ->
        (*Format.printf "%a@.%a@."
          Printtyp.raw_type_expr ty
          Printtyp.raw_type_expr p.pat_type;*)
        pattern_force := force :: !pattern_force;
        let extra = (TPatConstraint cty, loc, sp.ppat_attributes) in
        let p =
          if not separate then p else
          match p.pat_desc with
            TPatVar (id,s) ->
              {p with pat_type = ty;
               pat_desc = Tpat_alias
                 ({p with pat_desc = Tpat_any; pat_attributes = []}, id,s);
               pat_extra = [extra];
             }
          | _ -> {p with pat_type = ty;
                  pat_extra = extra :: p.pat_extra}
        in k p)*)

let type_pat ?(allow_existentials=false) ?constrs ?labels ?(mode=Normal)
    ?(explode=0) ?(lev=get_current_level()) env sp expected_ty =
  newtype_level := Some lev;
  try
    let r =
      type_pat ~no_existentials:(not allow_existentials) ~constrs ~labels
        ~mode ~explode ~env sp expected_ty (fun x -> x) in
    iter_pattern (fun p -> p.pat_env <- !env) r;
    newtype_level := None;
    r
  with e ->
    newtype_level := None;
    raise e

let add_pattern_variables ?check ?check_as env =
  let pv = get_ref pattern_variables in
  (List.fold_right
     (fun (id, ty, _name, loc, as_var) env ->
       let check = if as_var then check_as else check in
       Env.add_value ?check id
         {val_type = ty; val_kind = TValReg; Types.val_loc = loc} env
     )
     pv env,
   get_ref module_variables, pv)

let type_pattern ~lev env spat scope expected_ty =
  reset_pattern (*scope*) None true;
  let new_env = ref env in
  let pat = type_pat ~allow_existentials:true ~lev new_env spat expected_ty in
  let new_env, unpacks, _ =
    add_pattern_variables !new_env
      (*~check:(fun s -> Warnings.Unused_var_strict s)
      ~check_as:(fun s -> Warnings.Unused_var s)*) in
  (pat, new_env, get_ref pattern_force, unpacks)

let type_pattern_list env spatl scope expected_tys allow =
  reset_pattern (*scope*) None allow;
  let new_env = ref env in
  let type_pat (attrs, pat) ty =
    (*Builtin_attributes.warning_scope ~ppwarning:false attrs
      (fun () ->
         type_pat new_env pat ty
      )*)
    type_pat new_env pat ty
  in
  let patl = List.map2 type_pat spatl expected_tys in
  let new_env, unpacks, pv = add_pattern_variables !new_env in
  (patl, new_env, get_ref pattern_force, unpacks, pv)
