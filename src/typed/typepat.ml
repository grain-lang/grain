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
open Grain_utils
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
  | LabelMismatch of Identifier.t * (type_expr * type_expr) list
  | LabelMultiplyDefined of string
  | ExprTypeClash of (type_expr * type_expr) list * type_forcing_context option
  | RecursiveLocalConstraint of (type_expr * type_expr) list
  | MultiplyBoundVariable of string
  | ModulesNotAllowed
  | UnexpectedExistential
  | OrpatVars of Ident.t * Ident.t list
  | OrPatternTypeClash of Ident.t * (type_expr * type_expr) list
  | UnrefutedPattern of pattern

exception Error of Location.t * Env.t * error

let iter_ppat f p =
  match p.ppat_desc with
  | PPatAny
  | PPatVar _
  | PPatConstant _ -> ()
  | PPatTuple lst -> List.iter f lst
  | PPatRecord (fs, _) -> List.iter (fun (_, p) -> f p) fs
  | PPatAlias (p, _)
  | PPatConstraint (p,_) -> f p
  | PPatConstruct (_, lst) -> List.iter f lst
  | PPatOr(p1, p2) -> f p1; f p2


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
      Stdlib.compare (Ident.name x) (Ident.name y))
    vs

let enter_orpat_variables loc env  p1_vs p2_vs =
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
                raise(Error(loc, env, OrPatternTypeClash(x1, trace)))
            end;
          (x2,x1)::unify_vars rem1 rem2
          end
      | [],[] -> []
      | (x,_,_,_,_)::_, [] -> raise (Error (loc, env, OrpatVars (x, [])))
      | [],(y,_,_,_,_)::_  -> raise (Error (loc, env, OrpatVars (y, [])))
      | (x,_,_,_,_)::_, (y,_,_,_,_)::_ ->
          let err =
            if Ident.name x < Ident.name y
            then OrpatVars (x, vars p2_vs)
            else OrpatVars (y, vars p1_vs) in
          raise (Error (loc, env, err)) in
  unify_vars p1_vs p2_vs

let rec build_as_type env p =
  match p.pat_desc with
  | TPatAlias(p1, _, _) -> build_as_type env p1
  | TPatTuple(pl) ->
    let tyl = List.map (build_as_type env) pl in
    newty (TTyTuple tyl)
  | TPatRecord (lpl, _) ->
      let lbl = snd3 (List.hd lpl) in
      let ty = newvar () in
      let ppl = List.map (fun (_, l, p) -> l.lbl_pos, p) lpl in
      let do_label lbl =
        let _, ty_arg, ty_res = instance_label false lbl in
        unify_pat env {p with pat_type = ty} ty_res;
        let refinable =
          List.mem_assoc lbl.lbl_pos ppl &&
          match (repr lbl.lbl_arg).desc with TTyPoly _ -> false | _ -> true in
        if refinable then begin
          let arg = List.assoc lbl.lbl_pos ppl in
          unify_pat env {arg with pat_type = build_as_type env arg} ty_arg
        end else begin
          let _, ty_arg', ty_res' = instance_label false lbl in
          unify env ty_arg ty_arg';
          unify_pat env p ty_res'
        end in
      Array.iter do_label lbl.lbl_all;
      ty
  | TPatConstruct(_, cstr, pl) ->
    let keep = cstr.cstr_existentials <> [] in
    if keep then p.pat_type else
      let tyl = List.map (build_as_type env) pl in
      let ty_args, ty_res = instance_constructor cstr in
      List.iter2 (fun (p, ty) -> unify_pat env {p with pat_type = ty})
        (List.combine pl tyl) ty_args;
      ty_res
  | TPatOr(p1, p2) ->
    let ty1 = build_as_type env p1 and ty2 = build_as_type env p2 in
    unify_pat env {p2 with pat_type = ty2} ty1;
    ty1
  | TPatAny
  | TPatVar _
  | TPatConstant _ -> p.pat_type

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

let extract_concrete_variant env ty =
  match extract_concrete_typedecl env ty with
  | (p0, p, {type_kind=TDataVariant cstrs}) -> (p0, p, cstrs)
  | _ -> raise Not_found

let extract_concrete_record env ty =
  match extract_concrete_typedecl env ty with
    (p0, p, {type_kind=TDataRecord fields}) -> (p0, p, fields)
  | _ -> raise Not_found

let extract_label_names env ty =
  try
    let (_, _,fields) = extract_concrete_record env ty in
    List.map (fun l -> l.Types.rf_name) fields
  with Not_found ->
    assert false

(* Checks over the labels mentioned in a record pattern:
   no duplicate definitions (error); properly closed (warning) *)

let check_recordpat_labels loc lbl_pat_list closed =
  match lbl_pat_list with
  | [] -> ()                            (* should not happen *)
  | (_, label1, _) :: _ ->
      let all = label1.lbl_all in
      let defined = Array.make (Array.length all) false in
      let check_defined (_, label, _) =
        if defined.(label.lbl_pos)
        then raise(Error(loc, Env.empty, LabelMultiplyDefined label.lbl_name))
        else defined.(label.lbl_pos) <- true in
      List.iter check_defined lbl_pat_list;
      if closed = Closed && Warnings.is_active (Warnings.NonClosedRecordPattern "")
      then begin
        let undefined = ref [] in
        for i = 0 to Array.length all - 1 do
          if not defined.(i) then undefined := all.(i).lbl_name :: !undefined
        done;
        if !undefined <> [] then begin
          let u = String.concat ", " (List.rev !undefined) in
          Location.prerr_warning loc (Warnings.NonClosedRecordPattern u)
        end
      end

let rec find_record_qual = function
  | [] -> None
  | ({ txt = Identifier.IdentExternal (modname, _) }, _) :: _ -> Some modname
  | _ :: rest -> find_record_qual rest
  
let type_label_a_list ?labels loc closed env type_lbl_a opath lid_a_list k =
  let lbl_a_list =
    match lid_a_list, labels with
      ({txt=Identifier.IdentName s}, _)::_, Some labels when Hashtbl.mem labels s ->
        (* Special case for rebuilt syntax trees *)
        List.map
          (function lid, a -> match lid.txt with
            Identifier.IdentName s -> lid, Hashtbl.find labels s, a
          | _ -> assert false)
          lid_a_list
    | _ ->
        let lid_a_list =
          match find_record_qual lid_a_list with
            None -> lid_a_list
          | Some modname ->
              List.map
                (fun (lid, a as lid_a) ->
                  match lid.txt with Identifier.IdentName s ->
                    {lid with txt=Identifier.IdentExternal (modname, s)}, a
                  | _ -> lid_a)
                lid_a_list
        in
        disambiguate_lid_a_list loc closed env opath lid_a_list
  in
  (* Invariant: records are sorted in the typed tree *)
  let lbl_a_list =
    List.sort
      (fun (_,lbl1,_) (_,lbl2,_) -> compare lbl1.lbl_pos lbl2.lbl_pos)
      lbl_a_list
  in
  map_fold_cont type_lbl_a lbl_a_list k

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
  let unif (x : pattern) : pattern =
    unify_pat !env x (instance !env expected_ty);
    x
  in
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
        let (sp, constrs) = Parmatch.ppat_of_type !env expected_ty in
        if sp.ppat_desc = Parsetree.PPatAny then k' TPatAny else
        if mode = Inside_or then raise Need_backtrack else
        let explode =
          match sp.ppat_desc with
            (*Parsetree.Ppat_or _ -> explode - 5*)
          | _ -> explode - 1
        in
        type_pat ~constrs:(Some constrs) (*~labels:(Some labels)*)
          ~explode sp expected_ty k
      else k' TPatAny
  | PPatVar name ->
    let (_, exp_constrs) = Parmatch.ppat_of_type !env expected_ty in
      let constructor_candidates =
        match name.txt, constrs with
          s, Some constrs when Hashtbl.mem exp_constrs s ->
            [Hashtbl.find exp_constrs s, (fun () -> ())]
        | _ -> Typetexp.find_all_constructors !env name.loc (Identifier.IdentName name.txt)
      in
      (* Special case: If the name shadows a constructor, assume it's a constructor use. *)
      begin match constructor_candidates with
        | _ :: _ ->
          if !Grain_utils.Config.verbose then
            Printf.eprintf "Re-interpreting pattern variable '%s' as a constructor\n" name.txt;
          type_pat_aux ~constrs ~labels ~no_existentials ~mode ~explode ~env
            {sp with ppat_desc=PPatConstruct(Location.mkloc (Identifier.IdentName name.txt) name.loc, [])} expected_ty k
        | [] ->
          let id = (* PR#7330 *)
            if name.txt = "*extension*" then Ident.create name.txt else
              enter_variable loc name expected_ty
          in
          rp k {
            pat_desc = TPatVar (id, name);
            pat_loc = loc; pat_extra=[];
            pat_type = expected_ty;
            pat_env = !env }
      end
  | PPatConstraint({ppat_desc=PPatVar name; ppat_loc=lloc},
                   ({ptyp_desc=PTyPoly _} as sty)) ->
      (* explicitly polymorphic type *)
      assert (constrs = None);
      let cty, force = Typetexp.transl_simple_type_delayed !env sty in
      let ty = cty.ctyp_type in
      unify_pat_types lloc !env ty expected_ty;
      pattern_force := force :: !pattern_force;
      begin match ty.desc with
      | TTyPoly (body, tyl) ->
          begin_def ();
          let _, ty' = instance_poly ~keep_names:true false tyl body in
          end_def ();
          generalize ty';
          let id = enter_variable lloc name ty' in
          rp k {
            pat_desc = TPatVar (id, name);
            pat_loc = lloc;
            pat_extra = [TPatConstraint cty, loc];
            pat_type = ty;
            pat_env = !env
          }
      | _ -> assert false
      end
  | PPatAlias(sq, name) ->
    assert (constrs = None);
    type_pat sq expected_ty (fun q ->
        begin_def();
        let ty_var = build_as_type !env q in
        end_def();
        generalize ty_var;
        let id = enter_variable ~is_as_variable:true loc name ty_var in
        rp k {
          pat_desc = TPatAlias(q, id, name);
          pat_loc = loc;
          pat_extra = [];
          pat_type = q.pat_type;
          pat_env = !env
        })
  | PPatConstant cst ->
      let cst = constant_or_raise !env loc cst in
      unify_pat_types loc !env (type_constant cst) expected_ty;
      rp k {
        pat_desc = TPatConstant cst;
        pat_loc = loc; pat_extra=[];
        pat_type = expected_ty;
        pat_env = !env }
  | PPatTuple spl ->
      (*assert (List.length spl >= 2);*)
      let spl_ann = List.map (fun p -> (p,newvar ())) spl in
      let ty = newty (TTyTuple(List.map snd spl_ann)) in
      unify_pat_types loc !env ty expected_ty;
      map_fold_cont (fun (p,t) -> type_pat p t) spl_ann (fun pl ->
        rp k {
        pat_desc = TPatTuple pl;
        pat_loc = loc; pat_extra=[];
        pat_type = expected_ty;
        pat_env = !env })
  | PPatRecord (lid_pat_list, closed) ->
    assert (lid_pat_list <> []);
      let opath, record_ty =
        try
          let (p0, p,_) = extract_concrete_record !env expected_ty in
          begin_def ();
          let ty = instance !env expected_ty in
          end_def ();
          generalize_structure ty;
          Some (p0, p, true), ty
        with Not_found -> None, newvar ()
      in
      let type_label_pat (label_lid, label, sarg) k =
        begin_def ();
        let (_, ty_arg, ty_res) = instance_label false label in
        begin try
          unify_pat_types loc !env ty_res (instance !env record_ty)
        with Error(_loc, _env, PatternTypeClash(cl)) ->
          raise(Error(label_lid.loc, !env,
                      LabelMismatch(label_lid.txt, cl)))
        end;
        end_def ();
        generalize_structure ty_res;
        generalize_structure ty_arg;
        type_pat sarg ty_arg (fun arg ->
          k (label_lid, label, arg))
      in
      let make_record_pat lbl_pat_list =
        check_recordpat_labels loc lbl_pat_list closed;
        {
          pat_desc = TPatRecord (lbl_pat_list, closed);
          pat_loc = loc; pat_extra=[];
          pat_type = instance !env record_ty;
          pat_env = !env;
        }
      in
      let k' pat = rp k (unif pat) in
      begin match mode with
      | Normal ->
          k' (wrap_disambiguate "This record pattern is expected to have"
               (mk_expected expected_ty)
               (type_label_a_list loc false !env type_label_pat opath
                  lid_pat_list)
               make_record_pat)
      | _ -> failwith "Counter examples NYI"
      (* | Counter_example {labels; _} ->
          type_label_a_list ~labels loc false !env type_label_pat opath
            lid_pat_list (fun lbl_pat_list -> k' (make_record_pat lbl_pat_list)) *)
      end
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
      then <copied below>
        else unify_pat_types_gadt loc env ty_res expected_ty;*)
      unify_pat_types loc !env ty_res expected_ty;

      (*let rec check_non_escaping p =
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
      if constr.cstr_inlined <> None then List.iter check_non_escaping sargs;**)

      map_fold_cont (fun (p,t) -> type_pat p t) (List.combine sargs ty_args)
      (fun args ->
        rp k {
          pat_desc=TPatConstruct(lid, constr, args);
          pat_loc = loc; pat_extra=[];
          pat_type = expected_ty;
          pat_env = !env })
  | PPatOr(sp1, sp2) ->
    let state = save_state env in
    begin match
        if mode = Split_or || mode = Splitting_or then raise Need_backtrack;
        let initial_pattern_variables = !pattern_variables in
        let initial_module_variables = !module_variables in
        let p1 =
          try Some (type_pat ~mode:Inside_or sp1 expected_ty (fun x -> x))
          with Need_backtrack -> None in
        let p1_variables = !pattern_variables in
        let p1_module_variables = !module_variables in
        pattern_variables := initial_pattern_variables;
        module_variables := initial_module_variables;
        let p2 =
          try Some (type_pat ~mode:Inside_or sp2 expected_ty (fun x -> x))
          with Need_backtrack -> None in
        let p2_variables = !pattern_variables in
        match p1, p2 with
        | None, None -> raise Need_backtrack
        | Some p, None
        | None, Some p -> p (* No variables *)
        | Some p1, Some p2 ->
          let alpha_env = enter_orpat_variables loc !env p1_variables p2_variables in
          pattern_variables := p1_variables;
          module_variables := p1_module_variables;
          { pat_desc = TPatOr(p1, alpha_pat alpha_env p2);
            pat_loc = loc; pat_extra=[];
            pat_type = expected_ty;
            pat_env = !env }
      with
      | p -> rp k p
      | exception Need_backtrack when mode <> Inside_or ->
        assert (constrs <> None);
        set_state state env;
        let mode = if mode = Split_or then mode else Splitting_or in
        try type_pat ~mode sp1 expected_ty k with Error _ ->
          set_state state env;
          type_pat ~mode sp2 expected_ty k
    end
  | PPatConstraint(sp, sty) ->
    (* Separate when not already separated by !principal *)
    let separate = true in
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
        let extra = (TPatConstraint cty, loc) in
        let p =
          if not separate then p else
            match p.pat_desc with
              TPatVar (id,s) ->
              {p with pat_type = ty;
                      pat_desc = TPatAlias
                          ({p with pat_desc = TPatAny}, id,s);
                      pat_extra = [extra];
              }
            | _ -> {p with pat_type = ty;
                           pat_extra = extra :: p.pat_extra}
        in k p)


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

(* this function is passed to Partial.parmatch
   to type check gadt nonexhaustiveness *)
let partial_pred ~lev ?mode ?explode env expected_ty constrs p =
  let env = ref env in
  let state = save_state env in
  try
    reset_pattern None true;
    let typed_p =
      Ctype.with_passive_variants
        (type_pat ~allow_existentials:true ~lev
           ~constrs ~labels:None ?mode ?explode env p)
        expected_ty
    in
    set_state state env;
    (* types are invalidated but we don't need them here *)
    Some typed_p
  with Error _ ->
    set_state state env;
    None

let check_partial ?(lev=get_current_level ()) env expected_ty loc cases =
  let explode = match cases with [_] -> 5 | _ -> 0 in
  Parmatch.check_partial
    (partial_pred ~lev ~explode env expected_ty) loc cases

let check_unused ?(lev=get_current_level ()) env expected_ty cases =
  Parmatch.check_unused
    (fun refute constrs spat ->
      match
        partial_pred ~lev ~mode:Split_or ~explode:5
          env expected_ty constrs spat
      with
        Some pat when refute ->
          raise (Error (spat.ppat_loc, env, UnrefutedPattern pat))
      | r -> r)
    cases


let add_pattern_variables ?check ?check_as env =
  let pv = get_ref pattern_variables in
  (List.fold_right
     (fun (id, ty, _name, loc, as_var) env ->
       let check = if as_var then check_as else check in
       Env.add_value ?check id
         {val_type = ty; val_kind = TValReg; Types.val_loc = loc; val_fullpath = Path.PIdent id} env
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
    (*Format.eprintf "@[Typing pat: %s@]@."
      (Sexplib.Sexp.to_string_hum (Parsetree.sexp_of_pattern pat));*)
    let ret = type_pat new_env pat ty in
    (*Format.eprintf "@[Typed: %s [type: %a]@]@."
      (Sexplib.Sexp.to_string_hum (Typedtree.sexp_of_pattern ret))
      Printtyp.raw_type_expr ret.pat_type;*)
    ret
  in
  let patl = List.map2 type_pat spatl expected_tys in
  let new_env, unpacks, pv = add_pattern_variables !new_env in
  (patl, new_env, get_ref pattern_force, unpacks, pv)

open Format
open Printtyp
let report_type_expected_explanation expl ppf =
  match expl with
  | If_conditional ->
      fprintf ppf "the condition of an if-statement"
  | If_no_else_branch ->
      fprintf ppf "the result of a conditional with no else branch"
  | While_loop_conditional ->
      fprintf ppf "the condition of a while-loop"
  | While_loop_body ->
      fprintf ppf "the body of a while-loop"
  | For_loop_start_index ->
      fprintf ppf "a for-loop start index"
  | For_loop_stop_index ->
      fprintf ppf "a for-loop stop index"
  | For_loop_body ->
      fprintf ppf "the body of a for-loop"
  | Assert_condition ->
      fprintf ppf "the condition of an assertion"
  | Sequence_left_hand_side ->
      fprintf ppf "the left-hand side of a sequence"
  | Assign_not_box ->
      fprintf ppf "the left-hand side of an assignment"
let report_type_expected_explanation_opt expl ppf =
  match expl with
  | None -> ()
  | Some expl ->
      fprintf ppf "@ because it is in %t"
        (report_type_expected_explanation expl)
let report_error env ppf = function
  | ConstructorArityMismatch(lid, expected, provided) ->
    fprintf ppf
      "@[The constructor %a@ expects %i argument(s),@ \
       but is applied here to %i argument(s)@]"
      identifier lid expected provided
  | PatternTypeClash(trace) ->
    report_unification_error ppf env trace
      (function ppf ->
         fprintf ppf "This pattern matches values of type")
      (function ppf ->
         fprintf ppf "but a pattern was expected which matches values of type")
  | ExprTypeClash(trace, explanation) ->
    report_unification_error ppf env trace
      ~type_expected_explanation:(report_type_expected_explanation_opt explanation)
      (function ppf ->
         fprintf ppf "This expression has type")
      (function ppf ->
         fprintf ppf "but a expression was expected of type")
  | LabelMismatch(lid, trace) ->
    report_unification_error ppf env trace
      (function ppf ->
        fprintf ppf "The record field %s belongs to the type"
                (Identifier.string_of_ident lid))
      (function ppf ->
        fprintf ppf "but is mixed here with fields of type")
  | LabelMultiplyDefined(s) ->
    fprintf ppf "The record field label %s is defined several times" s
  | RecursiveLocalConstraint(trace) ->
    report_unification_error ppf env trace
      (function ppf ->
         fprintf ppf "Recursive local constraint when unifying")
      (function ppf -> fprintf ppf "with")
  | UnexpectedExistential ->
    fprintf ppf "Unexpected existential"
  | MultiplyBoundVariable(name) ->
    fprintf ppf "Variable %s is bound several times in this matching" name
  | ModulesNotAllowed ->
    fprintf ppf "Modules are not allowed in this pattern."
  | OrpatVars(id, valid_idents) ->
    fprintf ppf "Variable %s must occur on both sides of this | pattern"
      (Ident.name id);
    spellcheck_idents ppf id valid_idents
  | OrPatternTypeClash(id, trace) ->
    report_unification_error ppf env trace
      (function ppf ->
         fprintf ppf "The variable %s on the left-hand side of this \
                      or-pattern has type" (Ident.name id))
      (function ppf ->
         fprintf ppf "but on the right-hand side it has type")
  | UnrefutedPattern pat ->
      fprintf ppf
        "@[%s@ %s@ %a@]"
        "This match case could not be refuted."
        "Here is an example of a value that would reach it:"
        Printpat.top_pretty pat

let report_error env ppf err =
  wrap_printing_env ~error:true env (fun () -> report_error env ppf err)

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (Location.error_of_printer loc (report_error env) err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )
