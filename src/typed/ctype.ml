(* Modified version of OCaml's typing/ctype.mli module *)
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

(* Operations on core types *)

open Grain_parsing
open Misc
open Asttypes
open Types
open Btype

(*
   Type manipulation after type inference
   ======================================
   If one wants to manipulate a type after type inference (for
   instance, during code generation or in the debugger), one must
   first make sure that the type levels are correct, using the
   function [correct_levels]. Then, this type can be correctly
   manipulated by [apply], [expand_head] and [moregeneral].
*)

(*
   General notes
   =============
   - As much sharing as possible should be kept : it makes types
     smaller and better abbreviated.
     When necessary, some sharing can be lost. Types will still be
     printed correctly (+++ TO DO...), and abbreviations defined by a
     class do not depend on sharing thanks to constrained
     abbreviations. (Of course, even if some sharing is lost, typing
     will still be correct.)
   - All nodes of a type have a level : that way, one know whether a
     node need to be duplicated or not when instantiating a type.
   - Levels of a type are decreasing (generic level being considered
     as greatest).
   - The level of a type constructor is superior to the binding
     time of its path.
   - Recursive types without limitation should be handled (even if
     there is still an occur check). This avoid treating specially the
     case for objects, for instance. Furthermore, the occur check
     policy can then be easily changed.
*)

(**** Errors ****)

exception Unify of (type_expr * type_expr) list

exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list

exception Cannot_expand

exception Cannot_apply

exception Recursive_abbrev

(* GADT: recursive abbrevs can appear as a result of local constraints *)
exception Unification_recursive_abbrev of (type_expr * type_expr) list

(**** Type level management ****)

let current_level = ref 0
let nongen_level = ref 0
let global_level = ref 1
let saved_level = ref []

type levels =
    { current_level: int; nongen_level: int; global_level: int;
      saved_level: (int * int) list; }
let save_levels () =
  { current_level = !current_level;
    nongen_level = !nongen_level;
    global_level = !global_level;
    saved_level = !saved_level }
let set_levels l =
  current_level := l.current_level;
  nongen_level := l.nongen_level;
  global_level := l.global_level;
  saved_level := l.saved_level

let get_current_level () = !current_level
let init_def level = current_level := level; nongen_level := level
let begin_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level; nongen_level := !current_level
let begin_class_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level
let raise_nongen_level () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  nongen_level := !current_level
let end_def () =
  let (cl, nl) = List.hd !saved_level in
  saved_level := List.tl !saved_level;
  current_level := cl; nongen_level := nl

let reset_global_level () =
  global_level := !current_level + 1
let increase_global_level () =
  let gl = !global_level in
  global_level := !current_level;
  gl
let restore_global_level gl =
  global_level := gl

(**** Whether a path points to an object type (with hidden row variable) ****)
let is_object_type path =
  let name =
    match path with Path.PIdent id -> Ident.name id
    | Path.PExternal(_, s,_) -> s
  in name.[0] = '#'


(**** Abbreviations without parameters ****)
(* Shall reset after generalizing *)

let simple_abbrevs = ref TMemNil

let proper_abbrevs path tl abbrev =
  if tl <> [] || !Grain_utils.Config.principal ||
     is_object_type path
  then abbrev
  else simple_abbrevs

(**** Some type creators ****)

(* Re-export generic type creators *)

let newty2             = Btype.newty2
let newty desc         = newty2 !current_level desc

let newvar ?name ()         = newty2 !current_level (TTyVar name)
let newvar2 ?name level     = newty2 level (TTyVar name)
let new_global_var ?name () = newty2 !global_level (TTyVar name)

let newconstr path tyl = newty (TTyConstr (path, tyl, ref TMemNil))

let none = newty (TTyTuple [])                (* Clearly ill-formed type *)

(**** Representative of a type ****)

(* Re-export repr *)
let repr = repr

(**** Type maps ****)

module TypePairs =
  Hashtbl.Make (struct
    type t = type_expr * type_expr
    let equal (t1, t1') (t2, t2') = (t1 == t2) && (t1' == t2')
    let hash (t, t') = t.id + 93 * t'.id
 end)


(**** unification mode ****)

type unification_mode =
  | Expression (* unification in expression *)
  | Pattern (* unification in pattern which may add local constraints *)

let umode = ref Expression
let generate_equations = ref false
let assume_injective = ref false

let set_mode_pattern ~generate ~injective f =
  let old_unification_mode = !umode
  and old_gen = !generate_equations
  and old_inj = !assume_injective in
  try
    umode := Pattern;
    generate_equations := generate;
    assume_injective := injective;
    let ret = f () in
    umode := old_unification_mode;
    generate_equations := old_gen;
    assume_injective := old_inj;
    ret
  with e ->
    umode := old_unification_mode;
    generate_equations := old_gen;
    assume_injective := old_inj;
    raise e

(*** Checks for type definitions ***)

let in_current_module = function
  | Path.PIdent _ -> true
  | Path.PExternal _ -> false

let in_pervasives p =
  failwith "NYI: in_pervasives"
  (*in_current_module p &&
  try ignore (Env.find_type p Env.initial_safe_string); true
  with Not_found -> false*)

let is_datatype decl=
  match decl.type_kind with
  | TDataVariant _ -> true

                    (**************************************)
                    (*  Check genericity of type schemes  *)
                    (**************************************)


exception Non_closed of type_expr * bool

let free_variables = ref []
let really_closed = ref None

let rec free_vars_rec real ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    begin match ty.desc, !really_closed with
      | TTyVar _, _ ->
        free_variables := (ty, real) :: !free_variables
      | TTyConstr(path, tl, _), Some env ->
        begin try
            let (_, body, _) = Env.find_type_expansion path env in
            if (repr body).level <> generic_level then
              free_variables := (ty, real) :: !free_variables
          with Not_found -> ()
        end;
        List.iter (free_vars_rec true) tl
      | _    ->
        iter_type_expr (free_vars_rec true) ty
    end;
  end

let free_vars ?env ty =
  free_variables := [];
  really_closed := env;
  free_vars_rec true ty;
  let res = !free_variables in
  free_variables := [];
  really_closed := None;
  res

let free_variables ?env ty =
  let tl = List.map fst (free_vars ?env ty) in
  unmark_type ty;
  tl

let closed_type ty =
  match free_vars ty with
      []           -> ()
  | (v, real) :: _ -> raise (Non_closed (v, real))

let closed_parameterized_type params ty =
  List.iter mark_type params;
  let ok =
    try closed_type ty; true with Non_closed _ -> false in
  List.iter unmark_type params;
  unmark_type ty;
  ok

let closed_type_decl decl =
  try
    List.iter mark_type decl.type_params;
    begin match decl.type_kind with
    | TDataVariant v ->
        List.iter
          (fun {cd_args; cd_res; _} ->
            match cd_res with
            | Some _ -> ()
            | None ->
                match cd_args with
                  | TConstrTuple l ->  List.iter closed_type l
                  | TConstrSingleton -> ()
          )
          v
    end;
    begin match decl.type_manifest with
      None    -> ()
    | Some ty -> closed_type ty
    end;
    unmark_type_decl decl;
    None
  with Non_closed (ty, _) ->
    unmark_type_decl decl;
    Some ty

                            (**********************)
                            (*  Type duplication  *)
                            (**********************)


(* Duplicate a type, preserving only type variables *)
let duplicate_type ty =
  Subst.type_expr Subst.identity ty


                         (*****************************)
                         (*  Type level manipulation  *)
                         (*****************************)

(*
   It would be a bit more efficient to remove abbreviation expansions
   rather than generalizing them: these expansions will usually not be
   used anymore. However, this is not possible in the general case, as
   [expand_abbrev] (via [subst]) requires these expansions to be
   preserved. Does it worth duplicating this code ?
*)
let rec generalize ty =
  let ty = repr ty in
  if (ty.level > !current_level) && (ty.level <> generic_level) then begin
    set_level ty generic_level;
    begin match ty.desc with
      | TTyConstr(_, _, abbrev) ->
        iter_abbrev generalize !abbrev
      | _ -> ()
    end;
    iter_type_expr generalize ty
  end

let generalize ty =
  simple_abbrevs := TMemNil;
  generalize ty

(* Generalize the structure and lower the variables *)

let rec generalize_structure var_level ty =
  let ty = repr ty in
  if ty.level <> generic_level then begin
    if is_Tvar ty && ty.level > var_level then
      set_level ty var_level
    else if
      ty.level > !current_level &&
      match ty.desc with
      | TTyConstr (p, _, abbrev) ->
          (abbrev := TMemNil; true)
      | _ -> true
    then begin
      set_level ty generic_level;
      iter_type_expr (generalize_structure var_level) ty
    end
  end

let generalize_structure var_level ty =
  simple_abbrevs := TMemNil;
  generalize_structure var_level ty

(* Generalize the spine of a function, if the level >= !current_level *)

let rec generalize_spine ty =
  let ty = repr ty in
  if ty.level < !current_level || ty.level = generic_level then () else
    match ty.desc with
    | TTyArrow(tyl, ty2, _) ->
      set_level ty generic_level;
      List.iter generalize_spine tyl;
      generalize_spine ty2;
    | TTyPoly (ty', _) ->
      set_level ty generic_level;
      generalize_spine ty'
    | TTyTuple tyl ->
      set_level ty generic_level;
      List.iter generalize_spine tyl
    | TTyConstr (p, tyl, memo) ->
      set_level ty generic_level;
      memo := TMemNil;
      List.iter generalize_spine tyl
    | _ -> ()

let forward_try_expand_once = (* Forward declaration *)
  ref (fun _env _ty -> raise Cannot_expand)

(*
   Lower the levels of a type (assume [level] is not
   [generic_level]).
*)
(*
    The level of a type constructor must be greater than its binding
    time. That way, a type constructor cannot escape the scope of its
    definition, as would be the case in
      let x = ref []
      module M = struct type t let _ = (x : t list ref) end
    (without this constraint, the type system would actually be unsound.)
*)
let get_level env p =
  try
    match (Env.find_type p env).type_newtype_level with
      | None -> Path.binding_time p
      | Some (x, _) -> x
  with
    | Not_found ->
      (* no newtypes in predef *)
      Path.binding_time p

let rec normalize_package_path env p =
  failwith "NYI: normalize_package_path"
  (*let t =
    try (Env.find_modtype p env).mtd_type
    with Not_found -> None
  in
  match t with
  | Some (Mty_ident p) -> normalize_package_path env p
  | Some (Mty_signature _ | Mty_functor _ | Mty_alias _) | None ->
      match p with
        | Path.PExternal(p1, s, n) ->
          (* For module aliases *)
          let p1' = Env.normalize_path None env p1 in
          if Path.same p1 p1' then p else
          normalize_package_path env (Path.PExternal(p1', s, n))
      | _ -> p*)

let rec update_level env level expand ty =
  let ty = repr ty in
  if ty.level > level then begin
    match ty.desc with
    | TTyConstr(p, _tl, _abbrev) when level < get_level env p ->
        (* Try first to replace an abbreviation by its expansion. *)
        begin try
          (* if is_newtype env p then raise Cannot_expand; *)
          link_type ty (!forward_try_expand_once env ty);
          update_level env level expand ty
        with Cannot_expand ->
          (* +++ Levels should be restored... *)
          (* Format.printf "update_level: %i < %i@." level (get_level env p); *)
          if level < get_level env p then raise (Unify [(ty, newvar2 level)]);
          iter_type_expr (update_level env level expand) ty
        end
    | TTyConstr(_, _ :: _, _) when expand ->
        begin try
          link_type ty (!forward_try_expand_once env ty);
          update_level env level expand ty
        with Cannot_expand ->
          set_level ty level;
          iter_type_expr (update_level env level expand) ty
        end
    | _ ->
        set_level ty level;
        (* XXX what about abbreviations in TTyConstr ? *)
        iter_type_expr (update_level env level expand) ty
  end

(* First try without expanding, then expand everything,
   to avoid combinatorial blow-up *)
let update_level env level ty =
  let ty = repr ty in
  if ty.level > level then begin
    let snap = snapshot () in
    try
      update_level env level false ty
    with Unify _ ->
      backtrack snap;
      update_level env level true ty
  end

(* Generalize and lower levels of contravariant branches simultaneously *)

let rec generalize_expansive env var_level visited ty =
  let ty = repr ty in
  if ty.level = generic_level || ty.level <= var_level then () else
  if not (Hashtbl.mem visited ty.id) then begin
    Hashtbl.add visited ty.id ();
    match ty.desc with
    | TTyConstr(path, tyl, abbrev) ->
        (*let variance =
          try (Env.find_type path env).type_variance
          with Not_found -> List.map (fun _ -> Variance.may_inv) tyl in*)
        abbrev := TMemNil;
        List.iter (generalize_structure var_level) tyl
        (*List.iter2
          (fun v t ->
            if Variance.(mem May_weak v)
            then generalize_structure var_level t
            else generalize_expansive env var_level visited t)
          variance tyl*)
    | TTyArrow (tl, t2, _) ->
        List.iter (generalize_structure var_level) tl;
        generalize_expansive env var_level visited t2
    | _ ->
        iter_type_expr (generalize_expansive env var_level visited) ty
  end

let generalize_expansive env ty =
  simple_abbrevs := TMemNil;
  try
    generalize_expansive env !nongen_level (Hashtbl.create 7) ty
  with Unify ([_, ty'] as tr) ->
    raise (Unify ((ty, ty') :: tr))

let generalize_global ty = generalize_structure !global_level ty
let generalize_structure ty = generalize_structure !current_level ty

(* Correct the levels of type [ty]. *)
let correct_levels ty =
  duplicate_type ty

(* Only generalize the type ty0 in ty *)
let limited_generalize ty0 ty =
  let ty0 = repr ty0 in

  let graph = Hashtbl.create 17 in
  let idx = ref lowest_level in
  let roots = ref [] in

  let rec inverse pty ty =
    let ty = repr ty in
    if (ty.level > !current_level) || (ty.level = generic_level) then begin
      decr idx;
      Hashtbl.add graph !idx (ty, ref pty);
      if (ty.level = generic_level) || (ty == ty0) then
        roots := ty :: !roots;
      set_level ty !idx;
      iter_type_expr (inverse [ty]) ty
    end else if ty.level < lowest_level then begin
      let (_, parents) = Hashtbl.find graph ty.level in
      parents := pty @ !parents
    end

  and generalize_parents ty =
    let idx = ty.level in
    if idx <> generic_level then begin
      set_level ty generic_level;
      List.iter generalize_parents !(snd (Hashtbl.find graph idx))
    end
  in

  inverse [] ty;
  if ty0.level < lowest_level then
    iter_type_expr (inverse []) ty0;
  List.iter generalize_parents !roots;
  Hashtbl.iter
    (fun _ (ty, _) ->
       if ty.level <> generic_level then set_level ty !current_level)
    graph


(* Compute statically the free univars of all nodes in a type *)
(* This avoids doing it repeatedly during instantiation *)

type inv_type_expr =
    { inv_type : type_expr;
      mutable inv_parents : inv_type_expr list }

let rec inv_type hash pty ty =
  let ty = repr ty in
  try
    let inv = TypeHash.find hash ty in
    inv.inv_parents <- pty @ inv.inv_parents
  with Not_found ->
    let inv = { inv_type = ty; inv_parents = pty } in
    TypeHash.add hash ty inv;
    iter_type_expr (inv_type hash [inv]) ty

let compute_univars ty =
  let inverted = TypeHash.create 17 in
  inv_type inverted [] ty;
  let node_univars = TypeHash.create 17 in
  let rec add_univar univ inv =
    match inv.inv_type.desc with
    | TTyPoly(_ty, tl) when List.memq univ (List.map repr tl) -> ()
    | _ ->
        try
          let univs = TypeHash.find node_univars inv.inv_type in
          if not (TypeSet.mem univ !univs) then begin
            univs := TypeSet.add univ !univs;
            List.iter (add_univar univ) inv.inv_parents
          end
        with Not_found ->
          TypeHash.add node_univars inv.inv_type (ref(TypeSet.singleton univ));
          List.iter (add_univar univ) inv.inv_parents
  in
  TypeHash.iter (fun ty inv -> if is_Tunivar ty then add_univar ty inv)
    inverted;
  fun ty ->
    try !(TypeHash.find node_univars ty) with Not_found -> TypeSet.empty


                              (*******************)
                              (*  Instantiation  *)
                              (*******************)


let rec find_repr p1 =
  function
  | TMemNil ->
      None
  | TMemCons(p2, ty, _, _) when Path.same p1 p2 ->
      Some ty
  | TMemCons(_, _, _, rem) ->
      find_repr p1 rem
  | TMemLink {contents = rem} ->
      find_repr p1 rem

(*
   Generic nodes are duplicated, while non-generic nodes are left
   as-is.
   During instantiation, the description of a generic node is first
   replaced by a link to a stub ([Tsubst (newvar ())]). Once the
   copy is made, it replaces the stub.
   After instantiation, the description of generic node, which was
   stored by [save_desc], must be put back, using [cleanup_types].
*)

let abbreviations = ref (ref TMemNil)
  (* Abbreviation memorized. *)

(* partial: we may not wish to copy the non generic types
   before we call type_pat *)
let rec copy ?env ?partial ?keep_names ty =
  let copy = copy ?env ?partial ?keep_names in
  let ty = repr ty in
  match ty.desc with
  | TTySubst ty -> ty
  | _ ->
    if ty.level <> generic_level && partial = None then ty else
    (* We only forget types that are non generic and do not contain
       free univars *)
    let forget =
      if ty.level = generic_level then generic_level else
      match partial with
        None -> assert false
      | Some (free_univars, keep) ->
          if TypeSet.is_empty (free_univars ty) then
            if keep then ty.level else !current_level
          else generic_level
    in
    if forget <> generic_level then newty2 forget (TTyVar None) else
    let desc = ty.desc in
    save_desc ty desc;
    let t = newvar() in          (* Stub *)
    ty.desc <- TTySubst t;
    t.desc <-
      begin match desc with
      | TTyConstr(p, tl, _) ->
          let abbrevs = proper_abbrevs p tl !abbreviations in
          begin match find_repr p !abbrevs with
            Some ty when repr ty != t ->
              TTyLink ty
          | _ ->
          (*
             One must allocate a new reference, so that abbrevia-
             tions belonging to different branches of a type are
             independent.
             Moreover, a reference containing a [TMemCons] must be
             shared, so that the memorized expansion of an abbrevi-
             ation can be released by changing the content of just
             one reference.
          *)
              TTyConstr(p, List.map copy tl,
                       ref (match !(!abbreviations) with
                              TMemCons _ -> TMemLink !abbreviations
                            | abbrev  -> abbrev))
          end
      | _ -> copy_type_desc ?keep_names copy desc
      end;
    t

let simple_copy t = copy t

(**** Variants of instantiations ****)

(* TODO: Remove *)
let gadt_env env = None

let instance ?partial env sch =
  let env = gadt_env env in
  let partial =
    match partial with
      None -> None
    | Some keep -> Some (compute_univars sch, keep)
  in
  let ty = copy ?env ?partial sch in
  cleanup_types ();
  ty

let instance_def sch =
  let ty = copy sch in
  cleanup_types ();
  ty

let generic_instance env sch =
  let old = !current_level in
  current_level := generic_level;
  let ty = instance env sch in
  current_level := old;
  ty

let instance_list env schl =
  let env = gadt_env env in
  let tyl = List.map (fun t -> copy ?env t) schl in
  cleanup_types ();
  tyl

(* names given to new type constructors.
   Used for existential types and
   local constraints *)
let get_new_abstract_name s =
  let index =
    try Vars.find s !reified_var_counter + 1
    with Not_found -> 0 in
  reified_var_counter := Vars.add s index !reified_var_counter;
  if index = 0 && s <> "" && s.[String.length s - 1] <> '$' then s else
  Printf.sprintf "%s%d" s index

let new_declaration newtype manifest =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract;
    type_manifest = manifest;
    type_newtype_level = newtype;
    type_loc = Location.dummy_loc;
    type_immediate = false;
  }

let instance_constructor ?in_pattern cstr =
  begin match in_pattern with
  | None -> ()
  | Some (env, newtype_lev) ->
      let process existential =
        let decl = new_declaration (Some (newtype_lev, newtype_lev)) None in
        let name =
          match repr existential with
            {desc = TTyVar(Some name)} -> "$" ^ cstr.cstr_name ^ "_'" ^ name
          | _ -> "$" ^ cstr.cstr_name
        in
        let path = Path.PIdent (Ident.create (get_new_abstract_name name)) in
        let new_env = Env.add_local_type path decl !env in
        env := new_env;
        let to_unify = newty (TTyConstr(path,[],ref TMemNil)) in
        let tv = copy existential in
        assert (is_Tvar tv);
        link_type tv to_unify
      in
      List.iter process cstr.cstr_existentials
  end;
  let ty_res = copy cstr.cstr_res in
  let ty_args = List.map simple_copy  cstr.cstr_args in
  cleanup_types ();
  (ty_args, ty_res)

let instance_parameterized_type ?keep_names sch_args sch =
  let ty_args = List.map (fun t -> copy ?keep_names t) sch_args in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty)

let instance_parameterized_type_2 sch_args sch_lst sch =
  let ty_args = List.map simple_copy sch_args in
  let ty_lst = List.map simple_copy sch_lst in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty_lst, ty)

let map_kind f = function
  | Type_abstract -> Type_abstract
  | TDataVariant cl ->
      TDataVariant (
        List.map
          (fun c ->
             {c with
              cd_args = map_type_expr_cstr_args f c.cd_args;
              cd_res = may_map f c.cd_res
             })
          cl)

let instance_declaration decl =
  let decl =
    {decl with type_params = List.map simple_copy decl.type_params;
     type_manifest = may_map simple_copy decl.type_manifest;
     type_kind = map_kind simple_copy decl.type_kind;
    }
  in
  cleanup_types ();
  decl

(**** Instantiation for types with free universal variables ****)

let rec diff_list l1 l2 =
  if l1 == l2 then [] else
  match l1 with [] -> invalid_arg "Ctype.diff_list"
  | a :: l1 -> a :: diff_list l1 l2

let conflicts free bound =
  let bound = List.map repr bound in
  TypeSet.exists (fun t -> List.memq (repr t) bound) free

let delayed_copy = ref []
    (* copying to do later *)

(* Copy without sharing until there are no free univars left *)
(* all free univars must be included in [visited]            *)
let rec copy_sep fixed free bound visited ty =
  let ty = repr ty in
  let univars = free ty in
  if TypeSet.is_empty univars then
    if ty.level <> generic_level then ty else
    let t = newvar () in
    delayed_copy :=
      lazy (t.desc <- TTyLink(copy ty))
      :: !delayed_copy;
    t
  else try
    let t, bound_t = List.assq ty visited in
    let dl = if is_Tvar ty then [] else diff_list bound bound_t in
    if dl <> [] && conflicts univars dl then raise Not_found;
    t
  with Not_found -> begin
    let t = newvar() in          (* Stub *)
    let visited =
      match ty.desc with
        TTyArrow _ | TTyTuple _ | TTyConstr _ ->
          (ty,(t,bound)) :: visited
      | _ -> visited in
    let copy_rec = copy_sep fixed free bound visited in
    t.desc <-
      begin match ty.desc with
      | TTyPoly (t1, tl) ->
          let tl = List.map repr tl in
          let tl' = List.map (fun t -> newty t.desc) tl in
          let bound = tl @ bound in
          let visited =
            List.map2 (fun ty t -> ty,(t,bound)) tl tl' @ visited in
          TTyPoly (copy_sep fixed free bound visited t1, tl')
      | _ -> copy_type_desc copy_rec ty.desc
      end;
    t
  end

let instance_poly ?(keep_names=false) fixed univars sch =
  let univars = List.map repr univars in
  let copy_var ty =
    match ty.desc with
    | TTyUniVar name -> if keep_names then newty (TTyVar name) else newvar ()
    | _ -> assert false
  in
  let vars = List.map copy_var univars in
  let pairs = List.map2 (fun u v -> u, (v, [])) univars vars in
  delayed_copy := [];
  let ty = copy_sep fixed (compute_univars sch) [] pairs sch in
  List.iter Lazy.force !delayed_copy;
  delayed_copy := [];
  cleanup_types ();
  vars, ty

(**** Instantiation with parameter substitution ****)

let unify' = (* Forward declaration *)
  ref (fun _env _ty1 _ty2 -> raise (Unify []))

let subst env level priv abbrev ty params args body =
  if List.length params <> List.length args then raise (Unify []);
  let old_level = !current_level in
  current_level := level;
  try
    let body0 = newvar () in          (* Stub *)
    begin match ty with
      None      -> ()
    | Some ({desc = TTyConstr (path, tl, _)} as ty) ->
        let abbrev = proper_abbrevs path tl abbrev in
        memorize_abbrev abbrev priv path ty body0
    | _ ->
        assert false
    end;
    abbreviations := abbrev;
    let (params', body') = instance_parameterized_type params body in
    abbreviations := ref TMemNil;
    !unify' env body0 body';
    List.iter2 (!unify' env) params' args;
    current_level := old_level;
    body'
  with Unify _ as exn ->
    current_level := old_level;
    raise exn

(*
   Only the shape of the type matters, not whether it is generic or
   not. [generic_level] might be somewhat slower, but it ensures
   invariants on types are enforced (decreasing levels), and we don't
   care about efficiency here.
*)
let apply env params body args =
  try
    subst env generic_level Public (ref TMemNil) None params args body
  with
    Unify _ -> raise Cannot_apply

let () = Subst.ctype_apply_env_empty := apply Env.empty

                              (****************************)
                              (*  Abbreviation expansion  *)
                              (****************************)

(*
   If the environment has changed, memorized expansions might not
   be correct anymore, and so we flush the cache. This is safe but
   quite pessimistic: it would be enough to flush the cache when a
   type or module definition is overridden in the environment.
*)
let previous_env = ref Env.empty
(*let string_of_kind = function Public -> "public" | Private -> "private"*)
let check_abbrev_env env =
  if env != !previous_env then begin
    (* prerr_endline "cleanup expansion cache"; *)
    cleanup_abbrev ();
    previous_env := env
  end


(* Expand an abbreviation. The expansion is memorized. *)
(*
   Assume the level is greater than the path binding time of the
   expanded abbreviation.
*)
(*
   An abbreviation expansion will fail in either of these cases:
   1. The type constructor does not correspond to a manifest type.
   2. The type constructor is defined in an external file, and this
      file is not in the path (missing -I options).
   3. The type constructor is not in the "local" environment. This can
      happens when a non-generic type variable has been instantiated
      afterwards to the not yet defined type constructor. (Actually,
      this cannot happen at the moment due to the strong constraints
      between type levels and constructor binding time.)
   4. The expansion requires the expansion of another abbreviation,
      and this other expansion fails.
*)
let expand_abbrev_gen kind find_type_expansion env ty =
  check_abbrev_env env;
  match ty with
    {desc = TTyConstr (path, args, abbrev); level = level} ->
      let lookup_abbrev = proper_abbrevs path args abbrev in
      begin match find_expans kind path !lookup_abbrev with
        Some ty' ->
          (* prerr_endline
            ("found a "^string_of_kind kind^" expansion for "^Path.name path);*)
          if level <> generic_level then
            begin try
              update_level env level ty'
            with Unify _ ->
              (* XXX This should not happen.
                 However, levels are not correctly restored after a
                 typing error *)
              ()
            end;
          let ty' = repr ty' in
          (* assert (ty != ty'); *) (* PR#7324 *)
          ty'
      | None ->
          match find_type_expansion path env with
          | exception Not_found ->
            (* another way to expand is to normalize the path itself *)
            let path' = Env.normalize_path None env path in
            if Path.same path path' then raise Cannot_expand
            else newty2 level (TTyConstr (path', args, abbrev))
          | (params, body, lv) ->
            (* prerr_endline
              ("add a "^string_of_kind kind^" expansion for "^Path.name path);*)
            let ty' = subst env level kind abbrev (Some ty) params args body in
            ty'
      end
  | _ ->
      assert false

(* Expand respecting privacy *)
let expand_abbrev env ty =
  expand_abbrev_gen Public Env.find_type_expansion env ty

(* Expand once the head of a type *)
let expand_head_once env ty =
  try expand_abbrev env (repr ty) with Cannot_expand -> assert false

(* Check whether a type can be expanded *)
let safe_abbrev env ty =
  let snap = Btype.snapshot () in
  try ignore (expand_abbrev env ty); true
  with Cannot_expand | Unify _ ->
    Btype.backtrack snap;
    false

(* Expand the head of a type once.
   Raise Cannot_expand if the type cannot be expanded.
   May raise Unify, if a recursion was hidden in the type. *)
let try_expand_once env ty =
  let ty = repr ty in
  match ty.desc with
    TTyConstr _ -> repr (expand_abbrev env ty)
  | _ -> raise Cannot_expand

(* This one only raises Cannot_expand *)
let try_expand_safe env ty =
  let snap = Btype.snapshot () in
  try try_expand_once env ty
  with Unify _ ->
    Btype.backtrack snap; raise Cannot_expand

(* Fully expand the head of a type. *)
let rec try_expand_head try_once env ty =
  let ty' = try_once env ty in
  try try_expand_head try_once env ty'
  with Cannot_expand -> ty'

let try_expand_head try_once env ty =
  let ty' = try_expand_head try_once env ty in
  (* GADTs disabled *)
  ty'

(* Unsafe full expansion, may raise Unify. *)
let expand_head_unif env ty =
  try try_expand_head try_expand_once env ty with Cannot_expand -> repr ty

(* Safe version of expand_head, never fails *)
let expand_head env ty =
  try try_expand_head try_expand_safe env ty with Cannot_expand -> repr ty

let _ = forward_try_expand_once := try_expand_safe


(* Expand until we find a non-abstract type declaration *)

let rec extract_concrete_typedecl env ty =
  let ty = repr ty in
  match ty.desc with
    TTyConstr (p, _, _) ->
      let decl = Env.find_type p env in
      if decl.type_kind <> Type_abstract then (p, p, decl) else
      let ty =
        try try_expand_once env ty with Cannot_expand -> raise Not_found
      in
      let (_, p', decl) = extract_concrete_typedecl env ty in
        (p, p', decl)
  | _ -> raise Not_found

(* Implementing function [expand_head_opt], the compiler's own version of
   [expand_head] used for type-based optimisations.
   [expand_head_opt] uses [Env.find_type_expansion_opt] to access the
   manifest type information of private abstract data types which is
   normally hidden to the type-checker out of the implementation module of
   the private abbreviation. *)

let expand_abbrev_opt =
  expand_abbrev_gen Private Env.find_type_expansion_opt

let try_expand_once_opt env ty =
  let ty = repr ty in
  match ty.desc with
    TTyConstr _ -> repr (expand_abbrev_opt env ty)
  | _ -> raise Cannot_expand

let rec try_expand_head_opt env ty =
  let ty' = try_expand_once_opt env ty in
  begin try
    try_expand_head_opt env ty'
  with Cannot_expand ->
    ty'
  end

let expand_head_opt env ty =
  let snap = Btype.snapshot () in
  try try_expand_head_opt env ty
  with Cannot_expand | Unify _ -> (* expand_head shall never fail *)
    Btype.backtrack snap;
    repr ty

(* Make sure that the type parameters of the type constructor [ty]
   respect the type constraints *)
let enforce_constraints env ty =
  match ty with
    {desc = TTyConstr (path, args, _abbrev); level = level} ->
      begin try
        let decl = Env.find_type path env in
        ignore
          (subst env level Public (ref TMemNil) None decl.type_params args
             (newvar2 level))
      with Not_found -> ()
      end
  | _ ->
      assert false

(* Recursively expand the head of a type.
   Also expand #-types. *)
let full_expand env ty =
  repr (expand_head env ty)

(*
   Check whether the abbreviation expands to a well-defined type.
   During the typing of a class, abbreviations for correspondings
   types expand to non-generic types.
*)
let generic_abbrev env path =
  try
    let (_, body, _) = Env.find_type_expansion path env in
    (repr body).level = generic_level
  with
    Not_found ->
      false

let generic_private_abbrev env path =
  try
    match Env.find_type path env with
    | {type_kind = Type_abstract;
        type_manifest = Some body} ->
      (repr body).level = generic_level
    | _ -> false
  with Not_found -> false

let is_contractive env p =
  try
    let decl = Env.find_type p env in
    in_pervasives p && decl.type_manifest = None || is_datatype decl
  with Not_found -> false


                              (*****************)
                              (*  Occur check  *)
                              (*****************)


exception Occur

let rec occur_rec env allow_recursive visited ty0 = function
  | {desc=TTyLink ty} ->
      occur_rec env allow_recursive visited ty0 ty
  | ty ->
  if ty == ty0  then raise Occur;
  match ty.desc with
    TTyConstr(p, _tl, _abbrev) ->
      if allow_recursive && is_contractive env p then () else
      begin try
        if TypeSet.mem ty visited then raise Occur;
        let visited = TypeSet.add ty visited in
        iter_type_expr (occur_rec env allow_recursive visited ty0) ty
      with Occur -> try
        let ty' = try_expand_head try_expand_once env ty in
        (* This call used to be inlined, but there seems no reason for it.
           Message was referring to change in rev. 1.58 of the CVS repo. *)
        occur_rec env allow_recursive visited ty0 ty'
      with Cannot_expand ->
        raise Occur
      end
  | _ ->
      if allow_recursive ||  TypeSet.mem ty visited then () else begin
        let visited = TypeSet.add ty visited in
        iter_type_expr (occur_rec env allow_recursive visited ty0) ty
      end

let type_changed = ref false (* trace possible changes to the studied type *)

let merge r b = if b then r := true

let occur env ty0 ty =
  let allow_recursive = !Grain_utils.Config.recursive_types || !umode = Pattern  in
  let old = !type_changed in
  try
    while
      type_changed := false;
      occur_rec env allow_recursive TypeSet.empty ty0 ty;
      !type_changed
    do () (* prerr_endline "changed" *) done;
    merge type_changed old
  with exn ->
    merge type_changed old;
    raise (match exn with Occur -> Unify [] | _ -> exn)

let occur_in env ty0 t =
  try occur env ty0 t; false with Unify _ -> true

(* Check that a local constraint is well-founded *)
(* PR#6405: not needed since we allow recursion and work on normalized types *)
(* PR#6992: we actually need it for contractiveness *)
(* This is a simplified version of occur, only for the rectypes case *)

let rec local_non_recursive_abbrev strict visited env p ty =
  (*Format.eprintf "@[Check %s =@ %a@]@." (Path.name p) !Btype.print_raw ty;*)
  let ty = repr ty in
  if not (List.memq ty visited) then begin
    match ty.desc with
      TTyConstr(p', args, _abbrev) ->
        if Path.same p p' then raise Occur;
        if not strict && is_contractive env p' then () else
        let visited = ty :: visited in
        begin try
          (* try expanding, since [p] could be hidden *)
          local_non_recursive_abbrev strict visited env p
            (try_expand_head try_expand_once env ty)
        with Cannot_expand ->
          let params =
            try (Env.find_type p' env).type_params
            with Not_found -> args
          in
          List.iter2
            (fun tv ty ->
              let strict = strict || not (is_Tvar (repr tv)) in
              local_non_recursive_abbrev strict visited env p ty)
            params args
        end
    | _ ->
        if strict then (* PR#7374 *)
          let visited = ty :: visited in
          iter_type_expr (local_non_recursive_abbrev true visited env p) ty
  end

let local_non_recursive_abbrev env p ty =
  try
    local_non_recursive_abbrev false [] env p ty;
    true
  with Occur -> false


                   (*****************************)
                   (*  Polymorphic Unification  *)
                   (*****************************)

(* Since we cannot duplicate universal variables, unification must
   be done at meta-level, using bindings in univar_pairs *)
let rec unify_univar t1 t2 = function
    (cl1, cl2) :: rem ->
      let find_univ t cl =
        try
          let (_, r) = List.find (fun (t',_) -> t == repr t') cl in
          Some r
        with Not_found -> None
      in
      begin match find_univ t1 cl1, find_univ t2 cl2 with
        Some {contents=Some t'2}, Some _ when t2 == repr t'2 ->
          ()
      | Some({contents=None} as r1), Some({contents=None} as r2) ->
          set_univar r1 t2; set_univar r2 t1
      | None, None ->
          unify_univar t1 t2 rem
      | _ ->
          raise (Unify [])
      end
  | [] -> raise (Unify [])

(* Test the occurrence of free univars in a type *)
(* that's way too expensive. Must do some kind of caching *)
let occur_univar env ty =
  let visited = ref TypeMap.empty in
  let rec occur_rec bound ty =
    let ty = repr ty in
    if ty.level >= lowest_level &&
      if TypeSet.is_empty bound then
        (ty.level <- pivot_level - ty.level; true)
      else try
        let bound' = TypeMap.find ty !visited in
        if TypeSet.exists (fun x -> not (TypeSet.mem x bound)) bound' then
          (visited := TypeMap.add ty (TypeSet.inter bound bound') !visited;
           true)
        else false
      with Not_found ->
        visited := TypeMap.add ty bound !visited;
        true
    then
      match ty.desc with
        TTyUniVar _ ->
          if not (TypeSet.mem ty bound) then raise (Unify [ty, newgenvar ()])
      | TTyPoly (ty, tyl) ->
          let bound = List.fold_right TypeSet.add (List.map repr tyl) bound in
          occur_rec bound  ty
      | TTyConstr (_, [], _) -> ()
      | TTyConstr (p, tl, _) ->
          begin try
            let td = Env.find_type p env in
            List.iter2
              (fun t v ->
                if Variance.(mem May_pos v || mem May_neg v)
                then occur_rec bound t)
              tl td.type_variance
          with Not_found ->
            List.iter (occur_rec bound) tl
          end
      | _ -> iter_type_expr (occur_rec bound) ty
  in
  try
    occur_rec TypeSet.empty ty; unmark_type ty
  with exn ->
    unmark_type ty; raise exn

(* Grouping univars by families according to their binders *)
let add_univars =
  List.fold_left (fun s (t,_) -> TypeSet.add (repr t) s)

let get_univar_family univar_pairs univars =
  if univars = [] then TypeSet.empty else
  let insert s = function
      cl1, (_::_ as cl2) ->
        if List.exists (fun (t1,_) -> TypeSet.mem (repr t1) s) cl1 then
          add_univars s cl2
        else s
    | _ -> s
  in
  let s = List.fold_right TypeSet.add univars TypeSet.empty in
  List.fold_left insert s univar_pairs

(* Whether a family of univars escapes from a type *)
let univars_escape env univar_pairs vl ty =
  let family = get_univar_family univar_pairs vl in
  let visited = ref TypeSet.empty in
  let rec occur t =
    let t = repr t in
    if TypeSet.mem t !visited then () else begin
      visited := TypeSet.add t !visited;
      match t.desc with
        TTyPoly (t, tl) ->
          if List.exists (fun t -> TypeSet.mem (repr t) family) tl then ()
          else occur t
      | TTyUniVar _ ->
          if TypeSet.mem t family then raise Occur
      | TTyConstr (_, [], _) -> ()
      | TTyConstr (p, tl, _) ->
          begin try
            let td = Env.find_type p env in
            List.iter2
              (fun t v ->
                if Variance.(mem May_pos v || mem May_neg v) then occur t)
              tl td.type_variance
          with Not_found ->
            List.iter occur tl
          end
      | _ ->
          iter_type_expr occur t
    end
  in
  try occur ty; false with Occur -> true

(* Wrapper checking that no variable escapes and updating univar_pairs *)
let enter_poly env univar_pairs t1 tl1 t2 tl2 f =
  let old_univars = !univar_pairs in
  let known_univars =
    List.fold_left (fun s (cl,_) -> add_univars s cl)
      TypeSet.empty old_univars
  in
  let tl1 = List.map repr tl1 and tl2 = List.map repr tl2 in
  if List.exists (fun t -> TypeSet.mem t known_univars) tl1 &&
    univars_escape env old_univars tl1 (newty(TTyPoly(t2,tl2)))
  || List.exists (fun t -> TypeSet.mem t known_univars) tl2 &&
    univars_escape env old_univars tl2 (newty(TTyPoly(t1,tl1)))
  then raise (Unify []);
  let cl1 = List.map (fun t -> t, ref None) tl1
  and cl2 = List.map (fun t -> t, ref None) tl2 in
  univar_pairs := (cl1,cl2) :: (cl2,cl1) :: old_univars;
  try let res = f t1 t2 in univar_pairs := old_univars; res
  with exn -> univar_pairs := old_univars; raise exn

let univar_pairs = ref []


                              (*****************)
                              (*  Unification  *)
                              (*****************)



let rec has_cached_expansion p abbrev =
  match abbrev with
  | TMemNil                   -> false
  | TMemCons(p', _, _, rem)   -> Path.same p p' || has_cached_expansion p rem
  | TMemLink rem              -> has_cached_expansion p !rem

(**** Transform error trace ****)
(* +++ Move it to some other place ? *)

let expand_trace env trace =
  List.fold_right
    (fun (t1, t2) rem ->
       (repr t1, full_expand env t1)::(repr t2, full_expand env t2)::rem)
    trace []

(**** Unification ****)

(* Return whether [t0] occurs in [ty]. Objects are also traversed. *)
let deep_occur t0 ty =
  let rec occur_rec ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      if ty == t0 then raise Occur;
      ty.level <- pivot_level - ty.level;
      iter_type_expr occur_rec ty
    end
  in
  try
    occur_rec ty; unmark_type ty; false
  with Occur ->
    unmark_type ty; true

(*
   1. When unifying two non-abbreviated types, one type is made a link
      to the other. When unifying an abbreviated type with a
      non-abbreviated type, the non-abbreviated type is made a link to
      the other one. When unifying to abbreviated types, these two
      types are kept distincts, but they are made to (temporally)
      expand to the same type.
   2. Abbreviations with at least one parameter are systematically
      expanded. The overhead does not seem too high, and that way
      abbreviations where some parameters does not appear in the
      expansion, such as ['a t = int], are correctly handled. In
      particular, for this example, unifying ['a t] with ['b t] keeps
      ['a] and ['b] distincts. (Is it really important ?)
   3. Unifying an abbreviation ['a t = 'a] with ['a] should not yield
      ['a t as 'a]. Indeed, the type variable would otherwise be lost.
      This problem occurs for abbreviations expanding to a type
      variable, but also to many other constrained abbreviations (for
      instance, [(< x : 'a > -> unit) t = <x : 'a>]). The solution is
      that, if an abbreviation is unified with some subpart of its
      parameters, then the parameter actually does not get
      abbreviated.  It would be possible to check whether some
      information is indeed lost, but it probably does not worth it.
*)

let newtype_level = ref None

let get_newtype_level () =
  match !newtype_level with
  | None -> assert false
  | Some x -> x

(* a local constraint can be added only if the rhs
   of the constraint does not contain any Tvars.
   They need to be removed using this function *)
let reify env t =
  let newtype_level = get_newtype_level () in
  let create_fresh_constr lev name =
    let decl = new_declaration (Some (newtype_level, newtype_level)) None in
    let name = match name with Some s -> "$'"^s | _ -> "$" in
    let path = Path.PIdent (Ident.create (get_new_abstract_name name)) in
    let new_env = Env.add_local_type path decl !env in
    let t = newty2 lev (TTyConstr (path,[],ref TMemNil))  in
    env := new_env;
    t
  in
  let visited = ref TypeSet.empty in
  let rec iterator ty =
    let ty = repr ty in
    if TypeSet.mem ty !visited then () else begin
      visited := TypeSet.add ty !visited;
      match ty.desc with
      | TTyVar o ->
          let t = create_fresh_constr ty.level o in
          link_type ty t;
          if ty.level < newtype_level then
            raise (Unify [t, newvar2 ty.level])
      | TTyConstr (p, _, _) when is_object_type p ->
          iter_type_expr iterator (full_expand !env ty)
      | _ ->
          iter_type_expr iterator ty
    end
  in
  iterator t

let is_newtype env p =
  try
    let decl = Env.find_type p env in
    decl.type_newtype_level <> None &&
    decl.type_kind = Type_abstract
  with Not_found -> false

let non_aliasable p decl =
  (* in_pervasives p ||  (subsumed by in_current_module) *)
  in_current_module p && decl.type_newtype_level = None

let is_instantiable env p =
  try
    let decl = Env.find_type p env in
    decl.type_kind = Type_abstract &&
    decl.type_arity = 0 &&
    decl.type_manifest = None &&
    not (non_aliasable p decl)
  with Not_found -> false


(* PR#7113: -safe-string should be a global property *)
let compatible_paths p1 p2 =
  let open Predef in
  Path.same p1 p2 ||
  Path.same p1 path_bytes && Path.same p2 path_string ||
  Path.same p1 path_string && Path.same p2 path_bytes

(* Check for datatypes carefully; see PR#6348 *)
let rec expands_to_datatype env ty =
  let ty = repr ty in
  match ty.desc with
    TTyConstr (p, _, _) ->
      begin try
        is_datatype (Env.find_type p env) ||
        expands_to_datatype env (try_expand_once env ty)
      with Not_found | Cannot_expand -> false
      end
  | _ -> false

(* mcomp type_pairs subst env t1 t2 does not raise an
   exception if it is possible that t1 and t2 are actually
   equal, assuming the types in type_pairs are equal and
   that the mapping subst holds.
   Assumes that both t1 and t2 do not contain any tvars
   and that both their objects and variants are closed
 *)

let rec mcomp type_pairs env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else
  match (t1.desc, t2.desc) with
  | (TTyVar _, _)
  | (_, TTyVar _)  ->
      ()
  | (TTyConstr (p1, [], _), TTyConstr (p2, [], _)) when Path.same p1 p2 ->
      ()
  | _ ->
      let t1' = expand_head_opt env t1 in
      let t2' = expand_head_opt env t2 in
      (* Expansion may have changed the representative of the types... *)
      let t1' = repr t1' and t2' = repr t2' in
      if t1' == t2' then () else
      begin try TypePairs.find type_pairs (t1', t2')
      with Not_found ->
        TypePairs.add type_pairs (t1', t2') ();
        match (t1'.desc, t2'.desc) with
        | (TTyVar _, _)
        | (_, TTyVar _)  ->
            ()
        | (TTyArrow (t1, u1, _), TTyArrow (t2, u2, _)) ->
            mcomp_list type_pairs env t1 t2;
            mcomp type_pairs env u1 u2;
        | (TTyTuple tl1, TTyTuple tl2) ->
            mcomp_list type_pairs env tl1 tl2
        | (TTyConstr (p1, tl1, _), TTyConstr (p2, tl2, _)) ->
            mcomp_type_decl type_pairs env p1 p2 tl1 tl2
        | (TTyConstr (p, _, _), _) | (_, TTyConstr (p, _, _)) ->
            begin try
              let decl = Env.find_type p env in
              if non_aliasable p decl || is_datatype decl then raise (Unify [])
            with Not_found -> ()
            end
        | (TTyPoly (t1, []), TTyPoly (t2, [])) ->
            mcomp type_pairs env t1 t2
        | (TTyPoly (t1, tl1), TTyPoly (t2, tl2)) ->
            enter_poly env univar_pairs t1 tl1 t2 tl2
              (mcomp type_pairs env)
        | (TTyUniVar _, TTyUniVar _) ->
            unify_univar t1' t2' !univar_pairs
        | (_, _) ->
            raise (Unify [])
      end

and mcomp_list type_pairs env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (mcomp type_pairs env) tl1 tl2

and mcomp_type_decl type_pairs env p1 p2 tl1 tl2 =
  try
    let decl = Env.find_type p1 env in
    let decl' = Env.find_type p2 env in
    if compatible_paths p1 p2 then begin
      let inj =
        try List.map Variance.(mem Inj) (Env.find_type p1 env).type_variance
        with Not_found -> List.map (fun _ -> false) tl1
      in
      List.iter2
        (fun i (t1,t2) -> if i then mcomp type_pairs env t1 t2)
        inj (List.combine tl1 tl2)
    end else if non_aliasable p1 decl && non_aliasable p2 decl' then
      raise (Unify [])
    else
      match decl.type_kind, decl'.type_kind with
      | TDataVariant v1, TDataVariant v2 ->
          mcomp_list type_pairs env tl1 tl2;
          mcomp_variant_description type_pairs env v1 v2
      | Type_abstract, Type_abstract -> ()
      | Type_abstract, _ when not (non_aliasable p1 decl)-> ()
      | _, Type_abstract when not (non_aliasable p2 decl') -> ()
      | _ -> raise (Unify [])
  with Not_found -> ()

and mcomp_type_option type_pairs env t t' =
  match t, t' with
    None, None -> ()
  | Some t, Some t' -> mcomp type_pairs env t t'
  | _ -> raise (Unify [])

and mcomp_variant_description type_pairs env xs ys =
  let rec iter = fun x y ->
    match x, y with
    | c1 :: xs, c2 :: ys   ->
      mcomp_type_option type_pairs env c1.cd_res c2.cd_res;
      begin match c1.cd_args, c2.cd_args with
        | TConstrTuple l1, TConstrTuple l2 -> mcomp_list type_pairs env l1 l2
        | TConstrSingleton, TConstrSingleton -> ()
        | _ -> raise (Unify [])
      end;
     if Ident.name c1.cd_id = Ident.name c2.cd_id
      then iter xs ys
      else raise (Unify [])
    | [],[] -> ()
    | _ -> raise (Unify [])
  in
  iter xs ys

let mcomp env t1 t2 =
  mcomp (TypePairs.create 4) env t1 t2

(* Real unification *)

let find_lowest_level ty =
  let lowest = ref generic_level in
  let rec find ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      if ty.level < !lowest then lowest := ty.level;
      ty.level <- pivot_level - ty.level;
      iter_type_expr find ty
    end
  in find ty; unmark_type ty; !lowest

let find_newtype_level env path =
  try match (Env.find_type path env).type_newtype_level with
    Some x -> x
  | None -> raise Not_found
  with Not_found -> let lev = Path.binding_time path in (lev, lev)

let unify_eq_set = TypePairs.create 11

let order_type_pair t1 t2 =
  if t1.id <= t2.id then (t1, t2) else (t2, t1)

let add_type_equality t1 t2 =
  TypePairs.add unify_eq_set (order_type_pair t1 t2) ()

let eq_package_path env p1 p2 =
  Path.same p1 p2 ||
  Path.same (normalize_package_path env p1) (normalize_package_path env p2)

let nondep_type' = ref (fun _ _ _ -> assert false)
let package_subtype = ref (fun _ _ _ _ _ _ _ -> assert false)

let rec concat_longident lid1 =
  let open Longident in
  function
    Lident s -> Ldot (lid1, s)
  | Ldot (lid2, s) -> Ldot (concat_longident lid1 lid2, s)
  | Lapply (lid2, lid) -> Lapply (concat_longident lid1 lid2, lid)

let nondep_instance env level id ty =
  let ty = !nondep_type' env id ty in
  if level = generic_level then duplicate_type ty else
  let old = !current_level in
  current_level := level;
  let ty = instance env ty in
  current_level := old;
  ty

(* Find the type paths nl1 in the module type mty2, and add them to the
   list (nl2, tl2). raise Not_found if impossible *)
let complete_type_list ?(allow_absent=false) env nl1 lv2 mty2 nl2 tl2 =
  let id2 = Ident.create "Pkg" in
  let env' = Env.add_module id2 mty2 env in
  let rec complete nl1 ntl2 =
    match nl1, ntl2 with
      [], _ -> ntl2
    | n :: nl, (n2, _ as nt2) :: ntl' when n >= n2 ->
        nt2 :: complete (if n = n2 then nl else nl1) ntl'
    | n :: nl, _ ->
        try
          let path =
            Env.lookup_type (concat_longident (Longident.Lident "Pkg") n) env'
          in
          match Env.find_type path env' with
            {type_arity = 0; type_kind = Type_abstract;
             type_private = Public; type_manifest = Some t2} ->
               (n, nondep_instance env' lv2 id2 t2) :: complete nl ntl2
          | {type_arity = 0; type_kind = Type_abstract;
             type_private = Public; type_manifest = None} when allow_absent ->
               complete nl ntl2
          | _ -> raise Exit
        with
        | Not_found when allow_absent -> complete nl ntl2
        | Exit -> raise Not_found
  in
  complete nl1 (List.combine nl2 tl2)

(* raise Not_found rather than Unify if the module types are incompatible *)
let unify_package env unify_list lv1 p1 n1 tl1 lv2 p2 n2 tl2 =
  let ntl2 = complete_type_list env n1 lv2 (Mty_ident p2) n2 tl2
  and ntl1 = complete_type_list env n2 lv1 (Mty_ident p1) n1 tl1 in
  unify_list (List.map snd ntl1) (List.map snd ntl2);
  if eq_package_path env p1 p2
  || !package_subtype env p1 n1 tl1 p2 n2 tl2
  && !package_subtype env p2 n2 tl2 p1 n1 tl1 then () else raise Not_found


(* force unification in Reither when one side has a non-conjunctive type *)
let rigid_variants = ref false

(* drop not force unification in Reither, even in fixed case
   (not sound, only use it when checking exhaustiveness) *)
let passive_variants = ref false
let with_passive_variants f x =
  if !passive_variants then f x else
  match passive_variants := true; f x with
  | r           -> passive_variants := false; r
  | exception e -> passive_variants := false; raise e

let unify_eq t1 t2 =
  t1 == t2 ||
  match !umode with
  | Expression -> false
  | Pattern ->
      try TypePairs.find unify_eq_set (order_type_pair t1 t2); true
      with Not_found -> false

let unify1_var env t1 t2 =
  assert (is_TTyVar t1);
  occur env t1 t2;
  occur_univar env t2;
  let d1 = t1.desc in
  link_type t1 t2;
  try
    update_level env t1.level t2
  with Unify _ as e ->
    t1.desc <- d1;
    raise e

let rec unify (env:Env.t ref) t1 t2 =
  (* First step: special cases (optimizations) *)
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if unify_eq t1 t2 then () else
  let reset_tracing = check_trace_gadt_instances !env in

  try
    type_changed := true;
    begin match (t1.desc, t2.desc) with
      (TTyVar _, TTyConstr _) when deep_occur t1 t2 ->
        unify2 env t1 t2
    | (TTyConstr _, TTyVar _) when deep_occur t2 t1 ->
        unify2 env t1 t2
    | (TTyVar _, _) ->
        unify1_var !env t1 t2
    | (_, TTyVar _) ->
        unify1_var !env t2 t1
    | (TTyUniVar _, TTyUniVar _) ->
        unify_univar t1 t2 !univar_pairs;
        update_level !env t1.level t2;
        link_type t1 t2
    | (TTyConstr (p1, [], a1), TTyConstr (p2, [], a2))
          when Path.same p1 p2 (* && actual_mode !env = Old *)
            (* This optimization assumes that t1 does not expand to t2
               (and conversely), so we fall back to the general case
               when any of the types has a cached expansion. *)
            && not (has_cached_expansion p1 !a1
                 || has_cached_expansion p2 !a2) ->
        update_level !env t1.level t2;
        link_type t1 t2
    | (TTyConstr (p1, [], _), TTyConstr (p2, [], _))
      when Env.has_local_constraints !env
      && is_newtype !env p1 && is_newtype !env p2 ->
        (* Do not use local constraints more than necessary *)
        begin try
          if find_newtype_level !env p1 < find_newtype_level !env p2 then
            unify env t1 (try_expand_once !env t2)
          else
            unify env (try_expand_once !env t1) t2
        with Cannot_expand ->
          unify2 env t1 t2
        end
    | _ ->
        unify2 env t1 t2
    end;
    reset_trace_gadt_instances reset_tracing;
  with Unify trace ->
    reset_trace_gadt_instances reset_tracing;
    raise (Unify ((t1, t2)::trace))

and unify2 env t1 t2 =
  (* Second step: expansion of abbreviations *)
  (* Expansion may change the representative of the types. *)
  ignore (expand_head_unif !env t1);
  ignore (expand_head_unif !env t2);
  let t1' = expand_head_unif !env t1 in
  let t2' = expand_head_unif !env t2 in
  let lv = min t1'.level t2'.level in
  update_level !env lv t2;
  update_level !env lv t1;
  if unify_eq t1' t2' then () else

  let t1 = repr t1 and t2 = repr t2 in
  if !trace_gadt_instances then begin
    (* All types in chains already have the same ambiguity levels *)
    let ilevel t =
      match Env.gadt_instance_level !env t with None -> 0 | Some lv -> lv in
    let lv1 = ilevel t1 and lv2 = ilevel t2 in
    if lv1 > lv2 then Env.add_gadt_instance_chain !env lv1 t2 else
    if lv2 > lv1 then Env.add_gadt_instance_chain !env lv2 t1
  end;
  let t1, t2 =
    if !Clflags.principal
    && (find_lowest_level t1' < lv || find_lowest_level t2' < lv) then
      (* Expand abbreviations hiding a lower level *)
      (* Should also do it for parameterized types, after unification... *)
      (match t1.desc with TTyConstr (_, [], _) -> t1' | _ -> t1),
      (match t2.desc with TTyConstr (_, [], _) -> t2' | _ -> t2)
    else (t1, t2)
  in
  if unify_eq t1 t1' || not (unify_eq t2 t2') then
    unify3 env t1 t1' t2 t2'
  else
    try unify3 env t2 t2' t1 t1' with Unify trace ->
      raise (Unify (List.map (fun (x, y) -> (y, x)) trace))

and unify3 env t1 t1' t2 t2' =
  (* Third step: truly unification *)
  (* Assumes either [t1 == t1'] or [t2 != t2'] *)
  let d1 = t1'.desc and d2 = t2'.desc in
  let create_recursion = (t2 != t2') && (deep_occur t1' t2) in

  begin match (d1, d2) with (* handle vars and univars specially *)
    (TTyUniVar _, TTyUniVar _) ->
      unify_univar t1' t2' !univar_pairs;
      link_type t1' t2'
  | (TTyVar _, _) ->
      occur !env t1' t2;
      occur_univar !env t2;
      link_type t1' t2;
  | (_, TTyVar _) ->
      occur !env t2' t1;
      occur_univar !env t1;
      link_type t2' t1;
  | (Tfield _, Tfield _) -> (* special case for GADTs *)
      unify_fields env t1' t2'
  | _ ->
    begin match !umode with
    | Expression ->
        occur !env t1' t2';
        link_type t1' t2
    | Pattern ->
        add_type_equality t1' t2'
    end;
    try
      begin match (d1, d2) with
        | (TTyArrow (t1, u1, c1), TTyArrow (t2, u2, c2)) ->
          unify_list env t1 t2; unify env u1 u2;
          begin match commu_repr c1, commu_repr c2 with
            |  TComLink r, c2 -> set_commu r c2
            | c1, TComLink r -> set_commu r c1
            | _ -> ()
          end
      | (TTyTuple tl1, TTyTuple tl2) ->
          unify_list env tl1 tl2
      | (TTyConstr (p1, tl1, _), TTyConstr (p2, tl2, _)) when Path.same p1 p2 ->
          if !umode = Expression || not !generate_equations then
            unify_list env tl1 tl2
          else if !assume_injective then
            set_mode_pattern ~generate:true ~injective:false
                             (fun () -> unify_list env tl1 tl2)
          else if in_current_module p1 (* || in_pervasives p1 *)
                  || List.exists (expands_to_datatype !env) [t1'; t1; t2] then
            unify_list env tl1 tl2
          else
            let inj =
              try List.map Variance.(mem Inj)
                    (Env.find_type p1 !env).type_variance
              with Not_found -> List.map (fun _ -> false) tl1
            in
            List.iter2
              (fun i (t1, t2) ->
                if i then unify env t1 t2 else
                set_mode_pattern ~generate:false ~injective:false
                  begin fun () ->
                    let snap = snapshot () in
                    try unify env t1 t2 with Unify _ ->
                      backtrack snap;
                      reify env t1; reify env t2
                  end)
              inj (List.combine tl1 tl2)
      | (TTyConstr (_,_,_), _) | (_, TTyConstr (_,_,_)) when !umode = Pattern ->
          reify env t1';
          reify env t2';
          if !generate_equations then mcomp !env t1' t2'
      | (TTyPoly (t1, []), TTyPoly (t2, [])) ->
          unify env t1 t2
      | (TTyPoly (t1, tl1), TTyPoly (t2, tl2)) ->
          enter_poly !env univar_pairs t1 tl1 t2 tl2 (unify env)
      | (_, _) ->
          raise (Unify [])
      end;
      (* XXX Commentaires + changer "create_recursion"
         ||| Comments + change "create_recursion" *)
      if create_recursion then
        match t2.desc with
          TTyConstr (p, tl, abbrev) ->
            forget_abbrev abbrev p;
            let t2'' = expand_head_unif !env t2 in
            if not (closed_parameterized_type tl t2'') then
              link_type (repr t2) (repr t2')
        | _ ->
            () (* t2 has already been expanded by update_level *)
    with Unify trace ->
      t1'.desc <- d1;
      raise (Unify trace)
  end

and unify_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (unify env) tl1 tl2

let unify env ty1 ty2 =
  let snap = Btype.snapshot () in
  try
    unify env ty1 ty2
  with
    Unify trace ->
      undo_compress snap;
      raise (Unify (expand_trace !env trace))
  | Recursive_abbrev ->
      undo_compress snap;
      raise (Unification_recursive_abbrev (expand_trace !env [(ty1,ty2)]))

let unify_gadt ~newtype_level:lev (env:Env.t ref) ty1 ty2 =
  try
    univar_pairs := [];
    newtype_level := Some lev;
    set_mode_pattern ~generate:true ~injective:true
                     (fun () -> unify env ty1 ty2);
    newtype_level := None;
    TypePairs.clear unify_eq_set;
  with e ->
    newtype_level := None;
    TypePairs.clear unify_eq_set;
    raise e

let unify_var env t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  if t1 == t2 then () else
  match t1.desc, t2.desc with
    TTyVar _, TTyConstr _ when deep_occur t1 t2 ->
      unify (ref env) t1 t2
  | TTyVar _, _ ->
      begin try
        occur env t1 t2;
        update_level env t1.level t2;
        link_type t1 t2;
      with Unify trace ->
        let expanded_trace = expand_trace env ((t1,t2)::trace) in
        raise (Unify expanded_trace)
      end
  | _ ->
      unify (ref env) t1 t2

let _ = unify' := unify_var

let unify_pairs env ty1 ty2 pairs =
  univar_pairs := pairs;
  unify env ty1 ty2

let unify env ty1 ty2 =
  unify_pairs (ref env) ty1 ty2 []



(**** Special cases of unification ****)

let expand_head_trace env t =
  let t = expand_head_unif env t in
  t

                        (***********************************)
                        (*  Matching between type schemes  *)
                        (***********************************)

(*
   Update the level of [ty]. First check that the levels of generic
   variables from the subject are not lowered.
*)
let moregen_occur env level ty =
  let rec occur ty =
    let ty = repr ty in
    if ty.level > level then begin
      if is_Tvar ty && ty.level >= generic_level - 1 then raise Occur;
      ty.level <- pivot_level - ty.level;
      iter_type_expr occur ty
    end
  in
  begin try
    occur ty; unmark_type ty
  with Occur ->
    unmark_type ty; raise (Unify [])
  end;
  (* also check for free univars *)
  occur_univar env ty;
  update_level env level ty

let may_instantiate inst_nongen t1 =
  if inst_nongen then t1.level <> generic_level - 1
                 else t1.level =  generic_level

let rec moregen inst_nongen type_pairs env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (TTyVar _, _) when may_instantiate inst_nongen t1 ->
        moregen_occur env t1.level t2;
        occur env t1 t2;
        link_type t1 t2
    | (TTyConstr (p1, [], _), TTyConstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head env t1 in
        let t2' = expand_head env t2 in
        (* Expansion may have changed the representative of the types... *)
        let t1' = repr t1' and t2' = repr t2' in
        if t1' == t2' then () else
        begin try
          TypePairs.find type_pairs (t1', t2')
        with Not_found ->
          TypePairs.add type_pairs (t1', t2') ();
          match (t1'.desc, t2'.desc) with
            (TTyVar _, _) when may_instantiate inst_nongen t1' ->
              moregen_occur env t1'.level t2;
              link_type t1' t2
          | (TTyArrow (t1, u1, _), TTyArrow (t2, u2, _)) ->
              moregen_list inst_nongen type_pairs env t1 t2;
              moregen inst_nongen type_pairs env u1 u2
          | (TTyTuple tl1, TTyTuple tl2) ->
              moregen_list inst_nongen type_pairs env tl1 tl2
          | (TTyConstr (p1, tl1, _), TTyConstr (p2, tl2, _))
                when Path.same p1 p2 ->
              moregen_list inst_nongen type_pairs env tl1 tl2
          | (TTyPoly (t1, []), TTyPoly (t2, [])) ->
              moregen inst_nongen type_pairs env t1 t2
          | (TTyPoly (t1, tl1), TTyPoly (t2, tl2)) ->
              enter_poly env univar_pairs t1 tl1 t2 tl2
                (moregen inst_nongen type_pairs env)
          | (TTyUniVar _, TTyUniVar _) ->
              unify_univar t1' t2' !univar_pairs
          | (_, _) ->
              raise (Unify [])
        end
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and moregen_list inst_nongen type_pairs env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (moregen inst_nongen type_pairs env) tl1 tl2

(* Must empty univar_pairs first *)
let moregen inst_nongen type_pairs env patt subj =
  univar_pairs := [];
  moregen inst_nongen type_pairs env patt subj

(*
   Non-generic variable can be instantiated only if [inst_nongen] is
   true. So, [inst_nongen] should be set to false if the subject might
   contain non-generic variables (and we do not want them to be
   instantiated).
   Usually, the subject is given by the user, and the pattern
   is unimportant.  So, no need to propagate abbreviations.
*)
let moregeneral env inst_nongen pat_sch subj_sch =
  let old_level = !current_level in
  current_level := generic_level - 1;
  (*
     Generic variables are first duplicated with [instance].  So,
     their levels are lowered to [generic_level - 1].  The subject is
     then copied with [duplicate_type].  That way, its levels won't be
     changed.
  *)
  let subj = duplicate_type (instance env subj_sch) in
  current_level := generic_level;
  (* Duplicate generic variables *)
  let patt = instance env pat_sch in
  let res =
    try moregen inst_nongen (TypePairs.create 13) env patt subj; true with
      Unify _ -> false
  in
  current_level := old_level;
  res


(* Alternative approach: "rigidify" a type scheme,
   and check validity after unification *)
(* Simpler, no? *)

let rec rigidify_rec vars ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    match ty.desc with
    | TTyVar _ ->
        if not (List.memq ty !vars) then vars := ty :: !vars
    | _ ->
        iter_type_expr (rigidify_rec vars) ty
  end

let rigidify ty =
  let vars = ref [] in
  rigidify_rec vars ty;
  unmark_type ty;
  !vars

let all_distinct_vars env vars =
  let tyl = ref [] in
  List.for_all
    (fun ty ->
      let ty = expand_head env ty in
      if List.memq ty !tyl then false else
      (tyl := ty :: !tyl; is_Tvar ty))
    vars

let matches env ty ty' =
  let snap = snapshot () in
  let vars = rigidify ty in
  cleanup_abbrev ();
  let ok =
    try unify env ty ty'; all_distinct_vars env vars
    with Unify _ -> false
  in
  backtrack snap;
  ok


                 (*********************************************)
                 (*  Equivalence between parameterized types  *)
                 (*********************************************)

let expand_head_rigid env ty =
  let old = !rigid_variants in
  rigid_variants := true;
  let ty' = expand_head env ty in
  rigid_variants := old; ty'

let normalize_subst subst =
  if List.exists
      (function {desc=TTyLink _}, _ | _, {desc=TTyLink _} -> true | _ -> false)
      !subst
  then subst := List.map (fun (t1,t2) -> repr t1, repr t2) !subst

let rec eqtype rename type_pairs subst env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (TTyVar _, TTyVar _) when rename ->
        begin try
          normalize_subst subst;
          if List.assq t1 !subst != t2 then raise (Unify [])
        with Not_found ->
          if List.exists (fun (_, t) -> t == t2) !subst then raise (Unify []);
          subst := (t1, t2) :: !subst
        end
    | (TTyConstr (p1, [], _), TTyConstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head_rigid env t1 in
        let t2' = expand_head_rigid env t2 in
        (* Expansion may have changed the representative of the types... *)
        let t1' = repr t1' and t2' = repr t2' in
        if t1' == t2' then () else
        begin try
          TypePairs.find type_pairs (t1', t2')
        with Not_found ->
          TypePairs.add type_pairs (t1', t2') ();
          match (t1'.desc, t2'.desc) with
            (TTyVar _, TTyVar _) when rename ->
              begin try
                normalize_subst subst;
                if List.assq t1' !subst != t2' then raise (Unify [])
              with Not_found ->
                if List.exists (fun (_, t) -> t == t2') !subst
                then raise (Unify []);
                subst := (t1', t2') :: !subst
              end
          | (TTyArrow (t1, u1, _), TTyArrow (t2, u2, _)) ->
              eqtype_list rename type_pairs subst env t1 t2;
              eqtype rename type_pairs subst env u1 u2;
          | (TTyTuple tl1, TTyTuple tl2) ->
              eqtype_list rename type_pairs subst env tl1 tl2
          | (TTyConstr (p1, tl1, _), TTyConstr (p2, tl2, _))
                when Path.same p1 p2 ->
              eqtype_list rename type_pairs subst env tl1 tl2
          | (TTyPoly (t1, []), TTyPoly (t2, [])) ->
              eqtype rename type_pairs subst env t1 t2
          | (TTyPoly (t1, tl1), TTyPoly (t2, tl2)) ->
              enter_poly env univar_pairs t1 tl1 t2 tl2
                (eqtype rename type_pairs subst env)
          | (TTyUniVar _, TTyUniVar _) ->
              unify_univar t1' t2' !univar_pairs
          | (_, _) ->
              raise (Unify [])
        end
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and eqtype_list rename type_pairs subst env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (eqtype rename type_pairs subst env) tl1 tl2

(* Must empty univar_pairs first *)
let eqtype_list rename type_pairs subst env tl1 tl2 =
  univar_pairs := [];
  let snap = Btype.snapshot () in
  try eqtype_list rename type_pairs subst env tl1 tl2; backtrack snap
  with exn -> backtrack snap; raise exn

let eqtype rename type_pairs subst env t1 t2 =
  eqtype_list rename type_pairs subst env [t1] [t2]

(* Two modes: with or without renaming of variables *)
let equal env rename tyl1 tyl2 =
  try
    eqtype_list rename (TypePairs.create 11) (ref []) env tyl1 tyl2; true
  with
    Unify _ -> false
                              (*******************)
                              (*  Miscellaneous  *)
                              (*******************)

(* Utility for printing. The resulting type is not used in computation. *)
let rec unalias_object ty =
  let ty = repr ty in
  match ty.desc with
  | TTyVar _ ->
      newty2 ty.level ty.desc
  | TTyUniVar _ ->
      ty
  | TTyConstr _ ->
      newvar2 ty.level
  | _ ->
      assert false

let unalias ty =
  let ty = repr ty in
  match ty.desc with
  | TTyVar _ | TTyUniVar _ ->
      ty
  | _ ->
      newty2 ty.level ty.desc

(* Return the arity (as for curried functions) of the given type. *)
let rec arity ty =
  match (repr ty).desc with
  | TTyArrow(tl, _, _) -> List.length tl
  | _ -> 0

(* Check whether an abbreviation expands to itself. *)
let cyclic_abbrev env id ty =
  let rec check_cycle seen ty =
    let ty = repr ty in
    match ty.desc with
      TTyConstr (p, _tl, _abbrev) ->
        p = Path.PIdent id || List.memq ty seen ||
        begin try
          check_cycle (ty :: seen) (expand_abbrev_opt env ty)
        with
          Cannot_expand -> false
        | Unify _ -> true
        end
    | _ ->
        false
  in check_cycle [] ty

(* Check for non-generalizable type variables *)
exception Non_closed0
let visited = ref TypeSet.empty

let rec closed_schema_rec env ty =
  let ty = repr ty in
  if TypeSet.mem ty !visited then () else begin
    visited := TypeSet.add ty !visited;
    match ty.desc with
      TTyVar _ when ty.level <> generic_level ->
        raise Non_closed0
    | TTyConstr _ ->
        let old = !visited in
        begin try iter_type_expr (closed_schema_rec env) ty
        with Non_closed0 -> try
          visited := old;
          closed_schema_rec env (try_expand_head try_expand_safe env ty)
        with Cannot_expand ->
          raise Non_closed0
        end
    | _ ->
        iter_type_expr (closed_schema_rec env) ty
  end

(* Return whether all variables of type [ty] are generic. *)
let closed_schema env ty =
  visited := TypeSet.empty;
  try
    closed_schema_rec env ty;
    visited := TypeSet.empty;
    true
  with Non_closed0 ->
    visited := TypeSet.empty;
    false

(* Normalize a type before printing, saving... *)
(* Cannot use mark_type because deep_occur uses it too *)
let rec normalize_type_rec env visited ty =
  let ty = repr ty in
  if not (TypeSet.mem ty !visited) then begin
    visited := TypeSet.add ty !visited;
    begin
      match ty.desc with (* PR#7348 *)
        TTyConstr(Path.PExternal(m,i,pos), tl, _abbrev) ->
        let i' = String.sub i 0 (String.length i - 4) in
        log_type ty;
        ty.desc <- TTyConstr(Path.PExternal(m,i',pos), tl, ref TMemNil)
      | _ -> assert false
    end;
    iter_type_expr (normalize_type_rec env visited) ty
  end

let normalize_type env ty =
  normalize_type_rec env (ref TypeSet.empty) ty


                              (*************************)
                              (*  Remove dependencies  *)
                              (*************************)


(*
   Variables are left unchanged. Other type nodes are duplicated, with
   levels set to generic level.
   We cannot use Tsubst here, because unification may be called by
   expand_abbrev.
*)

let nondep_hash     = TypeHash.create 47
let nondep_variants = TypeHash.create 17
let clear_hash ()   =
  TypeHash.clear nondep_hash; TypeHash.clear nondep_variants

let rec nondep_type_rec env id ty =
  match ty.desc with
    TTyVar _ | TTyUniVar _ -> ty
  | TTyLink ty -> nondep_type_rec env id ty
  | _ -> try TypeHash.find nondep_hash ty
  with Not_found ->
    let ty' = newgenvar () in        (* Stub *)
    TypeHash.add nondep_hash ty ty';
    ty'.desc <-
      begin match ty.desc with
      | TTyConstr(p, tl, _abbrev) ->
          if Path.isfree id p then
            begin try
              TTyLink (nondep_type_rec env id
                       (expand_abbrev env (newty2 ty.level ty.desc)))
              (*
                 The [TTyLink] is important. The expanded type may be a
                 variable, or may not be completely copied yet
                 (recursive type), so one cannot just take its
                 description.
               *)
            with Cannot_expand | Unify _ ->
              raise Not_found
            end
          else
            TTyConstr(p, List.map (nondep_type_rec env id) tl, ref TMemNil)
      | _ -> copy_type_desc (nondep_type_rec env id) ty.desc
      end;
    ty'

let nondep_type env id ty =
  try
    let ty' = nondep_type_rec env id ty in
    clear_hash ();
    ty'
  with Not_found ->
    clear_hash ();
    raise Not_found

let () = nondep_type' := nondep_type

let unroll_abbrev id tl ty =
  let ty = repr ty and path = Path.PIdent id in
  if is_Tvar ty || (List.exists (deep_occur ty) tl)
  || is_object_type path then
    ty
  else
    let ty' = newty2 ty.level ty.desc in
    link_type ty (newty2 ty.level (TTyConstr (path, tl, ref TMemNil)));
    ty'

(* Preserve sharing inside type declarations. *)
let nondep_type_decl env mid id is_covariant decl =
  try
    let params = List.map (nondep_type_rec env mid) decl.type_params in
    let tk =
      try map_kind (nondep_type_rec env mid) decl.type_kind
      with Not_found when is_covariant -> Type_abstract
    and tm =
      try match decl.type_manifest with
        None -> None
      | Some ty ->
          Some (unroll_abbrev id params (nondep_type_rec env mid ty))
      with Not_found when is_covariant ->
        None
    in
    clear_hash ();
    { type_params = params;
      type_arity = decl.type_arity;
      type_kind = tk;
      type_manifest = tm;
      type_newtype_level = None;
      type_loc = decl.type_loc;
      type_immediate = decl.type_immediate;
    }
  with Not_found ->
    clear_hash ();
    raise Not_found

(* collapse conjunctive types in class parameters *)
let rec collapse_conj env visited ty =
  let ty = repr ty in
  if List.memq ty visited then () else
  let visited = ty :: visited in
  iter_type_expr (collapse_conj env visited) ty

let collapse_conj_params env params =
  List.iter (collapse_conj env []) params

let same_constr env t1 t2 =
  let t1 = expand_head env t1 in
  let t2 = expand_head env t2 in
  match t1.desc, t2.desc with
  | TTyConstr (p1, _, _), TTyConstr (p2, _, _) -> Path.same p1 p2
  | _ -> false

let () =
  Env.same_constr := same_constr

let maybe_pointer_type env typ =
   match (repr typ).desc with
  | TTyConstr(p, _args, _abbrev) ->
    begin try
      let type_decl = Env.find_type p env in
      not type_decl.type_immediate
    with Not_found -> true
    (* This can happen due to e.g. missing -I options,
       causing some .cmi files to be unavailable.
       Maybe we should emit a warning. *)
    end
  | _ -> true
