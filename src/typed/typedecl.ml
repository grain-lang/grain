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

(**** Typing of type definitions ****)

open Grain_parsing
open Misc
open Asttypes
open Parsetree
open Primitive
open Types
open Typetexp

type native_repr_kind = Unboxed | Untagged

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string
  | Cycle_in_def of string * type_expr
  | Definition_mismatch of type_expr * Includecore.type_mismatch list
  | Constraint_failed of type_expr * type_expr
  | Inconsistent_constraint of Env.t * (type_expr * type_expr) list
  | Type_clash of Env.t * (type_expr * type_expr) list
  | Parameters_differ of Path.t * type_expr * type_expr
  | Null_arity_external
  | Missing_native_external
  | Unbound_type_var of type_expr * type_declaration
  | Cannot_extend_private_type of Path.t
  | Not_extensible_type of Path.t
  | Extension_mismatch of Path.t * Includecore.type_mismatch list
  | Rebind_wrong_type of Longident.t * Env.t * (type_expr * type_expr) list
  | Rebind_mismatch of Longident.t * Path.t * Path.t
  | Rebind_private of Longident.t
  | Bad_variance of int * (bool * bool * bool) * (bool * bool * bool)
  | Unavailable_type_constructor of Path.t
  | Bad_fixed_type of string
  | Unbound_type_var_ext of type_expr * extension_constructor
  | Varying_anonymous
  | Val_in_structure
  | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type of native_repr_kind
  | Deep_unbox_or_untag_attribute of native_repr_kind
  | Bad_immediate_attribute
  | Bad_unboxed_attribute of string
  | Wrong_unboxed_type_float
  | Boxed_and_unboxed
  | Nonrec_gadt

open Typedtree

exception Error of Location.t * error

(* Enter all declared types in the environment as abstract types *)

let enter_type rec_flag env sdecl id =
  let needed =
    match rec_flag with
    | Asttypes.Nonrecursive ->
      false
        (*begin match sdecl.pdata_kind with
        | PDataVariant scds ->
            List.iter (fun cd ->
              if cd.pcd_res <> None then raise (Error(cd.pcd_loc, Nonrec_gadt)))
              scds
        | _ -> ()
        end;
        Btype.is_row_name (Ident.name id)*)
    | Asttypes.Recursive -> true
  in
  if not needed then env else
  let decl =
    { type_params =
        List.map (fun _ -> Btype.newgenvar ()) sdecl.pdata_params;
      type_arity = List.length sdecl.pdata_params;
      type_kind = TDataAbstract;
      type_manifest = None;
        (*begin match sdecl.pdata_manifest with None -> None
        | Some _ -> Some(Ctype.newvar ()) end;*)
      type_newtype_level = None;
      type_loc = sdecl.pdata_loc;
      type_immediate = false;
    }
  in
  Env.add_type ~check:true id decl env


let update_type temp_env env id loc =
  let path = Path.PIdent id in
  let decl = Env.find_type path temp_env in
  match decl.type_manifest with None -> ()
  | Some ty ->
      let params = List.map (fun _ -> Ctype.newvar ()) decl.type_params in
      try Ctype.unify env (Ctype.newconstr path params) ty
      with Ctype.Unify trace ->
        raise (Error(loc, Type_clash (env, trace)))


(* Translate one type declaration *)

module StringSet =
  Set.Make(struct
    type t = string
    let compare (x:t) y = compare x y
  end)

let make_params env params =
  let make_param sty =
    try
      transl_type_param env sty
    with Already_bound ->
      raise(Error(sty.ptyp_loc, Repeated_parameter))
  in
    List.map make_param params


let transl_constructor_arguments env closed = function
  | PConstrTuple l ->
    let l = List.map (transl_simple_type env closed) l in
    Types.TConstrTuple (List.map (fun t -> t.ctyp_type) l),
    TConstrTuple l
  | PConstrSingleton ->
    Types.TConstrSingleton,
    TConstrSingleton


let make_constructor env type_path type_params sargs =
  let args, targs =
    transl_constructor_arguments env true sargs
  in
  targs, None, args, None, type_params

(* Check that the variable [id] is present in the [univ] list. *)
let check_type_var loc univ id =
  let f t = (Btype.repr t).id = id in
  if not (List.exists f univ) then raise (Error (loc, Wrong_unboxed_type_float))



let transl_declaration env sdecl id =
  (* Bind type parameters *)
  reset_type_variables();
  Ctype.begin_def ();
  let tparams = make_params env sdecl.pdata_params in
  let params = List.map (fun cty -> cty.ctyp_type) tparams in
  (* [constraints]
     let cstrs = List.map
    (fun (sty, sty', loc) ->
      transl_simple_type env false sty,
      transl_simple_type env false sty', loc)
    sdecl.ptype_cstrs
  in*)
  (*let raw_status = get_unboxed_from_attributes sdecl in
  if raw_status.unboxed && not raw_status.default then begin
    match sdecl.pdata_kind with
    | PDataVariant [{pcd_args = PConstrTuple []; _}] ->
      raise(Error(sdecl.pdata_loc, Bad_unboxed_attribute
                    "its constructor has no argument"))
    | PDataVariant [{pcd_args = PConstrTuple [_]; _}] -> ()
    | PDataVariant _ ->
      raise(Error(sdecl.pdata_loc, Bad_unboxed_attribute
                    "it has more than one constructor"))
  end;
  let unboxed_status =
    match sdecl.ptype_kind with
    | PDataVariant [{pcd_args = PConstrTuple [_]; _}]
    | _ -> (* The type is not unboxable, mark it as boxed *)
      unboxed_false_default_false
  in
  let unbox = unboxed_status.unboxed in*)
  let (tkind, kind) =
    match sdecl.pdata_kind with
      | PDataVariant scstrs ->
        assert (scstrs <> []);
        let all_constrs = ref StringSet.empty in
        List.iter
          (fun {pcd_name = {txt = name}} ->
            if StringSet.mem name !all_constrs then
              raise(Error(sdecl.pdata_loc, Duplicate_constructor name));
            all_constrs := StringSet.add name !all_constrs)
          scstrs;
        if List.length
            (List.filter (fun cd -> cd.pcd_args <> PConstrSingleton) scstrs)
           > (Config.max_tag + 1) then
          raise(Error(sdecl.pdata_loc, Too_many_constructors));
        let make_cstr scstr =
          let name = Ident.create scstr.pcd_name.txt in
          let targs, tret_type, args, ret_type, cstr_params =
            make_constructor env (Path.PIdent id) params
                             scstr.pcd_args
          in
          (*if Config.flat_float_array && unbox then begin
            (* Cannot unbox a type when the argument can be both float and
               non-float because it interferes with the dynamic float array
               optimization. This can only happen when the type is a GADT
               and the argument is an existential type variable or an
               unboxed (or abstract) type constructor applied to some
               existential type variable. Of course we also have to rule
               out any abstract type constructor applied to anything that
               might be an existential type variable.
               There is a difficulty with existential variables created
               out of thin air (rather than bound by the declaration).
               See PR#7511 and GPR#1133 for details. *)
            match Datarepr.constructor_existentials args ret_type with
            | _, [] -> ()
            | [argty], _ex ->
                check_unboxed_gadt_arg sdecl.ptype_loc cstr_params env argty
            | _ -> assert false
          end;*)
          let tcstr =
            { cd_id = name;
              cd_name = scstr.pcd_name;
              cd_args = targs;
              cd_res = tret_type;
              cd_loc = scstr.pcd_loc; }
          in
          let cstr =
            { Types.cd_id = name;
              cd_args = args;
              cd_res = ret_type;
              cd_loc = scstr.pcd_loc; }
          in
            tcstr, cstr
        in
        let make_cstr scstr =
          (*Builtin_attributes.warning_scope scstr.pcd_attributes
            (fun () -> make_cstr scstr)*)
          make_cstr scstr
        in
        let tcstrs, cstrs = List.split (List.map make_cstr scstrs) in
          TDataVariant tcstrs, Types.TDataVariant cstrs
      in
    let (tman, man) = None, None in (*match sdecl.ptype_manifest with
        None -> None, None
      | Some sty ->
        let no_row = not (is_fixed_type sdecl) in
        let cty = transl_simple_type env no_row sty in
        Some cty, Some cty.ctyp_type
    in*)
    let decl =
      { type_params = params;
        type_arity = List.length params;
        type_kind = kind;
        type_manifest = man;
        type_newtype_level = None;
        type_loc = sdecl.pdata_loc;
        type_immediate = false;
      } in

  (* Check constraints *)
    (*List.iter
      (fun (cty, cty', loc) ->
        let ty = cty.ctyp_type in
        let ty' = cty'.ctyp_type in
        try Ctype.unify env ty ty' with Ctype.Unify tr ->
          raise(Error(loc, Inconsistent_constraint (env, tr))))
      cstrs;*)
    Ctype.end_def ();
  (* Add abstract row *)
    (*if is_fixed_type sdecl then begin
      let p =
        try Env.lookup_type (Identifier.IdentNmae(Ident.name id ^ "#row")) env
        with Not_found -> assert false in
      set_fixed_row env sdecl.ptype_loc p decl
    end;*)
  (* Check for cyclic abbreviations *)
    begin match decl.type_manifest with None -> ()
      | Some ty ->
        if Ctype.cyclic_abbrev env id ty then
          raise(Error(sdecl.pdata_loc, Recursive_abbrev sdecl.pdata_name.txt));
    end;
    {
      data_id = id;
      data_name = sdecl.pdata_name;
      data_params = tparams;
      data_type = decl;
      data_loc = sdecl.pdata_loc;
      data_kind = tkind;
    }

(* Generalize a type declaration *)

let generalize_decl decl =
  List.iter Ctype.generalize decl.type_params;
  Btype.iter_type_expr_kind Ctype.generalize decl.type_kind;
  begin match decl.type_manifest with
  | None    -> ()
  | Some ty -> Ctype.generalize ty
  end

(* Check that all constraints are enforced *)

module TypeSet = Btype.TypeSet
module TypeMap = Btype.TypeMap

(* Check that recursion is well-founded *)

let check_well_founded env loc path to_check ty =
  let visited = ref TypeMap.empty in
  let rec check ty0 parents ty =
    let ty = Btype.repr ty in
    if TypeSet.mem ty parents then begin
      (*Format.eprintf "@[%a@]@." Printtyp.raw_type_expr ty;*)
      if match ty0.desc with
      | TTyConstr (p, _, _) -> Path.same p path
      | _ -> false
      then raise (Error (loc, Recursive_abbrev (Path.name path)))
      else raise (Error (loc, Cycle_in_def (Path.name path, ty0)))
    end;
    let (fini, parents) =
      try
        let prev = TypeMap.find ty !visited in
        if TypeSet.subset parents prev then (true, parents) else
        (false, TypeSet.union parents prev)
      with Not_found ->
        (false, parents)
    in
    if fini then () else
    let rec_ok =
      match ty.desc with
      | TTyConstr(p,_,_) ->
          !Grain_utils.Config.recursive_types && Ctype.is_contractive env p
      | _ -> !Grain_utils.Config.recursive_types
    in
    let visited' = TypeMap.add ty parents !visited in
    let arg_exn =
      try
        visited := visited';
        let parents =
          if rec_ok then TypeSet.empty else TypeSet.add ty parents in
        Btype.iter_type_expr (check ty0 parents) ty;
        None
      with e ->
        visited := visited'; Some e
    in
    match ty.desc with
    | TTyConstr(p, _, _) when arg_exn <> None || to_check p ->
        if to_check p then may raise arg_exn
        else Btype.iter_type_expr (check ty0 TypeSet.empty) ty;
        begin try
          let ty' = Ctype.try_expand_once_opt env ty in
          let ty0 = if TypeSet.is_empty parents then ty else ty0 in
          check ty0 (TypeSet.add ty parents) ty'
        with
          Ctype.Cannot_expand -> may raise arg_exn
        end
    | _ -> may raise arg_exn
  in
  let snap = Btype.snapshot () in
  try Ctype.wrap_trace_gadt_instances env (check ty TypeSet.empty) ty
  with Ctype.Unify _ ->
    (* Will be detected by check_recursion *)
    Btype.backtrack snap

let check_well_founded_manifest env loc path decl =
  if decl.type_manifest = None then () else
  let args = List.map (fun _ -> Ctype.newvar()) decl.type_params in
  check_well_founded env loc path (Path.same path) (Ctype.newconstr path args)

let check_well_founded_decl env loc path decl to_check =
  let open Btype in
  let it =
    {type_iterators with
     it_type_expr = (fun _ -> check_well_founded env loc path to_check)} in
  it.it_type_declaration it (Ctype.instance_declaration decl)


(* Check for ill-defined abbrevs *)

let check_recursion env loc path decl to_check =
  (* to_check is true for potentially mutually recursive paths.
     (path, decl) is the type declaration to be checked. *)

  if decl.type_params = [] then () else

  let visited = ref [] in

  let rec check_regular cpath args prev_exp ty =
    let ty = Ctype.repr ty in
    if not (List.memq ty !visited) then begin
      visited := ty :: !visited;
      match ty.desc with
      | TTyConstr(path', args', _) ->
          if Path.same path path' then begin
            if not (Ctype.equal env false args args') then
              raise (Error(loc,
                     Parameters_differ(cpath, ty, Ctype.newconstr path args)))
          end
          (* Attempt to expand a type abbreviation if:
              1- [to_check path'] holds
                 (otherwise the expansion cannot involve [path]);
              2- we haven't expanded this type constructor before
                 (otherwise we could loop if [path'] is itself
                 a non-regular abbreviation). *)
          else if to_check path' && not (List.mem path' prev_exp) then begin
            try
              (* Attempt expansion *)
              let (params0, body0, _) = Env.find_type_expansion path' env in
              let (params, body) =
                Ctype.instance_parameterized_type params0 body0 in
              begin
                try List.iter2 (Ctype.unify env) params args'
                with Ctype.Unify _ ->
                  raise (Error(loc, Constraint_failed
                                 (ty, Ctype.newconstr path' params0)));
              end;
              check_regular path' args (path' :: prev_exp) body
            with Not_found -> ()
          end;
          List.iter (check_regular cpath args prev_exp) args'
      | TTyPoly(ty, tl) ->
          let (_, ty) = Ctype.instance_poly ~keep_names:true false tl ty in
          check_regular cpath args prev_exp ty
      | _ ->
          Btype.iter_type_expr (check_regular cpath args prev_exp) ty
    end in

  Misc.may
    (fun body ->
      let (args, body) =
        Ctype.instance_parameterized_type
          ~keep_names:true decl.type_params body in
      check_regular path args [] body)
    decl.type_manifest

let check_abbrev_recursion env id_loc_list to_check tdecl =
  let decl = tdecl.data_type in
  let id = tdecl.data_id in
  check_recursion env (List.assoc id id_loc_list) (Path.PIdent id) decl to_check


(* Check multiple declarations of labels/constructors *)

let check_duplicates sdecl_list =
  let labels = Hashtbl.create 7 and constrs = Hashtbl.create 7 in
  List.iter
    (fun sdecl -> match sdecl.pdata_kind with
       | PDataVariant cl ->
         List.iter
           (fun pcd ->
              try
                let name' = Hashtbl.find constrs pcd.pcd_name.txt in ()
                (*Location.prerr_warning pcd.pcd_loc
                  (Warnings.Duplicate_definitions
                     ("constructor", pcd.pcd_name.txt, name',
                      sdecl.ptype_name.txt))*)
              with Not_found ->
                Hashtbl.add constrs pcd.pcd_name.txt sdecl.pdata_name.txt)
           cl)
    sdecl_list

(* Force recursion to go through id for private types*)
let name_recursion sdecl id decl =
  match decl with
  | _ -> decl



(* Translate a set of type declarations, mutually recursive or not *)
let transl_data_decl env rec_flag sdecl_list =
  (*(* Add dummy types for fixed rows *)
  let fixed_types = List.filter is_fixed_type sdecl_list in
  let sdecl_list =
    List.map
      (fun sdecl ->
        let ptype_name =
          mkloc (sdecl.pdata_name.txt ^"#row") sdecl.pdata_name.loc in
        {sdecl with
         pdata_name; pdata_kind = Ptype_abstract; pdata_manifest = None})
      fixed_types
    @ sdecl_list
  in*)

  (* Create identifiers. *)
  let id_list =
    List.map (fun sdecl -> Ident.create sdecl.pdata_name.txt) sdecl_list
  in
  (*
     Since we've introduced fresh idents, make sure the definition
     level is at least the binding time of these events. Otherwise,
     passing one of the recursively-defined type constrs as argument
     to an abbreviation may fail.
  *)
  Ctype.init_def(Ident.current_time());
  Ctype.begin_def();
  (* Enter types. *)
  let temp_env =
    List.fold_left2 (enter_type rec_flag) env sdecl_list id_list in
  (* Translate each declaration. *)
  let current_slot = ref None in
  let warn_unused = Warnings.is_active (Warnings.Unused_type_declaration "") in
  let id_slots id =
    match rec_flag with
    | Asttypes.Recursive when warn_unused ->
        (* See typecore.ml for a description of the algorithm used
             to detect unused declarations in a set of recursive definitions. *)
        let slot = ref [] in
        (*let td = Env.find_type (Path.PIdent id) temp_env in
        let name = Ident.name id in*)
        (*Env.set_type_used_callback
          name td
          (fun old_callback ->
             match !current_slot with
             | Some slot -> slot := (name, td) :: !slot
             | None ->
                 List.iter (fun (name, d) -> Env.mark_type_used env name d)
                   (get_ref slot);
                 old_callback ()
          );*)
        id, Some slot
    | Asttypes.Recursive | Asttypes.Nonrecursive ->
        id, None
  in
  let transl_declaration name_sdecl (id, slot) =
    current_slot := slot;
    transl_declaration temp_env name_sdecl id
    (*Builtin_attributes.warning_scope
      name_sdecl.ptype_attributes
      (fun () -> transl_declaration temp_env name_sdecl id)*)
  in
  let tdecls =
    List.map2 transl_declaration sdecl_list (List.map id_slots id_list) in
  let decls =
    List.map (fun tdecl -> (tdecl.data_id, tdecl.data_type)) tdecls in
  current_slot := None;
  (* Check for duplicates *)
  check_duplicates sdecl_list;
  (* Build the final env. *)
  let newenv =
    List.fold_right
      (fun (id, decl) env -> Env.add_type ~check:true id decl env)
      decls env
  in
  (* Update stubs *)
  begin match rec_flag with
    | Asttypes.Nonrecursive -> ()
    | Asttypes.Recursive ->
      List.iter2
        (fun id sdecl -> update_type temp_env newenv id sdecl.pdata_loc)
        id_list sdecl_list
  end;
  (* Generalize type declarations. *)
  Ctype.end_def();
  List.iter (fun (_, decl) -> generalize_decl decl) decls;
  (* Check for ill-formed abbrevs *)
  let id_loc_list =
    List.map2 (fun id sdecl -> (id, sdecl.pdata_loc))
      id_list sdecl_list
  in
  List.iter (fun (id, decl) ->
    check_well_founded_manifest newenv (List.assoc id id_loc_list)
      (Path.PIdent id) decl)
    decls;
  let to_check =
    function Path.PIdent id -> List.mem_assoc id id_loc_list | _ -> false in
  List.iter (fun (id, decl) ->
    check_well_founded_decl newenv (List.assoc id id_loc_list) (Path.PIdent id)
      decl to_check)
    decls;
  List.iter (check_abbrev_recursion newenv id_loc_list to_check) tdecls;
  (* Check that all type variables are closed *)
  List.iter2
    (fun sdecl tdecl ->
      let decl = tdecl.data_type in
       match Ctype.closed_type_decl decl with
         Some ty -> raise(Error(sdecl.pdata_loc, Unbound_type_var(ty,decl)))
       | None   -> ())
    sdecl_list tdecls;
  (* Check that constraints are enforced *)
  (*List.iter2 (check_constraints newenv) sdecl_list decls;*)
  (* Name recursion *)
  let decls =
    List.map2 (fun sdecl (id, decl) -> id, name_recursion sdecl id decl)
      sdecl_list decls
  in
  (* Add variances to the environment *)
  (*let required =
    List.map
      (fun sdecl ->
         add_injectivity (List.map snd sdecl.pdata_params),
         sdecl.pdata_loc
      )
      sdecl_list
  in*)
  let final_decls, final_env = decls, List.fold_right
                                 (fun (id, decl) env -> Env.add_type ~check:true id decl env) decls env
    (*compute_properties_fixpoint env decls required
      (List.map init_variance decls)
      (List.map (fun _ -> false) decls)*)
  in
  (* FIXME: Check re-exportation *)
  (*List.iter2 (check_abbrev final_env) sdecl_list final_decls;*)
  (* Keep original declaration *)
  let final_decls =
    List.map2
      (fun tdecl (_id2, decl) ->
        { tdecl with data_type = decl }
      ) tdecls final_decls
  in
  (* Done *)
  (final_decls, final_env)
