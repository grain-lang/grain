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

(* typetexp.ml,v 1.34.4.9 2002/01/07 08:39:16 garrigue Exp *)

(* Typechecking of type expressions for the core language *)
open Grain_parsing
open Grain_utils
open Asttypes
open Misc
open Parsetree
open Typedtree
open Types
open Ctype

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Identifier.t
  | Unbound_type_constructor_2 of Path.t
  | Type_arity_mismatch of Identifier.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Identifier.t
  | Type_mismatch of (type_expr * type_expr) list
  | Alias_type_mismatch of (type_expr * type_expr) list
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * type_expr
  | Multiple_constraints_on_type of Identifier.t
  | Method_mismatch of string * type_expr * type_expr
  | Unbound_value of Identifier.t
  | Unbound_value_in_module of Identifier.t * string
  | Unbound_constructor of Identifier.t
  | Unbound_label of Identifier.t
  | Unbound_module of Identifier.t
  | Unbound_class of Identifier.t
  | Unbound_modtype of Identifier.t
  | Unbound_cltype of Identifier.t
  | Ill_typed_functor_application
      of Identifier.t * Identifier.t * Includemod.error list option
  | Illegal_reference_to_recursive_module
  | Wrong_use_of_module of Identifier.t * [ `Structure_used_as_functor
                                         | `Abstract_used_as_functor
                                         | `Functor_used_as_structure
                                         | `Abstract_used_as_structure
                                         | `Generative_used_as_applicative
                                         ]
  | Cannot_scrape_alias of Identifier.t * Path.t
  | Opened_object of Path.t option
  | Not_an_object of type_expr

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error


type variable_context = int * (string, type_expr) Tbl.t

(* Local definitions *)

let instance_list = Ctype.instance_list Env.empty

(* Narrowing unbound identifier errors. *)

let rec narrow_unbound_lid_error : 'a. _ -> _ -> _ -> _ -> 'a =
  fun env loc lid make_error ->
  let check_module mlid =
    try ignore (Env.lookup_module ~load:false mlid None env) with
    | Not_found ->
        Printf.eprintf "aye not there lad\n";
        narrow_unbound_lid_error env loc mlid (fun lid -> Unbound_module lid)
  in
  let error e = raise (Error (loc, env, e)) in
  begin match lid with
  | Identifier.IdentName _ -> ()
  | Identifier.IdentExternal (mlid, id) ->
      check_module mlid;
      error (Unbound_value_in_module (mlid, id))
      (* let md = Env.find_module (Env.lookup_module ~load:true mlid None env) None env in
      begin match Env.scrape_alias env md.md_type with
      | TModIdent _ ->
         error (Wrong_use_of_module (mlid, `Abstract_used_as_structure))
      | TModSignature _ -> ()
      | TModAlias _ -> ()
      end *)
  end;
  error (make_error lid)

let find_component (lookup : ?mark:_ -> _) make_error env loc lid =
  try
    match lid with
    | Identifier.IdentExternal (Identifier.IdentName "*predef*", s) ->
        lookup (Identifier.IdentName s) Env.initial_safe_string
    | _ ->
        lookup lid env
  with Not_found ->
    narrow_unbound_lid_error env loc lid make_error

let find_type env loc lid =
  let path =
    find_component Env.lookup_type (fun lid -> Unbound_type_constructor lid)
      env loc lid
  in
  let decl = Env.find_type path env in
  (path, decl)

let find_constructor =
  find_component Env.lookup_constructor (fun lid -> Unbound_constructor lid)
let find_all_constructors =
  find_component Env.lookup_all_constructors
    (fun lid -> Unbound_constructor lid)

let find_value env loc lid =
  Env.check_value_name (Identifier.last lid) loc;
  let (path, decl) as r =
    find_component Env.lookup_value (fun lid -> Unbound_value lid) env loc lid
  in
  r

let lookup_module ?(load=false) env loc lid filepath =
  find_component (fun ?mark lid env -> (Env.lookup_module ~load ~loc:loc ?mark lid filepath env))
    (fun lid -> Unbound_module lid) env loc lid

let find_module env loc lid filepath =
  let path = lookup_module ~load:true env loc lid filepath in
  let decl = Env.find_module path filepath env in
  (* No need to check for deprecated here, this is done in Env. *)
  (path, decl)

let find_modtype env loc lid =
  let (path, decl) as r =
    find_component (fun ?(mark=false) a b -> Env.lookup_modtype ~loc ~mark a b) (fun lid -> Unbound_modtype lid)
      env loc lid
  in
  r

let unbound_constructor_error env lid =
  narrow_unbound_lid_error env lid.loc lid.txt
    (fun lid -> Unbound_constructor lid)

(* Support for first-class modules. *)

let transl_modtype_identifier = ref (fun _ -> assert false)
let transl_modtype = ref (fun _ -> assert false)

(* Translation of type expressions *)

let type_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)
let univars        = ref ([] : (string * type_expr) list)
let pre_univars    = ref ([] : type_expr list)
let used_variables = ref (Tbl.empty : (string, type_expr * Location.t) Tbl.t)

let reset_type_variables () =
  reset_global_level ();
  Ctype.reset_reified_var_counter ();
  type_variables := Tbl.empty

let narrow () =
  (increase_global_level (), !type_variables)

let widen (gl, tv) =
  restore_global_level gl;
  type_variables := tv

let strict_ident c = (c = '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')

let validate_name = function
    None -> None
  | Some name as s ->
      if name <> "" && strict_ident name.[0] then s else None

let new_global_var ?name () =
  new_global_var ?name:(validate_name name) ()
let newvar ?name () =
  newvar ?name:(validate_name name) ()

let type_variable loc name =
  try
    Tbl.find name !type_variables
  with Not_found ->
    raise(Error(loc, Env.empty, Unbound_type_variable ("'" ^ name)))

let transl_type_param env styp =
  let loc = styp.ptyp_loc in
  match styp.ptyp_desc with
  | PTyAny ->
      let ty = new_global_var ~name:"_" () in
        { ctyp_desc = TTyAny; ctyp_type = ty; ctyp_env = env;
          ctyp_loc = loc; }
  | PTyVar name ->
      let ty =
        try
          if name <> "" && name.[0] = '_' then
            raise (Error (loc, Env.empty, Invalid_variable_name ("'" ^ name)));
          ignore (Tbl.find name !type_variables);
          raise Already_bound
        with Not_found ->
          let v = new_global_var ~name () in
            type_variables := Tbl.add name v !type_variables;
            v
      in
        { ctyp_desc = TTyVar name; ctyp_type = ty; ctyp_env = env;
          ctyp_loc = loc; }
  | _ -> assert false


let new_pre_univar ?name () =
  let v = newvar ?name () in pre_univars := v :: !pre_univars; v

let rec swap_list = function
    x :: y :: l -> y :: x :: swap_list l
  | l -> l

type policy = Fixed | Extensible | Univars

let rec transl_type env policy styp =
  transl_type_aux env policy styp

and transl_type_aux env policy styp =
  let loc = styp.ptyp_loc in
  let ctyp ctyp_desc ctyp_type =
    { ctyp_desc; ctyp_type; ctyp_env = env;
      ctyp_loc = loc; }
  in
  match styp.ptyp_desc with
  | PTyAny ->
      let ty =
        if policy = Univars then new_pre_univar () else
          if policy = Fixed then
            raise (Error (styp.ptyp_loc, env, Unbound_type_variable "_"))
          else newvar ()
      in
      ctyp TTyAny ty
  | PTyVar name ->
    let ty =
      if name <> "" && name.[0] = '_' then
        raise (Error (styp.ptyp_loc, env, Invalid_variable_name ("'" ^ name)));
      begin try
        instance env (List.assoc name !univars)
      with Not_found -> try
        instance env (fst(Tbl.find name !used_variables))
      with Not_found ->
        let v =
          if policy = Univars then new_pre_univar ~name () else newvar ~name ()
        in
        used_variables := Tbl.add name (v, styp.ptyp_loc) !used_variables;
        v
      end
    in
    ctyp (TTyVar name) ty
  | PTyArrow(st1, st2) ->
    let cty1 = List.map (transl_type env policy) st1 in
    let cty2 = transl_type env policy st2 in
    let ty1 = List.map (fun x -> x.ctyp_type) cty1 in
    let ty = newty (TTyArrow(ty1, cty2.ctyp_type, TComOk)) in
    ctyp (TTyArrow (cty1, cty2)) ty
  | PTyTuple stl ->
    assert (List.length stl >= 2);
    let ctys = List.map (transl_type env policy) stl in
    let ty = newty (TTyTuple (List.map (fun ctyp -> ctyp.ctyp_type) ctys)) in
    ctyp (TTyTuple ctys) ty
  | PTyConstr(lid, stl) ->
      let (path, decl) = find_type env lid.loc lid.txt in
      let stl =
        match stl with
        | [ {ptyp_desc=PTyAny} as t ] when decl.type_arity > 1 ->
            List.map (fun _ -> t) decl.type_params
        | _ -> stl
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, env,
                    Type_arity_mismatch(lid.txt, decl.type_arity,
                                        List.length stl)));
      let args = List.map (transl_type env policy) stl in
      let params = instance_list decl.type_params in
      let unify_param =
        match decl.type_manifest with
          None -> unify_var
        | Some ty ->
            if (repr ty).level = Btype.generic_level then unify_var else unify
      in
      List.iter2
        (fun (sty, cty) ty' ->
           try unify_param env ty' cty.ctyp_type with Unify trace ->
             raise (Error(sty.ptyp_loc, env, Type_mismatch (swap_list trace))))
        (List.combine stl args) params;
      let constr =
        newconstr path (List.map (fun ctyp -> ctyp.ctyp_type) args) in
      begin try
        Ctype.enforce_constraints env constr
      with Unify trace ->
        raise (Error(styp.ptyp_loc, env, Type_mismatch trace))
      end;
        ctyp (TTyConstr (path, lid, args)) constr
  | PTyPoly(vars, st) ->
    let vars = List.map (fun v -> v.txt) vars in
    begin_def();
    let new_univars = List.map (fun name -> name, newvar ~name ()) vars in
    let old_univars = !univars in
    univars := new_univars @ !univars;
    let cty = transl_type env policy st in
    let ty = cty.ctyp_type in
    univars := old_univars;
    end_def();
    generalize ty;
    let ty_list =
      List.fold_left
        (fun tyl (name, ty1) ->
           let v = ty1 (* would be a no-op right now *) (*Btype.proxy ty1*) in
           if deep_occur v ty then begin
             match v.desc with
               TTyVar name when v.level = Btype.generic_level ->
               v.desc <- TTyUniVar name;
               v :: tyl
             | _ ->
               raise (Error (styp.ptyp_loc, env, Cannot_quantify (name, v)))
           end else tyl)
        [] new_univars
    in
    let ty' = Btype.newgenty (TTyPoly(ty, List.rev ty_list)) in
    unify_var env (newvar()) ty';
    ctyp (TTyPoly (vars, cty)) ty'

let make_fixed_univars ty =
  (* TODO: Remove *)
  ()

let globalize_used_variables env fixed =
  let r = ref [] in
  Tbl.iter
    (fun name (ty, loc) ->
      let v = new_global_var () in
      let snap = Btype.snapshot () in
      if try unify env v ty; true with _ -> Btype.backtrack snap; false
      then try
        r := (loc, v,  Tbl.find name !type_variables) :: !r
      with Not_found ->
        if fixed && Btype.is_Tvar (repr ty) then
          raise(Error(loc, env, Unbound_type_variable ("'"^name)));
        let v2 = new_global_var () in
        r := (loc, v, v2) :: !r;
        type_variables := Tbl.add name v2 !type_variables)
    !used_variables;
  used_variables := Tbl.empty;
  fun () ->
    List.iter
      (function (loc, t1, t2) ->
        try unify env t1 t2 with Unify trace ->
          raise (Error(loc, env, Type_mismatch trace)))
      !r

let transl_simple_type env fixed styp =
  univars := []; used_variables := Tbl.empty;
  let typ = transl_type env (if fixed then Fixed else Extensible) styp in
  globalize_used_variables env fixed ();
  make_fixed_univars typ.ctyp_type;
  typ

let transl_simple_type_univars env styp =
  univars := []; used_variables := Tbl.empty; pre_univars := [];
  begin_def ();
  let typ = transl_type env Univars styp in
  (* Only keep already global variables in used_variables *)
  let new_variables = !used_variables in
  used_variables := Tbl.empty;
  Tbl.iter
    (fun name p ->
      if Tbl.mem name !type_variables then
        used_variables := Tbl.add name p !used_variables)
    new_variables;
  globalize_used_variables env false ();
  end_def ();
  generalize typ.ctyp_type;
  let univs =
    List.fold_left
      (fun acc v ->
        let v = repr v in
        match v.desc with
          TTyVar name when v.level = Btype.generic_level ->
            v.desc <- TTyUniVar name; v :: acc
        | _ -> acc)
      [] !pre_univars
  in
  make_fixed_univars typ.ctyp_type;
    { typ with ctyp_type =
        instance env (Btype.newgenty (TTyPoly (typ.ctyp_type, univs))) }

let transl_simple_type_delayed env styp =
  univars := []; used_variables := Tbl.empty;
  let typ = transl_type env Extensible styp in
  make_fixed_univars typ.ctyp_type;
  (typ, globalize_used_variables env false)

let transl_type_scheme env styp =
  reset_type_variables();
  begin_def();
  let typ = transl_simple_type env false styp in
  end_def();
  generalize typ.ctyp_type;
  typ


(* Error report *)

open Format
open Printtyp

let spellcheck ppf fold env lid =
  let choices ~path name =
    let env = fold (fun x xs -> x::xs) path env [] in
    Misc.spellcheck env name in
  match lid with
    | Identifier.IdentName s ->
       Misc.did_you_mean ppf (fun () -> choices ~path:None s)
    | Identifier.IdentExternal (r, s) ->
       Misc.did_you_mean ppf (fun () -> choices ~path:(Some r) s)

let fold_descr fold get_name f = fold (fun descr acc -> f (get_name descr) acc)
let fold_simple fold4 f = fold4 (fun name _path _descr acc -> f name acc)
let fold_persistent fold4 f = fold4 (fun name path _descr acc -> 
    if Ident.persistent (Path.head path)
    then f name acc
    else acc)

let fold_values = fold_simple Env.fold_values
let fold_types = fold_simple Env.fold_types
let fold_modules = fold_persistent Env.fold_modules
let fold_constructors = fold_descr Env.fold_constructors (fun d -> d.cstr_name)
let fold_modtypes = fold_simple Env.fold_modtypes

let report_error env ppf = function
  | Unbound_type_variable name ->
     (* we don't use "spellcheck" here: the function that raises this
        error seems not to be called anywhere, so it's unclear how it
        should be handled *)
    fprintf ppf "Unbound type parameter %s@." name
  | Unbound_type_constructor lid ->
    fprintf ppf "Unbound type constructor %a" Identifier.print lid;
    spellcheck ppf fold_types env lid;
  | Unbound_type_constructor_2 p ->
    fprintf ppf "The type constructor@ %a@ is not yet completely defined"
      path p
  | Type_arity_mismatch(lid, expected, provided) ->
    fprintf ppf
      "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
      Identifier.print lid expected provided
  | Bound_type_variable name ->
    fprintf ppf "Already bound type parameter '%s" name
  | Recursive_type ->
    fprintf ppf "This type is recursive"
  | Unbound_row_variable lid ->
      (* we don't use "spellcheck" here: this error is not raised
         anywhere so it's unclear how it should be handled *)
      fprintf ppf "Unbound row variable in #%a" identifier lid
  | Type_mismatch trace ->
      Printtyp.report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This type")
        (function ppf ->
           fprintf ppf "should be an instance of type")
  | Alias_type_mismatch trace ->
      Printtyp.report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This alias is bound to type")
        (function ppf ->
           fprintf ppf "but is used as an instance of type")
  | Present_has_conjunction l ->
      fprintf ppf "The present constructor %s has a conjunctive type" l
  | Present_has_no_type l ->
      fprintf ppf "The present constructor %s has no type" l
  | Constructor_mismatch (ty, ty') ->
      wrap_printing_env ~error:true env (fun ()  ->
        Printtyp.reset_and_mark_loops_list [ty; ty'];
        fprintf ppf "@[<hov>%s %a@ %s@ %a@]"
          "This variant type contains a constructor"
          Printtyp.type_expr ty
          "which should be"
          Printtyp.type_expr ty')
  | Not_a_variant ty ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf
        "@[The type %a@ does not expand to a polymorphic variant type@]"
        Printtyp.type_expr ty;
      begin match ty.desc with
        | TTyVar (Some s) ->
           (* PR#7012: help the user that wrote 'Foo instead of `Foo *)
           Misc.did_you_mean ppf (fun () -> ["`" ^ s])
        | _ -> ()
      end
  | Variant_tags (lab1, lab2) ->
      fprintf ppf
        "@[Variant tags `%s@ and `%s have the same hash value.@ %s@]"
        lab1 lab2 "Change one of them."
  | Invalid_variable_name name ->
      fprintf ppf "The type variable name %s is not allowed in programs" name
  | Cannot_quantify (name, v) ->
      fprintf ppf
        "@[<hov>The universal type variable '%s cannot be generalized:@ %s.@]"
        name
        (if Btype.is_Tvar v then "it escapes its scope" else
         if Btype.is_Tunivar v then "it is already bound to another variable"
         else "it is not a variable")
  | Multiple_constraints_on_type s ->
      fprintf ppf "Multiple constraints for type %a" identifier s
  | Method_mismatch (l, ty, ty') ->
      wrap_printing_env ~error:true env (fun ()  ->
        Printtyp.reset_and_mark_loops_list [ty; ty'];
        fprintf ppf "@[<hov>Method '%s' has type %a,@ which should be %a@]"
          l Printtyp.type_expr ty Printtyp.type_expr ty')
  | Unbound_value lid ->
      fprintf ppf "Unbound value %a" identifier lid;
      spellcheck ppf fold_values env lid;
  | Unbound_value_in_module (mlid, lid) ->
      fprintf ppf "Unbound value %s in module %a" lid identifier mlid
  | Unbound_module lid ->
      fprintf ppf "Unbound module %a" identifier lid;
      spellcheck ppf fold_modules env lid;
  | Unbound_constructor lid ->
      fprintf ppf "Unbound constructor %a" identifier lid;
      spellcheck ppf fold_constructors env lid;
  | Unbound_label _
  | Unbound_class _
  | Unbound_cltype _ ->
    failwith "Impossible: deprecated error type in typetexp"
  | Unbound_modtype lid ->
      fprintf ppf "Unbound module type %a" identifier lid;
      spellcheck ppf fold_modtypes env lid;
  | Ill_typed_functor_application (flid, mlid, details) ->
     (match details with
     | None ->
        fprintf ppf "@[Ill-typed functor application %a(%a)@]"
          identifier flid identifier mlid
     | Some inclusion_error ->
        fprintf ppf "@[The type of %a does not match %a's parameter@\n%a@]"
          identifier mlid identifier flid Includemod.report_error inclusion_error)
  | Illegal_reference_to_recursive_module ->
     fprintf ppf "Illegal recursive module reference"
  | Wrong_use_of_module (lid, details) ->
     (match details with
     | `Structure_used_as_functor ->
        fprintf ppf "@[The module %a is a structure, it cannot be applied@]"
          identifier lid
     | `Abstract_used_as_functor ->
        fprintf ppf "@[The module %a is abstract, it cannot be applied@]"
          identifier lid
     | `Functor_used_as_structure ->
        fprintf ppf "@[The module %a is a functor, \
                       it cannot have any components@]" identifier lid
     | `Abstract_used_as_structure ->
        fprintf ppf "@[The module %a is abstract, \
                       it cannot have any components@]" identifier lid
     | `Generative_used_as_applicative ->
        fprintf ppf "@[The functor %a is generative,@ it@ cannot@ be@ \
                       applied@ in@ type@ expressions@]" identifier lid)
  | Cannot_scrape_alias(lid, p) ->
      fprintf ppf
        "The module %a is an alias for module %a, which is missing"
        identifier lid path p
  | Opened_object nm ->
      fprintf ppf
        "Illegal open object type%a"
        (fun ppf -> function
             Some p -> fprintf ppf "@ %a" path p
           | None -> fprintf ppf "") nm
  | Not_an_object ty ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf "@[The type %a@ is not an object type@]"
        Printtyp.type_expr ty

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
