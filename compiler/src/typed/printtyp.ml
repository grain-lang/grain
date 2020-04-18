open Misc
open Grain_parsing
open Identifier
open Path
open Types
open Btype
open Ctype
open Format
open Outcometree

module String = Misc.Stdlib.String

(* Print a long identifier *)

let rec identifier ppf = function
  | IdentName s -> pp_print_string ppf s
  | IdentExternal(p, s) -> fprintf ppf "%a::%s" identifier p s

(* Print an identifier *)

let unique_names = ref Ident.empty

let ident_name id =
  try Ident.find_same id !unique_names with Not_found -> Ident.name id

let add_unique id =
  try ignore (Ident.find_same id !unique_names)
  with Not_found ->
    unique_names := Ident.add id (Ident.unique_toplevel_name id) !unique_names

let ident ppf id = pp_print_string ppf (ident_name id)

(* Print a path *)

let ident_pervasives = Ident.create_persistent "Stdlib"
let printing_env = ref Env.empty
let non_shadowed_pervasive = function
  | PExternal(PIdent id, s, _pos) as path ->
    Ident.same id ident_pervasives &&
    (try Path.same path (Env.lookup_type (IdentName s) !printing_env)
     with Not_found -> true)
  | _ -> false

let rec tree_of_path = function
  | PIdent id ->
    Oide_ident (ident_name id)
  | PExternal(_, s, _pos) as path when non_shadowed_pervasive path ->
    Oide_ident s
  | PExternal(p, s, _pos) ->
    Oide_dot (tree_of_path p, s)

let rec path ppf = function
  | PIdent id ->
    ident ppf id
  | PExternal(_, s, _pos) as path when non_shadowed_pervasive path ->
    pp_print_string ppf s
  | PExternal(p, s, _pos) ->
    path ppf p;
    pp_print_string ppf "::";
    pp_print_string ppf s

let rec string_of_out_ident = function
  | Oide_ident s -> s
  | Oide_dot (id, s) -> String.concat "." [string_of_out_ident id; s]

let string_of_path p = string_of_out_ident (tree_of_path p)

(* Print a recursive annotation *)

let tree_of_rec = function
  | TRecNot -> Orec_not
  | TRecFirst -> Orec_first
  | TRecNext -> Orec_next

(* Print a raw type expression, with sharing *)

let raw_list pr ppf = function
    [] -> fprintf ppf "[]"
  | a :: l ->
    fprintf ppf "@[<1>[%a%t]@]" pr a
      (fun ppf -> List.iter (fun x -> fprintf ppf ";@,%a" pr x) l)

let kind_vars = ref []
let kind_count = ref 0

let rec safe_commu_repr v = function
  | TComOk -> "TComOk"
  | TComUnknown -> "TComUnknown"
  | TComLink r ->
    if List.memq r v then "TComLink loop" else
      safe_commu_repr (r::v) !r

let rec safe_repr v = function
    {desc = TTyLink t} when not (List.memq t v) ->
    safe_repr (t::v) t
  | t -> t

let rec list_of_memo = function
  | TMemNil -> []
  | TMemCons(p, _t1, _t2, rem) -> p :: list_of_memo rem
  | TMemLink rem -> list_of_memo !rem

let print_name ppf = function
    None -> fprintf ppf "None"
  | Some name -> fprintf ppf "\"%s\"" name

let visited = ref []
let rec raw_type ppf ty =
  let ty = safe_repr [] ty in
  if List.memq ty !visited then fprintf ppf "{id=%d}" ty.id else begin
    visited := ty :: !visited;
    fprintf ppf "@[<1>{id=%d;level=%d;desc=@,%a}@]" ty.id ty.level
      raw_type_desc ty.desc
  end
and raw_type_list tl = raw_list raw_type tl
and raw_type_desc ppf = function
  | TTyVar name -> fprintf ppf "TTyVar %a" print_name name
  | TTyArrow(t1,t2,c) ->
    fprintf ppf "@[<hov1>TTyArrow(@,%a,@,%a,@,%s)@]"
      raw_type_list t1 raw_type t2
      (safe_commu_repr [] c)
  | TTyTuple tl ->
    fprintf ppf "@[<1>TTyTuple@,%a@]" raw_type_list tl
  | TTyRecord tl ->
    fprintf ppf "@[<1>TTyRecord@,%a@]" (raw_list (fun ppf (name, arg) -> fprintf ppf "%s=%a" name raw_type arg)) tl
  | TTyConstr (p, tl, abbrev) ->
    fprintf ppf "@[<hov1>TTyConstr(@,%a,@,%a,@,%a)@]" path p
      raw_type_list tl
      (raw_list path) (list_of_memo !abbrev)
  | TTyLink t -> fprintf ppf "@[<1>TTyLink@,%a@]" raw_type t
  | TTySubst t -> fprintf ppf "@[<1>TTySubst@,%a@]" raw_type t
  | TTyUniVar name -> fprintf ppf "TTyUnivar %a" print_name name
  | TTyPoly (t, tl) ->
    fprintf ppf "@[<hov1>TTyPoly(@,%a,@,%a)@]"
      raw_type t
      raw_type_list tl

let raw_type_expr ppf t =
  visited := []; kind_vars := []; kind_count := 0;
  raw_type ppf t;
  visited := []; kind_vars := []

let () = Btype.print_raw := raw_type_expr

(* Normalize paths *)

type param_subst = Id | Nth of int | Map of int list

let is_nth = function
    Nth _ -> true
  | _ -> false

let compose l1 = function
  | Id -> Map l1
  | Map l2 -> Map (List.map (List.nth l1) l2)
  | Nth n  -> Nth (List.nth l1 n)

let apply_subst s1 tyl =
  if tyl = [] then []
  (* cf. PR#7543: Typemod.type_package doesn't respect type constructor arity *)
  else
    match s1 with
      Nth n1 -> [List.nth tyl n1]
    | Map l1 -> List.map (List.nth tyl) l1
    | Id -> tyl

type best_path = Paths of Path.t list | Best of Path.t

let printing_depth = ref 0
let printing_cont = ref ([] : Env.iter_cont list)
let printing_old = ref Env.empty
let printing_pers = ref Concr.empty
module PathMap = Map.Make(Path)
let printing_map = ref PathMap.empty

let same_type t t' = repr t == repr t'

let rec index l x =
  match l with
    [] -> raise Not_found
  | a :: l -> if x == a then 0 else 1 + index l x

let rec uniq = function
    [] -> true
  | a :: l -> not (List.memq a l) && uniq l

let rec normalize_type_path ?(cache=false) env p =
  try
    let (params, ty, _) = Env.find_type_expansion p env in
    let params = List.map repr params in
    match repr ty with
      {desc = TTyConstr (p1, tyl, _)} ->
      let tyl = List.map repr tyl in
      if List.length params = List.length tyl
      && List.for_all2 (==) params tyl
      then normalize_type_path ~cache env p1
      else if cache || List.length params <= List.length tyl
              || not (uniq tyl) then (p, Id)
      else
        let l1 = List.map (index params) tyl in
        let (p2, s2) = normalize_type_path ~cache env p1 in
        (p2, compose l1 s2)
    | ty ->
      (p, Nth (index params ty))
  with
    Not_found ->
    (Env.normalize_path None env p, Id)

let penalty s =
  if s <> "" && s.[0] = '_' then
    10
  else
    try
      for i = 0 to String.length s - 2 do
        if s.[i] = '_' && s.[i + 1] = '_' then
          raise Exit
      done;
      1
    with Exit -> 10

let rec path_size = function
  | PIdent id ->
    penalty (Ident.name id), -Ident.binding_time id
  | PExternal (p, _, _) ->
    let (l, b) = path_size p in (1+l, b)

let same_printing_env env =
  let used_pers = Env.used_persistent () in
  Env.same_types !printing_old env && Concr.equal !printing_pers used_pers

let set_printing_env env =
  printing_env := env;
  if !Clflags.real_paths
  || !printing_env == Env.empty || same_printing_env env then () else
    begin
      (* printf "Reset printing_map@."; *)
      printing_old := env;
      printing_pers := Env.used_persistent ();
      printing_map := PathMap.empty;
      printing_depth := 0;
      (* printf "Recompute printing_map.@."; *)
      let cont =
        Env.iter_types
          (fun p (p', _decl) ->
             let (p1, s1) = normalize_type_path env p' ~cache:true in
             (* Format.eprintf "%a -> %a = %a@." path p path p' path p1 *)
             if s1 = Id then
               try
                 let r = PathMap.find p1 !printing_map in
                 match !r with
                   Paths l -> r := Paths (p :: l)
                 | Best p' -> r := Paths [p; p'] (* assert false *)
               with Not_found ->
                 printing_map := PathMap.add p1 (ref (Paths [p])) !printing_map)
          env in
      printing_cont := [cont];
    end

let wrap_printing_env env f =
  set_printing_env env;
  try_finally ~always:(fun () -> set_printing_env Env.empty) f

let wrap_printing_env ~error env f =
  if error then Env.without_cmis (wrap_printing_env env) f
  else wrap_printing_env env f

let is_unambiguous path env =
  let l = Env.find_shadowed_types path env in
  List.exists (Path.same path) l || (* concrete paths are ok *)
  match l with
    [] -> true
  | p :: rem ->
    (* allow also coherent paths:  *)
    let normalize p = fst (normalize_type_path ~cache:true env p) in
    let p' = normalize p in
    List.for_all (fun p -> Path.same (normalize p) p') rem ||
    (* also allow repeatedly defining and opening (for toplevel) *)
    let id = lid_of_path p in
    List.for_all (fun p -> lid_of_path p = id) rem &&
    Path.same p (Env.lookup_type id env)

let rec get_best_path r =
  match !r with
    Best p' -> p'
  | Paths [] -> raise Not_found
  | Paths l ->
    r := Paths [];
    List.iter
      (fun p ->
         (* Format.eprintf "evaluating %a@." path p; *)
         match !r with
           Best p' when path_size p >= path_size p' -> ()
         | _ -> if is_unambiguous p !printing_env then r := Best p)
      (* else Format.eprintf "%a ignored as ambiguous@." path p *)
      l;
    get_best_path r

let best_type_path p =
  if !Clflags.real_paths || !printing_env == Env.empty
  then (p, Id)
  else
    let (p', s) = normalize_type_path !printing_env p in
    let get_path () = get_best_path (PathMap.find  p' !printing_map) in
    while !printing_cont <> [] &&
          try fst (path_size (get_path ())) > !printing_depth with Not_found -> true
    do
      printing_cont := List.map snd (Env.run_iter_cont !printing_cont);
      incr printing_depth;
    done;
    let p'' = try get_path () with Not_found -> p' in
    (* Format.eprintf "%a = %a -> %a@." path p path p' path p''; *)
    (p'', s)

(* Print a type expression *)

let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0
let named_vars = ref ([] : string list)

let weak_counter = ref 1
let weak_var_map = ref TypeMap.empty
let named_weak_vars = ref String.Set.empty

let reset_names () = names := []; name_counter := 0; named_vars := []
let add_named_var ty =
  match ty.desc with
  | TTyVar (Some name) | TTyUniVar (Some name) ->
    if List.mem name !named_vars then () else
      named_vars := name :: !named_vars
  | _ -> ()

let name_is_already_used name =
  List.mem name !named_vars
  || List.exists (fun (_, name') -> name = name') !names
  || String.Set.mem name !named_weak_vars

let rec new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter))
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
         string_of_int(!name_counter / 26) in
  incr name_counter;
  if name_is_already_used name then new_name () else name

let rec new_weak_name ty () =
  let name = "weak" ^ string_of_int !weak_counter in
  incr weak_counter;
  if name_is_already_used name then new_weak_name ty ()
  else begin
    named_weak_vars := String.Set.add name !named_weak_vars;
    weak_var_map := TypeMap.add ty name !weak_var_map;
    name
  end

let name_of_type name_generator t =
  (* We've already been through repr at this stage, so t is our representative
     of the union-find class. *)
  try List.assq t !names with Not_found ->
  try TypeMap.find t !weak_var_map with Not_found ->
    let name =
      match t.desc with
      | TTyVar (Some name) | TTyUniVar (Some name) ->
        (* Some part of the type we've already printed has assigned another
         * unification variable to that name. We want to keep the name, so try
         * adding a number until we find a name that's not taken. *)
        let current_name = ref name in
        let i = ref 0 in
        while List.exists (fun (_, name') -> !current_name = name') !names do
          current_name := name ^ (string_of_int !i);
          i := !i + 1;
        done;
        !current_name
      | _ ->
        (* No name available, create a new one *)
        name_generator ()
    in
    (* Exception for type declarations *)
    if name <> "_" then names := (t, name) :: !names;
    name

let check_name_of_type t = ignore(name_of_type new_name t)

let remove_names tyl =
  let tyl = List.map repr tyl in
  names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !names

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)
let delayed = ref ([] : type_expr list)

let add_delayed t =
  if not (List.memq t !delayed) then delayed := t :: !delayed

let is_aliased ty = List.memq ty !aliased
let add_alias ty =
  let px = ty in
  if not (is_aliased px) then begin
    aliased := px :: !aliased;
    add_named_var px
  end

let aliasable ty =
  match ty.desc with
  | TTyVar _ | TTyUniVar _ | TTyPoly _ -> false
  | TTyConstr (p, _, _) ->
    not (is_nth (snd (best_type_path p)))
  | _ -> true


let rec mark_loops_rec visited ty =
  let ty = repr ty in
  let px = ty in
  if List.memq px visited && aliasable ty then add_alias px else
    let visited = px :: visited in
    match ty.desc with
    | TTyVar _ -> add_named_var ty
    | TTyArrow(ty1, ty2, _) ->
      List.iter (mark_loops_rec visited) ty1; mark_loops_rec visited ty2
    | TTyTuple tyl -> List.iter (mark_loops_rec visited) tyl
    | TTyRecord tyl -> List.iter (fun (_, arg) -> (mark_loops_rec visited) arg) tyl
    | TTyConstr(p, tyl, _) ->
      let (_p', s) = best_type_path p in
      List.iter (mark_loops_rec visited) (apply_subst s tyl)
    | TTySubst ty -> mark_loops_rec visited ty
    | TTyLink _ -> fatal_error "Printtyp.mark_loops_rec (2)"
    | TTyPoly (ty, tyl) ->
      List.iter (fun t -> add_alias t) tyl;
      mark_loops_rec visited ty
    | TTyUniVar _ -> add_named_var ty

let mark_loops ty =
  normalize_type Env.empty ty;
  mark_loops_rec [] ty;;

let reset_loop_marks () =
  visited_objects := []; aliased := []; delayed := []

let reset () =
  unique_names := Ident.empty; reset_names (); reset_loop_marks ()

let reset_and_mark_loops ty =
  reset (); mark_loops ty

let reset_and_mark_loops_list tyl =
  reset (); List.iter mark_loops tyl

(* Disabled in classic mode when printing an unification error *)
let print_labels = ref true

let rec tree_of_typexp sch ty =
  let ty = repr ty in
  let px = ty in
  if List.mem_assq px !names && not (List.memq px !delayed) then
    let mark = is_non_gen sch ty in
    let name = name_of_type (if mark then new_weak_name ty else new_name) px in
    Otyp_var (mark, name) else

    let pr_typ () =
      match ty.desc with
      | TTyVar _ ->
        (*let lev =
          if is_non_gen sch ty then "/" ^ string_of_int ty.level else "" in*)
        let non_gen = is_non_gen sch ty in
        let name_gen = if non_gen then new_weak_name ty else new_name in
        Otyp_var (non_gen, name_of_type name_gen ty)
      | TTyArrow(ty1, ty2, _) ->
        let pr_arrow ty1 ty2 =
          let t1 =
            tree_of_typlist sch ty1 in
          Otyp_arrow (t1, tree_of_typexp sch ty2) in
        pr_arrow ty1 ty2
      | TTyTuple tyl ->
        Otyp_tuple (tree_of_typlist sch tyl)
      | TTyRecord tyl ->
        Otyp_record (List.map (fun (name, arg) -> (name, false, tree_of_typexp sch arg)) tyl)
      | TTyConstr(p, tyl, _abbrev) ->
        let p', s = best_type_path p in
        let tyl' = apply_subst s tyl in
        if is_nth s && not (tyl'=[]) then tree_of_typexp sch (List.hd tyl') else
          Otyp_constr (tree_of_path p', tree_of_typlist sch tyl')
      | TTySubst ty ->
        tree_of_typexp sch ty
      | TTyLink _ ->
        fatal_error "Printtyp.tree_of_typexp"
      | TTyPoly (ty, []) ->
        tree_of_typexp sch ty
      | TTyPoly (ty, tyl) ->
        (*let print_names () =
          List.iter (fun (_, name) -> prerr_string (name ^ " ")) !names;
          prerr_string "; " in *)
        let tyl = List.map repr tyl in
        if tyl = [] then tree_of_typexp sch ty else begin
          let old_delayed = !delayed in
          (* Make the names delayed, so that the real type is
             printed once when used as proxy *)
          List.iter add_delayed tyl;
          let tl = List.map (name_of_type new_name) tyl in
          let tr = Otyp_poly (tl, tree_of_typexp sch ty) in
          (* Forget names when we leave scope *)
          remove_names tyl;
          delayed := old_delayed; tr
        end
      | TTyUniVar _ ->
        Otyp_var (false, name_of_type new_name ty)
    in
    if List.memq px !delayed then delayed := List.filter ((!=) px) !delayed;
    if is_aliased px && aliasable ty then begin
      check_name_of_type px;
      Otyp_alias (pr_typ (), name_of_type new_name px) end
    else pr_typ ()

and tree_of_typlist sch tyl =
  List.map (tree_of_typexp sch) tyl

and is_non_gen sch ty =
  sch && is_Tvar ty && ty.level <> generic_level

and tree_of_typfields sch rest = function
  | [] ->
    let rest =
      match rest.desc with
      | TTyVar _ | TTyUniVar _ -> Some (is_non_gen sch rest)
      | TTyConstr _ -> Some false
      | _ -> fatal_error "typfields (1)"
    in
    ([], rest)
  | (s, t) :: l ->
    let field = (s, tree_of_typexp sch t) in
    let (fields, rest) = tree_of_typfields sch rest l in
    (field :: fields, rest)

let typexp sch ppf ty =
  !Oprint.out_type ppf (tree_of_typexp sch ty)

let type_expr ppf ty = typexp false ppf ty

and type_sch ppf ty = typexp true ppf ty

and type_scheme ppf ty = reset_and_mark_loops ty; typexp true ppf ty

(* Maxence *)
let type_scheme_max ?(b_reset_names=true) ppf ty =
  if b_reset_names then reset_names () ;
  typexp true ppf ty
(* End Maxence *)

let tree_of_type_scheme ty = reset_and_mark_loops ty; tree_of_typexp true ty

(* Print one type declaration *)

let tree_of_constraints params =
  List.fold_right
    (fun ty list ->
       let ty' = unalias ty in
       if ty != ty' then
         let tr = tree_of_typexp true ty in
         (tr, tree_of_typexp true ty') :: list
       else list)
    params []

let filter_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
         let ty = repr ty in
         if List.memq ty tyl then Btype.newgenty (TTySubst ty) :: tyl
         else ty :: tyl)
      [] tyl
  in List.rev params

let mark_loops_constructor_arguments = function
  | TConstrTuple l -> List.iter mark_loops l
  | TConstrSingleton -> ()

let rec tree_of_type_decl id decl =

  reset();

  let params = filter_params decl.type_params in

  begin match decl.type_manifest with
    | Some ty ->
      let vars = free_variables ty in
      List.iter
        (function {desc = TTyVar (Some "_")} as ty ->
           if List.memq ty vars then ty.desc <- TTyVar None
                | _ -> ())
        params
    | None -> ()
  end;

  List.iter add_alias params;
  List.iter mark_loops params;
  List.iter check_name_of_type params;
  let ty_manifest =
    match decl.type_manifest with
    | None -> None
    | Some ty ->
      let ty = repr ty in
      mark_loops ty;
      Some ty
  in
  begin match decl.type_kind with
    | TDataAbstract -> ()
    | TDataRecord fields ->
      List.iter (fun {rf_type} -> mark_loops rf_type) fields
    | TDataVariant cstrs ->
      List.iter
        (fun c ->
           mark_loops_constructor_arguments c.cd_args;
           may mark_loops c.cd_res)
        cstrs
  end;

  let type_param =
    function
    | Otyp_var (_, id) -> id
    | _ -> "?"
  in
  let type_defined decl =
    let vari =
      List.map
        (fun ty ->
           (true,true))
        decl.type_params
    in
    (Ident.name id,
     List.map2 (fun ty cocn -> type_param (tree_of_typexp false ty), cocn)
       params vari)
  in
  let tree_of_manifest ty1 =
    match ty_manifest with
    | None -> ty1
    | Some ty -> Otyp_manifest (tree_of_typexp false ty, ty1)
  in
  let (name, args) = type_defined decl in
  let constraints = tree_of_constraints params in
  let ty =
    match decl.type_kind with
    | TDataAbstract ->
      begin match ty_manifest with
        | None -> (Otyp_abstract)
        | Some ty ->
          tree_of_typexp false ty
      end
    | TDataRecord fields ->
      Otyp_record (List.map (fun {rf_name; rf_type} -> Ident.name rf_name, false, tree_of_typexp false rf_type) fields)
    | TDataVariant cstrs ->
      tree_of_manifest (Otyp_sum (List.map tree_of_constructor cstrs))
  in
  { otype_name = name;
    otype_params = args;
    otype_type = ty;
    otype_immediate = false;
    otype_unboxed = false;
    otype_cstrs = constraints }

and tree_of_constructor_arguments = function
  | TConstrTuple l -> tree_of_typlist false l
  | TConstrSingleton -> []

and tree_of_constructor cd =
  let name = Ident.name cd.cd_id in
  let arg () = tree_of_constructor_arguments cd.cd_args in
  match cd.cd_res with
  | None -> (name, arg (), None)
  | Some res ->
    let nm = !names in
    names := [];
    let ret = tree_of_typexp false res in
    let args = arg () in
    names := nm;
    (name, args, Some ret)

let tree_of_type_declaration id decl rs =
  Osig_type (tree_of_type_decl id decl, tree_of_rec rs)

let type_declaration id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_type_declaration id decl TRecFirst)

let constructor_arguments ppf a =
  let tys = tree_of_constructor_arguments a in
  !Oprint.out_type ppf (Otyp_tuple tys)


(* Print a value declaration *)

let tree_of_value_description id decl =
  (* Format.eprintf "@[%a@]@." raw_type_expr decl.val_type; *)
  let id = Ident.name id in
  let ty = tree_of_type_scheme decl.val_type in
  let vd =
    { oval_name = id;
      oval_type = ty;
      oval_prims = [];
      oval_attributes = [] }
  in
  (*let vd =
    match decl.val_kind with
    | TValPrim p -> Primitive.print p vd
    | _ -> vd
    in*)
  Osig_value vd

let value_description id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_value_description id decl)

(* Print a module type *)

let wrap_env fenv ftree arg =
  let env = !printing_env in
  set_printing_env (fenv env);
  let tree = ftree arg in
  set_printing_env env;
  tree

let filter_rem_sig item rem =
  match item, rem with
  | _ ->
    ([], rem)

let dummy =
  { type_params = []; type_arity = 0; type_kind = TDataAbstract;
    type_manifest = None;
    type_newtype_level = None; type_loc = Location.dummy_loc;
    type_path = PIdent({stamp=(-1); name=""; flags=0});
    type_immediate = false;
  }

let hide_rec_items = function
  | TSigType(id, _decl, rs) ::rem
    when rs = TRecFirst && not !Clflags.real_paths ->
    let rec get_ids = function
        TSigType (id, _, TRecNext) :: rem ->
        id :: get_ids rem
      | _ -> []
    in
    let ids = id :: get_ids rem in
    set_printing_env
      (List.fold_right
         (fun id -> Env.add_type ~check:false (Ident.rename id) dummy)
         ids !printing_env)
  | _ -> ()

let rec tree_of_modtype ?(ellipsis=false) = function
  | TModIdent p ->
    Omty_ident (tree_of_path p)
  | TModAlias p ->
    Omty_alias (tree_of_path p)
  | TModSignature sg ->
    Omty_signature (if ellipsis then [Osig_ellipsis]
                    else tree_of_signature sg)

and tree_of_signature sg =
  wrap_env (fun env -> env) (tree_of_signature_rec !printing_env false) sg

and tree_of_signature_rec env' in_type_group = function
    [] -> []
  | item :: rem as items ->
    let in_type_group =
      match in_type_group, item with
        true, TSigType (_, _, TRecNext) -> true
      | _, TSigType (_, _, (TRecNot | TRecFirst)) ->
        set_printing_env env'; true
      | _ -> set_printing_env env'; false
    in
    let (sg, rem) = filter_rem_sig item rem in
    hide_rec_items items;
    let trees = trees_of_sigitem item in
    let env' = Env.add_signature (item :: sg) env' in
    trees @ tree_of_signature_rec env' in_type_group rem

and trees_of_sigitem = function
  | TSigValue(id, decl) ->
    [tree_of_value_description id decl]
  | TSigType(id, decl, rs) ->
    [tree_of_type_declaration id decl rs]
  | TSigModule(id, md, rs) ->
    [tree_of_module id md.md_type rs ~ellipsis:false]
  | TSigModType(id, decl) ->
    [tree_of_modtype_declaration id decl]

and tree_of_modtype_declaration id decl =
  let mty =
    match decl.mtd_type with
    | None -> Omty_abstract
    | Some mty -> tree_of_modtype mty
  in
  Osig_modtype (Ident.name id, mty)

and tree_of_module id ?ellipsis mty rs =
  Osig_module (Ident.name id, tree_of_modtype ?ellipsis mty, tree_of_rec rs)

let modtype ppf mty = !Oprint.out_module_type ppf (tree_of_modtype mty)
let modtype_declaration id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_modtype_declaration id decl)


(* Refresh weak variable map in the toplevel *)
let refresh_weak () =
  let refresh t name (m,s) =
    if is_non_gen true (repr t) then
      begin
        TypeMap.add t name m,
        String.Set.add name s
      end
    else m, s in
  let m, s =
    TypeMap.fold refresh !weak_var_map (TypeMap.empty, String.Set.empty)  in
  named_weak_vars := s;
  weak_var_map := m

let print_items showval env x =
  refresh_weak();
  let rec print showval env = function
    | [] -> []
    | item :: rem as items ->
      let (_sg, rem) = filter_rem_sig item rem in
      hide_rec_items items;
      let trees = trees_of_sigitem item in
      List.map (fun d -> (d, showval env item)) trees @
      print showval env rem in
  print showval env x

(* Print a signature body (used by -i when compiling a .ml) *)

let print_signature ppf tree =
  fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

let signature ppf sg =
  fprintf ppf "%a" print_signature (tree_of_signature sg)

(* Print an unification error *)

let same_path t t' =
  let t = repr t and t' = repr t' in
  t == t' ||
  match t.desc, t'.desc with
    TTyConstr(p,tl,_), TTyConstr(p',tl',_) ->
    let (p1, s1) = best_type_path p and (p2, s2)  = best_type_path p' in
    begin match s1, s2 with
        Nth n1, Nth n2 when n1 = n2 -> true
      | (Id | Map _), (Id | Map _) when Path.same p1 p2 ->
        let tl = apply_subst s1 tl and tl' = apply_subst s2 tl' in
        List.length tl = List.length tl' &&
        List.for_all2 same_type tl tl'
      | _ -> false
    end
  | _ ->
    false

let type_expansion t ppf t' =
  if same_path t t'
  then begin add_delayed t; type_expr ppf t end
  else
    let t' = if t == t' then unalias t' else t' in
    fprintf ppf "@[<2>%a@ =@ %a@]" type_expr t type_expr t'

let type_path_expansion tp ppf tp' =
  if Path.same tp tp' then path ppf tp else
    fprintf ppf "@[<2>%a@ =@ %a@]" path tp path tp'

let rec trace fst txt ppf = function
  | (t1, t1') :: (t2, t2') :: rem ->
    if not fst then fprintf ppf "@,";
    fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@] %a"
      (type_expansion t1) t1' txt (type_expansion t2) t2'
      (trace false txt) rem
  | _ -> ()

let rec filter_trace keep_last = function
  | (_, t1') :: (_, t2') :: [] when is_Tvar t1' || is_Tvar t2' ->
    []
  | (t1, t1') :: (t2, t2') :: rem ->
    let rem' = filter_trace keep_last rem in
    if same_path t1 t1' && same_path t2 t2' && not (keep_last && rem' = [])
    then rem'
    else (t1, t1') :: (t2, t2') :: rem'
  | _ -> []

let rec type_path_list ppf = function
  | [tp, tp'] -> type_path_expansion tp ppf tp'
  | (tp, tp') :: rem ->
    fprintf ppf "%a@;<2 0>%a"
      (type_path_expansion tp) tp'
      type_path_list rem
  | [] -> ()

(* Hide variant name and var, to force printing the expanded type *)
let hide_variant_name t =
  match repr t with
  | _ -> t

let prepare_expansion (t, t') =
  let t' = hide_variant_name t' in
  mark_loops t;
  if not (same_path t t') then mark_loops t';
  (t, t')

let may_prepare_expansion compact (t, t') =
  match (repr t').desc with
  | _ -> prepare_expansion (t, t')

let print_tags ppf fields =
  match fields with [] -> ()
                  | (t, _) :: fields ->
                    fprintf ppf "`%s" t;
                    List.iter (fun (t, _) -> fprintf ppf ",@ `%s" t) fields

let is_unit env ty =
  match (Ctype.expand_head env ty).desc with
  | TTyConstr (p, _, _) -> false (*Path.same p Predef.path_unit*)
  | _ -> false

let unifiable env ty1 ty2 =
  let snap = Btype.snapshot () in
  let res =
    try Ctype.unify env ty1 ty2; true
    with Unify _ -> false
  in
  Btype.backtrack snap;
  res

let explanation env unif t3 t4 : (Format.formatter -> unit) option =
  match t3.desc, t4.desc with
  (*| TTyArrow (ty1, ty2, _), _
    when is_unit env ty1 && unifiable env ty2 t4 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[Hint: Did you forget to provide `()' as argument?@]")
    | _, Tarrow (_, ty1, ty2, _)
    when is_unit env ty1 && unifiable env t3 ty2 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[Hint: Did you forget to wrap the expression using `fun () ->'?@]")*)
  | TTyConstr (p, _, _), TTyVar _
    when unif && t4.level < Path.binding_time p ->
    Some (fun ppf ->
        fprintf ppf
          "@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
          path p)
  | TTyVar _, TTyConstr (p, _, _)
    when unif && t3.level < Path.binding_time p ->
    Some (fun ppf ->
        fprintf ppf
          "@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
          path p)
  | TTyVar _, TTyUniVar _ | TTyUniVar _, TTyVar _ ->
    Some (fun ppf ->
        fprintf ppf "@,The universal variable %a would escape its scope"
          type_expr (if is_Tunivar t3 then t3 else t4))
  | TTyVar _, _ | _, TTyVar _ ->
    Some (fun ppf ->
        let t, t' = if is_Tvar t3 then (t3, t4) else (t4, t3) in
        if occur_in Env.empty t t' then
          fprintf ppf "@,@[<hov>The type variable %a occurs inside@ %a@]"
            type_expr t type_expr t'
        else
          fprintf ppf "@,@[<hov>This instance of %a is ambiguous:@ %s@]"
            type_expr t'
            "it would escape the scope of its equation")
  | _ ->
    None

let rec mismatch env unif = function
    (_, t) :: (_, t') :: rem ->
    begin match mismatch env unif rem with
        Some _ as m -> m
      | None -> explanation env unif t t'
    end
  | [] -> None
  | _ -> assert false

let explain mis ppf =
  match mis with
  | None -> ()
  | Some explain -> explain ppf

let warn_on_missing_def env ppf t =
  match t.desc with
  | TTyConstr (p,_,_) ->
    begin
      try
        ignore(Env.find_type p env : Types.type_declaration)
      with Not_found ->
        fprintf ppf
          "@,@[%a is abstract because no corresponding cmi file was found \
           in path.@]" path p
    end
  | _ -> ()

let ident_same_name id1 id2 =
  if Ident.equal id1 id2 && not (Ident.same id1 id2) then begin
    add_unique id1; add_unique id2
  end

let rec path_same_name p1 p2 =
  match p1, p2 with
    PIdent id1, PIdent id2 -> ident_same_name id1 id2
  | PExternal (p1, s1, _), PExternal (p2, s2, _) when s1 = s2 -> path_same_name p1 p2
  | _ -> ()

let type_same_name t1 t2 =
  match (repr t1).desc, (repr t2).desc with
    TTyConstr (p1, _, _), TTyConstr (p2, _, _) ->
    path_same_name (fst (best_type_path p1)) (fst (best_type_path p2))
  | _ -> ()

let rec trace_same_names = function
    (t1, t1') :: (t2, t2') :: rem ->
    type_same_name t1 t2; type_same_name t1' t2'; trace_same_names rem
  | _ -> ()

let unification_error env unif tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  trace_same_names tr;
  let tr = List.map (fun (t, t') -> (t, hide_variant_name t')) tr in
  let mis = mismatch env unif tr in
  match tr with
  | [] | _ :: [] -> assert false
  | t1 :: t2 :: tr ->
    try
      let tr = filter_trace (mis = None) tr in
      let t1, t1' = may_prepare_expansion (tr = []) t1
      and t2, t2' = may_prepare_expansion (tr = []) t2 in
      print_labels := true;
      let tr = List.map prepare_expansion tr in
      fprintf ppf
        "@[<v>\
         @[%t@;<1 2>%a@ \
         %t@;<1 2>%a\
         %t\
         @]%a%t\
         @]"
        txt1 (type_expansion t1) t1'
        txt2 (type_expansion t2) t2'
        ty_expect_explanation
        (trace false "is not compatible with type") tr
        (explain mis);
      if env <> Env.empty
      then begin
        warn_on_missing_def env ppf t1;
        warn_on_missing_def env ppf t2
      end;
      print_labels := true
    with exn ->
      print_labels := true;
      raise exn

let report_unification_error ppf env ?(unif=true) tr
    ?(type_expected_explanation = fun _ -> ())
    txt1 txt2 =
  wrap_printing_env ~error:true env
    (fun () -> unification_error env unif tr txt1 ppf txt2
        type_expected_explanation)
;;

let trace fst keep_last txt ppf tr =
  print_labels := true;
  trace_same_names tr;
  try match tr with
      t1 :: t2 :: tr' ->
      if fst then trace fst txt ppf (t1 :: t2 :: filter_trace keep_last tr')
      else trace fst txt ppf (filter_trace keep_last tr);
      print_labels := true
    | _ -> ()
  with exn ->
    print_labels := true;
    raise exn

let report_ambiguous_type_error ppf env (tp0, tp0') tpl txt1 txt2 txt3 =
  wrap_printing_env ~error:true env (fun () ->
      reset ();
      List.iter
        (fun (tp, tp') -> path_same_name tp0 tp; path_same_name tp0' tp')
        tpl;
      match tpl with
        [] -> assert false
      | [tp, tp'] ->
        fprintf ppf
          "@[%t@;<1 2>%a@ \
           %t@;<1 2>%a\
           @]"
          txt1 (type_path_expansion tp) tp'
          txt3 (type_path_expansion tp0) tp0'
      | _ ->
        fprintf ppf
          "@[%t@;<1 2>@[<hv>%a@]\
           @ %t@;<1 2>%a\
           @]"
          txt2 type_path_list tpl
          txt3 (type_path_expansion tp0) tp0')
