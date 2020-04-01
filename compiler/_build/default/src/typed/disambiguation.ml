open Grain_parsing
open Grain_utils
open Types
open Typedtree
open Btype
open Ctype
open Checkertypes

type error =
  | WrongName of string * type_expected * string * Path.t * string * string list
  | NameTypeMismatch of
      string * Identifier.t * (Path.t * Path.t) * (Path.t * Path.t) list

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

let mk_expected ?explanation ty = { ty; explanation; }

let rec expand_path env p =
  let decl =
    try Some (Env.find_type p env) with Not_found -> None
  in
  match decl with
    Some {type_manifest = Some ty} ->
    begin match repr ty with
        {desc=TTyConstr(p,_,_)} -> expand_path env p
      | _ -> p
      (* PR#6394: recursive module may introduce incoherent manifest *)
    end
  | _ ->
    let p' = Env.normalize_path None env p in
    if Path.same p p' then p else expand_path env p'

let compare_type_path env tpath1 tpath2 =
  Path.same (expand_path env tpath1) (expand_path env tpath2)

let label_of_kind kind =
  if kind = "record" then "field" else "constructor"

module NameChoice(Name : sig
    type t
    val type_kind: string
    val get_name: t -> string
    val get_type: t -> type_expr
    val get_descrs: Env.type_descriptions -> t list
    val unbound_name_error: Env.t -> Identifier.t loc -> 'a
    val in_env: t -> bool
  end) = struct
  open Name

  let get_type_path d =
    match (repr (get_type d)).desc with
    | TTyConstr(p, _, _) -> p
    | _ -> assert false

  let lookup_from_type env tpath lid =
    let descrs = get_descrs (Env.find_type_descrs tpath env) in
    (*Env.mark_type_used env (Path.last tpath) (Env.find_type tpath env);*)
    match lid.txt with
    | Identifier.IdentName s -> begin
        try
          List.find (fun nd -> get_name nd = s) descrs
        with Not_found ->
          let names = List.map get_name descrs in
          raise (Error (lid.loc, env,
                        WrongName ("", mk_expected (newvar ()),
                                    type_kind, tpath, s, names)))
      end
    | _ -> raise Not_found

  let rec unique eq acc = function
    | [] -> List.rev acc
    | x :: rem ->
      if List.exists (eq x) acc then unique eq acc rem
      else unique eq (x :: acc) rem

  let ambiguous_types env lbl others =
    let tpath = get_type_path lbl in
    let others =
      List.map (fun (lbl, _) -> get_type_path lbl) others in
    let tpaths = unique (compare_type_path env) [tpath] others in
    match tpaths with
    | [_] -> []
    | _ -> List.map Printtyp.string_of_path tpaths

  let disambiguate_by_type env tpath lbls =
    let check_type (lbl, _) =
      let lbl_tpath = get_type_path lbl in
      compare_type_path env tpath lbl_tpath
    in
    List.find check_type lbls

  let disambiguate ?(warn=Location.prerr_warning) ?(check_lk=fun _ _ -> ())
      ?scope lid env opath lbls =
    let scope = match scope with None -> lbls | Some l -> l in
    let lbl = match opath with
        None ->
        begin match lbls with
            [] -> unbound_name_error env lid
          | (lbl, use) :: rest ->
            use ();
            let paths = ambiguous_types env lbl rest in
            if paths <> [] then
              warn lid.loc
                (Warnings.AmbiguousName ([Identifier.last lid.txt],
                                          paths, false));
            lbl
        end
      | Some(tpath0, tpath, pr) ->
        let warn_pr () =
          let label = label_of_kind type_kind in
          warn lid.loc
            (Warnings.NotPrincipal
               ("this type-based " ^ label ^ " disambiguation"))
        in
        try
          let lbl, use = disambiguate_by_type env tpath scope in
          use ();
          if not pr then begin
            (* Check if non-principal type is affecting result *)
            match lbls with
              [] -> warn_pr ()
            | (lbl', _use') :: rest ->
              let lbl_tpath = get_type_path lbl' in
              if not (compare_type_path env tpath lbl_tpath) then warn_pr ()
              else
                let paths = ambiguous_types env lbl rest in
                if paths <> [] then
                  warn lid.loc
                    (Warnings.AmbiguousName ([Identifier.last lid.txt],
                                              paths, false))
          end;
          lbl
        with Not_found -> try
            let lbl = lookup_from_type env tpath lid in
            check_lk tpath lbl;
            if in_env lbl then
              begin
                let s = Printtyp.string_of_path tpath in
                warn lid.loc
                  (Warnings.NameOutOfScope (s, [Identifier.last lid.txt], false));
              end;
            if not pr then warn_pr ();
            lbl
          with Not_found ->
            if lbls = [] then unbound_name_error env lid else
              let tp = (tpath0, expand_path env tpath) in
              let tpl =
                List.map
                  (fun (lbl, _) ->
                     let tp0 = get_type_path lbl in
                     let tp = expand_path env tp0 in
                     (tp0, tp))
                  lbls
              in
              raise (Error (lid.loc, env,
                            NameTypeMismatch (type_kind, lid.txt, tp, tpl)))
    in
    lbl
end

module Label = NameChoice (struct
  type t = label_description
  let type_kind = "record"
  let get_name lbl = lbl.lbl_name
  let get_type lbl = lbl.lbl_res
  let get_descrs = snd
  let unbound_name_error = Typetexp.unbound_label_error
  let in_env _ = true
end)

module Constructor = NameChoice (struct
  type t = constructor_description
  let type_kind = "variant"
  let get_name cstr = cstr.cstr_name
  let get_type cstr = cstr.cstr_res
  let get_descrs = fst
  let unbound_name_error = Typetexp.unbound_constructor_error
  let in_env _ = true
end)

let disambiguate_label_by_ids keep closed ids labels =
  let check_ids (lbl, _) =
    let lbls = Hashtbl.create 8 in
    Array.iter (fun lbl -> Hashtbl.add lbls lbl.lbl_name ()) lbl.lbl_all;
    List.for_all (Hashtbl.mem lbls) ids
  and check_closed (lbl, _) =
    (not closed || List.length ids = Array.length lbl.lbl_all)
  in
  let labels' = List.filter check_ids labels in
  if keep && labels' = [] then (false, labels) else
  let labels'' = List.filter check_closed labels' in
  if keep && labels'' = [] then (false, labels') else (true, labels'')

(* Only issue warnings once per record constructor/pattern *)
let disambiguate_lid_a_list loc closed env opath lid_a_list =
  let ids = List.map (fun (lid, _) -> Identifier.last lid.txt) lid_a_list in
  let w_pr = ref false and w_amb = ref []
  and w_scope = ref [] and w_scope_ty = ref "" in
  let warn loc msg =
    let open Warnings in
    match msg with
    | NotPrincipal _ -> w_pr := true
    | AmbiguousName([s], l, _) -> w_amb := (s, l) :: !w_amb
    | NameOutOfScope(ty, [s], _) ->
        w_scope := s :: !w_scope; w_scope_ty := ty
    | _ -> Location.prerr_warning loc msg
  in
  let process_label lid =
    (* Strategy for each field:
       * collect all the labels in scope for that name
       * if the type is known and principal, just eventually warn
         if the real label was not in scope
       * fail if there is no known type and no label found
       * otherwise use other fields to reduce the list of candidates
       * if there is no known type reduce it incrementally, so that
         there is still at least one candidate (for error message)
       * if the reduced list is valid, call Label.disambiguate
     *)
    try
      let labels = Env.lookup_all_labels lid.txt env in
      if List.length labels = 0 then raise Not_found;
      let (ok, lbls) =
        match opath with
        | Some (_, _, true) ->
            (true, labels) (* disambiguate only checks scope *)
        | _  -> disambiguate_label_by_ids (opath=None) closed ids labels
      in
      if ok then Label.disambiguate lid env opath lbls ~warn
      else fst (List.hd lbls) (* will fail later *)
    with Not_found ->
      match opath with
      | None -> 
        Env.error (Env.Unbound_label(lid.loc, Identifier.string_of_ident lid.txt))
      | Some _ ->
        Label.disambiguate lid env opath [] ~warn
  in
  let lbl_a_list =
    List.map (fun (lid,a) -> lid, process_label lid, a) lid_a_list in
  if !w_pr then
    Location.prerr_warning loc
      (Warnings.NotPrincipal "this type-based record disambiguation")
  else begin
    match List.rev !w_amb with
      (_, types)::_ as amb ->
        let paths =
          List.map (fun (_,lbl,_) -> Label.get_type_path lbl) lbl_a_list in
        let path = List.hd paths in
        if List.for_all (compare_type_path env path) (List.tl paths) then
          Location.prerr_warning loc
            (Warnings.AmbiguousName (List.map fst amb, types, true))
        else
          List.iter
            (fun (s, l) -> Location.prerr_warning loc
                (Warnings.AmbiguousName ([s], l, false)))
            amb
    | _ -> ()
  end;
  if !w_scope <> [] then
    Location.prerr_warning loc
      (Warnings.NameOutOfScope (!w_scope_ty, List.rev !w_scope, true));
  lbl_a_list


(* Error report *)
let spellcheck ppf unbound_name valid_names =
  Misc.did_you_mean ppf (fun () ->
    Misc.spellcheck valid_names unbound_name
  )
let spellcheck_idents ppf unbound valid_idents =
  spellcheck ppf (Ident.name unbound) (List.map Ident.name valid_idents)

let wrap_disambiguate kind ty f x =
  try f x with Error (loc, env, WrongName ("",_,tk,tp,name,valid_names)) ->
    raise (Error (loc, env, WrongName (kind,ty,tk,tp,name,valid_names)))

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
  | Assign_not_array ->
      fprintf ppf "the left-hand side of an array item access"
  | Assign_not_array_index ->
      fprintf ppf "the argument to an array item access"

let report_type_expected_explanation_opt expl ppf =
  match expl with
  | None -> ()
  | Some expl ->
      fprintf ppf "@ because it is in %t"
        (report_type_expected_explanation expl)

let report_error env ppf = function
  | WrongName (eorp, ty_expected, kind, p, name, valid_names) ->
      let { ty; explanation } = ty_expected in
      reset_and_mark_loops ty;
      begin
      fprintf ppf "@[@[<2>%s type@ %a%t@]@ "
        eorp type_expr ty
        (report_type_expected_explanation_opt explanation);
      fprintf ppf "The %s %s does not belong to type %a@]"
        (label_of_kind kind)
        name (*kind*) path p;
       end;
      spellcheck ppf name valid_names;
  | NameTypeMismatch (kind, lid, tp, tpl) ->
      let name = label_of_kind kind in
      report_ambiguous_type_error ppf env tp tpl
        (function ppf ->
           fprintf ppf "The %s %a@ belongs to the %s type"
             name identifier lid kind)
        (function ppf ->
           fprintf ppf "The %s %a@ belongs to one of the following %s types:"
             name identifier lid kind)
        (function ppf ->
           fprintf ppf "but a %s was expected belonging to the %s type"
             name kind)

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

