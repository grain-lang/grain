open Grain_parsing
open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype

type type_forcing_context =
  | If_conditional
  | If_no_else_branch
  | While_loop_conditional
  | While_loop_body
  | For_loop_start_index
  | For_loop_stop_index
  | For_loop_body
  | Assert_condition
  | Sequence_left_hand_side

type type_expected = {
  ty: type_expr;
  explanation: type_forcing_context option;
}

type error =
    Polymorphic_label of Identifier.t
  | Constructor_arity_mismatch of Identifier.t * int * int
  | Label_mismatch of Identifier.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Or_pattern_type_clash of Ident.t * (type_expr * type_expr) list
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of (type_expr * type_expr) list * type_forcing_context option
  | Apply_non_function of type_expr
  | Label_multiply_defined of string
  | Label_missing of Ident.t list
  | Label_not_mutable of Identifier.t
  | Wrong_name of string * type_expected * string * Path.t * string * string list
  | Name_type_mismatch of
      string * Identifier.t * (Path.t * Path.t) * (Path.t * Path.t) list
  | Invalid_format of string
  | Undefined_method of type_expr * string * string list option
  | Undefined_inherited_method of string * string list
  | Virtual_class of Identifier.t
  | Private_type of type_expr
  | Private_label of Identifier.t * type_expr
  | Unbound_instance_variable of string * string list
  | Instance_variable_not_mutable of bool * string
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of
      type_expr * type_expr * (type_expr * type_expr) list * bool
  | Too_many_arguments of bool * type_expr * type_forcing_context option
  | Scoping_let_module of string * type_expr
  | Masked_instance_variable of Identifier.t
  | Not_a_variant_type of Identifier.t
  | Incoherent_label_order
  | Less_general of string * (type_expr * type_expr) list
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module of type_expr
  | Recursive_local_constraint of (type_expr * type_expr) list
  | Unexpected_existential
  | Unqualified_gadt_pattern of Path.t * string
  | Invalid_interval
  | Invalid_for_loop_index
  | No_value_clauses
  | Exception_pattern_below_toplevel
  | Inlined_record_escape
  | Inlined_record_expected
  | Unrefuted_pattern of pattern
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Literal_overflow of string
  | Unknown_literal of string * char
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_class_expr
  | Unbound_value_missing_rec of Identifier.t * Location.t

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error


(*
  Saving and outputting type information.
  We keep these function names short, because they have to be
  called each time we create a record of type [Typedtree.expression]
  or [Typedtree.pattern] that will end up in the typed AST.
*)
let re node =
  (*Cmt_format.add_saved_type (Cmt_format.Partial_expression node);
  Stypes.record (Stypes.Ti_expr node);*)
  node
;;
let rp node =
  (*Cmt_format.add_saved_type (Cmt_format.Partial_pattern node);
  Stypes.record (Stypes.Ti_pat node);*)
  node
;;

type recarg =
  | Allowed
  | Required
  | Rejected


let mk_expected ?explanation ty = { ty; explanation; }

let constant : Parsetree.constant -> (Asttypes.constant, error) result = function
  | PConstNumber(n) -> Ok(Const_int n)
  | _ -> failwith "nyi: constant"

let constant_or_raise env loc cst =
  match constant cst with
  | Ok c -> c
  | Error err -> raise (Error (loc, env, err))


(* Specific version of type_option, using newty rather than newgenty *)

(*let type_option ty =
  newty (TTyConstr(Predef.path_option,[ty], ref TMemNil))*)

let mkexp exp_desc exp_type exp_loc exp_env =
  { exp_desc; exp_type; exp_loc; exp_env; exp_extra = []; }

let option_none ty loc =
  let lid = Longident.Lident "None"
  and env = Env.initial_safe_string in
  let cnone = Env.lookup_constructor lid env in
  mkexp (TExpApp(mknoloc lid, cnone, [])) ty loc env

let option_some texp =
  let lid = Longident.Lident "Some" in
  let csome = Env.lookup_constructor lid Env.initial_safe_string in
  mkexp ( Texp_construct(mknoloc lid , csome, [texp]) )
    (type_option texp.exp_type) texp.exp_loc texp.exp_env

let extract_option_type env ty =
  match expand_head env ty with {desc = Tconstr(path, [ty], _)}
    when Path.same path Predef.path_option -> ty
                              | _ -> assert false

let extract_concrete_record env ty =
  match extract_concrete_typedecl env ty with
    (p0, p, {type_kind=Type_record (fields, _)}) -> (p0, p, fields)
  | _ -> raise Not_found

let extract_concrete_variant env ty =
  match extract_concrete_typedecl env ty with
    (p0, p, {type_kind=Type_variant cstrs}) -> (p0, p, cstrs)
  | (p0, p, {type_kind=Type_open}) -> (p0, p, [])
  | _ -> raise Not_found

let extract_label_names env ty =
  try
    let (_, _,fields) = extract_concrete_record env ty in
    List.map (fun l -> l.Types.ld_id) fields
  with Not_found ->
    assert false

(* Typing of patterns *)

(* unification inside type_pat*)
let unify_pat_types loc env ty ty' =
  try
    unify env ty ty'
  with
    Unify trace ->
    raise(Error(loc, env, Pattern_type_clash(trace)))
  | Tags(l1,l2) ->
    raise(Typetexp.Error(loc, env, Typetexp.Variant_tags (l1, l2)))

(* unification inside type_exp and type_expect *)
let unify_exp_types loc env ty expected_ty =
  (* Format.eprintf "@[%a@ %a@]@." Printtyp.raw_type_expr exp.exp_type
     Printtyp.raw_type_expr expected_ty; *)
  try
    unify env ty expected_ty
  with
    Unify trace ->
    raise(Error(loc, env, Expr_type_clash(trace, None)))
  | Tags(l1,l2) ->
    raise(Typetexp.Error(loc, env, Typetexp.Variant_tags (l1, l2)))

(* level at which to create the local type declarations *)
let newtype_level = ref None
let get_newtype_level () =
  match !newtype_level with
    Some y -> y
  | None -> assert false


(* Check that a type is generalizable at some level *)
let generalizable level ty =
  let rec check ty =
    let ty = repr ty in
    if ty.level < lowest_level then () else
    if ty.level <= level then raise Exit else
    (mark_type_node ty; iter_type_expr check ty)
  in
  try check ty; unmark_type ty; true
  with Exit -> unmark_type ty; false

(* Typing of expressions *)

let unify_exp env exp expected_ty =
  let loc = proper_exp_loc exp in
  unify_exp_types loc env exp.exp_type expected_ty

let rec type_exp ?recarg env sexp =
  (* We now delegate everything to type_expect *)
  type_expect ?recarg env sexp (mk_expected (newvar ()))

(* Typing of an expression with an expected type.
   This provide better error messages, and allows controlled
   propagation of return type information.
   In the principal case, [type_expected'] may be at generic_level.
 *)

and type_expect ?in_function ?recarg env sexp ty_expected_explained =
  (*let previous_saved_types = Cmt_format.get_saved_types () in*)
  let exp =
    type_expect_ ?in_function ?recarg env sexp ty_expected_explained
  in
  (*Cmt_format.set_saved_types
    (Cmt_format.Partial_expression exp :: previous_saved_types);*)
  exp

and with_explanation explanation f =
  match explanation with
  | None -> f ()
  | Some explanation ->
      try f ()
      with Error (loc', env', Expr_type_clash(trace', None))
        when not loc'.Location.loc_ghost ->
        raise (Error (loc', env', Expr_type_clash(trace', Some explanation)))

and type_expect_ ?in_function ?(recarg=Rejected) env sexp ty_expected_explained =
  let { ty = ty_expected; explanation } = ty_expected_explained in
  let loc = sexp.pexp_loc in
  (* Record the expression type before unifying it with the expected type *)
  let with_explanation = with_explanation explanation in
  let rue exp =
    with_explanation (fun () ->
      unify_exp env (re exp) (instance env ty_expected));
    exp
  in
  match sexp.pexp_desc with
  | PExpId id ->
    begin
      let (path, desc) = Typetexp.find_value env id.loc id.txt in
      rue {
        exp_desc = TExpIdent(path, id, desc);
        exp_loc = loc;
        exp_extra = [];
        exp_type = instance env desc.val_type;
        exp_env = env;
      }
    end
  | _ -> 
    failwith "NYI"

and type_function ?in_function loc attrs env ty_expected_explained l caselist =
  let { ty = ty_expected; explanation } = ty_expected_explained in
  let (loc_fun, ty_fun) =
    match in_function with Some p -> p
    | None -> (loc, instance env ty_expected)
  in
  let separate = !Clflags.principal || Env.has_local_constraints env in
  if separate then begin_def ();
  let (ty_arg, ty_res) =
    try filter_arrow env (instance env ty_expected) l
    with Unify _ ->
      match expand_head env ty_expected with
        {desc = Tarrow _} as ty ->
          raise(Error(loc, env, Abstract_wrong_label(l, ty, explanation)))
      | _ ->
          raise(Error(loc_fun, env,
                      Too_many_arguments (in_function <> None,
                                          ty_fun,
                                          explanation)))
  in
  let ty_arg =
    if is_optional l then
      let tv = newvar() in
      begin
        try unify env ty_arg (type_option tv)
        with Unify _ -> assert false
      end;
      type_option tv
    else ty_arg
  in
  if separate then begin
    end_def ();
    generalize_structure ty_arg;
    generalize_structure ty_res
  end;
  let cases, partial =
    type_cases ~in_function:(loc_fun,ty_fun) env ty_arg ty_res
      true loc caselist in
  let not_function ty =
    let ls, tvar = list_labels env ty in
    ls = [] && not tvar
  in
  if is_optional l && not_function ty_res then
    Location.prerr_warning (List.hd cases).c_lhs.pat_loc
      Warnings.Unerasable_optional_argument;
  let param = name_pattern "param" cases in
  re {
    exp_desc = Texp_function { arg_label = l; param; cases; partial; };
    exp_loc = loc; exp_extra = [];
    exp_type = instance env (newgenty (Tarrow(l, ty_arg, ty_res, Cok)));
    exp_attributes = attrs;
    exp_env = env }

and type_argument ?recarg env sarg ty_expected' ty_expected =
  (* ty_expected' may be generic *)
  let no_labels ty =
    let ls, tvar = list_labels env ty in
    not tvar && List.for_all ((=) Nolabel) ls
  in
  let rec is_inferred sexp =
    match sexp.pexp_desc with
      Pexp_ident _ | Pexp_apply _ | Pexp_field _ | Pexp_constraint _
    | Pexp_coerce _ | Pexp_send _ | Pexp_new _ -> true
    | Pexp_sequence (_, e) | Pexp_open (_, _, e) -> is_inferred e
    | Pexp_ifthenelse (_, e1, Some e2) -> is_inferred e1 && is_inferred e2
    | _ -> false
  in
  match expand_head env ty_expected' with
    {desc = Tarrow(Nolabel,ty_arg,ty_res,_); level = lv}
    when is_inferred sarg ->
      (* apply optional arguments when expected type is "" *)
      (* we must be very careful about not breaking the semantics *)
      if !Clflags.principal then begin_def ();
      let texp = type_exp env sarg in
      if !Clflags.principal then begin
        end_def ();
        generalize_structure texp.exp_type
      end;
      let rec make_args args ty_fun =
        match (expand_head env ty_fun).desc with
        | Tarrow (l,ty_arg,ty_fun,_) when is_optional l ->
            let ty = option_none (instance env ty_arg) sarg.pexp_loc in
            make_args ((l, Some ty) :: args) ty_fun
        | Tarrow (l,_,ty_res',_) when l = Nolabel || !Clflags.classic ->
            List.rev args, ty_fun, no_labels ty_res'
        | Tvar _ ->  List.rev args, ty_fun, false
        |  _ -> [], texp.exp_type, false
      in
      let args, ty_fun', simple_res = make_args [] texp.exp_type in
      let warn = !Clflags.principal &&
        (lv <> generic_level || (repr ty_fun').level <> generic_level)
      and texp = {texp with exp_type = instance env texp.exp_type}
      and ty_fun = instance env ty_fun' in
      if not (simple_res || no_labels ty_res) then begin
        unify_exp env texp ty_expected;
        texp
      end else begin
      unify_exp env {texp with exp_type = ty_fun} ty_expected;
      if args = [] then texp else
      (* eta-expand to avoid side effects *)
      let var_pair name ty =
        let id = Ident.create name in
        {pat_desc = Tpat_var (id, mknoloc name); pat_type = ty;pat_extra=[];
         pat_attributes = [];
         pat_loc = Location.none; pat_env = env},
        {exp_type = ty; exp_loc = Location.none; exp_env = env;
         exp_extra = []; exp_attributes = [];
         exp_desc =
         Texp_ident(Path.Pident id, mknoloc (Longident.Lident name),
                    {val_type = ty; val_kind = Val_reg;
                     val_attributes = [];
                     Types.val_loc = Location.none})}
      in
      let eta_pat, eta_var = var_pair "eta" ty_arg in
      let func texp =
        let e =
          {texp with exp_type = ty_res; exp_desc =
           Texp_apply
             (texp,
              args @ [Nolabel, Some eta_var])}
        in
        let cases = [case eta_pat e] in
        let param = name_pattern "param" cases in
        { texp with exp_type = ty_fun; exp_desc =
          Texp_function { arg_label = Nolabel; param; cases;
            partial = Total; } }
      in
      Location.prerr_warning texp.exp_loc
        (Warnings.Eliminated_optional_arguments
           (List.map (fun (l, _) -> Printtyp.string_of_label l) args));
      if warn then Location.prerr_warning texp.exp_loc
          (Warnings.Without_principality "eliminated optional argument");
      (* let-expand to have side effects *)
      let let_pat, let_var = var_pair "arg" texp.exp_type in
      re { texp with exp_type = ty_fun; exp_desc =
           Texp_let (Nonrecursive,
                     [{vb_pat=let_pat; vb_expr=texp; vb_attributes=[];
                       vb_loc=Location.none;
                      }],
                     func let_var) }
      end
  | _ ->
      let texp = type_expect ?recarg env sarg (mk_expected ty_expected') in
      unify_exp env texp ty_expected;
      texp

and type_application env funct sargs =
  (* funct.exp_type may be generic *)
  let result_type omitted ty_fun =
    List.fold_left
      (fun ty_fun (l,ty,lv) -> newty2 lv (Tarrow(l,ty,ty_fun,Cok)))
      ty_fun omitted
  in
  let has_label l ty_fun =
    let ls, tvar = list_labels env ty_fun in
    tvar || List.mem l ls
  in
  let ignored = ref [] in
  let rec type_unknown_args
      (args :
      (Asttypes.arg_label * (unit -> Typedtree.expression) option) list)
    omitted ty_fun = function
      [] ->
        (List.map
            (function l, None -> l, None
                | l, Some f -> l, Some (f ()))
           (List.rev args),
         instance env (result_type omitted ty_fun))
    | (l1, sarg1) :: sargl ->
        let (ty1, ty2) =
          let ty_fun = expand_head env ty_fun in
          match ty_fun.desc with
            Tvar _ ->
              let t1 = newvar () and t2 = newvar () in
              let not_identity = function
                  Texp_ident(_,_,{val_kind=Val_prim
                                  {Primitive.prim_name="%identity"}}) ->
                    false
                | _ -> true
              in
              if ty_fun.level >= t1.level && not_identity funct.exp_desc then
                Location.prerr_warning sarg1.pexp_loc Warnings.Unused_argument;
              unify env ty_fun (newty (Tarrow(l1,t1,t2,Clink(ref Cunknown))));
              (t1, t2)
          | Tarrow (l,t1,t2,_) when l = l1
            || !Clflags.classic && l1 = Nolabel && not (is_optional l) ->
              (t1, t2)
          | td ->
              let ty_fun =
                match td with Tarrow _ -> newty td | _ -> ty_fun in
              let ty_res = result_type (omitted @ !ignored) ty_fun in
              match ty_res.desc with
                Tarrow _ ->
                  if (!Clflags.classic || not (has_label l1 ty_fun)) then
                    raise (Error(sarg1.pexp_loc, env,
                                 Apply_wrong_label(l1, ty_res)))
                  else
                    raise (Error(funct.exp_loc, env, Incoherent_label_order))
              | _ ->
                  raise(Error(funct.exp_loc, env, Apply_non_function
                                (expand_head env funct.exp_type)))
        in
        let optional = is_optional l1 in
        let arg1 () =
          let arg1 = type_expect env sarg1 (mk_expected ty1) in
          if optional then
            unify_exp env arg1 (type_option(newvar()));
          arg1
        in
        type_unknown_args ((l1, Some arg1) :: args) omitted ty2 sargl
  in
  let ignore_labels =
    !Clflags.classic ||
    begin
      let ls, tvar = list_labels env funct.exp_type in
      not tvar &&
      let labels = List.filter (fun l -> not (is_optional l)) ls in
      List.length labels = List.length sargs &&
      List.for_all (fun (l,_) -> l = Nolabel) sargs &&
      List.exists (fun l -> l <> Nolabel) labels &&
      (Location.prerr_warning
         funct.exp_loc
         (Warnings.Labels_omitted
            (List.map Printtyp.string_of_label
                      (List.filter ((<>) Nolabel) labels)));
       true)
    end
  in
  let warned = ref false in
  let rec type_args args omitted ty_fun ty_fun0 ty_old sargs more_sargs =
    match expand_head env ty_fun, expand_head env ty_fun0 with
      {desc=Tarrow (l, ty, ty_fun, com); level=lv} as ty_fun',
      {desc=Tarrow (_, ty0, ty_fun0, _)}
      when (sargs <> [] || more_sargs <> []) && commu_repr com = Cok ->
        let may_warn loc w =
          if not !warned && !Clflags.principal && lv <> generic_level
          then begin
            warned := true;
            Location.prerr_warning loc w
          end
        in
        let name = label_name l
        and optional = is_optional l in
        let sargs, more_sargs, arg =
          if ignore_labels && not (is_optional l) then begin
            (* In classic mode, omitted = [] *)
            match sargs, more_sargs with
              (l', sarg0) :: _, _ ->
                raise(Error(sarg0.pexp_loc, env,
                            Apply_wrong_label(l', ty_old)))
            | _, (l', sarg0) :: more_sargs ->
                if l <> l' && l' <> Nolabel then
                  raise(Error(sarg0.pexp_loc, env,
                              Apply_wrong_label(l', ty_fun')))
                else
                  ([], more_sargs,
                   Some (fun () -> type_argument env sarg0 ty ty0))
            | _ ->
                assert false
          end else try
            let (l', sarg0, sargs, more_sargs) =
              try
                let (l', sarg0, sargs1, sargs2) = extract_label name sargs in
                if sargs1 <> [] then
                  may_warn sarg0.pexp_loc
                    (Warnings.Not_principal "commuting this argument");
                (l', sarg0, sargs1 @ sargs2, more_sargs)
              with Not_found ->
                let (l', sarg0, sargs1, sargs2) =
                  extract_label name more_sargs in
                if sargs1 <> [] || sargs <> [] then
                  may_warn sarg0.pexp_loc
                    (Warnings.Not_principal "commuting this argument");
                (l', sarg0, sargs @ sargs1, sargs2)
            in
            if not optional && is_optional l' then
              Location.prerr_warning sarg0.pexp_loc
                (Warnings.Nonoptional_label (Printtyp.string_of_label l));
            sargs, more_sargs,
            if not optional || is_optional l' then
              Some (fun () -> type_argument env sarg0 ty ty0)
            else begin
              may_warn sarg0.pexp_loc
                (Warnings.Not_principal "using an optional argument here");
              Some (fun () -> option_some (type_argument env sarg0
                                             (extract_option_type env ty)
                                             (extract_option_type env ty0)))
            end
          with Not_found ->
            sargs, more_sargs,
            if optional &&
              (List.mem_assoc Nolabel sargs
               || List.mem_assoc Nolabel more_sargs)
            then begin
              may_warn funct.exp_loc
                (Warnings.Without_principality "eliminated optional argument");
              ignored := (l,ty,lv) :: !ignored;
              Some (fun () -> option_none (instance env ty) Location.none)
            end else begin
              may_warn funct.exp_loc
                (Warnings.Without_principality "commuted an argument");
              None
            end
        in
        let omitted =
          if arg = None then (l,ty,lv) :: omitted else omitted in
        let ty_old = if sargs = [] then ty_fun else ty_old in
        type_args ((l,arg)::args) omitted ty_fun ty_fun0
          ty_old sargs more_sargs
    | _ ->
        match sargs with
          (l, sarg0) :: _ when ignore_labels ->
            raise(Error(sarg0.pexp_loc, env,
                        Apply_wrong_label(l, ty_old)))
        | _ ->
            type_unknown_args args omitted ty_fun0
              (sargs @ more_sargs)
  in
  let is_ignore funct =
    match funct.exp_desc with
      Texp_ident (_, _, {val_kind=Val_prim{Primitive.prim_name="%ignore"}}) ->
        (try ignore (filter_arrow env (instance env funct.exp_type) Nolabel);
             true
        with Unify _ -> false)
    | _ -> false
  in
  match sargs with
    (* Special case for ignore: avoid discarding warning *)
    [Nolabel, sarg] when is_ignore funct ->
      let ty_arg, ty_res =
        filter_arrow env (instance env funct.exp_type) Nolabel
      in
      let exp = type_expect env sarg (mk_expected ty_arg) in
      begin match (expand_head env exp.exp_type).desc with
      | Tarrow _ ->
          Location.prerr_warning exp.exp_loc Warnings.Partial_application
      | Tvar _ ->
          add_delayed_check (fun () -> check_application_result env false exp)
      | _ -> ()
      end;
      ([Nolabel, Some exp], ty_res)
  | _ ->
      let ty = funct.exp_type in
      if ignore_labels then
        type_args [] [] ty (instance env ty) ty [] sargs
      else
        type_args [] [] ty (instance env ty) ty sargs []

and type_construct env loc lid sarg ty_expected_explained attrs =
  let { ty = ty_expected; explanation } = ty_expected_explained in
  let opath =
    try
      let (p0, p,_) = extract_concrete_variant env ty_expected in
      Some(p0, p, ty_expected.level = generic_level || not !Clflags.principal)
    with Not_found -> None
  in
  let constrs = Typetexp.find_all_constructors env lid.loc lid.txt in
  let constr =
    wrap_disambiguate "This variant expression is expected to have"
      ty_expected_explained
      (Constructor.disambiguate lid env opath) constrs in
  Env.mark_constructor Env.Positive env (Longident.last lid.txt) constr;
  Builtin_attributes.check_deprecated loc constr.cstr_attributes
    constr.cstr_name;
  let sargs =
    match sarg with
      None -> []
    | Some {pexp_desc = Pexp_tuple sel} when
        constr.cstr_arity > 1 || Builtin_attributes.explicit_arity attrs
      -> sel
    | Some se -> [se] in
  if List.length sargs <> constr.cstr_arity then
    raise(Error(loc, env, Constructor_arity_mismatch
                  (lid.txt, constr.cstr_arity, List.length sargs)));
  let separate = !Clflags.principal || Env.has_local_constraints env in
  if separate then (begin_def (); begin_def ());
  let (ty_args, ty_res) = instance_constructor constr in
  let texp =
    re {
      exp_desc = Texp_construct(lid, constr, []);
      exp_loc = loc; exp_extra = [];
      exp_type = ty_res;
      exp_attributes = attrs;
      exp_env = env } in
  if separate then begin
    end_def ();
    generalize_structure ty_res;
    with_explanation explanation (fun () ->
      unify_exp env {texp with exp_type = instance_def ty_res}
                    (instance env ty_expected));
    end_def ();
    List.iter generalize_structure ty_args;
    generalize_structure ty_res;
  end;
  let ty_args0, ty_res =
    match instance_list env (ty_res :: ty_args) with
      t :: tl -> tl, t
    | _ -> assert false
  in
  let texp = {texp with exp_type = ty_res} in
  if not separate then unify_exp env texp (instance env ty_expected);
  let recarg =
    match constr.cstr_inlined with
    | None -> Rejected
    | Some _ ->
        begin match sargs with
        | [{pexp_desc =
              Pexp_ident _ |
              Pexp_record (_, (Some {pexp_desc = Pexp_ident _}| None))}] ->
            Required
        | _ ->
            raise (Error(loc, env, Inlined_record_expected))
        end
  in
  let args =
    List.map2 (fun e (t,t0) -> type_argument ~recarg env e t t0) sargs
      (List.combine ty_args ty_args0) in
  if constr.cstr_private = Private then
    raise(Error(loc, env, Private_type ty_res));
  (* NOTE: shouldn't we call "re" on this final expression? -- AF *)
  { texp with
    exp_desc = Texp_construct(lid, constr, args) }

(* Typing of statements (expressions whose values are discarded) *)

and type_statement ?explanation env sexp =
  let loc = (final_subexpression sexp).pexp_loc in
  begin_def();
  let exp = type_exp env sexp in
  end_def();
  let ty = expand_head env exp.exp_type and tv = newvar() in
  if is_Tvar ty && ty.level > tv.level then
      Location.prerr_warning loc Warnings.Nonreturning_statement;
  if !Clflags.strict_sequence then
    let expected_ty = instance_def Predef.type_unit in
    with_explanation explanation (fun () ->
      unify_exp env exp expected_ty);
    exp
  else begin
    begin match ty.desc with
    | Tarrow _ ->
        Location.prerr_warning loc Warnings.Partial_application
    | Tconstr (p, _, _) when Path.same p Predef.path_unit -> ()
    | Tvar _ ->
        add_delayed_check (fun () -> check_application_result env true exp)
    | _ ->
        Location.prerr_warning loc Warnings.Statement_type
    end;
    unify_var env tv ty;
    exp
  end

(* Typing of match cases *)

and type_cases ?in_function env ty_arg ty_res partial_flag loc caselist =
  (* ty_arg is _fully_ generalized *)
  let patterns = List.map (fun {pc_lhs=p} -> p) caselist in
  let contains_polyvars = List.exists contains_polymorphic_variant patterns in
  let erase_either = contains_polyvars && contains_variant_either ty_arg
  and has_gadts = List.exists (contains_gadt env) patterns in
(*  prerr_endline ( if has_gadts then "contains gadt" else "no gadt"); *)
  let ty_arg =
    if (has_gadts || erase_either) && not !Clflags.principal
    then correct_levels ty_arg else ty_arg
  and ty_res, env =
    if has_gadts && not !Clflags.principal then
      correct_levels ty_res, duplicate_ident_types caselist env
    else ty_res, env
  in
  let rec is_var spat =
    match spat.ppat_desc with
      Ppat_any | Ppat_var _ -> true
    | Ppat_alias (spat, _) -> is_var spat
    | _ -> false in
  let needs_exhaust_check =
    match caselist with
      [{pc_rhs = {pexp_desc = Pexp_unreachable}}] -> true
    | [{pc_lhs}] when is_var pc_lhs -> false
    | _ -> true
  in
  let init_env () =
    (* raise level for existentials *)
    begin_def ();
    Ident.set_current_time (get_current_level ());
    let lev = Ident.current_time () in
    Ctype.init_def (lev+1000);                 (* up to 1000 existentials *)
    (lev, Env.add_gadt_instance_level lev env)
  in
  let lev, env =
    if has_gadts then init_env () else (get_current_level (), env)
  in
(*  if has_gadts then
    Format.printf "lev = %d@.%a@." lev Printtyp.raw_type_expr ty_res; *)
  (* Do we need to propagate polymorphism *)
  let propagate =
    !Clflags.principal || has_gadts || (repr ty_arg).level = generic_level ||
    match caselist with
      [{pc_lhs}] when is_var pc_lhs -> false
    | _ -> true in
  if propagate then begin_def (); (* propagation of the argument *)
  let pattern_force = ref [] in
(*  Format.printf "@[%i %i@ %a@]@." lev (get_current_level())
    Printtyp.raw_type_expr ty_arg; *)
  let pat_env_list =
    List.map
      (fun {pc_lhs; pc_guard; pc_rhs} ->
        let loc =
          let open Location in
          match pc_guard with
          | None -> pc_rhs.pexp_loc
          | Some g -> {pc_rhs.pexp_loc with loc_start=g.pexp_loc.loc_start}
        in
        if !Clflags.principal then begin_def (); (* propagation of pattern *)
        let scope = Some (Annot.Idef loc) in
        let (pat, ext_env, force, unpacks) =
          let partial =
            if !Clflags.principal || erase_either
            then Some false else None in
          let ty_arg = instance ?partial env ty_arg in
          type_pattern ~lev env pc_lhs scope ty_arg
        in
        pattern_force := force @ !pattern_force;
        let pat =
          if !Clflags.principal then begin
            end_def ();
            iter_pattern (fun {pat_type=t} -> generalize_structure t) pat;
            { pat with pat_type = instance ext_env pat.pat_type }
          end else pat
        in
        (pat, (ext_env, unpacks)))
      caselist in
  (* Unify all cases (delayed to keep it order-free) *)
  let ty_arg' = newvar () in
  let unify_pats ty =
    List.iter (fun (pat, (ext_env, _)) -> unify_pat ext_env pat ty)
      pat_env_list in
  unify_pats ty_arg';
  (* Check for polymorphic variants to close *)
  let patl = List.map fst pat_env_list in
  if List.exists has_variants patl then begin
    Parmatch.pressure_variants env patl;
    List.iter (iter_pattern finalize_variant) patl
  end;
  (* `Contaminating' unifications start here *)
  List.iter (fun f -> f()) !pattern_force;
  (* Post-processing and generalization *)
  if propagate || erase_either then unify_pats (instance env ty_arg);
  if propagate then begin
    List.iter
      (iter_pattern (fun {pat_type=t} -> unify_var env t (newvar()))) patl;
    end_def ();
    List.iter (iter_pattern (fun {pat_type=t} -> generalize t)) patl;
  end;
  (* type bodies *)
  let in_function = if List.length caselist = 1 then in_function else None in
  let cases =
    List.map2
      (fun (pat, (ext_env, unpacks)) {pc_lhs; pc_guard; pc_rhs} ->
        let sexp = wrap_unpacks pc_rhs unpacks in
        let ty_res' =
          if !Clflags.principal then begin
            begin_def ();
            let ty = instance ~partial:true env ty_res in
            end_def ();
            generalize_structure ty; ty
          end
          else if contains_gadt env pc_lhs then correct_levels ty_res
          else ty_res in
(*        Format.printf "@[%i %i, ty_res' =@ %a@]@." lev (get_current_level())
          Printtyp.raw_type_expr ty_res'; *)
        let guard =
          match pc_guard with
          | None -> None
          | Some scond ->
              Some
                (type_expect ext_env (wrap_unpacks scond unpacks)
                   (mk_expected Predef.type_bool))
        in
        let exp =
          type_expect ?in_function ext_env sexp (mk_expected ty_res') in
        {
         c_lhs = pat;
         c_guard = guard;
         c_rhs = {exp with exp_type = instance env ty_res'}
        }
      )
      pat_env_list caselist
  in
  if !Clflags.principal || has_gadts then begin
    let ty_res' = instance env ty_res in
    List.iter (fun c -> unify_exp env c.c_rhs ty_res') cases
  end;
  let do_init = has_gadts || needs_exhaust_check in
  let lev, env =
    if do_init && not has_gadts then init_env () else lev, env in
  let ty_arg_check =
    if do_init then
      (* Hack: use for_saving to copy variables too *)
      Subst.type_expr (Subst.for_saving Subst.identity) ty_arg
    else ty_arg
  in
  let partial =
    if partial_flag then
      check_partial ~lev env ty_arg_check loc cases
    else
      Partial
  in
  let unused_check () =
    List.iter (fun (pat, (env, _)) -> check_absent_variant env pat)
      pat_env_list;
    check_unused ~lev env (instance env ty_arg_check) cases ;
    Parmatch.check_ambiguous_bindings cases
  in
  if contains_polyvars || do_init then
    add_delayed_check unused_check
  else
    unused_check ();
  (* Check for unused cases, do not delay because of gadts *)
  if do_init then begin
    end_def ();
    (* Ensure that existential types do not escape *)
    unify_exp_types loc env (instance env ty_res) (newvar ()) ;
  end;
  cases, partial

(* Typing of let bindings *)

and type_let ?(check = fun s -> Warnings.Unused_var s)
             ?(check_strict = fun s -> Warnings.Unused_var_strict s)
    env rec_flag spat_sexp_list scope allow =
  let open Ast_helper in
  begin_def();
  if !Clflags.principal then begin_def ();
  let is_fake_let =
    match spat_sexp_list with
    | [{pvb_expr={pexp_desc=Pexp_match(
           {pexp_desc=Pexp_ident({ txt = Longident.Lident "*opt*"})},_)}}] ->
        true (* the fake let-declaration introduced by fun ?(x = e) -> ... *)
    | _ ->
        false
  in
  let check = if is_fake_let then check_strict else check in
  let spatl =
    List.map
      (fun {pvb_pat=spat; pvb_expr=sexp; pvb_attributes=attrs} ->
        attrs,
        match spat.ppat_desc, sexp.pexp_desc with
          (Ppat_any | Ppat_constraint _), _ -> spat
        | _, Pexp_coerce (_, _, sty)
        | _, Pexp_constraint (_, sty) when !Clflags.principal ->
            (* propagate type annotation to pattern,
               to allow it to be generalized in -principal mode *)
            Pat.constraint_
              ~loc:{spat.ppat_loc with Location.loc_ghost=true}
              spat
              sty
        | _ -> spat)
      spat_sexp_list in
  let nvs = List.map (fun _ -> newvar ()) spatl in
  let (pat_list, new_env, force, unpacks, pv) =
    type_pattern_list env spatl scope nvs allow in
  let attrs_list = List.map fst spatl in
  let is_recursive = (rec_flag = Recursive) in
  (* If recursive, first unify with an approximation of the expression *)
  if is_recursive then
    List.iter2
      (fun pat binding ->
        let pat =
          match pat.pat_type.desc with
          | Tpoly (ty, tl) ->
              {pat with pat_type =
               snd (instance_poly ~keep_names:true false tl ty)}
          | _ -> pat
        in unify_pat env pat (type_approx env binding.pvb_expr))
      pat_list spat_sexp_list;
  (* Polymorphic variant processing *)
  List.iter
    (fun pat ->
      if has_variants pat then begin
        Parmatch.pressure_variants env [pat];
        iter_pattern finalize_variant pat
      end)
    pat_list;
  (* Generalize the structure *)
  let pat_list =
    if !Clflags.principal then begin
      end_def ();
      List.map
        (fun pat ->
          iter_pattern (fun pat -> generalize_structure pat.pat_type) pat;
          {pat with pat_type = instance env pat.pat_type})
        pat_list
    end else pat_list in
  (* Only bind pattern variables after generalizing *)
  List.iter (fun f -> f()) force;
  let exp_env =
    if is_recursive then new_env
    else if not is_recursive then begin
      (* add ghost bindings to help detecting missing "rec" keywords *)
      match spat_sexp_list with
      | {pvb_loc; _} :: _ -> maybe_add_pattern_variables_ghost pvb_loc env pv
      | _ -> assert false
    end
    else env in
  let current_slot = ref None in
  let rec_needed = ref false in
  let warn_about_unused_bindings =
    List.exists
      (fun attrs ->
         Builtin_attributes.warning_scope ~ppwarning:false attrs (fun () ->
           Warnings.is_active (check "") || Warnings.is_active (check_strict "") ||
           (is_recursive && (Warnings.is_active Warnings.Unused_rec_flag))))
      attrs_list
  in
  let pat_slot_list =
    (* Algorithm to detect unused declarations in recursive bindings:
       - During type checking of the definitions, we capture the 'value_used'
         events on the bound identifiers and record them in a slot corresponding
         to the current definition (!current_slot).
         In effect, this creates a dependency graph between definitions.
       - After type checking the definition (!current_slot = None),
         when one of the bound identifier is effectively used, we trigger
         again all the events recorded in the corresponding slot.
         The effect is to traverse the transitive closure of the graph created
         in the first step.
       We also keep track of whether *all* variables in a given pattern
       are unused. If this is the case, for local declarations, the issued
       warning is 26, not 27.
     *)
    List.map2
      (fun attrs pat ->
         Builtin_attributes.warning_scope ~ppwarning:false attrs (fun () ->
           if not warn_about_unused_bindings then pat, None
           else
             let some_used = ref false in
             (* has one of the identifier of this pattern been used? *)
             let slot = ref [] in
             List.iter
               (fun id ->
                  let vd = Env.find_value (Path.Pident id) new_env in
                  (* note: Env.find_value does not trigger the value_used event *)
                  let name = Ident.name id in
                  let used = ref false in
                  if not (name = "" || name.[0] = '_' || name.[0] = '#') then
                    add_delayed_check
                      (fun () ->
                         if not !used then
                           Location.prerr_warning vd.Types.val_loc
                             ((if !some_used then check_strict else check) name)
                      );
                  Env.set_value_used_callback
                    name vd
                    (fun () ->
                       match !current_slot with
                       | Some slot ->
                         slot := (name, vd) :: !slot; rec_needed := true
                       | None ->
                         List.iter
                           (fun (name, vd) -> Env.mark_value_used env name vd)
                           (get_ref slot);
                         used := true;
                         some_used := true
                    )
               )
               (Typedtree.pat_bound_idents pat);
             pat, Some slot
         ))
      attrs_list
      pat_list
  in
  let exp_list =
    List.map2
      (fun {pvb_expr=sexp; pvb_attributes; _} (pat, slot) ->
        let sexp =
          if rec_flag = Recursive then wrap_unpacks sexp unpacks else sexp in
        if is_recursive then current_slot := slot;
        match pat.pat_type.desc with
        | Tpoly (ty, tl) ->
            begin_def ();
            if !Grain_utils.Config.principal then begin_def ();
            let vars, ty' = instance_poly ~keep_names:true true tl ty in
            if !Grain_utils.Config.principal then begin
              end_def ();
              generalize_structure ty'
            end;
            let exp =
              Builtin_attributes.warning_scope pvb_attributes
                  (fun () -> type_expect exp_env sexp (mk_expected ty'))
            in
            end_def ();
            check_univars env true "definition" exp pat.pat_type vars;
            {exp with exp_type = instance env exp.exp_type}
        | _ ->
            Builtin_attributes.warning_scope pvb_attributes (fun () ->
              type_expect exp_env sexp (mk_expected pat.pat_type)))
      spat_sexp_list pat_slot_list in
  current_slot := None;
  if is_recursive && not !rec_needed
  && Warnings.is_active Warnings.Unused_rec_flag then begin
    let {pvb_pat; pvb_attributes} = List.hd spat_sexp_list in
    (* See PR#6677 *)
    Builtin_attributes.warning_scope ~ppwarning:false pvb_attributes
      (fun () ->
         Location.prerr_warning pvb_pat.ppat_loc Warnings.Unused_rec_flag
      )
  end;
  List.iter2
    (fun pat (attrs, exp) ->
       Builtin_attributes.warning_scope ~ppwarning:false attrs
         (fun () ->
            ignore(check_partial env pat.pat_type pat.pat_loc
                     [case pat exp])
         )
    )
    pat_list
    (List.map2 (fun (attrs, _) e -> attrs, e) spatl exp_list);
  end_def();
  List.iter2
    (fun pat exp ->
       if not (is_nonexpansive exp) then
         iter_pattern (fun pat -> generalize_expansive env pat.pat_type) pat)
    pat_list exp_list;
  List.iter
    (fun pat -> iter_pattern (fun pat -> generalize pat.pat_type) pat)
    pat_list;
  let l = List.combine pat_list exp_list in
  let l =
    List.map2
      (fun (p, e) pvb ->
        {vb_pat=p; vb_expr=e; vb_attributes=pvb.pvb_attributes;
         vb_loc=pvb.pvb_loc;
        })
      l spat_sexp_list
  in
  if is_recursive then
    List.iter
      (fun {vb_pat=pat} -> match pat.pat_desc with
           Tpat_var _ -> ()
         | Tpat_alias ({pat_desc=Tpat_any}, _, _) -> ()
         | _ -> raise(Error(pat.pat_loc, env, Illegal_letrec_pat)))
      l;
  (l, new_env, unpacks)

(* Typing of toplevel bindings *)
let type_binding env rec_flag spat_sexp_list scope =
  Typetexp.reset_type_variables();
  let (pat_exp_list, new_env, _unpacks) =
    type_let
      ~check:(fun s -> Warnings.Unused_value_declaration s)
      ~check_strict:(fun s -> Warnings.Unused_value_declaration s)
      env rec_flag spat_sexp_list scope false
  in
  (pat_exp_list, new_env)
let type_let env rec_flag spat_sexp_list scope =
  let (pat_exp_list, new_env, _unpacks) =
    type_let env rec_flag spat_sexp_list scope false in
  (pat_exp_list, new_env)


(* Typing of toplevel expressions *)
let type_statement env stmt =
  Typetexp.reset_type_variables();
  begin_def();
  let exp = type_exp env sexp in
  end_def();
  if not (is_nonexpansive exp) then generalize_expansive env exp.exp_type;
  generalize exp.exp_type;
  match sexp.pexp_desc with
    Pexp_ident lid ->
      (* Special case for keeping type variables when looking-up a variable *)
      let (_path, desc) = Env.lookup_value lid.txt env in
      {exp with exp_type = desc.val_type}
  | _ -> exp
