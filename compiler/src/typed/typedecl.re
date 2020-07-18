/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/**** Typing of type definitions ****/

open Grain_parsing;
open Misc;
open Asttypes;
open Parsetree;
open Primitive;
open Types;
open Typetexp;

type native_repr_kind =
  | Unboxed
  | Untagged;

type error =
  | Repeated_parameter
  | Duplicate_constructor(string)
  | Too_many_constructors
  | Duplicate_label(string)
  | Recursive_abbrev(string)
  | Cycle_in_def(string, type_expr)
  | Definition_mismatch(type_expr, list(Includecore.type_mismatch))
  | Constraint_failed(type_expr, type_expr)
  | Inconsistent_constraint(Env.t, list((type_expr, type_expr)))
  | Type_clash(Env.t, list((type_expr, type_expr)))
  | Parameters_differ(Path.t, type_expr, type_expr)
  | Null_arity_external
  | Missing_native_external
  | Unbound_type_var(type_expr, type_declaration)
  | Cannot_extend_private_type(Path.t)
  | Not_extensible_type(Path.t)
  | Extension_mismatch(Path.t, list(Includecore.type_mismatch))
  | Rebind_wrong_type(Identifier.t, Env.t, list((type_expr, type_expr)))
  | Rebind_mismatch(Identifier.t, Path.t, Path.t)
  | Rebind_private(Identifier.t)
  | Bad_variance(int, (bool, bool, bool), (bool, bool, bool))
  | Unavailable_type_constructor(Path.t)
  | Bad_fixed_type(string)
  /*| Unbound_type_var_ext of type_expr * extension_constructor*/
  | Varying_anonymous
  | Val_in_structure
  | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type(native_repr_kind)
  | Deep_unbox_or_untag_attribute(native_repr_kind)
  | Bad_immediate_attribute
  | Bad_unboxed_attribute(string)
  | Wrong_unboxed_type_float
  | Boxed_and_unboxed
  | Nonrec_gadt;

open Typedtree;

exception Error(Location.t, error);

/* Enter all declared types in the environment as abstract types */

let enter_type = (rec_flag, env, sdecl, id) => {
  let needed =
    switch (rec_flag) {
    | Asttypes.Nonrecursive => false
    /*begin match sdecl.pdata_kind with
      | PDataVariant scds ->
          List.iter (fun cd ->
            if cd.pcd_res <> None then raise (Error(cd.pcd_loc, Nonrec_gadt)))
            scds
      | _ -> ()
      end;
      Btype.is_row_name (Ident.name id)*/
    | Asttypes.Recursive => true
    };

  if (!needed) {
    env;
  } else {
    let decl = {
      type_params: List.map(_ => Btype.newgenvar(), sdecl.pdata_params),
      type_arity: List.length(sdecl.pdata_params),
      type_kind: TDataAbstract,
      type_manifest: None,
      /*begin match sdecl.pdata_manifest with None -> None
        | Some _ -> Some(Ctype.newvar ()) end;*/
      type_newtype_level: None,
      type_loc: sdecl.pdata_loc,
      type_path: PIdent(id),
      type_immediate: false,
    };

    Env.add_type(~check=true, id, decl, env);
  };
};

let update_type = (temp_env, env, id, loc) => {
  let path = Path.PIdent(id);
  let decl = Env.find_type(path, temp_env);
  switch (decl.type_manifest) {
  | None => ()
  | Some(ty) =>
    let params = List.map(_ => Ctype.newvar(), decl.type_params);
    try(Ctype.unify(env, Ctype.newconstr(path, params), ty)) {
    | Ctype.Unify(trace) =>
      raise(
        [@implicit_arity]
        Error(loc, [@implicit_arity] Type_clash(env, trace)),
      )
    };
  };
};

/* Translate one type declaration */

module StringSet =
  Set.Make({
    type t = string;
    let compare = (x: t, y) => compare(x, y);
  });

let make_params = (env, params) => {
  let make_param = sty =>
    try(transl_type_param(env, sty)) {
    | Already_bound =>
      raise([@implicit_arity] Error(sty.ptyp_loc, Repeated_parameter))
    };

  List.map(make_param, params);
};

let transl_labels = (env, closed, lbls) => {
  assert(lbls != []);
  let all_labels = ref(StringSet.empty);
  List.iter(
    ({pld_name: {txt: name, loc}}) => {
      let name = Identifier.string_of_ident(name);
      if (StringSet.mem(name, all_labels^)) {
        raise([@implicit_arity] Error(loc, Duplicate_label(name)));
      };
      all_labels := StringSet.add(name, all_labels^);
    },
    lbls,
  );
  let mk = ({pld_name: name, pld_type: arg, pld_mutable: mut, pld_loc: loc}) => {
    /* Builtin_attributes.warning_scope attrs
       (fun () -> */
    let arg = Ast_helper.Typ.force_poly(arg);
    let cty = transl_simple_type(env, closed, arg);
    let mut = mut == Mutable;
    {
      rf_name: Ident.create_persistent(Identifier.last(name.txt)),
      rf_type: cty,
      rf_mutable: mut,
      rf_loc: loc,
    };
  };
  /* ) */

  let lbls = List.map(mk, lbls);
  let lbls' =
    List.map(
      rf => {
        let ty = rf.rf_type.ctyp_type;
        let ty =
          switch (ty.desc) {
          | [@implicit_arity] TTyPoly(t, []) => t
          | _ => ty
          };
        {
          Types.rf_name: rf.rf_name,
          rf_type: ty,
          rf_mutable: rf.rf_mutable,
          rf_loc: rf.rf_loc,
        };
      },
      lbls,
    );
  (lbls, lbls');
};

let transl_constructor_arguments = (env, closed) =>
  fun
  | PConstrTuple(l) => {
      let l = List.map(transl_simple_type(env, closed), l);
      (Types.TConstrTuple(List.map(t => t.ctyp_type, l)), TConstrTuple(l));
    }
  | PConstrSingleton => (Types.TConstrSingleton, TConstrSingleton);

let make_constructor = (env, type_path, type_params, sargs) => {
  let (args, targs) = transl_constructor_arguments(env, true, sargs);

  (targs, None, args, None, type_params);
};

/* Check that the variable [id] is present in the [univ] list. */
let check_type_var = (loc, univ, id) => {
  let f = t => Btype.repr(t).id == id;
  if (!List.exists(f, univ)) {
    raise([@implicit_arity] Error(loc, Wrong_unboxed_type_float));
  };
};

let transl_declaration = (env, sdecl, id) => {
  /* Bind type parameters */
  reset_type_variables();
  Ctype.begin_def();
  let tparams = make_params(env, sdecl.pdata_params);
  let params = List.map(cty => cty.ctyp_type, tparams);
  /* [constraints]
        let cstrs = List.map
       (fun (sty, sty', loc) ->
         transl_simple_type env false sty,
         transl_simple_type env false sty', loc)
       sdecl.ptype_cstrs
     in*/
  /*let raw_status = get_unboxed_from_attributes sdecl in
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
    let unbox = unboxed_status.unboxed in*/
  let (tkind, kind) =
    switch (sdecl.pdata_kind) {
    | PDataVariant(scstrs) =>
      assert(scstrs != []);
      let all_constrs = ref(StringSet.empty);
      List.iter(
        ({pcd_name: {txt: name}}) => {
          if (StringSet.mem(name, all_constrs^)) {
            raise(
              [@implicit_arity]
              Error(sdecl.pdata_loc, Duplicate_constructor(name)),
            );
          };
          all_constrs := StringSet.add(name, all_constrs^);
        },
        scstrs,
      );
      if (List.length(
            List.filter(cd => cd.pcd_args != PConstrSingleton, scstrs),
          )
          > Config.max_tag
          + 1) {
        raise(
          [@implicit_arity] Error(sdecl.pdata_loc, Too_many_constructors),
        );
      };
      let make_cstr = scstr => {
        let name = Ident.create(scstr.pcd_name.txt);
        let (targs, tret_type, args, ret_type, cstr_params) =
          make_constructor(env, Path.PIdent(id), params, scstr.pcd_args);

        /*if Config.flat_float_array && unbox then begin
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
          end;*/
        let tcstr = {
          cd_id: name,
          cd_name: scstr.pcd_name,
          cd_args: targs,
          cd_res: tret_type,
          cd_loc: scstr.pcd_loc,
        };

        let cstr = {
          Types.cd_id: name,
          cd_args: args,
          cd_res: ret_type,
          cd_loc: scstr.pcd_loc,
        };

        (tcstr, cstr);
      };

      let make_cstr = scstr =>
        /*Builtin_attributes.warning_scope scstr.pcd_attributes
          (fun () -> make_cstr scstr)*/
        make_cstr(scstr);

      let (tcstrs, cstrs) = List.split(List.map(make_cstr, scstrs));
      (TDataVariant(tcstrs), Types.TDataVariant(cstrs));
    | PDataRecord(lbls) =>
      let (lbls, lbls') = transl_labels(env, true, lbls);
      (TDataRecord(lbls), Types.TDataRecord(lbls'));
    };

  let (tman, man) = (None, None); /*match sdecl.ptype_manifest with
        None -> None, None
      | Some sty ->
        let no_row = not (is_fixed_type sdecl) in
        let cty = transl_simple_type env no_row sty in
        Some cty, Some cty.ctyp_type
    in*/

  let decl = {
    type_params: params,
    type_arity: List.length(params),
    type_kind: kind,
    type_manifest: man,
    type_newtype_level: None,
    type_loc: sdecl.pdata_loc,
    type_path: PIdent(id),
    type_immediate: false,
  };

  /* Check constraints */
  /*List.iter
    (fun (cty, cty', loc) ->
      let ty = cty.ctyp_type in
      let ty' = cty'.ctyp_type in
      try Ctype.unify env ty ty' with Ctype.Unify tr ->
        raise(Error(loc, Inconsistent_constraint (env, tr))))
    cstrs;*/
  Ctype.end_def();
  /* Add abstract row */
  /*if is_fixed_type sdecl then begin
      let p =
        try Env.lookup_type (Identifier.IdentNmae(Ident.name id ^ "#row")) env
        with Not_found -> assert false in
      set_fixed_row env sdecl.ptype_loc p decl
    end;*/
  /* Check for cyclic abbreviations */
  switch (decl.type_manifest) {
  | None => ()
  | Some(ty) =>
    if (Ctype.cyclic_abbrev(env, id, ty)) {
      raise(
        [@implicit_arity]
        Error(sdecl.pdata_loc, Recursive_abbrev(sdecl.pdata_name.txt)),
      );
    }
  };
  {
    data_id: id,
    data_name: sdecl.pdata_name,
    data_params: tparams,
    data_type: decl,
    data_loc: sdecl.pdata_loc,
    data_kind: tkind,
  };
};

/* Generalize a type declaration */

let generalize_decl = decl => {
  List.iter(Ctype.generalize, decl.type_params);
  Btype.iter_type_expr_kind(Ctype.generalize, decl.type_kind);
  switch (decl.type_manifest) {
  | None => ()
  | Some(ty) => Ctype.generalize(ty)
  };
};

/* Check that all constraints are enforced */

module TypeSet = Btype.TypeSet;
module TypeMap = Btype.TypeMap;

/* Check that recursion is well-founded */

let check_well_founded = (env, loc, path, to_check, ty) => {
  let visited = ref(TypeMap.empty);
  let rec check = (ty0, parents, ty) => {
    let ty = Btype.repr(ty);
    if (TypeSet.mem(ty, parents)) {
      /*Format.eprintf "@[%a@]@." Printtyp.raw_type_expr ty;*/
      if (switch (ty0.desc) {
          | [@implicit_arity] TTyConstr(p, _, _) => Path.same(p, path)
          | _ => false
          }) {
        raise(
          [@implicit_arity] Error(loc, Recursive_abbrev(Path.name(path))),
        );
      } else {
        raise(
          [@implicit_arity]
          Error(loc, [@implicit_arity] Cycle_in_def(Path.name(path), ty0)),
        );
      };
    };
    let (fini, parents) =
      try({
        let prev = TypeMap.find(ty, visited^);
        if (TypeSet.subset(parents, prev)) {
          (true, parents);
        } else {
          (false, TypeSet.union(parents, prev));
        };
      }) {
      | Not_found => (false, parents)
      };

    if (fini) {
      ();
    } else {
      let rec_ok =
        switch (ty.desc) {
        | [@implicit_arity] TTyConstr(p, _, _) =>
          Grain_utils.Config.recursive_types^ && Ctype.is_contractive(env, p)
        | _ => Grain_utils.Config.recursive_types^
        };

      let visited' = TypeMap.add(ty, parents, visited^);
      let arg_exn =
        try(
          {
            visited := visited';
            let parents =
              if (rec_ok) {
                TypeSet.empty;
              } else {
                TypeSet.add(ty, parents);
              };
            Btype.iter_type_expr(check(ty0, parents), ty);
            None;
          }
        ) {
        | e =>
          visited := visited';
          Some(e);
        };

      switch (ty.desc) {
      | [@implicit_arity] TTyConstr(p, _, _)
          when arg_exn != None || to_check(p) =>
        if (to_check(p)) {
          Option.iter(raise, arg_exn);
        } else {
          Btype.iter_type_expr(check(ty0, TypeSet.empty), ty);
        };
        try({
          let ty' = Ctype.try_expand_once_opt(env, ty);
          let ty0 =
            if (TypeSet.is_empty(parents)) {
              ty;
            } else {
              ty0;
            };
          check(ty0, TypeSet.add(ty, parents), ty');
        }) {
        | Ctype.Cannot_expand => Option.iter(raise, arg_exn)
        };
      | _ => Option.iter(raise, arg_exn)
      };
    };
  };

  let snap = Btype.snapshot();
  /*try Ctype.wrap_trace_gadt_instances env (check ty TypeSet.empty) ty*/
  try(check(ty, TypeSet.empty, ty)) {
  | Ctype.Unify(_) =>
    /* Will be detected by check_recursion */
    Btype.backtrack(snap)
  };
};

let check_well_founded_manifest = (env, loc, path, decl) =>
  if (decl.type_manifest == None) {
    ();
  } else {
    let args = List.map(_ => Ctype.newvar(), decl.type_params);
    check_well_founded(
      env,
      loc,
      path,
      Path.same(path),
      Ctype.newconstr(path, args),
    );
  };

let check_well_founded_decl = (env, loc, path, decl, to_check) => {
  open Btype;
  let it = {
    ...type_iterators,
    it_type_expr: _ => check_well_founded(env, loc, path, to_check),
  };
  it.it_type_declaration(it, Ctype.instance_declaration(decl));
};

/* Check for ill-defined abbrevs */

let check_recursion = (env, loc, path, decl, to_check) =>
  /* to_check is true for potentially mutually recursive paths.
     (path, decl) is the type declaration to be checked. */
  if (decl.type_params == []) {
    ();
  } else {
    let visited = ref([]);

    let rec check_regular = (cpath, args, prev_exp, ty) => {
      let ty = Ctype.repr(ty);
      if (!List.memq(ty, visited^)) {
        visited := [ty, ...visited^];
        switch (ty.desc) {
        | [@implicit_arity] TTyConstr(path', args', _) =>
          if (Path.same(path, path')) {
            if (!Ctype.equal(env, false, args, args')) {
              raise(
                [@implicit_arity]
                Error(
                  loc,
                  [@implicit_arity]
                  Parameters_differ(cpath, ty, Ctype.newconstr(path, args)),
                ),
              );
            };
          } else if
            /* Attempt to expand a type abbreviation if:
               1- [to_check path'] holds
                  (otherwise the expansion cannot involve [path]);
               2- we haven't expanded this type constructor before
                  (otherwise we could loop if [path'] is itself
                  a non-regular abbreviation). */
            (to_check(path') && !List.mem(path', prev_exp)) {
            /* Attempt expansion */
            try({
              let (params0, body0, _) = Env.find_type_expansion(path', env);
              let (params, body) =
                Ctype.instance_parameterized_type(params0, body0);
              try(List.iter2(Ctype.unify(env), params, args')) {
              | Ctype.Unify(_) =>
                raise(
                  [@implicit_arity]
                  Error(
                    loc,
                    [@implicit_arity]
                    Constraint_failed(ty, Ctype.newconstr(path', params0)),
                  ),
                )
              };
              check_regular(path', args, [path', ...prev_exp], body);
            }) {
            | Not_found => ()
            };
          };
          List.iter(check_regular(cpath, args, prev_exp), args');
        | [@implicit_arity] TTyPoly(ty, tl) =>
          let (_, ty) = Ctype.instance_poly(~keep_names=true, false, tl, ty);
          check_regular(cpath, args, prev_exp, ty);
        | _ => Btype.iter_type_expr(check_regular(cpath, args, prev_exp), ty)
        };
      };
    };

    Option.iter(
      body => {
        let (args, body) =
          Ctype.instance_parameterized_type(
            ~keep_names=true,
            decl.type_params,
            body,
          );
        check_regular(path, args, [], body);
      },
      decl.type_manifest,
    );
  };

let check_abbrev_recursion = (env, id_loc_list, to_check, tdecl) => {
  let decl = tdecl.data_type;
  let id = tdecl.data_id;
  check_recursion(
    env,
    List.assoc(id, id_loc_list),
    Path.PIdent(id),
    decl,
    to_check,
  );
};

/* Check multiple declarations of labels/constructors */

let check_duplicates = sdecl_list => {
  let labels = Hashtbl.create(7)
  and constrs = Hashtbl.create(7);
  List.iter(
    sdecl =>
      switch (sdecl.pdata_kind) {
      | PDataVariant(cl) =>
        List.iter(
          pcd =>
            try({
              let name' = Hashtbl.find(constrs, pcd.pcd_name.txt);
              ignore(name');
            }) {
            /*Location.prerr_warning pcd.pcd_loc
              (Warnings.Duplicate_definitions
                 ("constructor", pcd.pcd_name.txt, name',
                  sdecl.ptype_name.txt))*/
            | Not_found =>
              Hashtbl.add(constrs, pcd.pcd_name.txt, sdecl.pdata_name.txt)
            },
          cl,
        )
      | PDataRecord(ll) =>
        List.iter(
          pld =>
            try({
              let name' =
                Hashtbl.find(labels, Identifier.last(pld.pld_name.txt));
              ignore(name');
            }) {
            /*Location.prerr_warning pld.pcd_loc
              (Warnings.Duplicate_definitions
                 ("constructor", pld.pcd_name.txt, name',
                  sdecl.ptype_name.txt))*/
            | Not_found =>
              Hashtbl.add(
                labels,
                Identifier.last(pld.pld_name.txt),
                sdecl.pdata_name.txt,
              )
            },
          ll,
        )
      },
    sdecl_list,
  );
};

/* Force recursion to go through id for private types*/
let name_recursion = (sdecl, id, decl) =>
  switch (decl) {
  | _ => decl
  };

/* Translate a set of type declarations, mutually recursive or not */
let transl_data_decl = (env, rec_flag, sdecl_list) => {
  /*(* Add dummy types for fixed rows *)
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
    in*/

  /* Create identifiers. */
  let id_list =
    List.map(sdecl => Ident.create(sdecl.pdata_name.txt), sdecl_list);

  /*
      Since we've introduced fresh idents, make sure the definition
      level is at least the binding time of these events. Otherwise,
      passing one of the recursively-defined type constrs as argument
      to an abbreviation may fail.
   */
  Ctype.init_def(Ident.current_time());
  Ctype.begin_def();
  /* Enter types. */
  let temp_env =
    List.fold_left2(enter_type(rec_flag), env, sdecl_list, id_list);
  /* Translate each declaration. */
  let current_slot = ref(None);
  let warn_unused = Warnings.is_active(Warnings.Unused_type_declaration(""));
  let id_slots = id =>
    switch (rec_flag) {
    | Asttypes.Recursive when warn_unused =>
      /* See typecore.ml for a description of the algorithm used
         to detect unused declarations in a set of recursive definitions. */
      let slot = ref([]);
      /*let td = Env.find_type (Path.PIdent id) temp_env in
        let name = Ident.name id in*/
      /*Env.set_type_used_callback
        name td
        (fun old_callback ->
           match !current_slot with
           | Some slot -> slot := (name, td) :: !slot
           | None ->
               List.iter (fun (name, d) -> Env.mark_type_used env name d)
                 (get_ref slot);
               old_callback ()
        );*/
      (id, Some(slot));
    | Asttypes.Recursive
    | Asttypes.Nonrecursive => (id, None)
    };

  let transl_declaration = (name_sdecl, (id, slot)) => {
    current_slot := slot;
    transl_declaration(temp_env, name_sdecl, id);
  };
  /*Builtin_attributes.warning_scope
    name_sdecl.ptype_attributes
    (fun () -> transl_declaration temp_env name_sdecl id)*/

  let tdecls =
    List.map2(transl_declaration, sdecl_list, List.map(id_slots, id_list));
  let decls = List.map(tdecl => (tdecl.data_id, tdecl.data_type), tdecls);
  current_slot := None;
  /* Check for duplicates */
  check_duplicates(sdecl_list);
  /* Build the final env. */
  let newenv =
    List.fold_right(
      ((id, decl), env) => Env.add_type(~check=true, id, decl, env),
      decls,
      env,
    );

  /* Update stubs */
  switch (rec_flag) {
  | Asttypes.Nonrecursive => ()
  | Asttypes.Recursive =>
    List.iter2(
      (id, sdecl) => update_type(temp_env, newenv, id, sdecl.pdata_loc),
      id_list,
      sdecl_list,
    )
  };
  /* Generalize type declarations. */
  Ctype.end_def();
  List.iter(((_, decl)) => generalize_decl(decl), decls);
  /* Check for ill-formed abbrevs */
  let id_loc_list =
    List.map2((id, sdecl) => (id, sdecl.pdata_loc), id_list, sdecl_list);

  List.iter(
    ((id, decl)) =>
      check_well_founded_manifest(
        newenv,
        List.assoc(id, id_loc_list),
        Path.PIdent(id),
        decl,
      ),
    decls,
  );
  let to_check =
    fun
    | Path.PIdent(id) => List.mem_assoc(id, id_loc_list)
    | _ => false;
  List.iter(
    ((id, decl)) =>
      check_well_founded_decl(
        newenv,
        List.assoc(id, id_loc_list),
        Path.PIdent(id),
        decl,
        to_check,
      ),
    decls,
  );
  List.iter(check_abbrev_recursion(newenv, id_loc_list, to_check), tdecls);
  /* Check that all type variables are closed */
  List.iter2(
    (sdecl, tdecl) => {
      let decl = tdecl.data_type;
      switch (Ctype.closed_type_decl(decl)) {
      | Some(ty) =>
        raise(
          [@implicit_arity]
          Error(
            sdecl.pdata_loc,
            [@implicit_arity] Unbound_type_var(ty, decl),
          ),
        )
      | None => ()
      };
    },
    sdecl_list,
    tdecls,
  );
  /* Check that constraints are enforced */
  /*List.iter2 (check_constraints newenv) sdecl_list decls;*/
  /* Name recursion */
  let decls =
    List.map2(
      (sdecl, (id, decl)) => (id, name_recursion(sdecl, id, decl)),
      sdecl_list,
      decls,
    );

  /* Add variances to the environment */
  /*let required =
      List.map
        (fun sdecl ->
           add_injectivity (List.map snd sdecl.pdata_params),
           sdecl.pdata_loc
        )
        sdecl_list
    in*/
  let (final_decls, final_env) = (
    decls,
    List.fold_right(
      ((id, decl), env) => Env.add_type(~check=true, id, decl, env),
      decls,
      env,
    ),
  );
  /*compute_properties_fixpoint env decls required
    (List.map init_variance decls)
    (List.map (fun _ -> false) decls)*/

  /* FIXME: Check re-exportation */
  /*List.iter2 (check_abbrev final_env) sdecl_list final_decls;*/
  /* Keep original declaration */
  let final_decls =
    List.map2(
      (tdecl, (_id2, decl)) => {...tdecl, data_type: decl},
      tdecls,
      final_decls,
    );

  /* Done */
  (final_decls, final_env);
};

type native_repr_attribute =
  | Native_repr_attr_absent
  | Native_repr_attr_present(native_repr_kind);

let get_native_repr_attribute = (attrs, ~global_repr) =>
  /*Attr_helper.get_no_payload_attribute ["unboxed"; "ocaml.unboxed"]  attrs,
    Attr_helper.get_no_payload_attribute ["untagged"; "ocaml.untagged"] attrs,*/
  switch (None, None, global_repr) {
  | (None, None, None) => Native_repr_attr_absent
  | (None, None, Some(repr)) => Native_repr_attr_present(repr)
  | (Some(_), None, None) => Native_repr_attr_present(Unboxed)
  | (None, Some(_), None) => Native_repr_attr_present(Untagged)
  | (Some({Location.loc}), _, _)
  | (_, Some({Location.loc}), _) =>
    raise([@implicit_arity] Error(loc, Multiple_native_repr_attributes))
  };

let native_repr_of_type = (env, kind, ty) =>
  switch (kind, Ctype.expand_head_opt(env, ty).desc) {
  | (Untagged, [@implicit_arity] TTyConstr(path, _, _))
      when Path.same(path, Builtin_types.path_number) =>
    Some(Untagged_int)
  /*| Unboxed, TTyConstr (path, _, _) when Path.same path Predef.path_float ->
      Some Unboxed_float
    | Unboxed, TTyConstr (path, _, _) when Path.same path Predef.path_int32 ->
      Some (Unboxed_integer Pint32)
    | Unboxed, TTyConstr (path, _, _) when Path.same path Predef.path_int64 ->
      Some (Unboxed_integer Pint64)
    | Unboxed, TTyConstr (path, _, _) when Path.same path Predef.path_nativeint ->
      Some (Unboxed_integer Pnativeint)*/
  | _ => None
  };

/* Raises an error when [core_type] contains an [@unboxed] or [@untagged]
   attribute in a strict sub-term. */
let error_if_has_deep_native_repr_attributes = core_type => {
  open Ast_iterator;
  let this_iterator = {
    ...default_iterator,
    typ: (iterator, core_type) => {
      switch (
        get_native_repr_attribute(
          [] /*core_type.ptyp_attributes*/,
          ~global_repr=None,
        )
      ) {
      | Native_repr_attr_present(kind) =>
        raise(
          [@implicit_arity]
          Error(core_type.ptyp_loc, Deep_unbox_or_untag_attribute(kind)),
        )
      | Native_repr_attr_absent => ()
      };
      default_iterator.typ(iterator, core_type);
    },
  };

  List.iter(default_iterator.typ(this_iterator), core_type);
};

let make_native_repr = (env, core_type, ty, ~global_repr) => {
  error_if_has_deep_native_repr_attributes(core_type);
  switch (
    get_native_repr_attribute([] /*core_type.ptyp_attributes*/, ~global_repr)
  ) {
  | Native_repr_attr_absent => Same_as_ocaml_repr
  | Native_repr_attr_present(kind) =>
    List.fold_left(
      (_, ty) =>
        switch (native_repr_of_type(env, kind, ty)) {
        | None =>
          raise(
            [@implicit_arity]
            Error(Location.dummy_loc, Cannot_unbox_or_untag_type(kind)),
          )
        | Some(repr) => repr
        },
      Same_as_ocaml_repr,
      ty,
    )
  };
};

let rec parse_native_repr_attributes = (env, core_type, ty, ~global_repr) =>
  switch (
    core_type.ptyp_desc,
    Ctype.repr(ty).desc,
    get_native_repr_attribute(
      [] /*core_type.ptyp_attributes*/,
      ~global_repr=None,
    ),
  ) {
  | (PTyArrow(_), TTyArrow(_), Native_repr_attr_present(kind)) =>
    raise(
      [@implicit_arity]
      Error(core_type.ptyp_loc, Cannot_unbox_or_untag_type(kind)),
    )
  | (
      [@implicit_arity] PTyArrow(ct1, ct2),
      [@implicit_arity] TTyArrow(t1, t2, _),
      _,
    ) =>
    let repr_arg = make_native_repr(env, ct1, t1, ~global_repr);
    let (repr_args, repr_res) =
      parse_native_repr_attributes(env, ct2, t2, ~global_repr);

    ([repr_arg, ...repr_args], repr_res);
  | (PTyArrow(_), _, _)
  | (_, TTyArrow(_), _) => assert(false)
  | _ => ([], make_native_repr(env, [core_type], [ty], ~global_repr))
  };

/* Translate a value declaration */
let transl_value_decl = (env, loc, valdecl) => {
  let cty = Typetexp.transl_type_scheme(env, valdecl.pval_type);
  let ty = cty.ctyp_type;
  let v =
    switch (valdecl.pval_prim) {
    /*[] when Env.is_in_signature env ->
      { val_type = ty; val_kind = TValReg; Types.val_loc = loc;
        (*val_attributes = valdecl.pval_attributes*) }*/
    /* | [] ->
       raise (Error(valdecl.pval_loc, Val_in_structure)) */
    | [prim] => {
        val_type: ty,
        val_kind: TValPrim(prim),
        Types.val_loc: loc,
        val_fullpath: Path.PIdent(Ident.create("<bogus>")) /*val_attributes = valdecl.pval_attributes*/,
        val_mutable: false,
      }
    | _ => {
        val_type: ty,
        val_kind: TValReg,
        Types.val_loc: loc,
        val_fullpath: Path.PIdent(Ident.create("<bogus>")) /*val_attributes = valdecl.pval_attributes*/,
        val_mutable: false,
      }
    };

  let (id, newenv) = {
    let name =
      Option.value(~default=valdecl.pval_name, valdecl.pval_name_alias).txt;
    Env.enter_value(name, v, env);
  };
  /*~check:(fun s -> Warnings.Unused_value_declaration s)*/

  let desc = {
    tvd_id: id,
    tvd_mod: valdecl.pval_mod,
    tvd_name: valdecl.pval_name,
    tvd_desc: cty,
    tvd_val: v,
    tvd_prim: valdecl.pval_prim,
    tvd_loc: valdecl.pval_loc,
  };

  (desc, newenv);
};

/*let transl_value_decl env loc valdecl =
  Builtin_attributes.warning_scope valdecl.pval_attributes
    (fun () -> transl_value_decl env loc valdecl)*/

/**** Error report ****/

open Format;

let explain_unbound_gen = (ppf, tv, tl, typ, kwd, pr) =>
  try({
    let ti = List.find(ti => Ctype.deep_occur(tv, typ(ti)), tl);
    let ty0 =
      /* Hack to force aliasing when needed */
      Btype.newgenty(TTyTuple([tv])) /*(Tobject(tv, ref None))*/;
    Printtyp.reset_and_mark_loops_list([typ(ti), ty0]);
    fprintf(
      ppf,
      ".@.@[<hov2>In %s@ %a@;<1 -2>the variable %a is unbound@]",
      kwd,
      pr,
      ti,
      Printtyp.type_expr,
      tv,
    );
  }) {
  | Not_found => ()
  };

let explain_unbound = (ppf, tv, tl, typ, kwd, lab) =>
  explain_unbound_gen(ppf, tv, tl, typ, kwd, (ppf, ti) =>
    fprintf(ppf, "%s%a", lab(ti), Printtyp.type_expr, typ(ti))
  );

let explain_unbound_single = (ppf, tv, ty) => {
  let trivial = ty =>
    explain_unbound(ppf, tv, [ty], t => t, "type", _ => "");
  switch (Ctype.repr(ty).desc) {
  /*Tobject(fi,_) ->
        let (tl, rv) = Ctype.flatten_fields fi in
        if rv == tv then trivial ty else
        explain_unbound ppf tv tl (fun (_,_,t) -> t)
          "method" (fun (lab,_,_) -> lab ^ ": ")
    | Tvariant row ->
        let row = Btype.row_repr row in
        if row.row_more == tv then trivial ty else
        explain_unbound ppf tv row.row_fields
          (fun (_l,f) -> match Btype.row_field_repr f with
            Rpresent (Some t) -> t
          | Reither (_,[t],_,_) -> t
          | Reither (_,tl,_,_) -> Btype.newgenty (Ttuple tl)
          | _ -> Btype.newgenty (Ttuple[]))
          "case" (fun (lab,_) -> "`" ^ lab ^ " of ")*/
  | _ => trivial(ty)
  };
};

let tys_of_constr_args =
  fun
  | Types.TConstrTuple(tl) => tl
  | Types.TConstrSingleton => [];
/*| Types.Cstr_record lbls -> List.map (fun l -> l.Types.ld_type) lbls*/

let report_error = ppf =>
  fun
  | Repeated_parameter =>
    fprintf(ppf, "A type parameter occurs several times")
  | Duplicate_constructor(s) =>
    fprintf(ppf, "Two constructors are named %s", s)
  | Too_many_constructors =>
    fprintf(
      ppf,
      "@[Too many non-constant constructors@ -- maximum is %i %s@]",
      Config.max_tag + 1,
      "non-constant constructors",
    )
  | Duplicate_label(s) => fprintf(ppf, "Two labels are named %s", s)
  | Recursive_abbrev(s) =>
    fprintf(ppf, "The type abbreviation %s is cyclic", s)
  | [@implicit_arity] Cycle_in_def(s, ty) => {
      Printtyp.reset_and_mark_loops(ty);
      fprintf(
        ppf,
        "@[<v>The definition of %s contains a cycle:@ %a@]",
        s,
        Printtyp.type_expr,
        ty,
      );
    }
  | [@implicit_arity] Definition_mismatch(ty, errs) => {
      Printtyp.reset_and_mark_loops(ty);
      fprintf(
        ppf,
        "@[<v>@[<hov>%s@ %s@;<1 2>%a@]%a@]",
        "This variant or record definition",
        "does not match that of type",
        Printtyp.type_expr,
        ty,
        Includecore.report_type_mismatch(
          "the original",
          "this",
          "definition",
        ),
        errs,
      );
    }
  | [@implicit_arity] Constraint_failed(ty, ty') => {
      Printtyp.reset_and_mark_loops(ty);
      Printtyp.mark_loops(ty');
      fprintf(
        ppf,
        "@[%s@ @[<hv>Type@ %a@ should be an instance of@ %a@]@]",
        "Constraints are not satisfied in this type.",
        Printtyp.type_expr,
        ty,
        Printtyp.type_expr,
        ty',
      );
    }
  | [@implicit_arity] Parameters_differ(path, ty, ty') => {
      Printtyp.reset_and_mark_loops(ty);
      Printtyp.mark_loops(ty');
      fprintf(
        ppf,
        "@[<hv>In the definition of %s, type@ %a@ should be@ %a@]",
        Path.name(path),
        Printtyp.type_expr,
        ty,
        Printtyp.type_expr,
        ty',
      );
    }
  | [@implicit_arity] Inconsistent_constraint(env, trace) => {
      fprintf(ppf, "The type constraints are not consistent.@.");
      Printtyp.report_unification_error(
        ppf,
        env,
        trace,
        ppf => fprintf(ppf, "Type"),
        ppf => fprintf(ppf, "is not compatible with type"),
      );
    }
  | [@implicit_arity] Type_clash(env, trace) =>
    Printtyp.report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf => fprintf(ppf, "This type constructor expands to type"),
      fun
      | ppf => fprintf(ppf, "but is used here with type"),
    )
  | Null_arity_external =>
    fprintf(ppf, "External identifiers must be functions")
  | Missing_native_external =>
    fprintf(
      ppf,
      "@[<hv>An external function with more than 5 arguments requires a second stub function@ for native-code compilation@]",
    )
  | [@implicit_arity] Unbound_type_var(ty, decl) => {
      fprintf(ppf, "A type variable is unbound in this type declaration");
      let ty = Ctype.repr(ty);
      switch (decl.type_kind, decl.type_manifest) {
      /*| Type_variant tl, _ ->
            explain_unbound_gen ppf ty tl (fun c ->
                let tl = tys_of_constr_args c.Types.cd_args in
                Btype.newgenty (Ttuple tl)
              )
              "case" (fun ppf c ->
                  fprintf ppf
                    "%s of %a" (Ident.name c.Types.cd_id)
                    Printtyp.constructor_arguments c.Types.cd_args)
        | Type_record (tl, _), _ ->
            explain_unbound ppf ty tl (fun l -> l.Types.ld_type)
              "field" (fun l -> Ident.name l.Types.ld_id ^ ": ")*/
      | (TDataAbstract, Some(ty')) => explain_unbound_single(ppf, ty, ty')
      | _ => ()
      };
    }
  /*| Unbound_type_var_ext (ty, ext) ->
    fprintf ppf "A type variable is unbound in this extension constructor";
    let args = tys_of_constr_args ext.ext_args in
    explain_unbound ppf ty args (fun c -> c) "type" (fun _ -> "")*/
  | Cannot_extend_private_type(path) =>
    fprintf(
      ppf,
      "@[%s@ %a@]",
      "Cannot extend private type definition",
      Printtyp.path,
      path,
    )
  | Not_extensible_type(path) =>
    fprintf(
      ppf,
      "@[%s@ %a@ %s@]",
      "Type definition",
      Printtyp.path,
      path,
      "is not extensible",
    )
  | [@implicit_arity] Extension_mismatch(path, errs) =>
    fprintf(
      ppf,
      "@[<v>@[<hov>%s@ %s@;<1 2>%s@]%a@]",
      "This extension",
      "does not match the definition of type",
      Path.name(path),
      Includecore.report_type_mismatch(
        "the type",
        "this extension",
        "definition",
      ),
      errs,
    )
  | [@implicit_arity] Rebind_wrong_type(lid, env, trace) =>
    Printtyp.report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf =>
        fprintf(
          ppf,
          "The constructor %a@ has type",
          Printtyp.identifier,
          lid,
        ),
      fun
      | ppf => fprintf(ppf, "but was expected to be of type"),
    )
  | [@implicit_arity] Rebind_mismatch(lid, p, p') =>
    fprintf(
      ppf,
      "@[%s@ %a@ %s@ %s@ %s@ %s@ %s@]",
      "The constructor",
      Printtyp.identifier,
      lid,
      "extends type",
      Path.name(p),
      "whose declaration does not match",
      "the declaration of type",
      Path.name(p'),
    )
  | Rebind_private(lid) =>
    fprintf(
      ppf,
      "@[%s@ %a@ %s@]",
      "The constructor",
      Printtyp.identifier,
      lid,
      "is private",
    )
  | [@implicit_arity] Bad_variance(n, v1, v2) => {
      let variance = ((p, n, i)) => {
        let inj = if (i) {"injective "} else {""};
        switch (p, n) {
        | (true, true) => inj ++ "invariant"
        | (true, false) => inj ++ "covariant"
        | (false, true) => inj ++ "contravariant"
        | (false, false) =>
          if (inj == "") {
            "unrestricted";
          } else {
            inj;
          }
        };
      };

      let suffix = n => {
        let teen = n mod 100 / 10 == 1;
        switch (n mod 10) {
        | 1 when !teen => "st"
        | 2 when !teen => "nd"
        | 3 when !teen => "rd"
        | _ => "th"
        };
      };

      if (n == (-1)) {
        fprintf(
          ppf,
          "@[%s@ %s@ It",
          "In this definition, a type variable has a variance that",
          "is not reflected by its occurrence in type parameters.",
        );
      } else if (n == (-2)) {
        fprintf(
          ppf,
          "@[%s@ %s@]",
          "In this definition, a type variable cannot be deduced",
          "from the type parameters.",
        );
      } else if (n == (-3)) {
        fprintf(
          ppf,
          "@[%s@ %s@ It",
          "In this definition, a type variable has a variance that",
          "cannot be deduced from the type parameters.",
        );
      } else {
        fprintf(
          ppf,
          "@[%s@ %s@ The %d%s type parameter",
          "In this definition, expected parameter",
          "variances are not satisfied.",
          n,
          suffix(n),
        );
      };
      if (n != (-2)) {
        fprintf(
          ppf,
          " was expected to be %s,@ but it is %s.@]",
          variance(v2),
          variance(v1),
        );
      };
    }
  | Unavailable_type_constructor(p) =>
    fprintf(
      ppf,
      "The definition of type %a@ is unavailable",
      Printtyp.path,
      p,
    )
  | Bad_fixed_type(r) => fprintf(ppf, "This fixed type %s", r)
  | Varying_anonymous =>
    fprintf(
      ppf,
      "@[%s@ %s@ %s@]",
      "In this GADT definition,",
      "the variance of some parameter",
      "cannot be checked",
    )
  | Val_in_structure =>
    fprintf(ppf, "Value declarations are only allowed in signatures")
  | Multiple_native_repr_attributes =>
    fprintf(ppf, "Too many [@@unboxed]/[@@untagged] attributes")
  | Cannot_unbox_or_untag_type(Unboxed) =>
    fprintf(
      ppf,
      "Don't know how to unbox this type. Only float, int32, int64 and nativeint can be unboxed",
    )
  | Cannot_unbox_or_untag_type(Untagged) =>
    fprintf(
      ppf,
      "Don't know how to untag this type. Only int can be untagged",
    )
  | Deep_unbox_or_untag_attribute(kind) =>
    fprintf(
      ppf,
      "The attribute '%s' should be attached to a direct argument or result of the primitive, it should not occur deeply into its type",
      switch (kind) {
      | Unboxed => "@unboxed"
      | Untagged => "@untagged"
      },
    )
  | Bad_immediate_attribute =>
    fprintf(
      ppf,
      "@[%s@ %s@]",
      "Types marked with the immediate attribute must be",
      "non-pointer types like int or bool",
    )
  | Bad_unboxed_attribute(msg) =>
    fprintf(ppf, "@[This type cannot be unboxed because@ %s.@]", msg)
  | Wrong_unboxed_type_float =>
    fprintf(
      ppf,
      "@[This type cannot be unboxed because@ it might contain both float and non-float values.@ You should annotate it with [%@%@ocaml.boxed].@]",
    )
  | Boxed_and_unboxed =>
    fprintf(ppf, "@[A type cannot be boxed and unboxed at the same time.@]")
  | Nonrec_gadt =>
    fprintf(ppf, "@[GADT case syntax cannot be used in a 'nonrec' block.@]");

let () =
  Location.register_error_of_exn(
    fun
    | [@implicit_arity] Error(loc, err) =>
      Some(Location.error_of_printer(loc, report_error, err))
    | _ => None,
  );
