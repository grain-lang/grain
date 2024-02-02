/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Typechecking of type expressions for the core language */
open Grain_parsing;
open Grain_utils;
open Asttypes;
open Misc;
open Parsetree;
open Typedtree;
open Types;
open Ctype;

exception Already_bound;

type error =
  | Unbound_type_variable(string)
  | Unbound_type_constructor(Identifier.t)
  | Unbound_type_constructor_2(Path.t)
  | Type_arity_mismatch(Identifier.t, int, int)
  | Bound_type_variable(string)
  | Recursive_type
  | Unbound_row_variable(Identifier.t)
  | Type_mismatch(list((type_expr, type_expr)))
  | Alias_type_mismatch(list((type_expr, type_expr)))
  | Present_has_conjunction(string)
  | Present_has_no_type(string)
  | Constructor_mismatch(type_expr, type_expr)
  | Not_a_variant(type_expr)
  | Variant_tags(string, string)
  | Invalid_variable_name(string)
  | Cannot_quantify(string, type_expr)
  | Multiple_constraints_on_type(Identifier.t)
  | Method_mismatch(string, type_expr, type_expr)
  | Unbound_value(Identifier.t)
  | Unbound_value_in_module(Identifier.t, string)
  | Unbound_constructor(Identifier.t)
  | Unbound_exception(Identifier.t)
  | Unbound_label(Identifier.t)
  | Unbound_module(Identifier.t)
  | Unbound_class(Identifier.t)
  | Unbound_modtype(Identifier.t)
  | Unbound_cltype(Identifier.t)
  | Ill_typed_functor_application(
      Identifier.t,
      Identifier.t,
      option(list(Includemod.error)),
    )
  | Illegal_reference_to_recursive_module
  | Wrong_use_of_module(
      Identifier.t,
      [
        | `Structure_used_as_functor
        | `Abstract_used_as_functor
        | `Functor_used_as_structure
        | `Abstract_used_as_structure
        | `Generative_used_as_applicative
      ],
    )
  | Cannot_scrape_alias(Identifier.t, Path.t)
  | Opened_object(option(Path.t))
  | Not_an_object(type_expr);

exception Error(Location.t, Env.t, error);
exception Error_forward(Location.error);

type variable_context = (int, Tbl.t(string, type_expr));

/* Local definitions */

let instance_list = Ctype.instance_list(Env.empty);

/* Narrowing unbound identifier errors. */

let rec narrow_unbound_lid_error: 'a. (_, _, _, _) => 'a =
  (env, loc, lid, make_error) => {
    let check_module = mlid =>
      try(ignore(Env.lookup_module(~load=false, mlid, None, env))) {
      | Not_found =>
        narrow_unbound_lid_error(env, loc, mlid, lid => Unbound_module(lid))
      };

    let error = e => raise(Error(loc, env, e));
    switch (lid) {
    | Identifier.IdentName(_) => ()
    | Identifier.IdentExternal(mlid, id) =>
      check_module(mlid);
      error(Unbound_value_in_module(mlid, id.txt));
    /* let md = Env.find_module (Env.lookup_module ~load:true mlid None env) None env in
       begin match Env.scrape_alias env md.md_type with
       | TModIdent _ ->
          error (Wrong_use_of_module (mlid, `Abstract_used_as_structure))
       | TModSignature _ -> ()
       | TModAlias _ -> ()
       end */
    };
    error(make_error(lid));
  };

let find_component = (lookup: (~mark: _=?) => _, make_error, env, loc, lid) =>
  try(
    switch (lid) {
    | Identifier.IdentExternal(Identifier.IdentName({txt: "*predef*"}), s) =>
      lookup(Identifier.IdentName(s), Env.initial_env)
    | _ => lookup(lid, env)
    }
  ) {
  | Not_found =>
    // TODO(#1506): Replace this with a more specific exception as it can eat errors from compilation of submodules
    narrow_unbound_lid_error(env, loc, lid, make_error)
  };

let find_type = (env, loc, lid) => {
  let path =
    find_component(
      Env.lookup_type,
      lid => Unbound_type_constructor(lid),
      env,
      loc,
      lid,
    );

  let decl = Env.find_type(path, env);
  (path, decl);
};

let find_constructor =
  find_component(Env.lookup_constructor, lid => Unbound_constructor(lid));
let find_exception = (env, loc, lid) => {
  let cstr =
    find_component(
      Env.lookup_constructor,
      lid => Unbound_exception(lid),
      env,
      loc,
      lid,
    );
  switch (cstr.cstr_tag) {
  | CstrExtension(_, _, _, ext) => ext
  | _ => raise(Error(loc, env, Unbound_exception(lid)))
  };
};
let find_all_constructors =
  find_component(Env.lookup_all_constructors, lid =>
    Unbound_constructor(lid)
  );

let find_value = (env, loc, lid) => {
  Env.check_value_name(Identifier.last(lid), loc);
  let (path, decl) as r =
    find_component(
      Env.lookup_value,
      lid => Unbound_value(lid),
      env,
      loc,
      lid,
    );

  r;
};

let lookup_module = (~load=false, env, loc, lid, filepath) =>
  find_component(
    (~mark=?, lid, env) =>
      Env.lookup_module(~load, ~loc, ~mark?, lid, filepath, env),
    lid => Unbound_module(lid),
    env,
    loc,
    lid,
  );

let find_module = (env, loc, lid) => {
  let path = lookup_module(env, loc, lid, None);
  let decl = Env.find_module(path, None, env);
  /* No need to check for deprecated here, this is done in Env. */
  (path, decl);
};

let find_modtype = (env, loc, lid) => {
  let (path, decl) as r =
    find_component(
      (~mark=false, a, b) => Env.lookup_modtype(~loc, ~mark, a, b),
      lid => Unbound_modtype(lid),
      env,
      loc,
      lid,
    );

  r;
};

let unbound_label_error = (env, lid) =>
  narrow_unbound_lid_error(env, lid.loc, lid.txt, lid => Unbound_label(lid));

let unbound_constructor_error = (env, lid) =>
  narrow_unbound_lid_error(env, lid.loc, lid.txt, lid =>
    Unbound_constructor(lid)
  );

/* Support for first-class modules. */

let transl_modtype_identifier = ref(_ => assert(false));
let transl_modtype = ref(_ => assert(false));

/* Translation of type expressions */

let type_variables = ref(Tbl.empty: Tbl.t(string, type_expr));
let univars = ref([]: list((string, type_expr)));
let pre_univars = ref([]: list(type_expr));
let used_variables = ref(Tbl.empty: Tbl.t(string, (type_expr, Location.t)));

let reset_type_variables = () => {
  reset_global_level();
  Ctype.reset_reified_var_counter();
  type_variables := Tbl.empty;
};

let narrow = () => (increase_global_level(), type_variables^);

let widen = ((gl, tv)) => {
  restore_global_level(gl);
  type_variables := tv;
};

let strict_ident = c =>
  c == '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';

let validate_name =
  fun
  | None => None
  | Some(name) as s =>
    if (name != "" && strict_ident(name.[0])) {
      s;
    } else {
      None;
    };

let new_global_var = (~name=?, ()) =>
  new_global_var(~name=?validate_name(name), ());
let newvar = (~name=?, ()) => newvar(~name=?validate_name(name), ());

let type_variable = (loc, name) =>
  try(Tbl.find(name, type_variables^)) {
  | Not_found =>
    raise(Error(loc, Env.empty, Unbound_type_variable("'" ++ name)))
  };

let transl_type_param = (env, styp) => {
  let loc = styp.ptyp_loc;
  switch (styp.ptyp_desc) {
  | PTyAny =>
    let ty = new_global_var(~name="_", ());
    {ctyp_desc: TTyAny, ctyp_type: ty, ctyp_env: env, ctyp_loc: loc};
  | PTyVar(name) =>
    let ty =
      try(
        {
          if (name != "" && name.[0] == '_') {
            raise(
              Error(loc, Env.empty, Invalid_variable_name("'" ++ name)),
            );
          };
          ignore(Tbl.find(name, type_variables^));
          raise(Already_bound);
        }
      ) {
      | Not_found =>
        let v = new_global_var(~name, ());
        type_variables := Tbl.add(name, v, type_variables^);
        v;
      };

    {ctyp_desc: TTyVar(name), ctyp_type: ty, ctyp_env: env, ctyp_loc: loc};
  | _ => assert(false)
  };
};

let new_pre_univar = (~name=?, ()) => {
  let v = newvar(~name?, ());
  pre_univars := [v, ...pre_univars^];
  v;
};

let rec swap_list =
  fun
  | [x, y, ...l] => [y, x, ...swap_list(l)]
  | l => l;

type policy =
  | Fixed
  | Extensible
  | Univars;

let rec transl_type = (env, policy, styp) =>
  transl_type_aux(env, policy, styp)

and transl_type_aux = (env, policy, styp) => {
  let loc = styp.ptyp_loc;
  let ctyp = (ctyp_desc, ctyp_type) => {
    ctyp_desc,
    ctyp_type,
    ctyp_env: env,
    ctyp_loc: loc,
  };

  switch (styp.ptyp_desc) {
  | PTyAny =>
    let ty =
      if (policy == Univars) {
        new_pre_univar();
      } else if (policy == Fixed) {
        raise(Error(styp.ptyp_loc, env, Unbound_type_variable("_")));
      } else {
        newvar();
      };

    ctyp(TTyAny, ty);
  | PTyVar(name) =>
    let ty = {
      if (name != "" && name.[0] == '_') {
        raise(
          Error(styp.ptyp_loc, env, Invalid_variable_name("'" ++ name)),
        );
      };
      try(instance(env, List.assoc(name, univars^))) {
      | Not_found =>
        try(instance(env, fst(Tbl.find(name, used_variables^)))) {
        | Not_found =>
          let v =
            if (policy == Univars) {
              new_pre_univar(~name, ());
            } else {
              newvar(~name, ());
            };

          used_variables :=
            Tbl.add(name, (v, styp.ptyp_loc), used_variables^);
          v;
        }
      };
    };

    ctyp(TTyVar(name), ty);
  | PTyArrow(stl, st2) =>
    let ctyl =
      List.map(
        st => {
          let ty = transl_type(env, policy, st.ptyp_arg_type);
          (st.ptyp_arg_label, ty);
        },
        stl,
      );
    let tyl =
      List.map(
        ((l, ty)) => {
          let ty =
            if (Btype.is_optional(l)) {
              newty(
                TTyConstr(
                  Builtin_types.path_option,
                  [ty.ctyp_type],
                  ref(TMemNil),
                ),
              );
            } else {
              ty.ctyp_type;
            };
          (l, ty);
        },
        ctyl,
      );
    let cty2 = transl_type(env, policy, st2);
    let ty = newty(TTyArrow(tyl, cty2.ctyp_type, TComOk));
    ctyp(TTyArrow(ctyl, cty2), ty);
  | PTyTuple(stl) =>
    assert(List.length(stl) >= 1);
    let ctys = List.map(transl_type(env, policy), stl);
    let ty = newty(TTyTuple(List.map(ctyp => ctyp.ctyp_type, ctys)));
    ctyp(TTyTuple(ctys), ty);
  | PTyConstr(lid, stl) =>
    let (path, decl) = find_type(env, lid.loc, lid.txt);
    let stl =
      switch (stl) {
      | [{ptyp_desc: PTyAny} as t] when decl.type_arity > 1 =>
        List.map(_ => t, decl.type_params)
      | _ => stl
      };

    if (List.length(stl) != decl.type_arity) {
      raise(
        Error(
          styp.ptyp_loc,
          env,
          Type_arity_mismatch(lid.txt, decl.type_arity, List.length(stl)),
        ),
      );
    };
    let args = List.map(transl_type(env, policy), stl);
    let params = instance_list(decl.type_params);
    let unify_param =
      switch (decl.type_manifest) {
      | None => unify_var
      | Some(ty) =>
        if (repr(ty).level == Btype.generic_level) {
          unify_var;
        } else {
          unify;
        }
      };

    List.iter2(
      ((sty, cty), ty') =>
        try(unify_param(env, ty', cty.ctyp_type)) {
        | Unify(trace) =>
          raise(Error(sty.ptyp_loc, env, Type_mismatch(swap_list(trace))))
        },
      List.combine(stl, args),
      params,
    );
    let constr = newconstr(path, List.map(ctyp => ctyp.ctyp_type, args));
    try(Ctype.enforce_constraints(env, constr)) {
    | Unify(trace) =>
      raise(Error(styp.ptyp_loc, env, Type_mismatch(trace)))
    };
    ctyp(TTyConstr(path, lid, args), constr);
  | PTyPoly(vars, st) =>
    let vars = List.map(v => v.txt, vars);
    begin_def();
    let new_univars = List.map(name => (name, newvar(~name, ())), vars);
    let old_univars = univars^;
    univars := new_univars @ univars^;
    let cty = transl_type(env, policy, st);
    let ty = cty.ctyp_type;
    univars := old_univars;
    end_def();
    generalize(ty);
    let ty_list =
      List.fold_left(
        (tyl, (name, ty1)) => {
          let v = ty1 /* would be a no-op right now */ /*Btype.proxy ty1*/;
          if (deep_occur(v, ty)) {
            switch (v.desc) {
            | TTyVar(name) when v.level == Btype.generic_level =>
              v.desc = TTyUniVar(name);
              [v, ...tyl];
            | _ =>
              raise(Error(styp.ptyp_loc, env, Cannot_quantify(name, v)))
            };
          } else {
            tyl;
          };
        },
        [],
        new_univars,
      );

    let ty' = Btype.newgenty(TTyPoly(ty, List.rev(ty_list)));
    unify_var(env, newvar(), ty');
    ctyp(TTyPoly(vars, cty), ty');
  };
};

let make_fixed_univars = ty =>
  // TODO: Remove
  ();

let globalize_used_variables = (env, fixed) => {
  let r = ref([]);
  Tbl.iter(
    (name, (ty, loc)) => {
      let v = new_global_var();
      let snap = Btype.snapshot();
      if (try(
            {
              unify(env, v, ty);
              true;
            }
          ) {
          | _ =>
            Btype.backtrack(snap);
            false;
          }) {
        try(r := [(loc, v, Tbl.find(name, type_variables^)), ...r^]) {
        | Not_found =>
          if (fixed && Btype.is_Tvar(repr(ty))) {
            raise(
              Error(
                loc,
                env,
                Unbound_type_variable(Printf.sprintf("'%s'", name)),
              ),
            );
          };
          let v2 = new_global_var();
          r := [(loc, v, v2), ...r^];
          type_variables := Tbl.add(name, v2, type_variables^);
        };
      };
    },
    used_variables^,
  );
  used_variables := Tbl.empty;
  () =>
    List.iter(
      fun
      | (loc, t1, t2) =>
        try(unify(env, t1, t2)) {
        | Unify(trace) => raise(Error(loc, env, Type_mismatch(trace)))
        },
      r^,
    );
};

let transl_simple_type = (env, fixed, styp) => {
  univars := [];
  used_variables := Tbl.empty;
  let typ = transl_type(env, if (fixed) {Fixed} else {Extensible}, styp);
  globalize_used_variables(env, fixed, ());
  make_fixed_univars(typ.ctyp_type);
  typ;
};

let transl_simple_type_univars = (env, styp) => {
  univars := [];
  used_variables := Tbl.empty;
  pre_univars := [];
  begin_def();
  let typ = transl_type(env, Univars, styp);
  /* Only keep already global variables in used_variables */
  let new_variables = used_variables^;
  used_variables := Tbl.empty;
  Tbl.iter(
    (name, p) =>
      if (Tbl.mem(name, type_variables^)) {
        used_variables := Tbl.add(name, p, used_variables^);
      },
    new_variables,
  );
  globalize_used_variables(env, false, ());
  end_def();
  generalize(typ.ctyp_type);
  let univs =
    List.fold_left(
      (acc, v) => {
        let v = repr(v);
        switch (v.desc) {
        | TTyVar(name) when v.level == Btype.generic_level =>
          v.desc = TTyUniVar(name);
          [v, ...acc];
        | _ => acc
        };
      },
      [],
      pre_univars^,
    );

  make_fixed_univars(typ.ctyp_type);
  {
    ...typ,
    ctyp_type: instance(env, Btype.newgenty(TTyPoly(typ.ctyp_type, univs))),
  };
};

let transl_simple_type_delayed = (env, styp) => {
  univars := [];
  used_variables := Tbl.empty;
  let typ = transl_type(env, Extensible, styp);
  make_fixed_univars(typ.ctyp_type);
  (typ, globalize_used_variables(env, false));
};

let transl_type_scheme = (env, styp) => {
  reset_type_variables();
  begin_def();
  let typ = transl_simple_type(env, false, styp);
  end_def();
  generalize(typ.ctyp_type);
  typ;
};

/* Error report */

open Format;
open Printtyp;

let spellcheck = (ppf, fold, env, lid) => {
  let choices = (~path, name) => {
    let env = fold((x, xs) => [x, ...xs], path, env, []);
    Misc.spellcheck(env, name);
  };
  switch (lid) {
  | Identifier.IdentName(s) =>
    Misc.did_you_mean(ppf, () => choices(~path=None, s.txt))
  | Identifier.IdentExternal(r, s) =>
    Misc.did_you_mean(ppf, () => choices(~path=Some(r), s.txt))
  };
};

let fold_descr = (fold, get_name, f) =>
  fold((descr, acc) => f(get_name(descr), acc));
let fold_simple = (fold4, f) =>
  fold4((name, _path, _descr, acc) => f(name, acc));
let fold_persistent = (fold4, f) =>
  fold4((name, path, _descr, acc) =>
    if (Ident.persistent(Path.head(path))) {
      f(name, acc);
    } else {
      acc;
    }
  );

let fold_values = fold_simple(Env.fold_values);
let fold_types = fold_simple(Env.fold_types);
let fold_modules = fold_persistent(Env.fold_modules);
let fold_constructors = fold_descr(Env.fold_constructors, d => d.cstr_name);
let fold_labels = fold_descr(Env.fold_labels, l => l.lbl_name);
let fold_modtypes = fold_simple(Env.fold_modtypes);

let type_attributes = attrs => {
  List.map(
    ({attr_name: {txt, loc}, attr_args}) =>
      switch (txt, attr_args) {
      | ("disableGC", []) => Location.mkloc(Disable_gc, loc)
      | ("unsafe", []) => Location.mkloc(Unsafe, loc)
      | ("externalName", [name]) =>
        Location.mkloc(External_name(name), loc)
      | _ => failwith("type_attributes: impossible by well-formedness")
      },
    attrs,
  );
};

let report_error = (env, ppf) =>
  fun
  | Unbound_type_variable(name) =>
    /* we don't use "spellcheck" here: the function that raises this
       error seems not to be called anywhere, so it's unclear how it
       should be handled */
    fprintf(ppf, "Unbound type parameter %s@.", name)
  | Unbound_type_constructor(lid) => {
      fprintf(ppf, "Unbound type constructor %a", Identifier.print, lid);
      spellcheck(ppf, fold_types, env, lid);
    }
  | Unbound_type_constructor_2(p) =>
    fprintf(
      ppf,
      "The type constructor@ %a@ is not yet completely defined",
      path,
      p,
    )
  | Type_arity_mismatch(lid, expected, provided) =>
    fprintf(
      ppf,
      "@[The type constructor %a@ expects %i argument(s),@ but is here applied to %i argument(s)@]",
      Identifier.print,
      lid,
      expected,
      provided,
    )
  | Bound_type_variable(name) =>
    fprintf(ppf, "Already bound type parameter '%s", name)
  | Recursive_type => fprintf(ppf, "This type is recursive")
  | Unbound_row_variable(lid) =>
    /* we don't use "spellcheck" here: this error is not raised
       anywhere so it's unclear how it should be handled */
    fprintf(ppf, "Unbound row variable in #%a", identifier, lid)
  | Type_mismatch(trace) =>
    Printtyp.report_unification_error(
      ppf,
      Env.empty,
      trace,
      fun
      | ppf => fprintf(ppf, "This type"),
      fun
      | ppf => fprintf(ppf, "should be an instance of type"),
    )
  | Alias_type_mismatch(trace) =>
    Printtyp.report_unification_error(
      ppf,
      Env.empty,
      trace,
      fun
      | ppf => fprintf(ppf, "This alias is bound to type"),
      fun
      | ppf => fprintf(ppf, "but is used as an instance of type"),
    )
  | Present_has_conjunction(l) =>
    fprintf(ppf, "The present constructor %s has a conjunctive type", l)
  | Present_has_no_type(l) =>
    fprintf(ppf, "The present constructor %s has no type", l)
  | Constructor_mismatch(ty, ty') =>
    wrap_printing_env(
      ~error=true,
      env,
      () => {
        Printtyp.reset_and_mark_loops_list([ty, ty']);
        fprintf(
          ppf,
          "@[<hov>%s %a@ %s@ %a@]",
          "This variant type contains a constructor",
          Printtyp.type_expr,
          ty,
          "which should be",
          Printtyp.type_expr,
          ty',
        );
      },
    )
  | Not_a_variant(ty) => {
      Printtyp.reset_and_mark_loops(ty);
      fprintf(
        ppf,
        "@[The type %a@ does not expand to a polymorphic variant type@]",
        Printtyp.type_expr,
        ty,
      );
      switch (ty.desc) {
      | TTyVar(Some(s)) =>
        /* PR#7012: help the user that wrote 'Foo instead of `Foo */
        Misc.did_you_mean(ppf, () => ["`" ++ s])
      | _ => ()
      };
    }
  | Variant_tags(lab1, lab2) =>
    fprintf(
      ppf,
      "@[Variant tags `%s@ and `%s have the same hash value.@ %s@]",
      lab1,
      lab2,
      "Change one of them.",
    )
  | Invalid_variable_name(name) =>
    fprintf(ppf, "The type variable name %s is not allowed in programs", name)
  | Cannot_quantify(name, v) =>
    fprintf(
      ppf,
      "@[<hov>The universal type variable '%s cannot be generalized:@ %s.@]",
      name,
      if (Btype.is_Tvar(v)) {
        "it escapes its scope";
      } else if (Btype.is_Tunivar(v)) {
        "it is already bound to another variable";
      } else {
        "it is not a variable";
      },
    )
  | Multiple_constraints_on_type(s) =>
    fprintf(ppf, "Multiple constraints for type %a", identifier, s)
  | Method_mismatch(l, ty, ty') =>
    wrap_printing_env(
      ~error=true,
      env,
      () => {
        Printtyp.reset_and_mark_loops_list([ty, ty']);
        fprintf(
          ppf,
          "@[<hov>Method '%s' has type %a,@ which should be %a@]",
          l,
          Printtyp.type_expr,
          ty,
          Printtyp.type_expr,
          ty',
        );
      },
    )
  | Unbound_value(lid) => {
      fprintf(ppf, "Unbound value %a", identifier, lid);
      spellcheck(ppf, fold_values, env, lid);
    }
  | Unbound_value_in_module(mlid, lid) =>
    fprintf(ppf, "Unbound value %s in module %a", lid, identifier, mlid)
  | Unbound_module(lid) => {
      fprintf(ppf, "Unbound module %a", identifier, lid);
      spellcheck(ppf, fold_modules, env, lid);
    }
  | Unbound_constructor(lid) => {
      fprintf(ppf, "Unbound constructor %a", identifier, lid);
      spellcheck(ppf, fold_constructors, env, lid);
    }
  | Unbound_exception(lid) => {
      fprintf(ppf, "Unbound exception %a", identifier, lid);
      spellcheck(ppf, fold_constructors, env, lid);
    }
  | Unbound_label(lid) => {
      fprintf(ppf, "Unbound record label %a", identifier, lid);
      spellcheck(ppf, fold_labels, env, lid);
    }
  | Unbound_class(_)
  | Unbound_cltype(_) =>
    failwith("Impossible: deprecated error type in typetexp")
  | Unbound_modtype(lid) => {
      fprintf(ppf, "Unbound module type %a", identifier, lid);
      spellcheck(ppf, fold_modtypes, env, lid);
    }
  | Ill_typed_functor_application(flid, mlid, details) =>
    switch (details) {
    | None =>
      fprintf(
        ppf,
        "@[Ill-typed functor application %a(%a)@]",
        identifier,
        flid,
        identifier,
        mlid,
      )
    | Some(inclusion_error) =>
      fprintf(
        ppf,
        "@[The type of %a does not match %a's parameter@\n%a@]",
        identifier,
        mlid,
        identifier,
        flid,
        Includemod.report_error,
        inclusion_error,
      )
    }
  | Illegal_reference_to_recursive_module =>
    fprintf(ppf, "Illegal recursive module reference")
  | Wrong_use_of_module(lid, details) =>
    switch (details) {
    | `Structure_used_as_functor =>
      fprintf(
        ppf,
        "@[The module %a is a structure, it cannot be applied@]",
        identifier,
        lid,
      )
    | `Abstract_used_as_functor =>
      fprintf(
        ppf,
        "@[The module %a is abstract, it cannot be applied@]",
        identifier,
        lid,
      )
    | `Functor_used_as_structure =>
      fprintf(
        ppf,
        "@[The module %a is a functor, it cannot have any components@]",
        identifier,
        lid,
      )
    | `Abstract_used_as_structure =>
      fprintf(
        ppf,
        "@[The module %a is abstract, it cannot have any components@]",
        identifier,
        lid,
      )
    | `Generative_used_as_applicative =>
      fprintf(
        ppf,
        "@[The functor %a is generative,@ it@ cannot@ be@ applied@ in@ type@ expressions@]",
        identifier,
        lid,
      )
    }
  | Cannot_scrape_alias(lid, p) =>
    fprintf(
      ppf,
      "The module %a is an alias for module %a, which is missing",
      identifier,
      lid,
      path,
      p,
    )
  | Opened_object(nm) =>
    fprintf(
      ppf,
      "Illegal open object type%a",
      ppf =>
        fun
        | Some(p) => fprintf(ppf, "@ %a", path, p)
        | None => fprintf(ppf, ""),
      nm,
    )
  | Not_an_object(ty) => {
      Printtyp.reset_and_mark_loops(ty);
      fprintf(
        ppf,
        "@[The type %a@ is not an object type@]",
        Printtyp.type_expr,
        ty,
      );
    };

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, env, err) =>
      Some(Location.error_of_printer(loc, report_error(env), err))
    | Error_forward(err) => Some(err)
    | _ => None,
  );
