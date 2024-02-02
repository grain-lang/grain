open Misc;
open Grain_parsing;
open Identifier;
open Path;
open Types;
open Btype;
open Ctype;
open Format;
open Outcometree;

module String = Misc.Stdlib.String;

/* Print a long identifier */

let rec identifier = ppf =>
  fun
  | IdentName(s) => pp_print_string(ppf, s.txt)
  | IdentExternal(p, s) => fprintf(ppf, "%a::%s", identifier, p, s.txt);

/* Print an identifier */

let unique_names = ref(Ident.empty);

let ident_name = id =>
  try(Ident.find_same(id, unique_names^)) {
  | Not_found => Ident.name(id)
  };

let add_unique = id =>
  try(ignore(Ident.find_same(id, unique_names^))) {
  | Not_found =>
    unique_names :=
      Ident.add(id, Ident.unique_toplevel_name(id), unique_names^)
  };

let ident = (ppf, id) => {
  let name = ident_name(id);
  if (Oprint.parenthesized_ident(name)) {
    fprintf(ppf, "(%s)", name);
  } else {
    pp_print_string(ppf, name);
  };
};

/* Print a path */

let ident_pervasives = Ident.create_persistent("Stdlib");
let printing_env = ref(Env.empty);
let non_shadowed_pervasive =
  fun
  | PExternal(PIdent(id), s) as path =>
    Ident.same(id, ident_pervasives)
    && (
      try(
        Path.same(
          path,
          Env.lookup_type(IdentName(mknoloc(s)), printing_env^),
        )
      ) {
      | Not_found => true
      }
    )
  | _ => false;

let rec tree_of_path =
  fun
  | PIdent(id) => Oide_ident(ident_name(id))
  | PExternal(_, s) as path when non_shadowed_pervasive(path) =>
    Oide_ident(s)
  | PExternal(p, s) => Oide_dot(tree_of_path(p), s);

let rec path = ppf =>
  fun
  | PIdent(id) => ident(ppf, id)
  | PExternal(_, s) as path when non_shadowed_pervasive(path) =>
    pp_print_string(ppf, s)
  | PExternal(p, s) => {
      path(ppf, p);
      pp_print_string(ppf, "::");
      pp_print_string(ppf, s);
    };

let rec string_of_out_ident =
  fun
  | Oide_ident(s) => s
  | Oide_dot(id, s) => String.concat(".", [string_of_out_ident(id), s]);

let string_of_path = p => string_of_out_ident(tree_of_path(p));

/* Print a recursive annotation */

let tree_of_rec =
  fun
  | TRecNot => Orec_not
  | TRecFirst => Orec_first
  | TRecNext => Orec_next;

/* Print a raw type expression, with sharing */

let raw_list = (pr, ppf) =>
  fun
  | [] => fprintf(ppf, "[]")
  | [a, ...l] =>
    fprintf(ppf, "@[<1>[%a%t]@]", pr, a, ppf =>
      List.iter(x => fprintf(ppf, ";@,%a", pr, x), l)
    );

let kind_vars = ref([]);
let kind_count = ref(0);

let rec safe_commu_repr = v =>
  fun
  | TComOk => "TComOk"
  | TComUnknown => "TComUnknown"
  | TComLink(r) =>
    if (List.memq(r, v)) {
      "TComLink loop";
    } else {
      safe_commu_repr([r, ...v], r^);
    };

let rec safe_repr = v =>
  fun
  | {desc: TTyLink(t)} when !List.memq(t, v) => safe_repr([t, ...v], t)
  | t => t;

let rec list_of_memo =
  fun
  | TMemNil => []
  | TMemCons(p, _t1, _t2, rem) => [p, ...list_of_memo(rem)]
  | TMemLink(rem) => list_of_memo(rem^);

let print_name = ppf =>
  fun
  | None => fprintf(ppf, "None")
  | Some(name) => fprintf(ppf, "\"%s\"", name);

let visited = ref([]);
let rec raw_type = (ppf, ty) => {
  let ty = safe_repr([], ty);
  if (List.memq(ty, visited^)) {
    fprintf(ppf, "{id=%d}", ty.id);
  } else {
    visited := [ty, ...visited^];
    fprintf(
      ppf,
      "@[<1>{id=%d;level=%d;desc=@,%a}@]",
      ty.id,
      ty.level,
      raw_type_desc,
      ty.desc,
    );
  };
}
and raw_argtype = (ppf, (l, ty)) => {
  fprintf(ppf, "@[<1>%s: %a@]", qualified_label_name(l), raw_type, ty);
}
and raw_type_list = tl => raw_list(raw_type, tl)
and raw_argtype_list = al => raw_list(raw_argtype, al)
and raw_type_desc = ppf =>
  fun
  | TTyVar(name) => fprintf(ppf, "TTyVar %a", print_name, name)
  | TTyArrow(a1, t2, c) =>
    fprintf(
      ppf,
      "@[<hov1>TTyArrow(@,%a,@,%a,@,%s)@]",
      raw_argtype_list,
      a1,
      raw_type,
      t2,
      safe_commu_repr([], c),
    )
  | TTyTuple(tl) => fprintf(ppf, "@[<1>TTyTuple@,%a@]", raw_type_list, tl)
  | TTyRecord(tl) =>
    fprintf(
      ppf,
      "@[<1>TTyRecord@,%a@]",
      raw_list((ppf, (name, arg)) =>
        fprintf(ppf, "%s=%a", name, raw_type, arg)
      ),
      tl,
    )
  | TTyConstr(p, tl, abbrev) =>
    fprintf(
      ppf,
      "@[<hov1>TTyConstr(@,%a,@,%a,@,%a)@]",
      path,
      p,
      raw_type_list,
      tl,
      raw_list(path),
      list_of_memo(abbrev^),
    )
  | TTyLink(t) => fprintf(ppf, "@[<1>TTyLink@,%a@]", raw_type, t)
  | TTySubst(t) => fprintf(ppf, "@[<1>TTySubst@,%a@]", raw_type, t)
  | TTyUniVar(name) => fprintf(ppf, "TTyUnivar %a", print_name, name)
  | TTyPoly(t, tl) =>
    fprintf(
      ppf,
      "@[<hov1>TTyPoly(@,%a,@,%a)@]",
      raw_type,
      t,
      raw_type_list,
      tl,
    );

let raw_type_expr = (ppf, t) => {
  visited := [];
  kind_vars := [];
  kind_count := 0;
  raw_type(ppf, t);
  visited := [];
  kind_vars := [];
};

let () = Btype.print_raw := raw_type_expr;

/* Normalize paths */

type param_subst =
  | Id
  | Nth(int)
  | Map(list(int));

let is_nth =
  fun
  | Nth(_) => true
  | _ => false;

let compose = l1 =>
  fun
  | Id => Map(l1)
  | Map(l2) => Map(List.map(List.nth(l1), l2))
  | Nth(n) => Nth(List.nth(l1, n));

let apply_subst = (s1, tyl) =>
  if (tyl == []) {
    [];
  } else {
    /* cf. PR#7543: Typemod.type_package doesn't respect type constructor arity */
    switch (s1) {
    | Nth(n1) => [List.nth(tyl, n1)]
    | Map(l1) => List.map(List.nth(tyl), l1)
    | Id => tyl
    };
  };

type best_path =
  | Paths(list(Path.t))
  | Best(Path.t);

let printing_depth = ref(0);
let printing_cont = ref([]: list(Env.iter_cont));
let printing_old = ref(Env.empty);
let printing_pers = ref(Concr.empty);
module PathMap = Map.Make(Path);
let printing_map = ref(PathMap.empty);

let same_type = (t, t') => repr(t) === repr(t');

let rec index = (l, x) =>
  switch (l) {
  | [] => raise(Not_found)
  | [a, ...l] =>
    if (x === a) {
      0;
    } else {
      1 + index(l, x);
    }
  };

let rec uniq =
  fun
  | [] => true
  | [a, ...l] => !List.memq(a, l) && uniq(l);

let rec normalize_type_path = (~cache=false, env, p) =>
  try({
    let (params, ty, _) = Env.find_type_expansion(p, env);
    let params = List.map(repr, params);
    switch (repr(ty)) {
    | {desc: TTyConstr(p1, tyl, _)} =>
      let tyl = List.map(repr, tyl);
      if (List.length(params) == List.length(tyl)
          && List.for_all2((===), params, tyl)) {
        normalize_type_path(~cache, env, p1);
      } else if (cache
                 || List.length(params) <= List.length(tyl)
                 || !uniq(tyl)) {
        (p, Id);
      } else {
        let l1 = List.map(index(params), tyl);
        let (p2, s2) = normalize_type_path(~cache, env, p1);
        (p2, compose(l1, s2));
      };
    | ty => (p, Nth(index(params, ty)))
    };
  }) {
  | Not_found => (Env.normalize_path(None, env, p), Id)
  };

let penalty = s =>
  if (s != "" && s.[0] == '_') {
    10;
  } else {
    try(
      {
        for (i in 0 to String.length(s) - 2) {
          if (s.[i] == '_' && s.[i + 1] == '_') {
            raise(Exit);
          };
        };
        1;
      }
    ) {
    | Exit => 10
    };
  };

let rec path_size =
  fun
  | PIdent(id) => (penalty(Ident.name(id)), - Ident.binding_time(id))
  | PExternal(p, _) => {
      let (l, b) = path_size(p);
      (1 + l, b);
    };

let same_printing_env = env => {
  let used_pers = Env.used_persistent();
  Env.same_types(printing_old^, env)
  && Concr.equal(printing_pers^, used_pers);
};

let set_printing_env = env => {
  printing_env := env;
  if (Clflags.real_paths^
      || printing_env^ === Env.empty
      || same_printing_env(env)) {
    ();
  } else {
    /* printf "Reset printing_map@."; */
    printing_old := env;
    printing_pers := Env.used_persistent();
    printing_map := PathMap.empty;
    printing_depth := 0;
    /* printf "Recompute printing_map.@."; */
    let cont =
      Env.iter_types(
        (p, (p', _decl)) => {
          let (p1, s1) = normalize_type_path(env, p', ~cache=true);
          /* Format.eprintf "%a -> %a = %a@." path p path p' path p1 */
          if (s1 == Id) {
            try({
              let r = PathMap.find(p1, printing_map^);
              switch (r^) {
              | Paths(l) => r := Paths([p, ...l])
              | Best(p') => r := Paths([p, p'])
              };
            }) {
            /* assert false */
            | Not_found =>
              printing_map :=
                PathMap.add(p1, ref(Paths([p])), printing_map^)
            };
          };
        },
        env,
      );
    printing_cont := [cont];
  };
};

let wrap_printing_env = (env, f) => {
  set_printing_env(env);
  try_finally(~always=() => set_printing_env(Env.empty), f);
};

let wrap_printing_env = (~error, env, f) =>
  if (error) {
    Env.without_cmis(wrap_printing_env(env), f);
  } else {
    wrap_printing_env(env, f);
  };

let is_unambiguous = (path, env) => {
  let l = Env.find_shadowed_types(path, env);
  List.exists(Path.same(path), l)  /* concrete paths are ok */
  || (
    switch (l) {
    | [] => true
    | [p, ...rem] =>
      /* allow also coherent paths:  */
      let normalize = p => fst(normalize_type_path(~cache=true, env, p));
      let p' = normalize(p);
      List.for_all(p => Path.same(normalize(p), p'), rem)
      /* also allow repeatedly defining and opening (for toplevel) */
      || {
        let id = lid_of_path(p);
        List.for_all(p => lid_of_path(p) == id, rem)
        && Path.same(p, Env.lookup_type(id, env));
      };
    }
  );
};

let rec get_best_path = r =>
  switch (r^) {
  | Best(p') => p'
  | Paths([]) => raise(Not_found)
  | Paths(l) =>
    r := Paths([]);
    List.iter(
      p =>
        /* Format.eprintf "evaluating %a@." path p; */
        switch (r^) {
        | Best(p') when path_size(p) >= path_size(p') => ()
        | _ =>
          if (is_unambiguous(p, printing_env^)) {
            r := Best(p);
          }
        },
      /* else Format.eprintf "%a ignored as ambiguous@." path p */
      l,
    );
    get_best_path(r);
  };

let best_type_path = p =>
  if (Clflags.real_paths^ || printing_env^ === Env.empty) {
    (p, Id);
  } else {
    let (p', s) = normalize_type_path(printing_env^, p);
    let get_path = () => get_best_path(PathMap.find(p', printing_map^));
    while (printing_cont^ != []
           && (
             try(fst(path_size(get_path())) > printing_depth^) {
             | Not_found => true
             }
           )) {
      printing_cont := List.map(snd, Env.run_iter_cont(printing_cont^));
      incr(printing_depth);
    };
    let p'' =
      try(get_path()) {
      | Not_found => p'
      };
    /* Format.eprintf "%a = %a -> %a@." path p path p' path p''; */
    (p'', s);
  };

/* Print a type expression */

let names = ref([]: list((type_expr, string)));
let name_counter = ref(0);
let named_vars = ref([]: list(string));

let weak_counter = ref(1);
let weak_var_map = ref(TypeMap.empty);
let named_weak_vars = ref(String.Set.empty);

let reset_names = () => {
  names := [];
  name_counter := 0;
  named_vars := [];
};
let add_named_var = ty =>
  switch (ty.desc) {
  | TTyVar(Some(name))
  | TTyUniVar(Some(name)) =>
    if (List.mem(name, named_vars^)) {
      ();
    } else {
      named_vars := [name, ...named_vars^];
    }
  | _ => ()
  };

let name_is_already_used = name =>
  List.mem(name, named_vars^)
  || List.exists(((_, name')) => name == name', names^)
  || String.Set.mem(name, named_weak_vars^);

let rec new_name = () => {
  let name =
    if (name_counter^ < 26) {
      String.make(1, Char.chr(97 + name_counter^));
    } else {
      String.make(1, Char.chr(97 + name_counter^ mod 26))
      ++ string_of_int(name_counter^ / 26);
    };
  incr(name_counter);
  if (name_is_already_used(name)) {
    new_name();
  } else {
    name;
  };
};

let rec new_weak_name = (ty, ()) => {
  let name = "weak" ++ string_of_int(weak_counter^);
  incr(weak_counter);
  if (name_is_already_used(name)) {
    new_weak_name(ty, ());
  } else {
    named_weak_vars := String.Set.add(name, named_weak_vars^);
    weak_var_map := TypeMap.add(ty, name, weak_var_map^);
    name;
  };
};

let name_of_type = (name_generator, t) =>
  /* We've already been through repr at this stage, so t is our representative
     of the union-find class. */
  try(List.assq(t, names^)) {
  | Not_found =>
    try(TypeMap.find(t, weak_var_map^)) {
    | Not_found =>
      let name =
        switch (t.desc) {
        | TTyVar(Some(name))
        | TTyUniVar(Some(name)) =>
          /* Some part of the type we've already printed has assigned another
           * unification variable to that name. We want to keep the name, so try
           * adding a number until we find a name that's not taken. */
          let current_name = ref(name);
          let i = ref(0);
          while (List.exists(((_, name')) => current_name^ == name', names^)) {
            current_name := name ++ string_of_int(i^);
            i := i^ + 1;
          };
          current_name^;
        | _ =>
          /* No name available, create a new one */
          name_generator()
        };

      /* Exception for type declarations */
      if (name != "_") {
        names := [(t, name), ...names^];
      };
      name;
    }
  };

let check_name_of_type = t => ignore(name_of_type(new_name, t));

let remove_names = tyl => {
  let tyl = List.map(repr, tyl);
  names := List.filter(((ty, _)) => !List.memq(ty, tyl), names^);
};

let visited_objects = ref([]: list(type_expr));
let aliased = ref([]: list(type_expr));
let delayed = ref([]: list(type_expr));

let add_delayed = t =>
  if (!List.memq(t, delayed^)) {
    delayed := [t, ...delayed^];
  };

let is_aliased = ty => List.memq(ty, aliased^);
let add_alias = ty => {
  let px = ty;
  if (!is_aliased(px)) {
    aliased := [px, ...aliased^];
    add_named_var(px);
  };
};

let aliasable = ty =>
  switch (ty.desc) {
  | TTyVar(_)
  | TTyUniVar(_)
  | TTyPoly(_) => false
  | TTyConstr(p, _, _) => !is_nth(snd(best_type_path(p)))
  | _ => true
  };

let rec mark_loops_rec = (visited, ty) => {
  let ty = repr(ty);
  let px = ty;
  if (List.memq(px, visited) && aliasable(ty)) {
    add_alias(px);
  } else {
    let visited = [px, ...visited];
    switch (ty.desc) {
    | TTyVar(_) => add_named_var(ty)
    | TTyArrow(a1, ty2, _) =>
      List.iter(((_, t)) => mark_loops_rec(visited, t), a1);
      mark_loops_rec(visited, ty2);
    | TTyTuple(tyl) => List.iter(mark_loops_rec(visited), tyl)
    | TTyRecord(tyl) =>
      List.iter(((_, arg)) => (mark_loops_rec(visited))(arg), tyl)
    | TTyConstr(p, tyl, _) =>
      let (_p', s) = best_type_path(p);
      List.iter(mark_loops_rec(visited), apply_subst(s, tyl));
    | TTySubst(ty) => mark_loops_rec(visited, ty)
    | TTyLink(_) => fatal_error("Printtyp.mark_loops_rec (2)")
    | TTyPoly(ty, tyl) =>
      List.iter(t => add_alias(t), tyl);
      mark_loops_rec(visited, ty);
    | TTyUniVar(_) => add_named_var(ty)
    };
  };
};

let mark_loops = ty => {
  normalize_type(Env.empty, ty);
  mark_loops_rec([], ty);
};

let reset_loop_marks = () => {
  visited_objects := [];
  aliased := [];
  delayed := [];
};

let reset = () => {
  unique_names := Ident.empty;
  reset_names();
  reset_loop_marks();
};

let reset_and_mark_loops = ty => {
  reset();
  mark_loops(ty);
};

let reset_and_mark_loops_list = tyl => {
  reset();
  List.iter(mark_loops, tyl);
};

/* Disabled in classic mode when printing an unification error */
let print_labels = ref(true);

let rec tree_of_typexp = (sch, ty) => {
  let ty = repr(ty);
  let px = ty;
  if (List.mem_assq(px, names^) && !List.memq(px, delayed^)) {
    let mark = is_non_gen(sch, ty);
    let name =
      name_of_type(
        if (mark) {
          new_weak_name(ty);
        } else {
          new_name;
        },
        px,
      );
    Otyp_var(mark, name);
  } else {
    let pr_typ = () =>
      switch (ty.desc) {
      | TTyVar(_) =>
        /*let lev =
          if is_non_gen sch ty then "/" ^ string_of_int ty.level else "" in*/
        let non_gen = is_non_gen(sch, ty);
        let name_gen =
          if (non_gen) {
            new_weak_name(ty);
          } else {
            new_name;
          };
        Otyp_var(non_gen, name_of_type(name_gen, ty));
      | TTyArrow(a1, ty2, _) =>
        let pr_arrow = (a1, ty2) => {
          let a1 = tree_of_argtyplist(sch, a1);
          Otyp_arrow(a1, tree_of_typexp(sch, ty2));
        };
        pr_arrow(a1, ty2);
      | TTyTuple(tyl) => Otyp_tuple(tree_of_typlist(sch, tyl))
      | TTyRecord(tyl) =>
        Otyp_record(
          List.map(
            ((name, arg)) => (name, false, tree_of_typexp(sch, arg)),
            tyl,
          ),
        )
      | TTyConstr(p, tyl, _abbrev) =>
        let (p', s) = best_type_path(p);
        let tyl' = apply_subst(s, tyl);
        if (is_nth(s) && !(tyl' == [])) {
          tree_of_typexp(sch, List.hd(tyl'));
        } else {
          Otyp_constr(tree_of_path(p'), tree_of_typlist(sch, tyl'));
        };
      | TTySubst(ty) => tree_of_typexp(sch, ty)
      | TTyLink(_) => fatal_error("Printtyp.tree_of_typexp")
      | TTyPoly(ty, []) => tree_of_typexp(sch, ty)
      | TTyPoly(ty, tyl) =>
        /*let print_names () =
          List.iter (fun (_, name) -> prerr_string (name ^ " ")) !names;
          prerr_string "; " in */
        let tyl = List.map(repr, tyl);
        if (tyl == []) {
          tree_of_typexp(sch, ty);
        } else {
          let old_delayed = delayed^;
          /* Make the names delayed, so that the real type is
             printed once when used as proxy */
          List.iter(add_delayed, tyl);
          let tl = List.map(name_of_type(new_name), tyl);
          let tr = Otyp_poly(tl, tree_of_typexp(sch, ty));
          /* Forget names when we leave scope */
          remove_names(tyl);
          delayed := old_delayed;
          tr;
        };
      | TTyUniVar(_) => Otyp_var(false, name_of_type(new_name, ty))
      };

    if (List.memq(px, delayed^)) {
      delayed := List.filter((!==)(px), delayed^);
    };
    if (is_aliased(px) && aliasable(ty)) {
      check_name_of_type(px);
      Otyp_alias(pr_typ(), name_of_type(new_name, px));
    } else {
      pr_typ();
    };
  };
}

and tree_of_typlist = (sch, tyl) => List.map(tree_of_typexp(sch), tyl)
and get_arg_type = ty => {
  switch (ty.desc) {
  | TTyConstr(_, [ty], _) => ty
  | TTyLink(ty) => get_arg_type(ty)
  | _ => failwith("Impossible: optional argument with non-option type")
  };
}
and tree_of_argtyplist = (sch, al) =>
  List.map(
    ((l, ty)) => {
      let ty =
        switch (l) {
        | Default(_) => get_arg_type(ty)
        | _ => ty
        };
      (qualified_label_name(l), tree_of_typexp(sch, ty));
    },
    al,
  )

and is_non_gen = (sch, ty) => sch && is_Tvar(ty) && ty.level != generic_level

and tree_of_typfields = (sch, rest) =>
  fun
  | [] => {
      let rest =
        switch (rest.desc) {
        | TTyVar(_)
        | TTyUniVar(_) => Some(is_non_gen(sch, rest))
        | TTyConstr(_) => Some(false)
        | _ => fatal_error("typfields (1)")
        };

      ([], rest);
    }
  | [(s, t), ...l] => {
      let field = (s, tree_of_typexp(sch, t));
      let (fields, rest) = tree_of_typfields(sch, rest, l);
      ([field, ...fields], rest);
    };

let typexp = (sch, ppf, ty) =>
  Oprint.out_type^(ppf, tree_of_typexp(sch, ty));

let type_expr = (ppf, ty) => {
  reset_and_mark_loops(ty);
  typexp(false, ppf, ty);
}

and type_sch = (ppf, ty) => typexp(true, ppf, ty)

and type_scheme = (ppf, ty) => {
  reset_and_mark_loops(ty);
  typexp(true, ppf, ty);
};

let string_of_type_sch = ty => {
  asprintf("%a", type_sch, ty);
};

let string_of_type_scheme = ty => {
  asprintf("%a", type_scheme, ty);
};

/* Maxence */
let type_scheme_max = (~b_reset_names=true, ppf, ty) => {
  if (b_reset_names) {
    reset_names();
  };
  typexp(true, ppf, ty);
};
/* End Maxence */

let tree_of_type_scheme = ty => {
  reset_and_mark_loops(ty);
  tree_of_typexp(true, ty);
};

/* Print one type declaration */

let tree_of_constraints = params =>
  List.fold_right(
    (ty, list) => {
      let ty' = unalias(ty);
      if (ty !== ty') {
        let tr = tree_of_typexp(true, ty);
        [(tr, tree_of_typexp(true, ty')), ...list];
      } else {
        list;
      };
    },
    params,
    [],
  );

let filter_params = tyl => {
  let params =
    List.fold_left(
      (tyl, ty) => {
        let ty = repr(ty);
        if (List.memq(ty, tyl)) {
          [Btype.newgenty(TTySubst(ty)), ...tyl];
        } else {
          [ty, ...tyl];
        };
      },
      [],
      tyl,
    );
  List.rev(params);
};

let mark_loops_constructor_arguments =
  fun
  | TConstrTuple(l) => List.iter(mark_loops, l)
  | TConstrRecord(rfs) => List.iter(rf => mark_loops(rf.rf_type), rfs)
  | TConstrSingleton => ();

let rec tree_of_type_decl = (id, decl) => {
  reset();

  let params = filter_params(decl.type_params);

  switch (decl.type_manifest) {
  | Some(ty) =>
    let vars = free_variables(ty);
    List.iter(
      fun
      | {desc: TTyVar(Some("_"))} as ty =>
        if (List.memq(ty, vars)) {
          ty.desc = TTyVar(None);
        }
      | _ => (),
      params,
    );
  | None => ()
  };

  List.iter(add_alias, params);
  List.iter(mark_loops, params);
  List.iter(check_name_of_type, params);
  let ty_manifest =
    switch (decl.type_manifest) {
    | None => None
    | Some(ty) =>
      let ty = repr(ty);
      mark_loops(ty);
      Some(ty);
    };

  switch (decl.type_kind) {
  | TDataOpen
  | TDataAbstract => ()
  | TDataRecord(fields) =>
    List.iter(({rf_type}) => mark_loops(rf_type), fields)
  | TDataVariant(cstrs) =>
    List.iter(
      c => {
        mark_loops_constructor_arguments(c.cd_args);
        Option.iter(mark_loops, c.cd_res);
      },
      cstrs,
    )
  };

  let type_param =
    fun
    | Otyp_var(_, id) => id
    | _ => "?";

  let type_defined = decl => {
    let vari = List.map(ty => (true, true), decl.type_params);

    (
      Ident.name(id),
      List.map2(
        (ty, cocn) => (type_param(tree_of_typexp(false, ty)), cocn),
        params,
        vari,
      ),
    );
  };

  let tree_of_manifest = ty1 =>
    switch (ty_manifest) {
    | None => ty1
    | Some(ty) => Otyp_manifest(tree_of_typexp(false, ty), ty1)
    };

  let (name, args) = type_defined(decl);
  let constraints = tree_of_constraints(params);
  let ty =
    switch (decl.type_kind) {
    | TDataAbstract =>
      switch (ty_manifest) {
      | None => Otyp_abstract
      | Some(ty) => tree_of_typexp(false, ty)
      }
    | TDataRecord(fields) =>
      Otyp_record(
        List.map(
          ({rf_name, rf_type}) =>
            (Ident.name(rf_name), false, tree_of_typexp(false, rf_type)),
          fields,
        ),
      )
    | TDataVariant(cstrs) =>
      tree_of_manifest(Otyp_sum(List.map(tree_of_constructor, cstrs)))
    | TDataOpen => tree_of_manifest(Otyp_open)
    };

  {
    otype_name: name,
    otype_params: args,
    otype_type: ty,
    otype_immediate: false,
    otype_unboxed: false,
    otype_cstrs: constraints,
  };
}

and tree_of_constructor_arguments =
  fun
  | TConstrTuple(l) => [Otyp_tuple(tree_of_typlist(false, l))]
  | TConstrRecord(l) => [Otyp_record(List.map(tree_of_label, l))]
  | TConstrSingleton => []

and tree_of_constructor = cd => {
  let name = Ident.name(cd.cd_id);
  let arg = () => tree_of_constructor_arguments(cd.cd_args);
  switch (cd.cd_res) {
  | None => (name, arg(), None)
  | Some(res) =>
    let nm = names^;
    names := [];
    let ret = tree_of_typexp(false, res);
    let args = arg();
    names := nm;
    (name, args, Some(ret));
  };
}

and tree_of_label = l => (
  Ident.name(l.rf_name),
  l.rf_mutable,
  tree_of_typexp(false, l.rf_type),
);

let tree_of_type_declaration = (id, decl, rs) =>
  Osig_type(tree_of_type_decl(id, decl), tree_of_rec(rs));

let type_declaration = (id, ppf, decl) =>
  Oprint.out_sig_item^(ppf, tree_of_type_declaration(id, decl, TRecFirst));

let string_of_type_declaration = (~ident, td) => {
  asprintf("%a", type_declaration(ident), td);
};

let constructor_arguments = (ppf, a) => {
  let tys = tree_of_constructor_arguments(a);
  Oprint.out_type^(ppf, Otyp_tuple(tys));
};

let constructor = (ppf, cstr) =>
  Oprint.out_constr^(ppf, tree_of_constructor(cstr));

let string_of_constructor = cstr => {
  let str = asprintf("%a", constructor, cstr);
  // Hacky workaround to avoid having to make invasive changes in Oprint
  Str.global_replace(Str.regexp("^  "), "", str);
};

/* Print an extension declaration */

let tree_of_extension_constructor = (id, ext, es) => {
  let ty_name = Path.name(ext.ext_type_path);
  let ty_params = filter_params(ext.ext_type_params);
  List.iter(add_alias, ty_params);
  List.iter(mark_loops, ty_params);
  List.iter(check_name_of_type, ty_params);
  mark_loops_constructor_arguments(ext.ext_args);
  let type_param =
    fun
    | Otyp_var(_, id) => id
    | _ => "?";

  let ty_params =
    List.map(ty => type_param(tree_of_typexp(false, ty)), ty_params);

  let name = Ident.name(id);
  let args = tree_of_constructor_arguments(ext.ext_args);

  let ext = {
    oext_name: name,
    oext_type_name: ty_name,
    oext_type_params: ty_params,
    oext_args: args,
  };

  let es =
    switch (es) {
    | TExtFirst => Oext_first
    | TExtNext => Oext_next
    | TExtException => Oext_exception
    };

  Osig_typext(ext, es);
};

let extension_constructor = (id, ppf, ext) =>
  Oprint.out_sig_item^(
    ppf,
    tree_of_extension_constructor(id, ext, TExtException),
  );

let extension_only_constructor = (id, ppf, ext) => {
  let name = Ident.name(id);
  let args = tree_of_constructor_arguments(ext.ext_args);
  Format.fprintf(ppf, "@[<hv>%a@]", Oprint.out_constr^, (name, args, None));
};

let string_of_extension_constructor = (~ident, ext) => {
  asprintf("%a", extension_constructor(ident), ext);
};

/* Print a value declaration */
let tree_of_value_description = (id, decl) => {
  let id = Ident.name(id);
  let ty = tree_of_type_scheme(decl.val_type);
  let vd = {
    oval_name: id,
    oval_type: ty,
    oval_prims: [],
    oval_attributes: [],
  };

  Osig_value(vd);
};

let value_description = (id, ppf, decl) =>
  Oprint.out_sig_item^(ppf, tree_of_value_description(id, decl));

let string_of_value_description = (~ident, vd) => {
  asprintf("%a", value_description(ident), vd);
};

/* Print a module type */

let wrap_env = (fenv, ftree, arg) => {
  let env = printing_env^;
  set_printing_env(fenv(env));
  let tree = ftree(arg);
  set_printing_env(env);
  tree;
};

let filter_rem_sig = (item, rem) =>
  switch (item, rem) {
  | _ => ([], rem)
  };

let dummy = {
  type_params: [],
  type_arity: 0,
  type_kind: TDataAbstract,
  type_manifest: None,
  type_newtype_level: None,
  type_loc: Location.dummy_loc,
  type_path: PIdent({stamp: (-1), name: "", flags: 0}),
  type_allocation: Managed,
};

let hide_rec_items =
  fun
  | [TSigType(id, _decl, rs), ...rem]
      when rs == TRecFirst && ! Clflags.real_paths^ => {
      let rec get_ids = (
        fun
        | [TSigType(id, _, TRecNext), ...rem] => [id, ...get_ids(rem)]
        | _ => []
      );

      let ids = [id, ...get_ids(rem)];
      set_printing_env(
        List.fold_right(
          id => Env.add_type(~check=false, Ident.rename(id), dummy),
          ids,
          printing_env^,
        ),
      );
    }
  | _ => ();

let rec tree_of_modtype = (~ellipsis=false) =>
  fun
  | TModIdent(p) => Omty_ident(tree_of_path(p))
  | TModAlias(p) => Omty_alias(tree_of_path(p))
  | TModSignature(sg) =>
    Omty_signature(
      if (ellipsis) {
        [Osig_ellipsis];
      } else {
        tree_of_signature(sg);
      },
    )

and tree_of_signature = sg =>
  wrap_env(env => env, tree_of_signature_rec(printing_env^, false), sg)

and tree_of_signature_rec = (env', in_type_group) =>
  fun
  | [] => []
  | [item, ...rem] as items => {
      let in_type_group =
        switch (in_type_group, item) {
        | (true, TSigType(_, _, TRecNext)) => true
        | (_, TSigType(_, _, TRecNot | TRecFirst)) =>
          set_printing_env(env');
          true;
        | _ =>
          set_printing_env(env');
          false;
        };

      let (sg, rem) = filter_rem_sig(item, rem);
      hide_rec_items(items);
      let trees = trees_of_sigitem(item);
      let env' = Env.add_signature([item, ...sg], env');
      trees @ tree_of_signature_rec(env', in_type_group, rem);
    }

and trees_of_sigitem =
  fun
  | TSigValue(id, decl) => [tree_of_value_description(id, decl)]
  | TSigType(id, decl, rs) => [tree_of_type_declaration(id, decl, rs)]
  | TSigTypeExt(id, ext, es) => [tree_of_extension_constructor(id, ext, es)]
  | TSigModule(id, md, rs) => [
      tree_of_module(id, md.md_type, rs, ~ellipsis=false),
    ]
  | TSigModType(id, decl) => [tree_of_modtype_declaration(id, decl)]

and tree_of_modtype_declaration = (id, decl) => {
  let mty =
    switch (decl.mtd_type) {
    | None => Omty_abstract
    | Some(mty) => tree_of_modtype(mty)
    };

  Osig_modtype(Ident.name(id), mty);
}

and tree_of_module = (id, ~ellipsis=?, mty, rs) =>
  Osig_module(
    Ident.name(id),
    tree_of_modtype(~ellipsis?, mty),
    tree_of_rec(rs),
  );

let modtype = (ppf, mty) =>
  Oprint.out_module_type^(ppf, tree_of_modtype(mty));
let modtype_declaration = (id, ppf, decl) =>
  Oprint.out_sig_item^(ppf, tree_of_modtype_declaration(id, decl));

/* Refresh weak variable map in the toplevel */
let refresh_weak = () => {
  let refresh = (t, name, (m, s)) =>
    if (is_non_gen(true, repr(t))) {
      (TypeMap.add(t, name, m), String.Set.add(name, s));
    } else {
      (m, s);
    };
  let (m, s) =
    TypeMap.fold(refresh, weak_var_map^, (TypeMap.empty, String.Set.empty));
  named_weak_vars := s;
  weak_var_map := m;
};

let print_items = (showval, env, x) => {
  refresh_weak();
  let rec print = (showval, env) =>
    fun
    | [] => []
    | [item, ...rem] as items => {
        let (_sg, rem) = filter_rem_sig(item, rem);
        hide_rec_items(items);
        let trees = trees_of_sigitem(item);
        List.map(d => (d, showval(env, item)), trees)
        @ print(showval, env, rem);
      };
  print(showval, env, x);
};

/* Print a signature body (used by -i when compiling a .ml) */

let print_signature = (ppf, tree) =>
  fprintf(ppf, "@[<v>%a@]", Oprint.out_signature^, tree);

let signature = (ppf, sg) =>
  fprintf(ppf, "%a", print_signature, tree_of_signature(sg));

/* Print an unification error */

let same_path = (t, t') => {
  let t = repr(t)
  and t' = repr(t');
  t === t'
  || (
    switch (t.desc, t'.desc) {
    | (TTyConstr(p, tl, _), TTyConstr(p', tl', _)) =>
      let (p1, s1) = best_type_path(p)
      and (p2, s2) = best_type_path(p');
      switch (s1, s2) {
      | (Nth(n1), Nth(n2)) when n1 == n2 => true
      | (Id | Map(_), Id | Map(_)) when Path.same(p1, p2) =>
        let tl = apply_subst(s1, tl)
        and tl' = apply_subst(s2, tl');
        List.length(tl) == List.length(tl')
        && List.for_all2(same_type, tl, tl');
      | _ => false
      };
    | _ => false
    }
  );
};

let type_expansion = (t, ppf, t') =>
  if (same_path(t, t')) {
    add_delayed(t);
    type_expr(ppf, t);
  } else {
    let t' =
      if (t === t') {
        unalias(t');
      } else {
        t';
      };
    fprintf(ppf, "@[<2>%a@ =@ %a@]", type_expr, t, type_expr, t');
  };

let type_path_expansion = (tp, ppf, tp') =>
  if (Path.same(tp, tp')) {
    path(ppf, tp);
  } else {
    fprintf(ppf, "@[<2>%a@ =@ %a@]", path, tp, path, tp');
  };

let rec trace = (fst, txt, ppf) =>
  fun
  | [(t1, t1'), (t2, t2'), ...rem] => {
      if (!fst) {
        fprintf(ppf, "@,");
      };
      fprintf(
        ppf,
        "@[Type@;<1 2>%a@ %s@;<1 2>%a@] %a",
        type_expansion(t1),
        t1',
        txt,
        type_expansion(t2),
        t2',
        trace(false, txt),
        rem,
      );
    }
  | _ => ();

let rec filter_trace = keep_last =>
  fun
  | [(_, t1'), (_, t2')] when is_Tvar(t1') || is_Tvar(t2') => []
  | [(t1, t1'), (t2, t2'), ...rem] => {
      let rem' = filter_trace(keep_last, rem);
      if (same_path(t1, t1')
          && same_path(t2, t2')
          && !(keep_last && rem' == [])) {
        rem';
      } else {
        [(t1, t1'), (t2, t2'), ...rem'];
      };
    }
  | _ => [];

let rec type_path_list = ppf =>
  fun
  | [(tp, tp')] => type_path_expansion(tp, ppf, tp')
  | [(tp, tp'), ...rem] =>
    fprintf(
      ppf,
      "%a@;<2 0>%a",
      type_path_expansion(tp),
      tp',
      type_path_list,
      rem,
    )
  | [] => ();

/* Hide variant name and var, to force printing the expanded type */
let hide_variant_name = t =>
  switch (repr(t)) {
  | _ => t
  };

let prepare_expansion = ((t, t')) => {
  let t' = hide_variant_name(t');
  mark_loops(t);
  if (!same_path(t, t')) {
    mark_loops(t');
  };
  (t, t');
};

let may_prepare_expansion = (compact, (t, t')) =>
  switch (repr(t').desc) {
  | _ => prepare_expansion((t, t'))
  };

let print_tags = (ppf, fields) =>
  switch (fields) {
  | [] => ()
  | [(t, _), ...fields] =>
    fprintf(ppf, "`%s", t);
    List.iter(((t, _)) => fprintf(ppf, ",@ `%s", t), fields);
  };

let is_unit = (env, ty) =>
  switch (Ctype.expand_head(env, ty).desc) {
  | TTyConstr(p, _, _) => false /*Path.same p Predef.path_unit*/
  | _ => false
  };

let unifiable = (env, ty1, ty2) => {
  let snap = Btype.snapshot();
  let res =
    try(
      {
        Ctype.unify(env, ty1, ty2);
        true;
      }
    ) {
    | Unify(_) => false
    };

  Btype.backtrack(snap);
  res;
};

let explanation = (env, unif, t3, t4): option(Format.formatter => unit) =>
  switch (t3.desc, t4.desc) {
  /*| TTyArrow (ty1, ty2, _), _
    when is_unit env ty1 && unifiable env ty2 t4 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[Hint: Did you forget to provide `()' as argument?@]")
    | _, Tarrow (_, ty1, ty2, _)
    when is_unit env ty1 && unifiable env t3 ty2 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[Hint: Did you forget to wrap the expression using `fun () ->'?@]")*/
  | (TTyConstr(p, _, _), TTyVar(_))
      when unif && t4.level < Path.binding_time(p) =>
    Some(
      ppf =>
        fprintf(
          ppf,
          "@,@[The type constructor@;<1 2>%a@ would escape its scope@]",
          path,
          p,
        ),
    )
  | (TTyVar(_), TTyConstr(p, _, _))
      when unif && t3.level < Path.binding_time(p) =>
    Some(
      ppf =>
        fprintf(
          ppf,
          "@,@[The type constructor@;<1 2>%a@ would escape its scope@]",
          path,
          p,
        ),
    )
  | (TTyVar(_), TTyUniVar(_))
  | (TTyUniVar(_), TTyVar(_)) =>
    Some(
      ppf =>
        fprintf(
          ppf,
          "@,The universal variable %a would escape its scope",
          type_expr,
          if (is_Tunivar(t3)) {
            t3;
          } else {
            t4;
          },
        ),
    )
  | (TTyVar(_), _)
  | (_, TTyVar(_)) =>
    Some(
      ppf => {
        let (t, t') =
          if (is_Tvar(t3)) {
            (t3, t4);
          } else {
            (t4, t3);
          };
        if (occur_in(Env.empty, t, t')) {
          fprintf(
            ppf,
            "@,@[<hov>The type variable %a occurs inside@ %a@]",
            type_expr,
            t,
            type_expr,
            t',
          );
        } else {
          fprintf(
            ppf,
            "@,@[<hov>This instance of %a is ambiguous:@ %s@]",
            type_expr,
            t',
            "it would escape the scope of its equation",
          );
        };
      },
    )
  | _ => None
  };

let rec mismatch = (env, unif) =>
  fun
  | [(_, t), (_, t'), ...rem] =>
    switch (mismatch(env, unif, rem)) {
    | Some(_) as m => m
    | None => explanation(env, unif, t, t')
    }
  | [] => None
  | _ => assert(false);

let explain = (mis, ppf) =>
  switch (mis) {
  | None => ()
  | Some(explain) => explain(ppf)
  };

let warn_on_missing_def = (env, ppf, t) =>
  switch (t.desc) {
  | TTyConstr(p, _, _) =>
    try(ignore(Env.find_type(p, env): Types.type_declaration)) {
    | Not_found =>
      fprintf(
        ppf,
        "@,@[%a is abstract because no corresponding cmi file was found in path.@]",
        path,
        p,
      )
    }
  | _ => ()
  };

let ident_same_name = (id1, id2) =>
  if (Ident.equal(id1, id2) && !Ident.same(id1, id2)) {
    add_unique(id1);
    add_unique(id2);
  };

let rec path_same_name = (p1, p2) =>
  switch (p1, p2) {
  | (PIdent(id1), PIdent(id2)) => ident_same_name(id1, id2)
  | (PExternal(p1, s1), PExternal(p2, s2)) when s1 == s2 =>
    path_same_name(p1, p2)
  | _ => ()
  };

let type_same_name = (t1, t2) =>
  switch (repr(t1).desc, repr(t2).desc) {
  | (TTyConstr(p1, _, _), TTyConstr(p2, _, _)) =>
    path_same_name(fst(best_type_path(p1)), fst(best_type_path(p2)))
  | _ => ()
  };

let rec trace_same_names =
  fun
  | [(t1, t1'), (t2, t2'), ...rem] => {
      type_same_name(t1, t2);
      type_same_name(t1', t2');
      trace_same_names(rem);
    }
  | _ => ();

let unification_error =
    (env, unif, tr, txt1, ppf, txt2, ty_expect_explanation) => {
  reset();
  trace_same_names(tr);
  let tr = List.map(((t, t')) => (t, hide_variant_name(t')), tr);
  let mis = mismatch(env, unif, tr);
  switch (tr) {
  | []
  | [_] => assert(false)
  | [t1, t2, ...tr] =>
    try({
      let tr = filter_trace(mis == None, tr);
      let (t1, t1') = may_prepare_expansion(tr == [], t1)
      and (t2, t2') = may_prepare_expansion(tr == [], t2);
      print_labels := true;
      let tr = List.map(prepare_expansion, tr);
      fprintf(
        ppf,
        "@[<v>@[%t@;<1 2>%a@ %t@;<1 2>%a%t@]%a%t@]",
        txt1,
        type_expansion(t1),
        t1',
        txt2,
        type_expansion(t2),
        t2',
        ty_expect_explanation,
        trace(false, "is not compatible with type"),
        tr,
        explain(mis),
      );
      if (env != Env.empty) {
        warn_on_missing_def(env, ppf, t1);
        warn_on_missing_def(env, ppf, t2);
      };
      print_labels := true;
    }) {
    | exn =>
      print_labels := true;
      raise(exn);
    }
  };
};

let report_unification_error =
    (ppf, env, ~unif=true, tr, ~type_expected_explanation=_ => (), txt1, txt2) =>
  wrap_printing_env(~error=true, env, () =>
    unification_error(
      env,
      unif,
      tr,
      txt1,
      ppf,
      txt2,
      type_expected_explanation,
    )
  );

let trace = (fst, keep_last, txt, ppf, tr) => {
  print_labels := true;
  trace_same_names(tr);
  try(
    switch (tr) {
    | [t1, t2, ...tr'] =>
      if (fst) {
        trace(fst, txt, ppf, [t1, t2, ...filter_trace(keep_last, tr')]);
      } else {
        trace(fst, txt, ppf, filter_trace(keep_last, tr));
      };
      print_labels := true;
    | _ => ()
    }
  ) {
  | exn =>
    print_labels := true;
    raise(exn);
  };
};

let report_ambiguous_type_error =
    (ppf, env, (tp0, tp0'), tpl, txt1, txt2, txt3) =>
  wrap_printing_env(
    ~error=true,
    env,
    () => {
      reset();
      List.iter(
        ((tp, tp')) => {
          path_same_name(tp0, tp);
          path_same_name(tp0', tp');
        },
        tpl,
      );
      switch (tpl) {
      | [] => assert(false)
      | [(tp, tp')] =>
        fprintf(
          ppf,
          "@[%t@;<1 2>%a@ %t@;<1 2>%a@]",
          txt1,
          type_path_expansion(tp),
          tp',
          txt3,
          type_path_expansion(tp0),
          tp0',
        )
      | _ =>
        fprintf(
          ppf,
          "@[%t@;<1 2>@[<hv>%a@]@ %t@;<1 2>%a@]",
          txt2,
          type_path_list,
          tpl,
          txt3,
          type_path_expansion(tp0),
          tp0',
        )
      };
    },
  );
