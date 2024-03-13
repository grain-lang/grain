/** Typing of patterns */;
/* Taken from OCaml's typing/typecore.ml module. Original copyright: */
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

open Grain_parsing;
open Grain_utils;
open Misc;
open Asttypes;
open Parsetree;
open Types;
open Typedtree;
open Btype;
open Ctype;
open Checkertypes;
open Disambiguation;

type error =
  | ConstructorArityMismatch(Identifier.t, int, int)
  | PatternTypeClash(list((type_expr, type_expr)))
  | LabelMismatch(Identifier.t, list((type_expr, type_expr)))
  | LabelMultiplyDefined(string)
  | ExprTypeClash(
      list((type_expr, type_expr)),
      option(type_forcing_context),
    )
  | RecursiveLocalConstraint(list((type_expr, type_expr)))
  | MultiplyBoundVariable(string)
  | ModulesNotAllowed
  | UnexpectedExistential
  | OrpatVars(Ident.t, list(Ident.t))
  | OrPatternTypeClash(Ident.t, list((type_expr, type_expr)))
  | UnrefutedPattern(pattern)
  | InlineRecordPatternMisuse(Identifier.t, string, string);

let ident_empty = {
  txt: Identifier.IdentName(Location.mknoloc("[]")),
  loc: Location.dummy_loc,
};
let ident_cons = {
  txt: Identifier.IdentName(Location.mknoloc("[...]")),
  loc: Location.dummy_loc,
};

exception Error(Location.t, Env.t, error);

let iter_ppat = (f, p) =>
  switch (p.ppat_desc) {
  | PPatAny
  | PPatVar(_)
  | PPatConstant(_)
  | PPatConstruct(_, PPatConstrSingleton) => ()
  | PPatTuple(lst) => List.iter(f, lst)
  | PPatList(lst) =>
    List.iter(
      item => {
        switch (item) {
        | ListItem(p) => f(p)
        | ListSpread(p, _) => f(p)
        }
      },
      lst,
    )
  | PPatArray(lst) => List.iter(f, lst)
  | PPatRecord(fs, _)
  | PPatConstruct(_, PPatConstrRecord(fs, _)) =>
    List.iter(((_, p)) => f(p), fs)
  | PPatAlias(p, _)
  | PPatConstraint(p, _) => f(p)
  | PPatConstruct(_, PPatConstrTuple(lst)) => List.iter(f, lst)
  | PPatOr(p1, p2) =>
    f(p1);
    f(p2);
  };

let map_fold_cont = (f, xs, k) =>
  List.fold_right(
    (x, k, ys) => f(x, y => k([y, ...ys])),
    xs,
    ys => k(List.rev(ys)),
    [],
  );

/* unification inside type_pat*/
let unify_pat_types = (loc, env, ty, ty') =>
  try(unify(env, ty, ty')) {
  | Unify(trace) => raise(Error(loc, env, PatternTypeClash(trace)))
  };

/* unification inside type_exp and type_expect */
let unify_exp_types = (loc, env, ty, expected_ty) =>
  /* Format.eprintf "@[%a@ %a@]@." Printtyp.raw_type_expr exp.exp_type
     Printtyp.raw_type_expr expected_ty; */
  try(unify(env, ty, expected_ty)) {
  | Unify(trace) => raise(Error(loc, env, ExprTypeClash(trace, None)))
  };

/* level at which to create the local type declarations */
let newtype_level = ref(None);
let get_newtype_level = () =>
  switch (newtype_level^) {
  | Some(y) => y
  | None => assert(false)
  };

let unify_pat_types_gadt = (loc, env, ty, ty') => {
  let newtype_level =
    switch (newtype_level^) {
    | None => assert(false)
    | Some(x) => x
    };

  try(unify_gadt(~newtype_level, env, ty, ty')) {
  | Unify(trace) => raise(Error(loc, env^, PatternTypeClash(trace)))
  | Unification_recursive_abbrev(trace) =>
    raise(Error(loc, env^, RecursiveLocalConstraint(trace)))
  };
};

/* Creating new conjunctive types is not allowed when typing patterns */

let unify_pat = (env, pat, expected_ty) =>
  unify_pat_types(pat.pat_loc, env, pat.pat_type, expected_ty);

/* make all Reither present in open variants */
let finalize_variant = pat =>
  switch (pat.pat_desc) {
  | _ => ()
  };

let rec iter_pattern = (f, p) => {
  f(p);
  iter_pattern_desc(iter_pattern(f), p.pat_desc);
};

let has_variants = p => false;

/* pattern environment */
let pattern_variables =
  ref(
    []:
        list(
          (Ident.t, type_expr, loc(string), Location.t, bool) /* as-variable */,
        ),
  );
let pattern_force = ref([]: list(unit => unit));
let pattern_scope = ref(None: option(Annot.ident));
let allow_modules = ref(false);
let module_variables = ref([]: list((loc(string), Location.t)));
let reset_pattern = (scope, allow) => {
  pattern_variables := [];
  pattern_force := [];
  pattern_scope := scope;
  allow_modules := allow;
  module_variables := [];
};

let enter_variable = (~is_module=false, ~is_as_variable=false, loc, name, ty) => {
  if (List.exists(
        ((id, _, _, _, _)) => Ident.name(id) == name.txt,
        pattern_variables^,
      )) {
    raise(Error(loc, Env.empty, MultiplyBoundVariable(name.txt)));
  };
  let id = Ident.create(name.txt);
  pattern_variables :=
    [(id, ty, name, loc, is_as_variable), ...pattern_variables^];
  if (is_module) {
    /* Note: unpack patterns enter a variable of the same name */
    if (! allow_modules^) {
      raise(Error(loc, Env.empty, ModulesNotAllowed));
    };
    module_variables := [(name, loc), ...module_variables^];
  } else {
    (); /* moved to genannot */
      /*may (fun s -> Stypes.record (Stypes.An_ident (name.loc, name.txt, s)))
        !pattern_scope;*/
  };
  id;
};

let sort_pattern_variables = vs =>
  List.sort(
    ((x, _, _, _, _), (y, _, _, _, _)) =>
      Stdlib.compare(Ident.name(x), Ident.name(y)),
    vs,
  );

let enter_orpat_variables = (loc, env, p1_vs, p2_vs) => {
  /* unify_vars operate on sorted lists */

  let p1_vs = sort_pattern_variables(p1_vs)
  and p2_vs = sort_pattern_variables(p2_vs);

  let rec unify_vars = (p1_vs, p2_vs) => {
    let vars = vs => List.map(((x, _t, _, _l, _a)) => x, vs);
    switch (p1_vs, p2_vs) {
    | ([(x1, t1, _, _l1, _a1), ...rem1], [(x2, t2, _, _l2, _a2), ...rem2])
        when Ident.equal(x1, x2) =>
      if (x1 === x2) {
        unify_vars(rem1, rem2);
      } else {
        try(unify(env, t1, t2)) {
        | Unify(trace) =>
          raise(Error(loc, env, OrPatternTypeClash(x1, trace)))
        };
        [(x2, x1), ...unify_vars(rem1, rem2)];
      }
    | ([], []) => []
    | ([(x, _, _, _, _), ..._], []) =>
      raise(Error(loc, env, OrpatVars(x, [])))
    | ([], [(y, _, _, _, _), ..._]) =>
      raise(Error(loc, env, OrpatVars(y, [])))
    | ([(x, _, _, _, _), ..._], [(y, _, _, _, _), ..._]) =>
      let err =
        if (Ident.name(x) < Ident.name(y)) {
          OrpatVars(x, vars(p2_vs));
        } else {
          OrpatVars(y, vars(p1_vs));
        };
      raise(Error(loc, env, err));
    };
  };
  unify_vars(p1_vs, p2_vs);
};

let rec build_as_type = (env, p) =>
  switch (p.pat_desc) {
  | TPatAlias(p1, _, _) => build_as_type(env, p1)
  | TPatTuple(pl) =>
    let tyl = List.map(build_as_type(env), pl);
    newty(TTyTuple(tyl));
  | TPatRecord(lpl, _) =>
    let lbl = snd3(List.hd(lpl));
    let ty = newvar();
    let ppl = List.map(((_, l, p)) => (l.lbl_pos, p), lpl);
    let do_label = lbl => {
      let (_, ty_arg, ty_res) = instance_label(false, lbl);
      unify_pat(env, {...p, pat_type: ty}, ty_res);
      let refinable =
        List.mem_assoc(lbl.lbl_pos, ppl)
        && (
          switch (repr(lbl.lbl_arg).desc) {
          | TTyPoly(_) => false
          | _ => true
          }
        );
      if (refinable) {
        let arg = List.assoc(lbl.lbl_pos, ppl);
        unify_pat(env, {...arg, pat_type: build_as_type(env, arg)}, ty_arg);
      } else {
        let (_, ty_arg', ty_res') = instance_label(false, lbl);
        unify(env, ty_arg, ty_arg');
        unify_pat(env, p, ty_res');
      };
    };
    Array.iter(do_label, lbl.lbl_all);
    ty;
  | TPatConstruct(_, cstr, pl) =>
    let keep = cstr.cstr_existentials != [];
    if (keep) {
      p.pat_type;
    } else {
      let tyl = List.map(build_as_type(env), pl);
      let (ty_args, ty_res) = instance_constructor(cstr);
      List.iter2(
        ((p, ty)) => unify_pat(env, {...p, pat_type: ty}),
        List.combine(pl, tyl),
        ty_args,
      );
      ty_res;
    };
  | TPatOr(p1, p2) =>
    let ty1 = build_as_type(env, p1)
    and ty2 = build_as_type(env, p2);
    unify_pat(env, {...p2, pat_type: ty2}, ty1);
    ty1;
  | TPatAny
  | TPatVar(_)
  | TPatArray(_)
  | TPatConstant(_) => p.pat_type
  };

/* Remember current state for backtracking.
   No variable information, as we only backtrack on
   patterns without variables (cf. assert statements). */
type state = {
  snapshot: Btype.snapshot,
  levels: Ctype.levels,
  env: Env.t,
};
let save_state = env => {
  snapshot: Btype.snapshot(),
  levels: Ctype.save_levels(),
  env: env^,
};
let set_state = (s, env) => {
  Btype.backtrack(s.snapshot);
  Ctype.set_levels(s.levels);
  env := s.env;
};

/* type_pat does not generate local constraints inside or patterns */
type type_pat_mode =
  | Normal
  | Splitting_or /* splitting an or-pattern */
  | Inside_or /* inside a non-split or-pattern */
  | Split_or; /* always split or-patterns */

exception Need_backtrack;

let extract_concrete_variant = (env, ty) =>
  switch (extract_concrete_typedecl(env, ty)) {
  | (p0, p, {type_kind: TDataVariant(cstrs)}) => (p0, p, cstrs)
  | _ => raise(Not_found)
  };

let extract_concrete_record = (env, ty) =>
  switch (extract_concrete_typedecl(env, ty)) {
  | (p0, p, {type_kind: TDataRecord(fields)}) => (p0, p, fields)
  | _ => raise(Not_found)
  };

let extract_label_names = (env, ty) =>
  try({
    let (_, _, fields) = extract_concrete_record(env, ty);
    List.map(l => l.Types.rf_name, fields);
  }) {
  | Not_found => assert(false)
  };

/* Checks over the labels mentioned in a record pattern:
   no duplicate definitions (error); properly closed (warning) */

let check_recordpat_labels = (loc, lbl_pat_list, closed) =>
  switch (lbl_pat_list) {
  | [] => () /* should not happen */
  | [(_, label1, _), ..._] =>
    let all = label1.lbl_all;
    let defined = Array.make(Array.length(all), false);
    let check_defined = ((_, label, _)) =>
      if (defined[label.lbl_pos]) {
        raise(Error(loc, Env.empty, LabelMultiplyDefined(label.lbl_name)));
      } else {
        defined[label.lbl_pos] = true;
      };
    List.iter(check_defined, lbl_pat_list);
    if (closed == Closed
        && Warnings.is_active(Warnings.NonClosedRecordPattern(""))) {
      let undefined = ref([]);
      for (i in 0 to Array.length(all) - 1) {
        if (!defined[i]) {
          undefined := [all[i].lbl_name, ...undefined^];
        };
      };
      if (undefined^ != []) {
        let u = String.concat(", ", List.rev(undefined^));
        Location.prerr_warning(loc, Warnings.NonClosedRecordPattern(u));
      };
    };
  };

let rec find_record_qual =
  fun
  | [] => None
  | [({txt: Identifier.IdentExternal(modname, _)}, _), ..._] =>
    Some(modname)
  | [_, ...rest] => find_record_qual(rest);

let type_label_a_list =
    (~labels=?, loc, closed, env, type_lbl_a, opath, lid_a_list, k) => {
  let lbl_a_list =
    switch (lid_a_list, labels) {
    | ([({txt: Identifier.IdentName(s)}, _), ..._], Some(labels))
        when Hashtbl.mem(labels, s) =>
      /* Special case for rebuilt syntax trees */
      List.map(
        fun
        | (lid, a) =>
          switch (lid.txt) {
          | Identifier.IdentName(s) => (lid, Hashtbl.find(labels, s), a)
          | _ => assert(false)
          },
        lid_a_list,
      )
    | _ =>
      let lid_a_list =
        switch (find_record_qual(lid_a_list)) {
        | None => lid_a_list
        | Some(modname) =>
          List.map(
            ((lid, a) as lid_a) =>
              switch (lid.txt) {
              | Identifier.IdentName(s) => (
                  {...lid, txt: Identifier.IdentExternal(modname, s)},
                  a,
                )
              | _ => lid_a
              },
            lid_a_list,
          )
        };

      disambiguate_lid_a_list(loc, closed, env, opath, lid_a_list);
    };

  /* Invariant: records are sorted in the typed tree */
  let lbl_a_list =
    List.sort(
      ((_, lbl1, _), (_, lbl2, _)) => compare(lbl1.lbl_pos, lbl2.lbl_pos),
      lbl_a_list,
    );

  map_fold_cont(type_lbl_a, lbl_a_list, k);
};

/* type_pat propagates the expected type as well as maps for
   constructors and labels.
   Unification may update the typing environment. */
/* constrs <> None => called from parmatch: backtrack on or-patterns
   explode > 0 => explode Ppat_any for gadts */
let rec type_pat =
        (
          ~constrs,
          ~labels,
          ~no_existentials,
          ~mode,
          ~explode,
          ~env,
          sp,
          expected_ty,
          k,
        ) =>
  type_pat_aux(
    ~constrs,
    ~labels,
    ~no_existentials,
    ~mode,
    ~explode,
    ~env,
    sp,
    expected_ty,
    k,
  )
/*Builtin_attributes.warning_scope sp.ppat_attributes
  (fun () ->
     type_pat_aux ~constrs ~labels ~no_existentials ~mode ~explode ~env
       sp expected_ty k
  )*/

and type_pat_aux =
    (
      ~constrs,
      ~labels,
      ~no_existentials,
      ~mode,
      ~explode,
      ~env,
      sp,
      expected_ty,
      k,
    ) => {
  let mode' =
    if (mode == Splitting_or) {
      Normal;
    } else {
      mode;
    };
  let type_pat =
      (
        ~constrs=constrs,
        ~labels=labels,
        ~mode=mode',
        ~explode=explode,
        ~env=env,
      ) =>
    type_pat(~constrs, ~labels, ~no_existentials, ~mode, ~explode, ~env);
  let loc = sp.ppat_loc;
  let unif = (x: pattern): pattern => {
    unify_pat(env^, x, instance(env^, expected_ty));
    x;
  };

  let rp = (k, x): pattern =>
    if (constrs == None) {
      k(rp(x));
    } else {
      k(x);
    };
  switch (sp.ppat_desc) {
  | PPatAny =>
    let k' = d =>
      rp(
        k,
        {
          pat_desc: d,
          pat_loc: loc,
          pat_extra: [],
          pat_type: expected_ty,
          pat_env: env^,
        },
      );

    if (explode > 0) {
      let (sp, constrs) = Parmatch.ppat_of_type(env^, expected_ty);
      if (sp.ppat_desc == Parsetree.PPatAny) {
        k'(TPatAny);
      } else if (mode == Inside_or) {
        raise(Need_backtrack);
      } else {
        let explode =
          switch (sp.ppat_desc) {
          /*Parsetree.Ppat_or _ -> explode - 5*/
          | _ => explode - 1
          };

        type_pat(
          ~constrs=Some(constrs), /*~labels:(Some labels)*/
          ~explode,
          sp,
          expected_ty,
          k,
        );
      };
    } else {
      k'(TPatAny);
    };
  | PPatVar(name) =>
    let (_, exp_constrs) = Parmatch.ppat_of_type(env^, expected_ty);
    let constructor_candidates =
      switch (name.txt, constrs) {
      | (s, Some(constrs)) when Hashtbl.mem(exp_constrs, s) => [
          (Hashtbl.find(exp_constrs, s), (() => ())),
        ]
      | _ =>
        Typetexp.find_all_constructors(
          env^,
          name.loc,
          Identifier.IdentName(name),
        )
      };

    /* Special case: If the name shadows a constructor, assume it's a constructor use. */
    switch (constructor_candidates) {
    | [_, ..._] =>
      if (Grain_utils.Config.verbose^) {
        Printf.eprintf(
          "Re-interpreting pattern variable '%s' as a constructor\n",
          name.txt,
        );
      };
      type_pat_aux(
        ~constrs,
        ~labels,
        ~no_existentials,
        ~mode,
        ~explode,
        ~env,
        {
          ...sp,
          ppat_desc:
            PPatConstruct(
              Location.mkloc(Identifier.IdentName(name), name.loc),
              PPatConstrTuple([]),
            ),
        },
        expected_ty,
        k,
      );
    | [] =>
      let id =
        /* PR#7330 */
        if (name.txt == "*extension*") {
          Ident.create(name.txt);
        } else {
          enter_variable(loc, name, expected_ty);
        };

      rp(
        k,
        {
          pat_desc: TPatVar(id, name),
          pat_loc: loc,
          pat_extra: [],
          pat_type: expected_ty,
          pat_env: env^,
        },
      );
    };
  | PPatConstraint(
      {ppat_desc: PPatVar(name), ppat_loc: lloc},
      {ptyp_desc: PTyPoly(_)} as sty,
    ) =>
    /* explicitly polymorphic type */
    assert(constrs == None);
    let (cty, force) = Typetexp.transl_simple_type_delayed(env^, sty);
    let ty = cty.ctyp_type;
    unify_pat_types(lloc, env^, ty, expected_ty);
    pattern_force := [force, ...pattern_force^];
    switch (ty.desc) {
    | TTyPoly(body, tyl) =>
      begin_def();
      let (_, ty') = instance_poly(~keep_names=true, false, tyl, body);
      end_def();
      generalize(ty');
      let id = enter_variable(lloc, name, ty');
      rp(
        k,
        {
          pat_desc: TPatVar(id, name),
          pat_loc: lloc,
          pat_extra: [(TPatConstraint(cty), loc)],
          pat_type: ty,
          pat_env: env^,
        },
      );
    | _ => assert(false)
    };
  | PPatAlias(sq, name) =>
    assert(constrs == None);
    type_pat(
      sq,
      expected_ty,
      q => {
        begin_def();
        let ty_var = build_as_type(env^, q);
        end_def();
        generalize(ty_var);
        let id = enter_variable(~is_as_variable=true, loc, name, ty_var);
        rp(
          k,
          {
            pat_desc: TPatAlias(q, id, name),
            pat_loc: loc,
            pat_extra: [],
            pat_type: q.pat_type,
            pat_env: env^,
          },
        );
      },
    );
  | PPatConstant(cst) =>
    let cst = constant_or_raise(env^, loc, cst);
    unify_pat_types(loc, env^, type_constant(cst), expected_ty);
    rp(
      k,
      {
        pat_desc: TPatConstant(cst),
        pat_loc: loc,
        pat_extra: [],
        pat_type: expected_ty,
        pat_env: env^,
      },
    );
  | PPatTuple(spl) =>
    /*assert (List.length spl >= 2);*/
    let spl_ann = List.map(p => (p, newvar()), spl);
    let ty = newty(TTyTuple(List.map(snd, spl_ann)));
    unify_pat_types(loc, env^, ty, expected_ty);
    map_fold_cont(
      ((p, t)) => type_pat(p, t),
      spl_ann,
      pl =>
        rp(
          k,
          {
            pat_desc: TPatTuple(pl),
            pat_loc: loc,
            pat_extra: [],
            pat_type: expected_ty,
            pat_env: env^,
          },
        ),
    );
  | PPatList(spl) =>
    let convert_list = (~loc, a) => {
      open Ast_helper;
      let empty = Pattern.tuple_construct(~loc, ident_empty, []);
      let a = List.rev(a);
      switch (a) {
      | [] => empty
      | [base, ...rest] =>
        let base =
          switch (base) {
          | ListItem(pat) =>
            Pattern.tuple_construct(~loc, ident_cons, [pat, empty])
          | ListSpread(pat, _) => pat
          };
        List.fold_left(
          (acc, pat) => {
            switch (pat) {
            | ListItem(pat) =>
              Pattern.tuple_construct(~loc, ident_cons, [pat, acc])
            | ListSpread(_, loc) =>
              raise(
                SyntaxError(
                  loc,
                  "A list spread can only appear at the end of a list.",
                ),
              )
            }
          },
          base,
          rest,
        );
      };
    };
    type_pat(
      ~constrs,
      ~labels,
      ~mode=mode',
      ~explode,
      ~env,
      convert_list(~loc=sp.ppat_loc, spl),
      expected_ty,
      k,
    );
  | PPatArray(spl) =>
    let arr_ty = newgenvar();
    unify_pat_types(
      loc,
      env^,
      Builtin_types.type_array(arr_ty),
      expected_ty,
    );
    map_fold_cont(
      p => type_pat(p, arr_ty),
      spl,
      pl =>
        rp(
          k,
          {
            pat_desc: TPatArray(pl),
            pat_loc: loc,
            pat_extra: [],
            pat_type: expected_ty,
            pat_env: env^,
          },
        ),
    );
  | PPatRecord(lid_pat_list, closed) =>
    assert(lid_pat_list != []);
    let (opath, record_ty) =
      try({
        let (p0, p, _) = extract_concrete_record(env^, expected_ty);
        begin_def();
        let ty = instance(env^, expected_ty);
        end_def();
        generalize_structure(ty);
        (Some((p0, p, true)), ty);
      }) {
      | Not_found => (None, newvar())
      };

    let type_label_pat = ((label_lid, label, sarg), k) => {
      begin_def();
      let (_, ty_arg, ty_res) = instance_label(false, label);
      try(unify_pat_types(loc, env^, ty_res, instance(env^, record_ty))) {
      | Error(_loc, _env, PatternTypeClash(cl)) =>
        raise(Error(label_lid.loc, env^, LabelMismatch(label_lid.txt, cl)))
      };
      end_def();
      generalize_structure(ty_res);
      generalize_structure(ty_arg);
      type_pat(sarg, ty_arg, arg => k((label_lid, label, arg)));
    };

    let make_record_pat = lbl_pat_list => {
      check_recordpat_labels(loc, lbl_pat_list, closed);
      {
        pat_desc: TPatRecord(lbl_pat_list, closed),
        pat_loc: loc,
        pat_extra: [],
        pat_type: instance(env^, record_ty),
        pat_env: env^,
      };
    };

    let k' = pat => rp(k, unif(pat));
    k'(
      wrap_disambiguate(
        "This record pattern is expected to have",
        mk_expected(expected_ty),
        type_label_a_list(
          loc,
          false,
          env^,
          type_label_pat,
          opath,
          lid_pat_list,
        ),
        make_record_pat,
      ),
    );
  | PPatConstruct(lid, sarg) =>
    let (sargs, is_record_pat) =
      switch (sarg) {
      | PPatConstrSingleton => ([], false)
      | PPatConstrTuple(sargs) => (sargs, false)
      | PPatConstrRecord(rfs, c) =>
        let desc =
          switch (rfs) {
          // trick to get `C { _ }` pattern to work properly
          | [] => PPatAny
          | _ => PPatRecord(rfs, c)
          };
        ([{ppat_desc: desc, ppat_loc: loc}], true);
      };
    let opath =
      try({
        let (p0, p, _) = extract_concrete_variant(env^, expected_ty);
        Some((p0, p, true));
      }) {
      | Not_found => None
      };

    let candidates =
      switch (lid.txt, constrs) {
      | (Identifier.IdentName({txt: s}), Some(constrs))
          when Hashtbl.mem(constrs, s) => [
          (Hashtbl.find(constrs, s), (() => ())),
        ]
      | _ => Typetexp.find_all_constructors(env^, lid.loc, lid.txt)
      };

    /*let check_lk tpath constr =
      if constr.cstr_generalized then
        raise (Error (lid.loc, !env,
                      Unqualified_gadt_pattern (tpath, constr.cstr_name)))
      in*/
    let constr =
      wrap_disambiguate(
        "This variant pattern is expected to have",
        mk_expected(expected_ty),
        Constructor.disambiguate(lid, env^, opath),
        candidates,
      );

    /*if constr.cstr_generalized && constrs <> None && mode = Inside_or
      then raise Need_backtrack;*/
    /*Env.mark_constructor Env.Pattern !env (Identifier.last lid.txt) constr;*/
    /*Builtin_attributes.check_deprecated loc constr.cstr_attributes
      constr.cstr_name;*/
    if (no_existentials && constr.cstr_existentials != []) {
      raise(Error(loc, env^, UnexpectedExistential));
    };
    /* if constructor is gadt, we must verify that the expected type has the
       correct head */
    /*if constr.cstr_generalized then
      unify_head_only loc !env expected_ty constr;*/
    /*begin match sargs with
      | [{ppat_desc = Ppat_constant _} as sp]
        when Builtin_attributes.warn_on_literal_pattern
            constr.cstr_attributes ->
          Location.prerr_warning sp.ppat_loc
            Warnings.Fragile_literal_pattern
      | _ -> ()
      end;*/
    let is_record_cstr = constr.cstr_inlined != None;
    if (is_record_pat != is_record_cstr) {
      raise(
        Error(
          loc,
          env^,
          InlineRecordPatternMisuse(
            lid.txt,
            if (is_record_cstr) {"record"} else {"tuple"},
            if (is_record_pat) {"record"} else {"tuple"},
          ),
        ),
      );
    };
    if (List.length(sargs) != constr.cstr_arity) {
      raise(
        Error(
          loc,
          env^,
          ConstructorArityMismatch(
            lid.txt,
            constr.cstr_arity,
            List.length(sargs),
          ),
        ),
      );
    };
    let (ty_args, ty_res) =
      instance_constructor(~in_pattern=(env, get_newtype_level()), constr);

    /* PR#7214: do not use gadt unification for toplevel lets */
    /*if not constr.cstr_generalized || mode = Inside_or || no_existentials
      then <copied below>
        else unify_pat_types_gadt loc env ty_res expected_ty;*/
    unify_pat_types(loc, env^, ty_res, expected_ty);

    /*let rec check_non_escaping p =
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
      if constr.cstr_inlined <> None then List.iter check_non_escaping sargs;**/

    map_fold_cont(
      ((p, t)) => type_pat(p, t),
      List.combine(sargs, ty_args),
      args =>
        rp(
          k,
          {
            pat_desc: TPatConstruct(lid, constr, args),
            pat_loc: loc,
            pat_extra: [],
            pat_type: expected_ty,
            pat_env: env^,
          },
        ),
    );
  | PPatOr(sp1, sp2) =>
    let state = save_state(env);
    switch (
      {
        if (mode == Split_or || mode == Splitting_or) {
          raise(Need_backtrack);
        };
        let initial_pattern_variables = pattern_variables^;
        let initial_module_variables = module_variables^;
        let p1 =
          try(Some(type_pat(~mode=Inside_or, sp1, expected_ty, x => x))) {
          | Need_backtrack => None
          };
        let p1_variables = pattern_variables^;
        let p1_module_variables = module_variables^;
        pattern_variables := initial_pattern_variables;
        module_variables := initial_module_variables;
        let p2 =
          try(Some(type_pat(~mode=Inside_or, sp2, expected_ty, x => x))) {
          | Need_backtrack => None
          };
        let p2_variables = pattern_variables^;
        switch (p1, p2) {
        | (None, None) => raise(Need_backtrack)
        | (Some(p), None)
        | (None, Some(p)) => p /* No variables */
        | (Some(p1), Some(p2)) =>
          let alpha_env =
            enter_orpat_variables(loc, env^, p1_variables, p2_variables);
          pattern_variables := p1_variables;
          module_variables := p1_module_variables;
          {
            pat_desc: TPatOr(p1, alpha_pat(alpha_env, p2)),
            pat_loc: loc,
            pat_extra: [],
            pat_type: expected_ty,
            pat_env: env^,
          };
        };
      }
    ) {
    | p => rp(k, p)
    | exception Need_backtrack when mode != Inside_or =>
      assert(constrs != None);
      set_state(state, env);
      let mode =
        if (mode == Split_or) {
          mode;
        } else {
          Splitting_or;
        };
      try(type_pat(~mode, sp1, expected_ty, k)) {
      | Error(_) =>
        set_state(state, env);
        type_pat(~mode, sp2, expected_ty, k);
      };
    };
  | PPatConstraint(sp, sty) =>
    begin_def();
    let (cty, force) = Typetexp.transl_simple_type_delayed(env^, sty);
    let ty = cty.ctyp_type;
    let (ty, expected_ty') = {
      end_def();
      generalize_structure(ty);
      (instance(env^, ty), instance(env^, ty));
    };

    unify_pat_types(loc, env^, ty, expected_ty);
    type_pat(
      sp,
      expected_ty',
      p => {
        /*Format.printf "%a@.%a@."
          Printtyp.raw_type_expr ty
          Printtyp.raw_type_expr p.pat_type;*/
        pattern_force := [force, ...pattern_force^];
        let extra = (TPatConstraint(cty), loc);
        let p =
          switch (p.pat_desc) {
          | TPatVar(id, s) => {
              ...p,
              pat_type: ty,
              pat_desc: TPatAlias({...p, pat_desc: TPatAny}, id, s),
              pat_extra: [extra],
            }
          | _ => {...p, pat_type: ty, pat_extra: [extra, ...p.pat_extra]}
          };

        k(p);
      },
    );
  };
};

let type_pat =
    (
      ~allow_existentials=false,
      ~constrs=?,
      ~labels=?,
      ~mode=Normal,
      ~explode=0,
      ~lev=get_current_level(),
      env,
      sp,
      expected_ty,
    ) => {
  newtype_level := Some(lev);
  try({
    let r =
      type_pat(
        ~no_existentials=!allow_existentials,
        ~constrs,
        ~labels,
        ~mode,
        ~explode,
        ~env,
        sp,
        expected_ty,
        x =>
        x
      );
    iter_pattern(p => p.pat_env = env^, r);
    newtype_level := None;
    r;
  }) {
  | e =>
    newtype_level := None;
    raise(e);
  };
};

/* this function is passed to Partial.parmatch
   to type check gadt nonexhaustiveness */
let partial_pred = (~lev, ~mode=?, ~explode=?, env, expected_ty, constrs, p) => {
  let env = ref(env);
  let state = save_state(env);
  try(
    {
      reset_pattern(None, true);
      let typed_p =
        Ctype.with_passive_variants(
          type_pat(
            ~allow_existentials=true,
            ~lev,
            ~constrs,
            ~labels=None,
            ~mode?,
            ~explode?,
            env,
            p,
          ),
          expected_ty,
        );

      set_state(state, env);
      /* types are invalidated but we don't need them here */
      Some(typed_p);
    }
  ) {
  | Error(_) =>
    set_state(state, env);
    None;
  };
};

let check_partial = (~lev=get_current_level(), env, expected_ty, loc, cases) => {
  let explode =
    switch (cases) {
    | [_] => 5
    | _ => 0
    };
  Parmatch.check_partial(
    partial_pred(~lev, ~explode, env, expected_ty),
    loc,
    cases,
  );
};

let check_unused = (~lev=get_current_level(), env, expected_ty, cases) =>
  Parmatch.check_unused(
    (refute, constrs, spat) =>
      switch (
        partial_pred(
          ~lev,
          ~mode=Split_or,
          ~explode=5,
          env,
          expected_ty,
          constrs,
          spat,
        )
      ) {
      | Some(pat) when refute =>
        raise(Error(spat.ppat_loc, env, UnrefutedPattern(pat)))
      | r => r
      },
    cases,
  );

let add_pattern_variables =
    (~check=?, ~check_as=?, ~mut=false, ~global=false, env) => {
  let pv = get_ref(pattern_variables);
  (
    List.fold_right(
      ((id, ty, _name, loc, as_var), env) => {
        let check = if (as_var) {check_as} else {check};
        Env.add_value(
          ~check?,
          id,
          {
            val_type: ty,
            val_repr: Type_utils.repr_of_type(env, ty),
            val_kind: TValReg,
            Types.val_loc: loc,
            val_internalpath: Path.PIdent(id),
            val_fullpath: Path.PIdent(id),
            val_mutable: mut,
            val_global: global,
          },
          env,
        );
      },
      pv,
      env,
    ),
    get_ref(module_variables),
    pv,
  );
};

let type_pattern = (~lev, env, spat, scope, expected_ty) => {
  reset_pattern(/*scope*/ None, true);
  let new_env = ref(env);
  let pat =
    type_pat(~allow_existentials=true, ~lev, new_env, spat, expected_ty);
  let (new_env, unpacks, _) = add_pattern_variables(new_env^) /*~check:(fun s -> Warnings.Unused_var_strict s)  ~check_as:(fun s -> Warnings.Unused_var s)*/;

  (pat, new_env, get_ref(pattern_force), unpacks);
};

let type_pattern_list =
    (~mut=false, ~global=false, env, spatl, scope, expected_tys, allow) => {
  reset_pattern(/*scope*/ None, allow);
  let new_env = ref(env);
  let type_pat = ((attrs, pat), ty) => {
    /*Builtin_attributes.warning_scope ~ppwarning:false attrs
      (fun () ->
         type_pat new_env pat ty
      )*/
    /*Format.eprintf "@[Typing pat: %s@]@."
      (Sexplib.Sexp.to_string_hum (Parsetree.sexp_of_pattern pat));*/
    let ret = type_pat(new_env, pat, ty);
    /*Format.eprintf "@[Typed: %s [type: %a]@]@."
      (Sexplib.Sexp.to_string_hum (Typedtree.sexp_of_pattern ret))
      Printtyp.raw_type_expr ret.pat_type;*/
    ret;
  };

  let patl = List.map2(type_pat, spatl, expected_tys);
  let (new_env, unpacks, pv) =
    add_pattern_variables(~mut, ~global, new_env^);
  (patl, new_env, get_ref(pattern_force), unpacks, pv);
};

open Format;
open Printtyp;
let report_type_expected_explanation = (expl, ppf) =>
  switch (expl) {
  | If_conditional => fprintf(ppf, "the condition of an if-statement")
  | If_no_else_branch =>
    fprintf(ppf, "the result of a conditional with no else branch")
  | Loop_conditional => fprintf(ppf, "the condition of a loop")
  | Loop_body => fprintf(ppf, "the body of a loop")
  | Assert_condition => fprintf(ppf, "the condition of an assertion")
  | Sequence_left_hand_side =>
    fprintf(ppf, "the left-hand side of a sequence")
  | Assign_not_box => fprintf(ppf, "the left-hand side of a box assignment")
  | Assign_not_array =>
    fprintf(ppf, "the left-hand side of an array index access")
  | Assign_not_array_index =>
    fprintf(ppf, "the index expression of an array access")
  };
let report_type_expected_explanation_opt = (expl, ppf) =>
  switch (expl) {
  | None => ()
  | Some(expl) =>
    fprintf(
      ppf,
      "@ because it is in %t",
      report_type_expected_explanation(expl),
    )
  };
let report_error = (env, ppf) =>
  fun
  | ConstructorArityMismatch(lid, expected, provided) =>
    fprintf(
      ppf,
      "@[The constructor %a@ expects %i argument(s),@ but is applied here to %i argument(s)@]",
      identifier,
      lid,
      expected,
      provided,
    )
  | PatternTypeClash(trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf => fprintf(ppf, "This pattern matches values of type"),
      fun
      | ppf =>
        fprintf(
          ppf,
          "but a pattern was expected which matches values of type",
        ),
    )
  | ExprTypeClash(trace, explanation) =>
    report_unification_error(
      ppf,
      env,
      trace,
      ~type_expected_explanation=
        report_type_expected_explanation_opt(explanation),
      fun
      | ppf => fprintf(ppf, "This expression has type"),
      fun
      | ppf => fprintf(ppf, "but a expression was expected of type"),
    )
  | LabelMismatch(lid, trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf =>
        fprintf(
          ppf,
          "The record field %s belongs to the type",
          Identifier.string_of_ident(lid),
        ),
      fun
      | ppf => fprintf(ppf, "but is mixed here with fields of type"),
    )
  | LabelMultiplyDefined(s) =>
    fprintf(ppf, "The record field label %s is defined several times", s)
  | RecursiveLocalConstraint(trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf => fprintf(ppf, "Recursive local constraint when unifying"),
      fun
      | ppf => fprintf(ppf, "with"),
    )
  | UnexpectedExistential => fprintf(ppf, "Unexpected existential")
  | MultiplyBoundVariable(name) =>
    fprintf(ppf, "Variable %s is bound several times in this matching", name)
  | ModulesNotAllowed =>
    fprintf(ppf, "Modules are not allowed in this pattern.")
  | OrpatVars(id, valid_idents) => {
      fprintf(
        ppf,
        "Variable %s must occur on both sides of this | pattern",
        Ident.name(id),
      );
      spellcheck_idents(ppf, id, valid_idents);
    }
  | OrPatternTypeClash(id, trace) =>
    report_unification_error(
      ppf,
      env,
      trace,
      fun
      | ppf =>
        fprintf(
          ppf,
          "The variable %s on the left-hand side of this or-pattern has type",
          Ident.name(id),
        ),
      fun
      | ppf => fprintf(ppf, "but on the right-hand side it has type"),
    )
  | UnrefutedPattern(pat) =>
    fprintf(
      ppf,
      "@[%s@ %s@ %a@]",
      "This match case could not be refuted.",
      "Here is an example of a value that would reach it:",
      Printpat.top_pretty,
      pat,
    )
  | InlineRecordPatternMisuse(cstr_name, cstr_type, pat_type) =>
    fprintf(
      ppf,
      "@[%a is a %s constructor but a %s constructor pattern was given.@]",
      identifier,
      cstr_name,
      cstr_type,
      pat_type,
    );

let report_error = (env, ppf, err) =>
  wrap_printing_env(~error=true, env, () => report_error(env, ppf, err));

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, env, err) =>
      Some(Location.error_of_printer(loc, report_error(env), err))
    | Error_forward(err) => Some(err)
    | _ => None,
  );
