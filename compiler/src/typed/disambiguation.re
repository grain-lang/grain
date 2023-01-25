open Grain_parsing;
open Grain_utils;
open Types;
open Typedtree;
open Btype;
open Ctype;
open Checkertypes;

type error =
  | WrongName(string, type_expected, string, Path.t, string, list(string))
  | NameTypeMismatch(
      string,
      Identifier.t,
      (Path.t, Path.t),
      list((Path.t, Path.t)),
    );

exception Error(Location.t, Env.t, error);
exception Error_forward(Location.error);

let mk_expected = (~explanation=?, ty) => {ty, explanation};

let rec expand_path = (env, p) => {
  let decl =
    try(Some(Env.find_type(p, env))) {
    | Not_found => None
    };

  switch (decl) {
  | Some({type_manifest: Some(ty)}) =>
    switch (repr(ty)) {
    | {desc: TTyConstr(p, _, _)} => expand_path(env, p)
    | _ => p
    /* PR#6394: recursive module may introduce incoherent manifest */
    }
  | _ =>
    let p' = Env.normalize_path(None, env, p);
    if (Path.same(p, p')) {
      p;
    } else {
      expand_path(env, p');
    };
  };
};

let compare_type_path = (env, tpath1, tpath2) =>
  Path.same(expand_path(env, tpath1), expand_path(env, tpath2));

let label_of_kind = kind =>
  if (kind == "record") {
    "field";
  } else {
    "constructor";
  };

module NameChoice =
       (
         Name: {
           type t;
           let type_kind: string;
           let get_name: t => string;
           let get_type: t => type_expr;
           let get_descrs: Env.type_descriptions => list(t);
           let unbound_name_error: (Env.t, loc(Identifier.t)) => 'a;
           let in_env: t => bool;
         },
       ) => {
  open Name;

  let get_type_path = d =>
    switch (repr(get_type(d)).desc) {
    | TTyConstr(p, _, _) => p
    | _ => assert(false)
    };

  let lookup_from_type = (env, tpath, lid) => {
    let descrs = get_descrs(Env.find_type_descrs(tpath, env));
    /*Env.mark_type_used env (Path.last tpath) (Env.find_type tpath env);*/
    switch (lid.txt) {
    | Identifier.IdentName(s) =>
      try(List.find(nd => get_name(nd) == s.txt, descrs)) {
      | Not_found =>
        let names = List.map(get_name, descrs);
        raise(
          Error(
            lid.loc,
            env,
            WrongName(
              "",
              mk_expected(newvar()),
              type_kind,
              tpath,
              s.txt,
              names,
            ),
          ),
        );
      }
    | _ => raise(Not_found)
    };
  };

  let rec unique = (eq, acc) =>
    fun
    | [] => List.rev(acc)
    | [x, ...rem] =>
      if (List.exists(eq(x), acc)) {
        unique(eq, acc, rem);
      } else {
        unique(eq, [x, ...acc], rem);
      };

  let ambiguous_types = (env, lbl, others) => {
    let tpath = get_type_path(lbl);
    let others = List.map(((lbl, _)) => get_type_path(lbl), others);
    let tpaths = unique(compare_type_path(env), [tpath], others);
    switch (tpaths) {
    | [_] => []
    | _ => List.map(Printtyp.string_of_path, tpaths)
    };
  };

  let disambiguate_by_type = (env, tpath, lbls) => {
    let check_type = ((lbl, _)) => {
      let lbl_tpath = get_type_path(lbl);
      compare_type_path(env, tpath, lbl_tpath);
    };

    List.find(check_type, lbls);
  };

  let disambiguate =
      (
        ~warn=Location.prerr_warning,
        ~check_lk=(_, _) => (),
        ~scope=?,
        lid,
        env,
        opath,
        lbls,
      ) => {
    let scope =
      switch (scope) {
      | None => lbls
      | Some(l) => l
      };
    let lbl =
      switch (opath) {
      | None =>
        switch (lbls) {
        | [] => unbound_name_error(env, lid)
        | [(lbl, use), ...rest] =>
          use();
          let paths = ambiguous_types(env, lbl, rest);
          if (paths != []) {
            warn(
              lid.loc,
              Warnings.AmbiguousName(
                [Identifier.last(lid.txt)],
                paths,
                false,
              ),
            );
          };
          lbl;
        }
      | Some((tpath0, tpath, pr)) =>
        let warn_pr = () => {
          let label = label_of_kind(type_kind);
          warn(
            lid.loc,
            Warnings.NotPrincipal(
              "this type-based " ++ label ++ " disambiguation",
            ),
          );
        };

        try({
          let (lbl, use) = disambiguate_by_type(env, tpath, scope);
          use();
          if (!pr) {
            /* Check if non-principal type is affecting result */
            switch (lbls) {
            | [] => warn_pr()
            | [(lbl', _use'), ...rest] =>
              let lbl_tpath = get_type_path(lbl');
              if (!compare_type_path(env, tpath, lbl_tpath)) {
                warn_pr();
              } else {
                let paths = ambiguous_types(env, lbl, rest);
                if (paths != []) {
                  warn(
                    lid.loc,
                    Warnings.AmbiguousName(
                      [Identifier.last(lid.txt)],
                      paths,
                      false,
                    ),
                  );
                };
              };
            };
          };
          lbl;
        }) {
        | Not_found =>
          try({
            let lbl = lookup_from_type(env, tpath, lid);
            check_lk(tpath, lbl);
            if (in_env(lbl)) {
              let s = Printtyp.string_of_path(tpath);
              warn(
                lid.loc,
                Warnings.NameOutOfScope(
                  s,
                  [Identifier.last(lid.txt)],
                  false,
                ),
              );
            };
            if (!pr) {
              warn_pr();
            };
            lbl;
          }) {
          | Not_found =>
            if (lbls == []) {
              unbound_name_error(env, lid);
            } else {
              let tp = (tpath0, expand_path(env, tpath));
              let tpl =
                List.map(
                  ((lbl, _)) => {
                    let tp0 = get_type_path(lbl);
                    let tp = expand_path(env, tp0);
                    (tp0, tp);
                  },
                  lbls,
                );

              raise(
                Error(
                  lid.loc,
                  env,
                  NameTypeMismatch(type_kind, lid.txt, tp, tpl),
                ),
              );
            }
          }
        };
      };

    lbl;
  };
};

module Label =
  NameChoice({
    type t = label_description;
    let type_kind = "record";
    let get_name = lbl => lbl.lbl_name;
    let get_type = lbl => lbl.lbl_res;
    let get_descrs = snd;
    let unbound_name_error = Typetexp.unbound_label_error;
    let in_env = _ => true;
  });

module Constructor =
  NameChoice({
    type t = constructor_description;
    let type_kind = "variant";
    let get_name = cstr => cstr.cstr_name;
    let get_type = cstr => cstr.cstr_res;
    let get_descrs = fst;
    let unbound_name_error = Typetexp.unbound_constructor_error;
    let in_env = _ => true;
  });

let disambiguate_label_by_ids = (keep, closed, ids, labels) => {
  let check_ids = ((lbl, _)) => {
    let lbls = Hashtbl.create(8);
    Array.iter(lbl => Hashtbl.add(lbls, lbl.lbl_name, ()), lbl.lbl_all);
    List.for_all(Hashtbl.mem(lbls), ids);
  }
  and check_closed = ((lbl, _)) =>
    !closed || List.length(ids) == Array.length(lbl.lbl_all);

  let labels' = List.filter(check_ids, labels);
  if (keep && labels' == []) {
    (false, labels);
  } else {
    let labels'' = List.filter(check_closed, labels');
    if (keep && labels'' == []) {
      (false, labels');
    } else {
      (true, labels'');
    };
  };
};

/* Only issue warnings once per record constructor/pattern */
let disambiguate_lid_a_list = (loc, closed, env, opath, lid_a_list) => {
  let ids = List.map(((lid, _)) => Identifier.last(lid.txt), lid_a_list);
  let w_pr = ref(false)
  and w_amb = ref([])
  and w_scope = ref([])
  and w_scope_ty = ref("");
  let warn = (loc, msg) =>
    Warnings.(
      switch (msg) {
      | NotPrincipal(_) => w_pr := true
      | AmbiguousName([s], l, _) => w_amb := [(s, l), ...w_amb^]
      | NameOutOfScope(ty, [s], _) =>
        w_scope := [s, ...w_scope^];
        w_scope_ty := ty;
      | _ => Location.prerr_warning(loc, msg)
      }
    );

  let process_label = lid =>
    /* Strategy for each field:
       * collect all the labels in scope for that name
       * if the type is known and principal, just eventually warn
         if the real label was not in scope
       * fail if there is no known type and no label found
       * otherwise use other fields to reduce the list of candidates
       * if there is no known type reduce it incrementally, so that
         there is still at least one candidate (for error message)
       * if the reduced list is valid, call Label.disambiguate
     */
    try({
      let labels = Env.lookup_all_labels(lid.txt, env);
      if (List.length(labels) == 0) {
        raise(Not_found);
      };
      let (ok, lbls) =
        switch (opath) {
        | Some((_, _, true)) => (true, labels) /* disambiguate only checks scope */
        | _ => disambiguate_label_by_ids(opath == None, closed, ids, labels)
        };

      if (ok) {
        Label.disambiguate(lid, env, opath, lbls, ~warn);
      } else {
        fst(List.hd(lbls));
      };
    }) {
    /* will fail later */
    | Not_found =>
      switch (opath) {
      | None =>
        let id_str = Identifier.string_of_ident(lid.txt);
        let inline_record_field =
          Env.fold_constructors(
            (cstr, suggestion) => {
              switch (suggestion) {
              | Some(_) => suggestion
              | None =>
                switch (cstr.cstr_inlined) {
                | Some({type_kind: TDataRecord(rfs), _})
                    when
                      List.exists(rf => rf.Types.rf_name.name == id_str, rfs) =>
                  Some(cstr.cstr_name)
                | _ => None
                }
              }
            },
            None,
            env,
            None,
          );
        Env.error(
          switch (inline_record_field) {
          | None => Env.Unbound_label(lid.loc, id_str)
          | Some(suggestion) =>
            Env.Unbound_label_with_alt(lid.loc, id_str, suggestion)
          },
        );
      | Some(_) => Label.disambiguate(lid, env, opath, [], ~warn)
      }
    };

  let lbl_a_list =
    List.map(((lid, a)) => (lid, process_label(lid), a), lid_a_list);
  if (w_pr^) {
    Location.prerr_warning(
      loc,
      Warnings.NotPrincipal("this type-based record disambiguation"),
    );
  } else {
    switch (List.rev(w_amb^)) {
    | [(_, types), ..._] as amb =>
      let paths =
        List.map(((_, lbl, _)) => Label.get_type_path(lbl), lbl_a_list);
      let path = List.hd(paths);
      if (List.for_all(compare_type_path(env, path), List.tl(paths))) {
        Location.prerr_warning(
          loc,
          Warnings.AmbiguousName(List.map(fst, amb), types, true),
        );
      } else {
        List.iter(
          ((s, l)) =>
            Location.prerr_warning(
              loc,
              Warnings.AmbiguousName([s], l, false),
            ),
          amb,
        );
      };
    | _ => ()
    };
  };
  if (w_scope^ != []) {
    Location.prerr_warning(
      loc,
      Warnings.NameOutOfScope(w_scope_ty^, List.rev(w_scope^), true),
    );
  };
  lbl_a_list;
};

/* Error report */
let spellcheck = (ppf, unbound_name, valid_names) =>
  Misc.did_you_mean(ppf, () => Misc.spellcheck(valid_names, unbound_name));
let spellcheck_idents = (ppf, unbound, valid_idents) =>
  spellcheck(ppf, Ident.name(unbound), List.map(Ident.name, valid_idents));

let wrap_disambiguate = (kind, ty, f, x) =>
  try(f(x)) {
  | Error(loc, env, WrongName("", _, tk, tp, name, valid_names)) =>
    raise(Error(loc, env, WrongName(kind, ty, tk, tp, name, valid_names)))
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
    fprintf(ppf, "the left-hand side of an array item access")
  | Assign_not_array_index =>
    fprintf(ppf, "the argument to an array item access")
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
  | WrongName(eorp, ty_expected, kind, p, name, valid_names) => {
      let {ty, explanation} = ty_expected;
      reset_and_mark_loops(ty);
      {
        fprintf(
          ppf,
          "@[@[<2>%s type@ %a%t@]@ ",
          eorp,
          type_expr,
          ty,
          report_type_expected_explanation_opt(explanation),
        );
        fprintf(
          ppf,
          "The %s %s does not belong to type %a@]",
          label_of_kind(kind),
          name,
          /*kind*/ path,
          p,
        );
      };
      spellcheck(ppf, name, valid_names);
    }
  | NameTypeMismatch(kind, lid, tp, tpl) => {
      let name = label_of_kind(kind);
      report_ambiguous_type_error(
        ppf,
        env,
        tp,
        tpl,
        fun
        | ppf =>
          fprintf(
            ppf,
            "The %s %a@ belongs to the %s type",
            name,
            identifier,
            lid,
            kind,
          ),
        fun
        | ppf =>
          fprintf(
            ppf,
            "The %s %a@ belongs to one of the following %s types:",
            name,
            identifier,
            lid,
            kind,
          ),
        fun
        | ppf =>
          fprintf(
            ppf,
            "but a %s was expected belonging to the %s type",
            name,
            kind,
          ),
      );
    };

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
