open Parsetree;
open Parsetree_iter;
open Grain_utils;

type wferr =
  | MalformedString(Location.t)
  | ExternalAlias(string, Location.t)
  | ModuleImportNameShouldNotBeExternal(string, Location.t)
  | TyvarNameShouldBeLowercase(string, Location.t)
  | EmptyRecordPattern(Location.t)
  | RHSLetRecMayOnlyBeFunction(Location.t)
  | NoLetRecMut(Location.t)
  | RationalZeroDenominator(Location.t)
  | UnknownAttribute(string, string, Location.t)
  | InvalidAttributeArity(string, int, Location.t)
  | AttributeDisallowed(string, Location.t)
  | LoopControlOutsideLoop(string, Location.t)
  | ReturnStatementOutsideFunction(Location.t)
  | MismatchedReturnStyles(Location.t)
  | LocalIncludeStatement(Location.t)
  | ProvidedMultipleTimes(string, Location.t)
  | MutualRecTypesMissingRec(Location.t)
  | MutualRecExtraneousNonfirstRec(Location.t);

exception Error(wferr);

let prepare_error =
  Printf.(
    Location.(
      fun
      | MalformedString(loc) => errorf(~loc, "Malformed string literal")
      | ExternalAlias(name, loc) =>
        errorf(~loc, "Alias '%s' should be at most one level deep.", name)
      | ModuleImportNameShouldNotBeExternal(name, loc) =>
        errorf(~loc, "Module name '%s' should contain only one module.", name)
      | TyvarNameShouldBeLowercase(var, loc) =>
        errorf(~loc, "Type variable '%s' should be lowercase.", var)
      | EmptyRecordPattern(loc) =>
        errorf(
          ~loc,
          "A record pattern must contain at least one named field.",
        )
      | RHSLetRecMayOnlyBeFunction(loc) =>
        errorf(
          ~loc,
          "let rec may only be used with recursive function definitions.",
        )
      | NoLetRecMut(loc) =>
        errorf(~loc, "let rec may not be used with the `mut` keyword.")
      | RationalZeroDenominator(loc) =>
        errorf(~loc, "Rational numbers may not have a denominator of zero.")
      | UnknownAttribute(attr_context, attr, loc) =>
        errorf(~loc, "Unknown %s attribute `%s`.", attr_context, attr)
      | InvalidAttributeArity(attr, arity, loc) =>
        switch (arity) {
        | 0 => errorf(~loc, "Attribute `%s` expects no arguments.", attr)
        | 1 => errorf(~loc, "Attribute `%s` expects one argument.", attr)
        | _ =>
          errorf(~loc, "Attribute `%s` expects %d arguments.", attr, arity)
        }
      | AttributeDisallowed(msg, loc) => errorf(~loc, "%s", msg)
      | LoopControlOutsideLoop(control, loc) =>
        errorf(~loc, "`%s` statement used outside of a loop.", control)
      | ReturnStatementOutsideFunction(loc) =>
        errorf(~loc, "`return` statement used outside of a function.")
      | MismatchedReturnStyles(loc) =>
        errorf(
          ~loc,
          "All returned values must use the `return` keyword if the function returns early.",
        )
      | LocalIncludeStatement(loc) =>
        errorf(
          ~loc,
          "`include` statements may only appear at the file level.",
        )
      | ProvidedMultipleTimes(name, loc) =>
        errorf(
          ~loc,
          "%s was provided multiple times, but can only be provided once.",
          name,
        )
      | MutualRecTypesMissingRec(loc) =>
        errorf(
          ~loc,
          "Mutually recursive type groups must include `rec` on the first type in the group.",
        )
      | MutualRecExtraneousNonfirstRec(loc) =>
        errorf(
          ~loc,
          "The `rec` keyword should only appear on the first type in the mutually recursive type group.",
        )
    )
  );

let () =
  Location.register_error_of_exn(
    fun
    | Error(err) => Some(prepare_error(err))
    | _ => None,
  );

type well_formedness_checker = {
  errs: ref(list(wferr)),
  iter_hooks: hooks,
};

let malformed_strings = (errs, super) => {
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpConstant(PConstString({txt: s})) =>
      if (!Utf8.validString(s)) {
        errs := [MalformedString(loc), ...errs^];
      }
    | _ => ()
    };
    super.enter_expression(e);
  };

  let enter_pattern = ({ppat_desc: desc, ppat_loc: loc} as p) => {
    switch (desc) {
    | PPatConstant(PConstString({txt: s})) =>
      if (!Utf8.validString(s)) {
        errs := [MalformedString(loc), ...errs^];
      }
    | _ => ()
    };
    super.enter_pattern(p);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_expression,
      enter_pattern,
    },
  };
};

let no_empty_record_patterns = (errs, super) => {
  let enter_toplevel_stmt = ({ptop_desc: desc, ptop_loc: loc} as e) => {
    switch (desc) {
    | PTopLet(_, _, _, vbs) =>
      List.iter(
        fun
        | {pvb_pat: {ppat_desc: PPatRecord(fields, _)}} =>
          if (List.length(fields) == 0) {
            errs := [EmptyRecordPattern(loc), ...errs^];
          }
        | _ => (),
        vbs,
      )
    | _ => ()
    };
    super.enter_toplevel_stmt(e);
  };
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpLet(_, _, vbs) =>
      List.iter(
        fun
        | {pvb_pat: {ppat_desc: PPatRecord(fields, _)}} =>
          if (List.length(fields) == 0) {
            errs := [EmptyRecordPattern(loc), ...errs^];
          }
        | _ => (),
        vbs,
      )
    | _ => ()
    };
    super.enter_expression(e);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_toplevel_stmt,
      enter_expression,
    },
  };
};

let only_functions_oh_rhs_letrec = (errs, super) => {
  let enter_toplevel_stmt = ({ptop_desc: desc, ptop_loc: loc} as e) => {
    switch (desc) {
    | PTopLet(_, Recursive, _, vbs) =>
      List.iter(
        fun
        | {pvb_expr: {pexp_desc: PExpLambda(_)}} => ()
        | {pvb_loc} => errs := [RHSLetRecMayOnlyBeFunction(loc), ...errs^],
        vbs,
      )
    | _ => ()
    };
    super.enter_toplevel_stmt(e);
  };
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpLet(Recursive, _, vbs) =>
      List.iter(
        fun
        | {pvb_expr: {pexp_desc: PExpLambda(_)}} => ()
        | {pvb_loc} => errs := [RHSLetRecMayOnlyBeFunction(loc), ...errs^],
        vbs,
      )
    | _ => ()
    };
    super.enter_expression(e);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_toplevel_stmt,
      enter_expression,
    },
  };
};

let no_letrec_mut = (errs, super) => {
  let enter_toplevel_stmt = ({ptop_desc: desc, ptop_loc: loc} as e) => {
    switch (desc) {
    | PTopLet(_, Recursive, Mutable, vbs) =>
      errs := [NoLetRecMut(loc), ...errs^]
    | _ => ()
    };
    super.enter_toplevel_stmt(e);
  };
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpLet(Recursive, Mutable, vbs) =>
      errs := [NoLetRecMut(loc), ...errs^]
    | _ => ()
    };
    super.enter_expression(e);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_toplevel_stmt,
      enter_expression,
    },
  };
};

let string_is_all_zeros_and_underscores = s => {
  String.for_all(c => c == '0' || c == '_', s);
};

let literal_has_zero_deniminator = s => {
  let s = String_utils.slice(~first=0, ~last=-1, s);
  switch (String.split_on_char('/', s)) {
  | [n, d] => string_is_all_zeros_and_underscores(d)
  | _ => false
  };
};

let no_zero_denominator_rational = (errs, super) => {
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpConstant(PConstNumber(PConstNumberRational({denominator})))
        when string_is_all_zeros_and_underscores(denominator.txt) =>
      errs := [RationalZeroDenominator(loc), ...errs^]
    | PExpConstant(PConstRational({txt: s}))
        when literal_has_zero_deniminator(s) =>
      errs := [RationalZeroDenominator(loc), ...errs^]
    | _ => ()
    };
    super.enter_expression(e);
  };
  let enter_pattern = ({ppat_desc: desc, ppat_loc: loc} as p) => {
    switch (desc) {
    | PPatConstant(PConstNumber(PConstNumberRational({denominator})))
        when string_is_all_zeros_and_underscores(denominator.txt) =>
      errs := [RationalZeroDenominator(loc), ...errs^]
    | PPatConstant(PConstRational({txt: s}))
        when literal_has_zero_deniminator(s) =>
      errs := [RationalZeroDenominator(loc), ...errs^]
    | _ => ()
    };
    super.enter_pattern(p);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_expression,
      enter_pattern,
    },
  };
};

type known_attribute = {
  name: string,
  arity: int,
};

let disallowed_attributes = (errs, super) => {
  let validate_against_known = (attrs, known_attributes, context) => {
    List.iter(
      ({Asttypes.attr_name: {txt, loc}, attr_args: args}) => {
        switch (List.find_opt(({name}) => name == txt, known_attributes)) {
        | Some({arity}) when List.length(args) != arity =>
          errs := [InvalidAttributeArity(txt, arity, loc), ...errs^]
        | None => errs := [UnknownAttribute(context, txt, loc), ...errs^]
        | _ => ()
        }
      },
      attrs,
    );
  };

  let known_expr_attributes = [
    {name: "disableGC", arity: 0},
    {name: "unsafe", arity: 0},
    {name: "externalName", arity: 1},
  ];

  let enter_expression = ({pexp_attributes: attrs} as e) => {
    switch (
      List.find_opt(
        ({Asttypes.attr_name: {txt}}) => txt == "externalName",
        attrs,
      )
    ) {
    | Some({Asttypes.attr_name: {txt, loc}}) =>
      errs :=
        [
          AttributeDisallowed(
            "`externalName` is only allowed on top-level let bindings and `foreign` statements.",
            loc,
          ),
        ]
    | None => ()
    };
    validate_against_known(attrs, known_expr_attributes, "expression");
    super.enter_expression(e);
  };

  let enter_toplevel_stmt =
      ({ptop_desc: desc, ptop_attributes: attrs} as top) => {
    switch (
      List.find_opt(
        ({Asttypes.attr_name: {txt}}) => txt == "externalName",
        attrs,
      )
    ) {
    | Some({Asttypes.attr_name: {txt, loc}}) =>
      switch (desc) {
      | PTopForeign(_)
      | PTopLet(
          _,
          _,
          _,
          [
            {
              pvb_pat: {
                ppat_desc:
                  PPatVar(_) | PPatConstraint({ppat_desc: PPatVar(_)}, _),
              },
            },
          ],
        ) =>
        ()
      | PTopLet(_, _, _, [_]) =>
        errs :=
          [
            AttributeDisallowed(
              "`externalName` cannot be used with a destructuring pattern.",
              loc,
            ),
          ]
      | PTopLet(_, _, _, [_, _, ..._]) =>
        errs :=
          [
            AttributeDisallowed(
              "`externalName` cannot be used on a `let` with multiple bindings.",
              loc,
            ),
          ]
      | _ =>
        errs :=
          [
            AttributeDisallowed(
              "`externalName` is only allowed on `foreign` statements and `let` bindings.",
              loc,
            ),
          ]
      }
    | None => ()
    };
    validate_against_known(attrs, known_expr_attributes, "top-level");
    super.enter_toplevel_stmt(top);
  };

  let enter_parsed_program = ({attributes} as prog) => {
    let known_module_attributes = [
      {name: "runtimeMode", arity: 0},
      {name: "noPervasives", arity: 0},
    ];
    validate_against_known(attributes, known_module_attributes, "module");
    super.enter_parsed_program(prog);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_expression,
      enter_toplevel_stmt,
      enter_parsed_program,
    },
  };
};

let no_loop_control_statement_outside_of_loop = (errs, super) => {
  let ctx = ref([]);
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpWhile(_)
    | PExpFor(_) => ctx := [true, ...ctx^]
    | PExpLambda(_) => ctx := [false, ...ctx^]
    | PExpContinue =>
      switch (ctx^) {
      // No loop context means we're not in a loop
      | []
      | [false, ..._] =>
        errs := [LoopControlOutsideLoop("continue", loc), ...errs^]
      | _ => ()
      }
    | PExpBreak =>
      switch (ctx^) {
      // No loop context means we're not in a loop
      | []
      | [false, ..._] =>
        errs := [LoopControlOutsideLoop("break", loc), ...errs^]
      | _ => ()
      }
    | _ => ()
    };
    super.enter_expression(e);
  };

  let leave_expression = ({pexp_desc: desc} as e) => {
    switch (desc) {
    | PExpWhile(_)
    | PExpFor(_)
    | PExpLambda(_) => ctx := List.tl(ctx^)
    | _ => ()
    };
    super.leave_expression(e);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_expression,
      leave_expression,
    },
  };
};

let malformed_return_statements = (errs, super) => {
  let rec has_returning_branch = exp => {
    switch (exp.pexp_desc) {
    | PExpReturn(_) => true
    | PExpBlock(expressions) =>
      let rec find = expressions => {
        switch (expressions) {
        | [] => false
        | [expression] => has_returning_branch(expression)
        | [_, ...rest] => find(rest)
        };
      };
      find(expressions);
    | PExpIf(_, _, None) =>
      // If expressions with no else branch are not considered
      false
    | PExpIf(_, ifso, Some(ifnot)) =>
      has_returning_branch(ifso) || has_returning_branch(ifnot)
    | PExpMatch(_, {txt: branches}) =>
      List.exists(branch => has_returning_branch(branch.pmb_body), branches)
    | _ => false
    };
  };
  let rec collect_non_returning_branches = (exp, acc) => {
    switch (exp.pexp_desc) {
    | PExpReturn(_)
    // Throwing an error or failing also exits the function immediately
    | PExpApp(
        {pexp_desc: PExpId({txt: IdentName({txt: "throw" | "fail"})})},
        _,
      ) => acc
    | PExpBlock(expressions) =>
      let rec collect = expressions => {
        switch (expressions) {
        | [] => acc
        | [expression] => collect_non_returning_branches(expression, acc)
        | [_, ...rest] => collect(rest)
        };
      };
      collect(expressions);
    | PExpIf(_, ifso, Some(ifnot)) when has_returning_branch(exp) =>
      collect_non_returning_branches(ifso, [])
      @ collect_non_returning_branches(ifnot, acc)
    | PExpMatch(_, {txt: branches}) when has_returning_branch(exp) =>
      List.fold_left(
        (acc, branch) =>
          collect_non_returning_branches(branch.pmb_body, acc),
        acc,
        branches,
      )
    | _ => [exp, ...acc]
    };
  };
  let ctx = ref([]);
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpLambda(_) =>
      // Push a context to record return statements for the current function
      // This is a stack because functions can be nested
      ctx := [ref(false), ...ctx^]
    | PExpReturn(_) =>
      switch (ctx^) {
      | [] =>
        // No function context means we're not in a function
        errs := [ReturnStatementOutsideFunction(loc), ...errs^]
      | [hd, ..._] => hd := true
      }
    | _ => ()
    };

    super.enter_expression(e);
  };

  let leave_expression = ({pexp_desc: desc} as e) => {
    // The expression has been iterated; pop the context if the expression was a function
    switch (desc) {
    | PExpLambda(_, body) =>
      let has_return = (List.hd(ctx^))^;
      ctx := List.tl(ctx^);
      if (has_return) {
        List.iter(
          exp => {errs := [MismatchedReturnStyles(exp.pexp_loc), ...errs^]},
          collect_non_returning_branches(body, []),
        );
      };
    | _ => ()
    };
    super.leave_expression(e);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_expression,
      leave_expression,
    },
  };
};

let no_local_include = (errs, super) => {
  let file_level = ref([true]);
  let enter_toplevel_stmt = ({ptop_desc: desc, ptop_loc: loc} as top) => {
    switch (desc) {
    | PTopInclude(_) when !List.hd(file_level^) =>
      errs := [LocalIncludeStatement(loc), ...errs^]
    | PTopModule(_) => file_level := [false, ...file_level^]
    | _ => ()
    };
    super.enter_toplevel_stmt(top);
  };

  let leave_toplevel_stmt = ({ptop_desc: desc} as top) => {
    switch (desc) {
    | PTopModule(_) => file_level := List.tl(file_level^)
    | _ => ()
    };
    super.leave_toplevel_stmt(top);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_toplevel_stmt,
      leave_toplevel_stmt,
    },
  };
};

type provided_multiple_times_ctx = {
  modules: Hashtbl.t(string, unit),
  types: Hashtbl.t(string, unit),
  exceptions: Hashtbl.t(string, unit),
  values: Hashtbl.t(string, unit),
};

let provided_multiple_times = (errs, super) => {
  let rec extract_bindings = (binds, pattern) =>
    switch (pattern.ppat_desc) {
    | PPatAny => binds
    | PPatVar(bind) => [bind, ...binds]
    | PPatTuple(pats) => List.fold_left(extract_bindings, binds, pats)
    | PPatList(pats) =>
      List.fold_left(
        (binds, item) => {
          switch (item) {
          | ListItem(p) => extract_bindings(binds, p)
          | ListSpread(p, loc) => extract_bindings(binds, p)
          }
        },
        binds,
        pats,
      )
    | PPatArray(pats) => List.fold_left(extract_bindings, binds, pats)
    | PPatRecord(pats, _) =>
      List.fold_left(
        (binds, (_, pat)) => extract_bindings(binds, pat),
        binds,
        pats,
      )
    | PPatConstant(_) => binds
    | PPatConstraint(pat, _) => extract_bindings(binds, pat)
    | PPatConstruct(_, cstr) =>
      switch (cstr) {
      | PPatConstrRecord(pats, _) =>
        List.fold_left(
          (binds, (_, pat)) => extract_bindings(binds, pat),
          binds,
          pats,
        )
      | PPatConstrTuple(pats) =>
        List.fold_left(extract_bindings, binds, pats)
      | PPatConstrSingleton => []
      }
    | PPatOr(pat1, pat2) =>
      extract_bindings([], pat1) @ extract_bindings(binds, pat2)
    | PPatAlias(pat, bind) => extract_bindings([bind, ...binds], pat)
    };

  let ctx =
    ref([
      {
        modules: Hashtbl.create(64),
        types: Hashtbl.create(64),
        exceptions: Hashtbl.create(64),
        values: Hashtbl.create(64),
      },
    ]);

  let enter_module = (p, d) => {
    ctx :=
      [
        {
          modules: Hashtbl.create(64),
          types: Hashtbl.create(64),
          exceptions: Hashtbl.create(64),
          values: Hashtbl.create(64),
        },
        ...ctx^,
      ];
    super.enter_module(p, d);
  };

  let leave_module = (p, d) => {
    ctx := List.tl(ctx^);
    super.leave_module(p, d);
  };

  let enter_toplevel_stmt = ({ptop_desc: desc} as top) => {
    let {values, modules, types, exceptions} = List.hd(ctx^);
    switch (desc) {
    | PTopModule(Provided | Abstract, {pmod_name, pmod_loc}) =>
      if (Hashtbl.mem(modules, pmod_name.txt)) {
        errs := [ProvidedMultipleTimes(pmod_name.txt, pmod_loc), ...errs^];
      } else {
        Hashtbl.add(modules, pmod_name.txt, ());
      }
    | PTopForeign(
        Provided | Abstract,
        {pval_name, pval_name_alias, pval_loc},
      ) =>
      let name = Option.value(~default=pval_name, pval_name_alias);
      if (Hashtbl.mem(values, name.txt)) {
        errs := [ProvidedMultipleTimes(name.txt, pval_loc), ...errs^];
      } else {
        Hashtbl.add(values, name.txt, ());
      };
    | PTopPrimitive(Provided | Abstract, {pprim_ident, pprim_loc}) =>
      if (Hashtbl.mem(values, pprim_ident.txt)) {
        errs := [ProvidedMultipleTimes(pprim_ident.txt, pprim_loc), ...errs^];
      } else {
        Hashtbl.add(values, pprim_ident.txt, ());
      }
    | PTopData(decls) =>
      List.iter(
        decl => {
          switch (decl) {
          | (Provided | Abstract, {pdata_name, pdata_loc}, _) =>
            if (Hashtbl.mem(types, pdata_name.txt)) {
              errs :=
                [ProvidedMultipleTimes(pdata_name.txt, pdata_loc), ...errs^];
            } else {
              Hashtbl.add(types, pdata_name.txt, ());
            }
          | (NotProvided, _, _) => ()
          }
        },
        decls,
      )
    | PTopLet(Provided | Abstract, _, _, binds) =>
      List.iter(
        bind => {
          let names = extract_bindings([], bind.pvb_pat);
          List.iter(
            name =>
              if (Hashtbl.mem(values, name.txt)) {
                errs := [ProvidedMultipleTimes(name.txt, name.loc), ...errs^];
              } else {
                Hashtbl.add(values, name.txt, ());
              },
            names,
          );
        },
        binds,
      )
    | PTopException(
        Provided | Abstract,
        {ptyexn_constructor: {pext_name, pext_loc}},
      ) =>
      if (Hashtbl.mem(values, pext_name.txt)) {
        errs := [ProvidedMultipleTimes(pext_name.txt, pext_loc), ...errs^];
      } else {
        Hashtbl.add(values, pext_name.txt, ());
      }
    | PTopProvide(items) =>
      let apply_alias = (name, alias) => {
        let old_name = Identifier.string_of_ident(name.txt);
        let new_name =
          switch (alias) {
          | Some(alias) => Identifier.string_of_ident(alias.txt)
          | None => old_name
          };
        (old_name, new_name);
      };
      List.iter(
        item => {
          switch (item) {
          | PProvideType({name, alias, loc}) =>
            let (_, name) = apply_alias(name, alias);
            if (Hashtbl.mem(types, name)) {
              errs := [ProvidedMultipleTimes(name, loc), ...errs^];
            } else {
              Hashtbl.add(types, name, ());
            };
          | PProvideException({name, alias, loc}) =>
            let (_, name) = apply_alias(name, alias);
            if (Hashtbl.mem(exceptions, name)) {
              errs := [ProvidedMultipleTimes(name, loc), ...errs^];
            } else {
              Hashtbl.add(exceptions, name, ());
            };
          | PProvideModule({name, alias, loc}) =>
            let (_, name) = apply_alias(name, alias);
            if (Hashtbl.mem(modules, name)) {
              errs := [ProvidedMultipleTimes(name, loc), ...errs^];
            } else {
              Hashtbl.add(modules, name, ());
            };
          | PProvideValue({name, alias, loc}) =>
            let (_, name) = apply_alias(name, alias);
            if (Hashtbl.mem(values, name)) {
              errs := [ProvidedMultipleTimes(name, loc), ...errs^];
            } else {
              Hashtbl.add(values, name, ());
            };
          }
        },
        items,
      );
    | PTopModule(NotProvided, _)
    | PTopForeign(NotProvided, _)
    | PTopPrimitive(NotProvided, _)
    | PTopLet(NotProvided, _, _, _)
    | PTopException(NotProvided, _)
    | PTopInclude(_)
    | PTopExpr(_) => ()
    };
    super.enter_toplevel_stmt(top);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_toplevel_stmt,
      enter_module,
      leave_module,
    },
  };
};

let mutual_rec_type_improper_rec_keyword = (errs, super) => {
  let enter_toplevel_stmt = ({ptop_desc: desc, ptop_loc: loc} as e) => {
    switch (desc) {
    | PTopData([(_, first_decl, _), ...[_, ..._] as rest_decls]) =>
      if (first_decl.pdata_rec != Recursive) {
        errs := [MutualRecTypesMissingRec(loc), ...errs^];
      } else {
        List.iter(
          ((_, decl, _)) =>
            switch (decl) {
            | {pdata_rec: Recursive} =>
              errs := [MutualRecExtraneousNonfirstRec(loc), ...errs^]
            | _ => ()
            },
          rest_decls,
        );
      }
    | _ => ()
    };
    super.enter_toplevel_stmt(e);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_toplevel_stmt,
    },
  };
};

let array_index_non_integer = (errs, super) => {
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpArrayGet(_, {pexp_desc: PExpConstant(PConstNumber(number_type))})
    | PExpArraySet({
        index: {pexp_desc: PExpConstant(PConstNumber(number_type))},
      }) =>
      switch (number_type) {
      | PConstNumberFloat({txt}) =>
        let warning = Warnings.ArrayIndexNonInteger(txt);
        if (Warnings.is_active(warning)) {
          Location.prerr_warning(loc, warning);
        };
      | PConstNumberRational({
          numerator: {txt: numerator},
          denominator: {txt: denominator},
        }) =>
        let warning =
          Warnings.ArrayIndexNonInteger(numerator ++ "/" ++ denominator);
        if (Grain_utils.Warnings.is_active(warning)) {
          Location.prerr_warning(loc, warning);
        };
      | _ => ()
      }
    | _ => ()
    };
    super.enter_expression(e);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_expression,
    },
  };
};

let compose_well_formedness = ({errs, iter_hooks}, cur) =>
  cur(errs, iter_hooks);

let well_formedness_checks = [
  malformed_strings,
  no_empty_record_patterns,
  only_functions_oh_rhs_letrec,
  no_letrec_mut,
  no_zero_denominator_rational,
  disallowed_attributes,
  no_loop_control_statement_outside_of_loop,
  malformed_return_statements,
  no_local_include,
  provided_multiple_times,
  mutual_rec_type_improper_rec_keyword,
  array_index_non_integer,
];

let well_formedness_checker = () =>
  List.fold_left(
    compose_well_formedness,
    {errs: ref([]), iter_hooks: default_hooks},
    well_formedness_checks,
  );

let check_well_formedness = program => {
  let {errs, iter_hooks} = well_formedness_checker();

  Parsetree_iter.iter_parsed_program(iter_hooks, program);

  // TODO(#1503): We should be able to raise _all_ errors at once
  List.iter(e => raise(Error(e)), errs^);
};
