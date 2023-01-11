open Parsetree;
open Ast_iterator;
open Grain_utils;

type wferr =
  | MalformedString(Location.t)
  | IllegalCharacterLiteral(string, Location.t)
  | MultipleModuleName(Location.t)
  | TypeNameShouldBeUppercase(string, Location.t)
  | IllegalAliasName(string, Location.t)
  | ExternalAlias(string, Location.t)
  | ModuleNameShouldBeUppercase(string, Location.t)
  | ModuleImportNameShouldNotBeExternal(string, Location.t)
  | TyvarNameShouldBeLowercase(string, Location.t)
  | ExportAllShouldOnlyAppearOnce(Location.t)
  | EmptyRecordPattern(Location.t)
  | RHSLetRecMayOnlyBeFunction(Location.t)
  | NoLetRecMut(Location.t)
  | RationalZeroDenominator(Location.t)
  | UnknownAttribute(string, Location.t)
  | InvalidAttributeArity(string, int, Location.t)
  | AttributeDisallowed(string, Location.t)
  | LoopControlOutsideLoop(string, Location.t)
  | ReturnStatementOutsideFunction(Location.t)
  | MismatchedReturnStyles(Location.t);

exception Error(wferr);

let prepare_error =
  Printf.(
    Location.(
      fun
      | MalformedString(loc) => errorf(~loc, "Malformed string literal")
      | IllegalCharacterLiteral(cl, loc) =>
        errorf(
          ~loc,
          "This character literal contains multiple characters: '%s'\nDid you mean to create the string \"%s\" instead?",
          cl,
          cl,
        )
      | MultipleModuleName(loc) =>
        errorf(~loc, "Multiple modules in identifier")
      | TypeNameShouldBeUppercase(name, loc) =>
        errorf(~loc, "Type '%s' should have an uppercase name.", name)
      | IllegalAliasName(name, loc) =>
        errorf(~loc, "Alias '%s' should have proper casing.", name)
      | ExternalAlias(name, loc) =>
        errorf(~loc, "Alias '%s' should be at most one level deep.", name)
      | ModuleNameShouldBeUppercase(name, loc) =>
        errorf(~loc, "Module '%s' should have an uppercase name.", name)
      | ModuleImportNameShouldNotBeExternal(name, loc) =>
        errorf(~loc, "Module name '%s' should contain only one module.", name)
      | TyvarNameShouldBeLowercase(var, loc) =>
        errorf(~loc, "Type variable '%s' should be lowercase.", var)
      | ExportAllShouldOnlyAppearOnce(loc) =>
        errorf(~loc, "An 'export *' statement should appear at most once.")
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
      | UnknownAttribute(attr, loc) =>
        errorf(~loc, "Unknown attribute `%s`.", attr)
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
  iterator,
};

let malformed_strings = (errs, super) => {
  let iter_expr = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpConstant(PConstString(s)) =>
      if (!Utf8.validString(s)) {
        errs := [MalformedString(loc), ...errs^];
      }
    | _ => ()
    };
    super.expr(self, e);
  };
  let iterator = {...super, expr: iter_expr};
  {errs, iterator};
};

let malformed_characters = (errs, super) => {
  let iter_expr = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpConstant(PConstChar(c)) =>
      if (String_utils.Utf8.utf_length_at_offset(c, 0) != String.length(c)) {
        errs := [IllegalCharacterLiteral(c, loc), ...errs^];
      }
    | _ => ()
    };
    super.expr(self, e);
  };
  let iterator = {...super, expr: iter_expr};
  {errs, iterator};
};

let malformed_identifiers = (errs, super) => {
  open Identifier;
  let iter_expr = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpId({txt: IdentExternal(IdentExternal(_), _)}) =>
      errs := [MultipleModuleName(loc), ...errs^]
    | _ => ()
    };
    super.expr(self, e);
  };
  let iter_import = (self, import) => {
    let xor = (p, q) => p && !q || !p && q;
    let casing_mismatch = (orig, alias) => {
      let o = orig.[0];
      let a = alias.[0];
      xor(
        Char_utils.is_uppercase_letter(o),
        Char_utils.is_uppercase_letter(a),
      );
    };
    List.iter(
      pimp_val =>
        switch (pimp_val) {
        | PImportValues(values) =>
          List.iter(
            ((name, alias)) =>
              switch (name, alias) {
              | (
                  {txt: IdentName({txt: orig})},
                  Some({txt: IdentName({txt: alias}), loc}),
                )
                  when casing_mismatch(orig, alias) =>
                errs := [IllegalAliasName(alias, loc), ...errs^]
              | (_, Some({txt: IdentExternal(_) as alias, loc})) =>
                errs :=
                  [
                    ExternalAlias(Identifier.string_of_ident(alias), loc),
                    ...errs^,
                  ]
              | _ => ()
              },
            values,
          )
        | _ => ()
        },
      import.pimp_val,
    );
    super.import(self, import);
  };
  let iterator = {...super, expr: iter_expr, import: iter_import};
  {errs, iterator};
};

let types_have_correct_case = (errs, super) => {
  let check_uppercase = (loc, s) => {
    let first_char = s.[0];
    if (!Char_utils.is_uppercase_letter(first_char)) {
      errs := [TypeNameShouldBeUppercase(s, loc), ...errs^];
    };
  };
  let iter_data =
      (
        self,
        {pdata_name: {loc: name_loc, txt: name}, pdata_loc: loc, _} as d,
      ) => {
    check_uppercase(name_loc, name);
    super.data(self, d);
  };
  // TODO(#1502): The parser should read in uppercase types as PTyConstr instances
  let iterator = {...super, data: iter_data};
  {errs, iterator};
};

let modules_have_correct_case = (errs, super) => {
  let check_uppercase = (loc, s) => {
    let first_char = s.[0];
    if (!Char_utils.is_uppercase_letter(first_char)) {
      errs := [ModuleNameShouldBeUppercase(s, loc), ...errs^];
    };
  };
  let iter_mod = (self, import) => {
    List.iter(
      fun
      | PImportModule({loc: name_loc, txt: IdentName({txt: name})}) =>
        check_uppercase(name_loc, name)
      | PImportModule(_) /* IdentExternal handled by another WF rule */
      | PImportAllExcept(_)
      | PImportValues(_) => (),
      import.pimp_val,
    );
    super.import(self, import);
  };
  let iterator = {...super, import: iter_mod};
  {errs, iterator};
};

let module_imports_not_external = (errs, super) => {
  let check_name = (loc, id) =>
    switch (id) {
    | Identifier.IdentName(_) => ()
    | Identifier.IdentExternal(_) =>
      errs :=
        [
          ModuleImportNameShouldNotBeExternal(
            Identifier.string_of_ident(id),
            loc,
          ),
          ...errs^,
        ]
    };
  let iter_mod = (self, import) => {
    List.iter(
      fun
      | PImportModule({loc: name_loc, txt: name}) =>
        check_name(name_loc, name)
      | PImportAllExcept(_)
      | PImportValues(_) => (),
      import.pimp_val,
    );
    super.import(self, import);
  };
  let iterator = {...super, import: iter_mod};
  {errs, iterator};
};

let only_has_one_export_all = (errs, super) => {
  let count_export = ref(0);
  let iter_export_all = (self, {ptop_desc: desc, ptop_loc: loc} as e) => {
    let check_export_count = () =>
      if (count_export^ > 1) {
        errs := [ExportAllShouldOnlyAppearOnce(loc), ...errs^];
      };
    switch (desc) {
    | PTopExportAll(_) =>
      incr(count_export);
      check_export_count();
    | _ => ()
    };
    super.toplevel(self, e);
  };
  let iterator = {...super, toplevel: iter_export_all};
  {errs, iterator};
};

let no_empty_record_patterns = (errs, super) => {
  let iter_toplevel_binds = (self, {ptop_desc: desc, ptop_loc: loc} as e) => {
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
    super.toplevel(self, e);
  };
  let iter_binds = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
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
    super.expr(self, e);
  };
  let iterator = {...super, toplevel: iter_toplevel_binds, expr: iter_binds};
  {errs, iterator};
};

let only_functions_oh_rhs_letrec = (errs, super) => {
  let iter_toplevel_binds = (self, {ptop_desc: desc, ptop_loc: loc} as e) => {
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
    super.toplevel(self, e);
  };
  let iter_binds = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
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
    super.expr(self, e);
  };
  let iterator = {...super, toplevel: iter_toplevel_binds, expr: iter_binds};
  {errs, iterator};
};

let no_letrec_mut = (errs, super) => {
  let iter_toplevel_binds = (self, {ptop_desc: desc, ptop_loc: loc} as e) => {
    switch (desc) {
    | PTopLet(_, Recursive, Mutable, vbs) =>
      errs := [NoLetRecMut(loc), ...errs^]
    | _ => ()
    };
    super.toplevel(self, e);
  };
  let iter_binds = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpLet(Recursive, Mutable, vbs) =>
      errs := [NoLetRecMut(loc), ...errs^]
    | _ => ()
    };
    super.expr(self, e);
  };
  let iterator = {...super, toplevel: iter_toplevel_binds, expr: iter_binds};
  {errs, iterator};
};

let no_zero_denominator_rational = (errs, super) => {
  let iter_expr = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpConstant(PConstNumber(PConstNumberRational(_, d))) when d == "0" =>
      errs := [RationalZeroDenominator(loc), ...errs^]
    | _ => ()
    };
    super.expr(self, e);
  };
  let iter_pat = (self, {ppat_desc: desc, ppat_loc: loc} as p) => {
    switch (desc) {
    | PPatConstant(PConstNumber(PConstNumberRational(_, d))) when d == "0" =>
      errs := [RationalZeroDenominator(loc), ...errs^]
    | _ => ()
    };
    super.pat(self, p);
  };
  let iterator = {...super, expr: iter_expr, pat: iter_pat};
  {errs, iterator};
};

type known_attribute = {
  name: string,
  arity: int,
};

let known_attributes = [
  {name: "disableGC", arity: 0},
  {name: "unsafe", arity: 0},
  {name: "externalName", arity: 1},
];

let valid_attributes = (errs, super) => {
  let iter_attributes = (({txt, loc}, args)) => {
    switch (List.find_opt(({name}) => name == txt, known_attributes)) {
    | Some({arity}) when List.length(args) != arity =>
      errs := [InvalidAttributeArity(txt, arity, loc), ...errs^]
    | None => errs := [UnknownAttribute(txt, loc), ...errs^]
    | _ => ()
    };
  };

  let iter_expr = (self, {pexp_attributes: attrs} as e) => {
    List.iter(iter_attributes, attrs);
    super.expr(self, e);
  };
  let iter_toplevel = (self, {ptop_attributes: attrs} as top) => {
    List.iter(iter_attributes, attrs);
    super.toplevel(self, top);
  };
  let iterator = {...super, expr: iter_expr, toplevel: iter_toplevel};
  {errs, iterator};
};

let disallowed_attributes = (errs, super) => {
  let iter_expr = (self, {pexp_desc: desc, pexp_attributes: attrs} as e) => {
    switch (List.find_opt((({txt}, _)) => txt == "externalName", attrs)) {
    | Some(({txt, loc}, _)) =>
      errs :=
        [
          AttributeDisallowed(
            "`externalName` is only allowed on top-level let bindings and `foreign` statements.",
            loc,
          ),
        ]
    | None => ()
    };
    super.expr(self, e);
  };
  let iter_toplevel =
      (self, {ptop_desc: desc, ptop_attributes: attrs} as top) => {
    switch (List.find_opt((({txt}, _)) => txt == "externalName", attrs)) {
    | Some(({txt, loc}, _)) =>
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
    super.toplevel(self, top);
  };
  let iterator = {...super, expr: iter_expr, toplevel: iter_toplevel};
  {errs, iterator};
};

let no_loop_control_statement_outside_of_loop = (errs, super) => {
  let in_loop = ref(false);
  let iter_expr = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
    let after = in_loop^;
    switch (desc) {
    | PExpWhile(_)
    | PExpFor(_) => in_loop := true
    | PExpLambda(_) => in_loop := false
    | PExpContinue =>
      if (! in_loop^) {
        errs := [LoopControlOutsideLoop("continue", loc), ...errs^];
      }
    | PExpBreak =>
      if (! in_loop^) {
        errs := [LoopControlOutsideLoop("break", loc), ...errs^];
      }
    | _ => ()
    };
    super.expr(self, e);
    in_loop := after;
  };
  let iterator = {...super, expr: iter_expr};
  {errs, iterator};
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
    | PExpIf(_, _, {pexp_desc: PExpBlock([])}) =>
      // If expressions with no else branch are not considered
      false
    | PExpIf(_, ifso, ifnot) =>
      has_returning_branch(ifso) || has_returning_branch(ifnot)
    | PExpMatch(_, branches) =>
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
    | PExpIf(_, ifso, ifnot) when has_returning_branch(exp) =>
      collect_non_returning_branches(ifso, [])
      @ collect_non_returning_branches(ifnot, acc)
    | PExpMatch(_, branches) when has_returning_branch(exp) =>
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
  let iter_expr = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
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

    super.expr(self, e);

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
  };
  let iterator = {...super, expr: iter_expr};
  {errs, iterator};
};

let compose_well_formedness = ({errs, iterator}, cur) =>
  cur(errs, iterator);

let well_formedness_checks = [
  malformed_strings,
  malformed_characters,
  malformed_identifiers,
  types_have_correct_case,
  modules_have_correct_case,
  module_imports_not_external,
  only_has_one_export_all,
  no_empty_record_patterns,
  only_functions_oh_rhs_letrec,
  no_letrec_mut,
  no_zero_denominator_rational,
  valid_attributes,
  disallowed_attributes,
  no_loop_control_statement_outside_of_loop,
  malformed_return_statements,
];

let well_formedness_checker = () =>
  List.fold_left(
    compose_well_formedness,
    {errs: ref([]), iterator: default_iterator},
    well_formedness_checks,
  );

let check_well_formedness = ({statements}) => {
  let checker = well_formedness_checker();
  List.iter(checker.iterator.toplevel(checker.iterator), statements);
  // TODO(#1503): We should be able to raise _all_ errors at once
  List.iter(e => raise(Error(e)), checker.errs^);
};
