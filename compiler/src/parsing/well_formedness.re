open Parsetree;
open Parsetree_iter;
open Grain_utils;

type wferr =
  | MalformedString(Location.t)
  | IllegalCharacterLiteral(string, Location.t)
  | ExternalAlias(string, Location.t)
  | ModuleImportNameShouldNotBeExternal(string, Location.t)
  | TyvarNameShouldBeLowercase(string, Location.t)
  | EmptyRecordPattern(Location.t)
  | RHSLetRecMayOnlyBeFunction(Location.t)
  | NoLetRecMut(Location.t)
  | RationalZeroDenominator(Location.t)
  | UnknownAttribute(string, Location.t)
  | InvalidAttributeArity(string, int, Location.t)
  | AttributeDisallowed(string, Location.t)
  | LoopControlOutsideLoop(string, Location.t)
  | ReturnStatementOutsideFunction(Location.t)
  | MismatchedReturnStyles(Location.t)
  | LocalIncludeStatement(Location.t);

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
      | LocalIncludeStatement(loc) =>
        errorf(
          ~loc,
          "`include` statements may only appear at the file level.",
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
    | PExpConstant(PConstString(s)) =>
      if (!Utf8.validString(s)) {
        errs := [MalformedString(loc), ...errs^];
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

let malformed_characters = (errs, super) => {
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpConstant(PConstChar(c)) =>
      if (String_utils.Utf8.utf_length_at_offset(c, 0) != String.length(c)) {
        errs := [IllegalCharacterLiteral(c, loc), ...errs^];
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

let no_zero_denominator_rational = (errs, super) => {
  let enter_expression = ({pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpConstant(PConstNumber(PConstNumberRational(_, d))) when d == "0" =>
      errs := [RationalZeroDenominator(loc), ...errs^]
    | _ => ()
    };
    super.enter_expression(e);
  };
  let enter_pattern = ({ppat_desc: desc, ppat_loc: loc} as p) => {
    switch (desc) {
    | PPatConstant(PConstNumber(PConstNumberRational(_, d))) when d == "0" =>
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

let known_attributes = [
  {name: "disableGC", arity: 0},
  {name: "unsafe", arity: 0},
  {name: "externalName", arity: 1},
];

let valid_attributes = (errs, super) => {
  let enter_attribute = (({txt, loc}, args) as attr) => {
    switch (List.find_opt(({name}) => name == txt, known_attributes)) {
    | Some({arity}) when List.length(args) != arity =>
      errs := [InvalidAttributeArity(txt, arity, loc), ...errs^]
    | None => errs := [UnknownAttribute(txt, loc), ...errs^]
    | _ => ()
    };
    super.enter_attribute(attr);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_attribute,
    },
  };
};

let disallowed_attributes = (errs, super) => {
  let enter_expression = ({pexp_desc: desc, pexp_attributes: attrs} as e) => {
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
    super.enter_expression(e);
  };
  let enter_toplevel_stmt =
      ({ptop_desc: desc, ptop_attributes: attrs} as top) => {
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
    super.enter_toplevel_stmt(top);
  };

  {
    errs,
    iter_hooks: {
      ...super,
      enter_expression,
      enter_toplevel_stmt,
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

let compose_well_formedness = ({errs, iter_hooks}, cur) =>
  cur(errs, iter_hooks);

let well_formedness_checks = [
  malformed_strings,
  malformed_characters,
  no_empty_record_patterns,
  only_functions_oh_rhs_letrec,
  no_letrec_mut,
  no_zero_denominator_rational,
  valid_attributes,
  disallowed_attributes,
  no_loop_control_statement_outside_of_loop,
  malformed_return_statements,
  no_local_include,
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
