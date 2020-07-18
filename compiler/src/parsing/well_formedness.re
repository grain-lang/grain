open Parsetree;
open Ast_iterator;
open Grain_utils;

type wferr =
  | MalformedString(Location.t)
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
  | NoLetRecMut(Location.t);

exception Error(wferr);

let prepare_error =
  Printf.(
    Location.(
      fun
      | MalformedString(loc) => errorf(~loc, "Malformed string literal")
      | MultipleModuleName(loc) =>
        errorf(~loc, "Multiple modules in identifier")
      | [@implicit_arity] TypeNameShouldBeUppercase(name, loc) =>
        errorf(~loc, "Type '%s' should have an uppercase name.", name)
      | [@implicit_arity] IllegalAliasName(name, loc) =>
        errorf(~loc, "Alias '%s' should have proper casing.", name)
      | [@implicit_arity] ExternalAlias(name, loc) =>
        errorf(~loc, "Alias '%s' should be at most one level deep.", name)
      | [@implicit_arity] ModuleNameShouldBeUppercase(name, loc) =>
        errorf(~loc, "Module '%s' should have an uppercase name.", name)
      | [@implicit_arity] ModuleImportNameShouldNotBeExternal(name, loc) =>
        errorf(~loc, "Module name '%s' should contain only one module.", name)
      | [@implicit_arity] TyvarNameShouldBeLowercase(var, loc) =>
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

let malformed_identifiers = (errs, super) => {
  open Identifier;
  let iter_expr = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | PExpId({txt: [@implicit_arity] IdentExternal(IdentExternal(_), _)}) =>
      errs := [MultipleModuleName(loc), ...errs^]
    | _ => ()
    };
    super.expr(self, e);
  };
  let iter_import = (self, imports) => {
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
      ({pimp_val}) =>
        switch (pimp_val) {
        | PImportValues(values) =>
          List.iter(
            ((name, alias)) =>
              switch (name, alias) {
              | (
                  {txt: IdentName(orig)},
                  Some({txt: IdentName(alias), loc}),
                )
                  when casing_mismatch(orig, alias) =>
                errs :=
                  [[@implicit_arity] IllegalAliasName(alias, loc), ...errs^]
              | (_, Some({txt: IdentExternal(_) as alias, loc})) =>
                errs :=
                  [
                    [@implicit_arity]
                    ExternalAlias(Identifier.string_of_ident(alias), loc),
                    ...errs^,
                  ]
              | _ => ()
              },
            values,
          )
        | _ => ()
        },
      imports,
    );
    super.import(self, imports);
  };
  let iterator = {...super, expr: iter_expr, import: iter_import};
  {errs, iterator};
};

let types_have_correct_case = (errs, super) => {
  let check_uppercase = (loc, s) => {
    let first_char = s.[0];
    if (!Char_utils.is_uppercase_letter(first_char)) {
      errs := [[@implicit_arity] TypeNameShouldBeUppercase(s, loc), ...errs^];
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
  /* FIXME: The parser should read in uppercase types as PTyConstr instances */
  let iterator = {...super, data: iter_data};
  {errs, iterator};
};

let modules_have_correct_case = (errs, super) => {
  let check_uppercase = (loc, s) => {
    let first_char = s.[0];
    if (!Char_utils.is_uppercase_letter(first_char)) {
      errs :=
        [[@implicit_arity] ModuleNameShouldBeUppercase(s, loc), ...errs^];
    };
  };
  let iter_mods = (self, imports) => {
    List.iter(
      ({pimp_mod_alias: alias}) =>
        switch (alias) {
        | Some({loc: name_loc, txt: IdentName(name)}) =>
          check_uppercase(name_loc, name)
        | Some(_) /* IdentExternal handled by another WF rule */
        | None => ()
        },
      imports,
    );
    super.import(self, imports);
  };
  let iterator = {...super, import: iter_mods};
  {errs, iterator};
};

let module_imports_not_external = (errs, super) => {
  let check_name = (loc, id) =>
    switch (id) {
    | Identifier.IdentName(_) => ()
    | Identifier.IdentExternal(_) =>
      errs :=
        [
          [@implicit_arity]
          ModuleImportNameShouldNotBeExternal(
            Identifier.string_of_ident(id),
            loc,
          ),
          ...errs^,
        ]
    };
  let iter_mods = (self, imports) => {
    List.iter(
      ({pimp_mod_alias: alias}) =>
        switch (alias) {
        | Some({loc: name_loc, txt: name}) => check_name(name_loc, name)
        | None => ()
        },
      imports,
    );
    super.import(self, imports);
  };
  let iterator = {...super, import: iter_mods};
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
    | [@implicit_arity] PTopLet(_, _, _, vbs) =>
      List.iter(
        fun
        | {pvb_pat: {ppat_desc: [@implicit_arity] PPatRecord(fields, _)}} =>
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
    | [@implicit_arity] PExpLet(_, _, vbs, _) =>
      List.iter(
        fun
        | {pvb_pat: {ppat_desc: [@implicit_arity] PPatRecord(fields, _)}} =>
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
    | [@implicit_arity] PTopLet(_, Recursive, _, vbs) =>
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
    | [@implicit_arity] PExpLet(Recursive, _, vbs, _) =>
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
    | [@implicit_arity] PTopLet(_, Recursive, Mutable, vbs) =>
      errs := [NoLetRecMut(loc), ...errs^]
    | _ => ()
    };
    super.toplevel(self, e);
  };
  let iter_binds = (self, {pexp_desc: desc, pexp_loc: loc} as e) => {
    switch (desc) {
    | [@implicit_arity] PExpLet(Recursive, Mutable, vbs, _) =>
      errs := [NoLetRecMut(loc), ...errs^]
    | _ => ()
    };
    super.expr(self, e);
  };
  let iterator = {...super, toplevel: iter_toplevel_binds, expr: iter_binds};
  {errs, iterator};
};

let compose_well_formedness = ({errs, iterator}, cur) =>
  cur(errs, iterator);

let well_formedness_checks = [
  malformed_strings,
  malformed_identifiers,
  types_have_correct_case,
  modules_have_correct_case,
  module_imports_not_external,
  only_has_one_export_all,
  no_empty_record_patterns,
  only_functions_oh_rhs_letrec,
  no_letrec_mut,
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
  /* FIXME: We should be able to raise _all_ errors at once */
  List.iter(e => raise(Error(e)), checker.errs^);
};
