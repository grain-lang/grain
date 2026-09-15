open Grain_typed;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let value_kind = desc =>
  switch (desc.Types.val_repr) {
  | ReprFunction(_) => Function
  | _ => Value
  };

let value_candidate = (~context, name, desc) =>
  make_candidate(
    ~source=Typedtree,
    ~label=name,
    ~kind=value_kind(desc),
    ~detail=
      Printtyp.string_of_value_description(~ident=Ident.create(name), desc),
    ~sort_group=Sort_group.Typed,
    ~context,
    (),
  );

let env_values = (~context, ~keep=(_name, _desc) => true, env) =>
  Env.fold_values(
    (name, _path, desc, acc) =>
      keep(name, desc)
        ? [value_candidate(~context, name, desc), ...acc] : acc,
    None,
    env,
    [],
  );

let value_matches_expected_type = (~env, ~expected_type, ty) =>
  switch (expected_type) {
  | None => true
  | Some(expected_type) =>
    try(
      {
        Ctype.unify(
          env,
          Ctype.instance(env, ty),
          Ctype.instance(env, expected_type),
        );
        true;
      }
    ) {
    | Ctype.Unify(_) => false
    | exn =>
      Trace.log(
        "Completion typed_env value match failed: " ++ Printexc.to_string(exn),
      );
      false;
    }
  };

let env_expression_start_values = (~context, ~expected_type=?, env) =>
  env_values(
    ~context,
    ~keep=
      (name, desc) =>
        String.length(name) > 0
        && Syntax.Text.is_ident_char(name.[0])
        && value_matches_expected_type(
             ~env,
             ~expected_type,
             desc.Types.val_type,
           ),
    env,
  );

let type_kind = decl =>
  switch (decl.Types.type_kind) {
  | TDataRecord(_) => Class
  | TDataAbstract
  | TDataOpen
  | TDataVariant(_) => TypeParameter
  };

let env_types = (~context, ~include_variants=false, env) =>
  Env.fold_types(
    (name, _path, (decl, _descrs), acc) =>
      switch (decl.Types.type_kind) {
      | TDataVariant(_) when !include_variants => acc
      | _ => [
          make_candidate(
            ~source=Typedtree,
            ~label=name,
            ~kind=type_kind(decl),
            ~detail=
              Printtyp.string_of_type_declaration(
                ~ident=Ident.create(name),
                decl,
              ),
            ~sort_group=Sort_group.Typed,
            ~context,
            (),
          ),
          ...acc,
        ]
      },
    None,
    env,
    [],
  );

let env_modules =
    (
      ~context,
      ~sort_group=Sort_group.Typed,
      ~exclude_module_names=?,
      ~keep_module=?,
      env,
    ) =>
  Env.fold_modules(
    (name, _path, decl, acc) => {
      let excluded =
        switch (exclude_module_names) {
        | Some(excluded) =>
          List.exists(excluded_name => excluded_name == name, excluded)
        | None => false
        };
      let kept =
        switch (keep_module) {
        | Some(keep) => keep(decl)
        | None => true
        };
      if (excluded || !kept) {
        acc;
      } else {
        [
          make_candidate(
            ~source=Typedtree,
            ~label=name,
            ~kind=Module,
            ~sort_group,
            ~context,
            (),
          ),
          ...acc,
        ];
      };
    },
    None,
    env,
    [],
  );

let result_matches_expected = (~env, ~expected_type, ~what, result_type) =>
  switch (expected_type) {
  | None => true
  | Some(expected_type) =>
    try(Env.same_constr^(env, expected_type, result_type)) {
    | Not_found => false
    | exn =>
      Trace.log(
        "Completion typed_env "
        ++ what
        ++ " match failed: "
        ++ Printexc.to_string(exn),
      );
      false;
    }
  };

let constructor_matches = (~env, ~expected_type, cstr) =>
  result_matches_expected(
    ~env,
    ~expected_type,
    ~what="constructor",
    cstr.Types.cstr_res,
  );

let label_matches = (~env, ~expected_type, label) =>
  result_matches_expected(
    ~env,
    ~expected_type,
    ~what="label",
    label.Types.lbl_res,
  );

let env_constructors = (~context, ~expected_type=?, env) =>
  Env.fold_constructors(
    (cstr, acc) =>
      if (constructor_matches(~env, ~expected_type, cstr)) {
        [
          make_candidate(
            ~source=Typedtree,
            ~label=cstr.Types.cstr_name,
            ~kind=Constructor,
            ~detail=Printtyp.string_of_type_scheme(cstr.Types.cstr_res),
            ~sort_group=Sort_group.Typed,
            ~context,
            (),
          ),
          ...acc,
        ];
      } else {
        acc;
      },
    None,
    env,
    [],
  );

let env_labels = (~context, ~expected_type=?, env) =>
  Env.fold_labels(
    (label, acc) =>
      if (label_matches(~env, ~expected_type, label)) {
        [
          make_candidate(
            ~source=Typedtree,
            ~label=label.Types.lbl_name,
            ~kind=Field,
            ~detail=Printtyp.string_of_type_scheme(label.Types.lbl_arg),
            ~sort_group=Sort_group.Typed,
            ~context,
            (),
          ),
          ...acc,
        ];
      } else {
        acc;
      },
    None,
    env,
    [],
  );

let in_scope_candidates = (~context, ~exclude_module_names=?, env) =>
  env_values(~context, env)
  @ env_modules(~context, ~exclude_module_names?, env)
  @ env_constructors(~context, env)
  @ env_labels(~context, env);

let expression_start_candidates =
    (~context, ~expected_type=?, ~exclude_module_names=?, env) =>
  env_expression_start_values(~context, ~expected_type?, env)
  @ env_constructors(~context, ~expected_type?, env)
  @ env_modules(~context, ~exclude_module_names?, env);

let pattern_candidates = (~context, ~expected_type=?, env) =>
  env_constructors(~context, ~expected_type?, env)
  @ env_labels(~context, ~expected_type?, env);

let import_path_candidates =
    (~context, ~exclude_module_names=?, ~keep_module=?, env) =>
  env_modules(
    ~context,
    ~sort_group=Sort_group.Import,
    ~exclude_module_names?,
    ~keep_module?,
    env,
  );
