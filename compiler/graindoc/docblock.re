open Grain;
open Grain_typed;
open Grain_utils;
open Grain_diagnostics;

type param = {
  param_id: string,
  param_type: string,
  param_msg: string,
};

type since = {since_version: string};

type history = {
  history_version: string,
  history_msg: string,
};

type returns = {
  returns_type: string,
  returns_msg: string,
};

type deprecation = {deprecation_msg: string};

type throw = {
  throw_type: string,
  throw_msg: string,
};

type example = {example_txt: string};

type t =
  | Type({
      module_namespace: option(string),
      name: string,
      type_sig: string,
      description: option(string),
      deprecations: list(deprecation),
      since: option(since),
      history: list(history),
      examples: list(example),
    })
  | Value({
      module_namespace: option(string),
      name: string,
      type_sig: string,
      description: option(string),
      deprecations: list(deprecation),
      since: option(since),
      history: list(history),
      params: list(param),
      returns: option(returns),
      throws: list(throw),
      examples: list(example),
    })
  | Module({
      module_namespace: option(string),
      name: string,
      description: option(string),
      deprecations: list(deprecation),
      since: option(since),
      history: list(history),
      examples: list(example),
      provided,
    })

and provided = {
  provided_types: list(t),
  provided_values: list(t),
  provided_modules: list(t),
};

exception
  MissingFlag({
    flag: string,
    attr: string,
  });

exception MissingLabeledParamType({name: string});
exception MissingUnlabeledParamType({idx: int});
exception MissingReturnType;
exception AttributeAppearsMultipleTimes({attr: string});
exception
  InvalidAttribute({
    name: string,
    attr: string,
  });

let () =
  Printexc.register_printer(exn => {
    switch (exn) {
    | MissingFlag({flag, attr}) =>
      let msg =
        Printf.sprintf(
          "Must provide %s when generating docs with `%s` attribute.",
          flag,
          attr,
        );
      Some(msg);
    | MissingLabeledParamType({name}) =>
      let msg =
        Printf.sprintf(
          "Unable to find a matching function parameter for %s. Make sure a parameter exists with this label or use `@param <param_index> %s` for unlabeled parameters.",
          name,
          name,
        );
      Some(msg);
    | MissingUnlabeledParamType({idx}) =>
      let msg =
        Printf.sprintf(
          "Unable to find a type for parameter at index %d. Make sure a parameter exists at this index in the parameter list.",
          idx,
        );
      Some(msg);
    | MissingReturnType =>
      let msg = "Unable to find a return type. Please file an issue!";
      Some(msg);
    | AttributeAppearsMultipleTimes({attr}) =>
      let msg =
        Printf.sprintf("Attribute @%s is only allowed to appear once.", attr);
      Some(msg);
    | InvalidAttribute({name, attr}) =>
      let msg = Printf.sprintf("Invalid attribute @%s on %s", attr, name);
      Some(msg);
    | _ => None
    }
  });

let title_for_api = (~module_namespace, name) => {
  switch (module_namespace) {
  | Some(module_namespace) =>
    Format.sprintf("%s.%s", module_namespace, Markdown.bold(name))
  | None => name
  };
};

let title_for_namepace = (~module_namespace, name) => {
  switch (module_namespace) {
  | Some(module_namespace) => Format.sprintf("%s.%s", module_namespace, name)
  | None => name
  };
};

let output_for_since = (~current_version, {since_version}) => {
  let current_version =
    switch (current_version) {
    | Some(version) => version
    | None => raise(MissingFlag({flag: "--current-version", attr: "@since"}))
    };
  let (<) = Version.String.less_than;
  if (current_version < since_version) {
    Format.sprintf("Added in %s", Html.code("next"));
  } else {
    Format.sprintf("Added in %s", Html.code(since_version));
  };
};

let output_for_history = (~current_version, {history_version, history_msg}) => {
  let current_version =
    switch (current_version) {
    | Some(version) => version
    | None =>
      raise(MissingFlag({flag: "--current-version", attr: "@history"}))
    };
  let (<) = Version.String.less_than;
  if (current_version < history_version) {
    [Html.code("next"), history_msg];
  } else {
    [Html.code(history_version), history_msg];
  };
};

let output_for_params = params => {
  Markdown.table(
    ~headers=["param", "type", "description"],
    List.map(
      ({param_id, param_type, param_msg}) => {
        [Markdown.code(param_id), Markdown.code(param_type), param_msg]
      },
      params,
    ),
  );
};

let output_for_returns = ({returns_type, returns_msg}) => {
  Markdown.table(
    ~headers=["type", "description"],
    // Returns is only 1 item but we want to put it in a table, so we wrap in an outer list
    [[Markdown.code(returns_type), returns_msg]],
  );
};

let output_for_throws = throws => {
  // Used for joining multiple `@throws` annotations with the exact same type
  module StringMap = Map.Make(String);

  List.fold_left(
    (map, {throw_type, throw_msg}) => {
      StringMap.update(
        throw_type,
        descs => {
          switch (descs) {
          | None => Some([throw_msg])
          | Some(descs) => Some([throw_msg, ...descs])
          }
        },
        map,
      )
    },
    StringMap.empty,
    throws,
  )
  |> StringMap.bindings
  |> List.map(((exception_type, exception_descriptions)) => {
       Markdown.paragraph(Markdown.code(exception_type))
       ++ Markdown.bullet_list(List.rev(exception_descriptions))
     })
  |> String.concat("");
};

let types_for_function = (~ident, vd: Types.value_description) => {
  switch (Ctype.repr(vd.val_type).desc) {
  | TTyArrow(args, returns, _) => (Some(args), Some(returns))
  | _ => (None, None)
  };
};

let lookup_arg_by_label = (name, args_opt) => {
  Option.bind(args_opt, args =>
    List.find_opt(
      ((label: Grain_parsing.Asttypes.argument_label, _)) =>
        switch (label) {
        | Default(l)
        | Labeled(l) => l.txt == name
        | _ => false
        },
      args,
    )
  );
};

let lookup_type_expr = (~idx, type_exprs) => {
  Option.bind(type_exprs, te => List.nth_opt(te, idx));
};

let get_comments_from_loc = (loc: Grain_parsing.Location.t) => {
  open Compile;

  let comments =
    switch (
      compile_file(
        ~is_root_file=true,
        ~hook=stop_after_parse,
        loc.loc_start.pos_fname,
      )
    ) {
    | exception exn => []
    | {cstate_desc: Parsed(parsed_program)} => parsed_program.comments
    | _ => failwith("Invalid compilation state")
    };

  Comments.to_ordered(comments);
};

let for_value_description =
    (~module_namespace, ~ident: Ident.t, vd: Types.value_description) => {
  let loc = vd.val_loc;
  let comments = get_comments_from_loc(loc);
  let name = Format.asprintf("%a", Printtyp.ident, ident);
  let type_sig = Printtyp.string_of_value_description(~ident, vd);
  let comment =
    Comments.Doc.ending_on(~lnum=loc.loc_start.pos_lnum - 1, comments);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, attributes)) => (description, attributes)
    | None => (None, [])
    };

  let (args, return_type) = types_for_function(~ident, vd);

  let (deprecations, since, history, params, returns, throws, examples) =
    List.fold_left(
      (
        (deprecations, since, history, params, returns, throws, examples),
        attr: Comment_attributes.t,
      ) => {
        switch (attr) {
        | Deprecated({attr_desc}) => (
            [{deprecation_msg: attr_desc}, ...deprecations],
            since,
            history,
            params,
            returns,
            throws,
            examples,
          )
        | Since({attr_version}) =>
          switch (since) {
          | Some(_) => raise(AttributeAppearsMultipleTimes({attr: "since"}))
          | None => (
              deprecations,
              Some({since_version: attr_version}),
              history,
              params,
              returns,
              throws,
              examples,
            )
          }
        | History({attr_version: history_version, attr_desc: history_msg}) => (
            deprecations,
            since,
            [{history_version, history_msg}, ...history],
            params,
            returns,
            throws,
            examples,
          )
        | Param({attr_id: param_id, attr_desc: param_msg}) =>
          let (param_id, param_type) =
            switch (param_id) {
            | PositionalParam(idx) =>
              switch (lookup_type_expr(~idx, args)) {
              | Some((_, typ)) => (
                  string_of_int(idx),
                  Printtyp.string_of_type_sch(typ),
                )
              | None => raise(MissingUnlabeledParamType({idx: idx}))
              }
            | LabeledParam(name) =>
              switch (lookup_arg_by_label(name, args)) {
              | Some((Labeled(_), typ)) => (
                  name,
                  Printtyp.string_of_type_sch(typ),
                )
              // Default parameters have the type Option<a>; extract the type from the Option
              | Some((Default(_), {desc: TTyConstr(_, [typ], _)})) => (
                  "?" ++ name,
                  Printtyp.string_of_type_sch(typ),
                )
              | _ => raise(MissingLabeledParamType({name: name}))
              }
            };

          (
            deprecations,
            since,
            history,
            [{param_id, param_type, param_msg}, ...params],
            returns,
            throws,
            examples,
          );
        | Returns({attr_desc: returns_msg}) =>
          switch (returns) {
          | Some(_) =>
            raise(AttributeAppearsMultipleTimes({attr: "returns"}))
          | None =>
            let returns_type =
              switch (return_type) {
              | Some(typ) => Printtyp.string_of_type_sch(typ)
              | None => raise(MissingReturnType)
              };
            (
              deprecations,
              since,
              history,
              params,
              Some({returns_msg, returns_type}),
              throws,
              examples,
            );
          }
        | Throws({attr_type: throw_type, attr_desc: throw_msg}) => (
            deprecations,
            since,
            history,
            params,
            returns,
            [{throw_type, throw_msg}, ...throws],
            examples,
          )
        | Example({attr_desc}) => (
            deprecations,
            since,
            history,
            params,
            returns,
            throws,
            [{example_txt: attr_desc}, ...examples],
          )
        }
      },
      // deprecations, since, history, params, returns, throws, examples
      ([], None, [], [], None, [], []),
      attributes,
    );

  Value({
    module_namespace,
    name,
    type_sig,
    description,
    deprecations: List.rev(deprecations),
    since,
    history: List.rev(history),
    params: List.rev(params),
    returns,
    throws: List.rev(throws),
    examples: List.rev(examples),
  });
};

let for_type_declaration =
    (~module_namespace, ~ident: Ident.t, td: Types.type_declaration) => {
  let loc = td.type_loc;
  let comments = get_comments_from_loc(loc);
  let name = Format.asprintf("%a", Printtyp.ident, ident);
  let type_sig = Printtyp.string_of_type_declaration(~ident, td);
  let comment =
    Comments.Doc.ending_on(~lnum=loc.loc_start.pos_lnum - 1, comments);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, attributes)) => (description, attributes)
    | None => (None, [])
    };

  let (deprecations, since, history, examples) =
    List.fold_left(
      ((deprecations, since, history, examples), attr: Comment_attributes.t) => {
        switch (attr) {
        | Deprecated({attr_desc}) => (
            [{deprecation_msg: attr_desc}, ...deprecations],
            since,
            history,
            examples,
          )
        | Since({attr_version}) =>
          switch (since) {
          | Some(_) => raise(AttributeAppearsMultipleTimes({attr: "since"}))
          | None => (
              deprecations,
              Some({since_version: attr_version}),
              history,
              examples,
            )
          }
        | History({attr_version: history_version, attr_desc: history_msg}) => (
            deprecations,
            since,
            [{history_version, history_msg}, ...history],
            examples,
          )
        | Param({attr_id: param_id, attr_desc: param_msg}) =>
          raise(InvalidAttribute({name, attr: "param"}))
        | Returns({attr_desc: returns_msg}) =>
          raise(InvalidAttribute({name, attr: "returns"}))
        | Throws({attr_type: throw_type, attr_desc: throw_msg}) =>
          raise(InvalidAttribute({name, attr: "throws"}))
        | Example({attr_desc}) => (
            deprecations,
            since,
            history,
            [{example_txt: attr_desc}, ...examples],
          )
        }
      },
      // deprecations, since, history, examples
      ([], None, [], []),
      attributes,
    );

  Type({
    module_namespace,
    name,
    type_sig,
    description,
    deprecations: List.rev(deprecations),
    since,
    history: List.rev(history),
    examples: List.rev(examples),
  });
};

let rec traverse_signature_items = (~module_namespace, signature_items) => {
  let {provided_types, provided_values, provided_modules} =
    List.fold_left(
      (
        {provided_types, provided_values, provided_modules},
        sig_item: Types.signature_item,
      ) => {
        switch (sig_item) {
        | TSigType(ident, td, _) =>
          let docblock = for_type_declaration(~module_namespace, ~ident, td);
          {
            provided_types: [docblock, ...provided_types],
            provided_values,
            provided_modules,
          };
        | TSigValue(ident, vd) =>
          let docblock = for_value_description(~module_namespace, ~ident, vd);
          {
            provided_types,
            provided_values: [docblock, ...provided_values],
            provided_modules,
          };
        | TSigModule(
            ident,
            {md_type: TModSignature(signature_items), md_loc},
            _,
          ) =>
          let name = Format.asprintf("%a", Printtyp.ident, ident);
          let docblock =
            for_signature_items(
              ~module_namespace,
              ~name,
              ~loc=md_loc,
              signature_items,
            );
          {
            provided_types,
            provided_values,
            provided_modules: [docblock, ...provided_modules],
          };
        | TSigTypeExt(_)
        | TSigModType(_)
        | TSigModule(_) => {provided_types, provided_values, provided_modules}
        }
      },
      {provided_types: [], provided_values: [], provided_modules: []},
      signature_items,
    );

  {
    provided_types: List.rev(provided_types),
    provided_values: List.rev(provided_values),
    provided_modules: List.rev(provided_modules),
  };
}
and for_signature_items =
    (
      ~module_namespace,
      ~name,
      ~loc: Grain_parsing.Location.t,
      signature_items,
    ) => {
  let comments = get_comments_from_loc(loc);
  let comment =
    Comments.Doc.ending_on(~lnum=loc.loc_start.pos_lnum - 1, comments);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, attributes)) => (description, attributes)
    | None => (None, [])
    };

  let (deprecations, since, history, examples) =
    List.fold_left(
      ((deprecations, since, history, examples), attr: Comment_attributes.t) => {
        switch (attr) {
        | Deprecated({attr_desc}) => (
            [{deprecation_msg: attr_desc}, ...deprecations],
            since,
            history,
            examples,
          )
        | Since({attr_version}) =>
          switch (since) {
          | Some(_) => raise(AttributeAppearsMultipleTimes({attr: "since"}))
          | None => (
              deprecations,
              Some({since_version: attr_version}),
              history,
              examples,
            )
          }
        | History({attr_version: history_version, attr_desc: history_msg}) => (
            deprecations,
            since,
            [{history_version, history_msg}, ...history],
            examples,
          )
        | Param({attr_id: param_id, attr_desc: param_msg}) =>
          raise(InvalidAttribute({name, attr: "param"}))
        | Returns({attr_desc: returns_msg}) =>
          raise(InvalidAttribute({name, attr: "returns"}))
        | Throws({attr_type: throw_type, attr_desc: throw_msg}) =>
          raise(InvalidAttribute({name, attr: "throws"}))
        | Example({attr_desc}) => (
            deprecations,
            since,
            history,
            [{example_txt: attr_desc}, ...examples],
          )
        }
      },
      // deprecations, since, history, examples
      ([], None, [], []),
      attributes,
    );

  let provided =
    switch (signature_items) {
    | [] => {provided_types: [], provided_values: [], provided_modules: []}
    | _ =>
      let namespace = title_for_namepace(~module_namespace, name);

      traverse_signature_items(
        ~module_namespace=Some(namespace),
        signature_items,
      );
    };

  Module({
    module_namespace,
    name,
    description,
    deprecations: List.rev(deprecations),
    since,
    history: List.rev(history),
    examples: List.rev(examples),
    provided,
  });
};

let rec to_markdown = (~current_version, ~heading_level, docblock) => {
  let buf = Buffer.create(0);

  let next_heading_level = heading_level + 1;

  switch (docblock) {
  | Type({name, module_namespace})
  | Value({name, module_namespace}) =>
    Buffer.add_string(
      buf,
      Markdown.heading(
        ~level=next_heading_level,
        title_for_api(~module_namespace, name),
      ),
    )
  | Module({name, module_namespace: Some(_) as module_namespace}) =>
    Buffer.add_string(
      buf,
      Markdown.heading(
        ~level=heading_level,
        title_for_namepace(~module_namespace, name),
      ),
    )
  | Module(_) => () // No title for top-level modules
  };

  switch (docblock) {
  | Type({deprecations: []})
  | Value({deprecations: []})
  | Module({deprecations: []}) => ()
  | Type({deprecations})
  | Value({deprecations})
  | Module({deprecations}) =>
    List.iter(
      ({deprecation_msg}) =>
        Buffer.add_string(
          buf,
          Markdown.blockquote(
            Markdown.bold("Deprecated:") ++ " " ++ deprecation_msg,
          ),
        ),
      deprecations,
    )
  };

  switch (docblock) {
  // Type and Value descriptions are printed after signature, etc
  | Type(_)
  | Value(_)
  | Module({description: None}) => ()
  | Module({description: Some(desc)}) =>
    Buffer.add_string(buf, Markdown.paragraph(desc))
  };

  switch (docblock) {
  | Type({since: None, history: []})
  | Value({since: None, history: []})
  | Module({since: None, history: []}) => ()
  | Type({since, history})
  | Value({since, history})
  | Module({since, history}) =>
    let summary =
      Option.fold(
        ~none="History",
        ~some=output_for_since(~current_version),
        since,
      );
    let disabled =
      switch (history) {
      | [] => true
      | _ => false
      };
    let details =
      switch (history) {
      | [] => "No other changes yet."
      | _ =>
        Html.table(
          ~headers=["version", "changes"],
          List.map(output_for_history(~current_version), history),
        )
      };
    Buffer.add_string(buf, Html.details(~disabled, ~summary, details));
  };

  switch (docblock) {
  | Module(_) => ()
  | Type({type_sig})
  | Value({type_sig}) =>
    Buffer.add_string(buf, Markdown.code_block(type_sig))
  };

  switch (docblock) {
  | Type({description: None})
  | Value({description: None})
  // Module description comes first
  | Module(_) => ()
  // Guard isn't be needed because we turn an empty string into None during extraction
  | Type({description: Some(desc)})
  | Value({description: Some(desc)}) =>
    Buffer.add_string(buf, Markdown.paragraph(desc))
  };

  switch (docblock) {
  | Type(_)
  | Value({params: []})
  | Module(_) => ()
  | Value({params}) =>
    Buffer.add_string(buf, Markdown.paragraph("Parameters:"));
    Buffer.add_string(buf, output_for_params(params));
  };

  switch (docblock) {
  | Type(_)
  | Value({returns: None})
  | Module(_) => ()
  | Value({returns: Some(returns)}) =>
    Buffer.add_string(buf, Markdown.paragraph("Returns:"));
    Buffer.add_string(buf, output_for_returns(returns));
  };

  switch (docblock) {
  | Type(_)
  | Value({throws: []})
  | Module(_) => ()
  | Value({throws}) =>
    Buffer.add_string(buf, Markdown.paragraph("Throws:"));

    Buffer.add_string(buf, output_for_throws(throws));
  };

  switch (docblock) {
  | Type({examples: []})
  | Value({examples: []})
  | Module({examples: []}) => ()
  | Type({examples})
  | Value({examples}) =>
    Buffer.add_string(buf, Markdown.paragraph("Examples:"));
    List.iter(
      ({example_txt}) =>
        Buffer.add_string(buf, Markdown.code_block(example_txt)),
      examples,
    );
  // No "Examples:" paragraph for module examples
  | Module({examples}) =>
    List.iter(
      ({example_txt}) =>
        Buffer.add_string(buf, Markdown.code_block(example_txt)),
      examples,
    )
  };

  switch (docblock) {
  | Type(_)
  | Value(_)
  | Module({provided: {provided_types: []}}) => ()
  | Module({module_namespace, name, provided: {provided_types}}) =>
    let namespace = title_for_namepace(~module_namespace, name);
    Buffer.add_string(
      buf,
      Markdown.heading(~level=next_heading_level, "Types"),
    );
    Buffer.add_string(
      buf,
      Markdown.paragraph(
        "Type declarations included in the " ++ namespace ++ " module.",
      ),
    );
    List.iter(
      item =>
        Buffer.add_buffer(
          buf,
          to_markdown(
            ~current_version,
            ~heading_level=next_heading_level,
            item,
          ),
        ),
      provided_types,
    );
  };

  switch (docblock) {
  | Type(_)
  | Value(_)
  | Module({provided: {provided_values: []}}) => ()
  | Module({module_namespace, name, provided: {provided_values}}) =>
    let namespace = title_for_namepace(~module_namespace, name);
    Buffer.add_string(
      buf,
      Markdown.heading(~level=next_heading_level, "Values"),
    );
    Buffer.add_string(
      buf,
      Markdown.paragraph(
        "Functions and constants included in the " ++ namespace ++ " module.",
      ),
    );
    List.iter(
      item =>
        Buffer.add_buffer(
          buf,
          to_markdown(
            ~current_version,
            ~heading_level=next_heading_level,
            item,
          ),
        ),
      provided_values,
    );
  };

  switch (docblock) {
  | Type(_)
  | Value(_)
  | Module({provided: {provided_modules: []}}) => ()
  | Module({module_namespace, name, provided: {provided_modules}}) =>
    List.iter(
      item =>
        Buffer.add_buffer(
          buf,
          to_markdown(
            ~current_version,
            ~heading_level=next_heading_level,
            item,
          ),
        ),
      provided_modules,
    )
  };

  buf;
};
