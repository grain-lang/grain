open Grain;
open Grain_typed;
open Grain_utils;
open Grain_diagnostics;

type param = {
  param_name: string,
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
      module_namespace: string,
      module_name: string,
      name: string,
      type_sig: string,
      description: option(string),
      deprecations: list(deprecation),
      since: option(since),
      history: list(history),
      examples: list(example),
    })
  | Value({
      module_namespace: string,
      module_name: string,
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
    });

exception
  MissingFlag({
    flag: string,
    attr: string,
  });

exception MissingType;
exception InvalidAttribute;

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
    | _ => None
    }
  });

let enumerate_provides = program => {
  let id_tbl = ref(Ident.empty);

  let rec pattern_ids = ({pat_desc, pat_loc}: Typedtree.pattern) => {
    switch (pat_desc) {
    | TPatVar(id, _) => [(id, pat_loc)]
    | TPatAlias(subpat, id, _) => [(id, pat_loc), ...pattern_ids(subpat)]
    | TPatTuple(pats)
    | TPatArray(pats)
    | TPatConstruct(_, _, pats) => List.concat(List.map(pattern_ids, pats))
    | TPatRecord(elts, _) =>
      List.concat(List.map(((_, _, pat)) => pattern_ids(pat), elts))
    | _ => []
    };
  };

  module ProvideIterator =
    TypedtreeIter.MakeIterator({
      include TypedtreeIter.DefaultIteratorArgument;

      let enter_toplevel_stmt =
          ({ttop_desc, ttop_attributes}: Typedtree.toplevel_stmt) => {
        switch (ttop_desc) {
        | TTopData(decls) =>
          List.iter(
            ({data_id, data_loc}: Typedtree.data_declaration) => {
              id_tbl := Ident.add(data_id, data_loc, id_tbl^)
            },
            decls,
          )
        | TTopProvide(decls) =>
          List.iter(
            ({tex_id, tex_loc}: Typedtree.provide_declaration) => {
              id_tbl := Ident.add(tex_id, tex_loc, id_tbl^)
            },
            decls,
          )
        | TTopForeign({tvd_id, tvd_loc}) =>
          id_tbl := Ident.add(tvd_id, tvd_loc, id_tbl^)
        | TTopLet(_, _, vbinds) =>
          List.iter(
            ({vb_pat}: Typedtree.value_binding) => {
              List.iter(
                ((id, loc)) => {id_tbl := Ident.add(id, loc, id_tbl^)},
                pattern_ids(vb_pat),
              )
            },
            vbinds,
          )
        | TTopModule(_)
        | TTopInclude(_)
        | TTopException(_)
        | TTopExpr(_) => ()
        };
      };
    });

  ProvideIterator.iter_typed_program(program);

  id_tbl^;
};

let location_for_ident =
    (~provides: Ident.tbl(Grain_parsing.Location.t), ident) => {
  snd(Ident.find_name(Ident.name(ident), provides));
};

let title_for_api = (~module_name, ident: Ident.t) => {
  Format.asprintf("%s.**%a**", module_name, Printtyp.ident, ident);
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
      ({param_name, param_type, param_msg}) => {
        [Markdown.code(param_name), Markdown.code(param_type), param_msg]
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

let lookup_type_expr = (~idx, type_exprs) => {
  Option.bind(type_exprs, te => List.nth_opt(te, idx));
};

let for_value_description =
    (
      ~comments,
      ~provides,
      ~module_name,
      ~module_namespace,
      ~ident: Ident.t,
      vd: Types.value_description,
    ) => {
  let loc = location_for_ident(~provides, ident);
  let name = title_for_api(~module_name, ident);
  let type_sig = Printtyp.string_of_value_description(~ident, vd);
  let comment =
    Comments.Doc.ending_on(~lnum=loc.loc_start.pos_lnum - 1, comments);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, attributes)) => (description, attributes)
    | None => (None, [])
    };

  let (arg_types, return_type) = types_for_function(~ident, vd);

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
          // TODO(#787): Should we fail if more than one `@since` attribute?
          (
            deprecations,
            Some({since_version: attr_version}),
            history,
            params,
            returns,
            throws,
            examples,
          )
        | History({attr_version: history_version, attr_desc: history_msg}) => (
            deprecations,
            since,
            [{history_version, history_msg}, ...history],
            params,
            returns,
            throws,
            examples,
          )
        | Param({attr_name: param_name, attr_desc: param_msg}) =>
          // TODO: Use label lookups when labeled parameters are introduced

          let param_type =
            switch (lookup_type_expr(~idx=List.length(params), arg_types)) {
            | Some(typ) => Printtyp.string_of_type_sch(typ)
            | None => raise(MissingType)
            };

          (
            deprecations,
            since,
            history,
            [{param_name, param_type, param_msg}, ...params],
            returns,
            throws,
            examples,
          );
        | Returns({attr_desc: returns_msg}) =>
          let returns_type =
            switch (return_type) {
            | Some(typ) => Printtyp.string_of_type_sch(typ)
            | None => raise(MissingType)
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
    module_name,
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
    (
      ~comments,
      ~provides,
      ~module_name,
      ~module_namespace,
      ~ident: Ident.t,
      td: Types.type_declaration,
    ) => {
  let loc = location_for_ident(~provides, ident);
  let name = title_for_api(~module_name, ident);
  let type_sig = Printtyp.string_of_type_declaration(~ident, td);
  let comment =
    Comments.Doc.ending_on(~lnum=loc.loc_start.pos_lnum - 1, comments);

  let (description, attributes) =
    switch (comment) {
    | Some((_, description, _)) => (description, [])
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
          // TODO(#787): Should we fail if more than one `@since` attribute?
          (
            deprecations,
            Some({since_version: attr_version}),
            history,
            examples,
          )
        | History({attr_version: history_version, attr_desc: history_msg}) => (
            deprecations,
            since,
            [{history_version, history_msg}, ...history],
            examples,
          )
        | Param({attr_name: param_name, attr_desc: param_msg}) =>
          raise(InvalidAttribute)
        | Returns({attr_desc: returns_msg}) => raise(InvalidAttribute)
        | Throws({attr_type: throw_type, attr_desc: throw_msg}) =>
          raise(InvalidAttribute)
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
    module_name,
    name,
    type_sig,
    description,
    deprecations: List.rev(deprecations),
    since,
    history: List.rev(history),
    examples: List.rev(examples),
  });
};

let to_markdown = (~current_version, docblock) => {
  let buf = Buffer.create(0);

  switch (docblock) {
  | Type({name})
  | Value({name}) =>
    Buffer.add_string(buf, Markdown.heading(~level=3, name))
  };

  switch (docblock) {
  | Type({deprecations: []})
  | Value({deprecations: []}) => ()
  | Type({deprecations})
  | Value({deprecations}) =>
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
  | Type({since: None, history: []})
  | Value({since: None, history: []}) => ()
  | Type({since, history})
  | Value({since, history}) =>
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
  | Type({type_sig})
  | Value({type_sig}) =>
    Buffer.add_string(buf, Markdown.code_block(type_sig))
  };

  switch (docblock) {
  | Type({description: None})
  | Value({description: None}) => ()
  // Guard isn't be needed because we turn an empty string into None during extraction
  | Type({description: Some(desc)})
  | Value({description: Some(desc)}) =>
    Buffer.add_string(buf, Markdown.paragraph(desc))
  };

  switch (docblock) {
  | Type(_)
  | Value({params: []}) => ()
  | Value({params}) =>
    Buffer.add_string(buf, Markdown.paragraph("Parameters:"));
    Buffer.add_string(buf, output_for_params(params));
  };

  switch (docblock) {
  | Type(_)
  | Value({returns: None}) => ()
  | Value({returns: Some(returns)}) =>
    Buffer.add_string(buf, Markdown.paragraph("Returns:"));
    Buffer.add_string(buf, output_for_returns(returns));
  };

  switch (docblock) {
  | Type(_)
  | Value({throws: []}) => ()
  | Value({throws}) =>
    Buffer.add_string(buf, Markdown.paragraph("Throws:"));

    Buffer.add_string(buf, output_for_throws(throws));
  };

  switch (docblock) {
  | Type({examples: []})
  | Value({examples: []}) => ()
  | Type({examples})
  | Value({examples}) =>
    Buffer.add_string(buf, Markdown.paragraph("Examples:"));
    List.iter(
      ({example_txt}) =>
        Buffer.add_string(buf, Markdown.code_block(example_txt)),
      examples,
    );
  };

  buf;
};
