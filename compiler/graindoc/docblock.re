open Grain_parsing;
open Grain_typed;
open Grain_diagnostics;
open Docir;

// Error handling
type error =
  // | MissingFlag({
  //     flag: string,
  //     attr: string,
  //   })
  | MissingLabeledParamType({name: string})
  | MissingUnlabeledParamType({idx: int})
  | ParameterAttributeAppearsMultipleTimes({param_name: string})
  | AttributeAppearsOnNonFunction({attr: string})
  | AttributeAppearsMultipleTimes({attr: string});
// | InvalidAttribute({
//     name: string,
//     attr: string,
//   });

exception Error(Location.t, error);

let report_error = (ppf, err) => {
  switch (err) {
  // | MissingFlag({flag, attr}) =>
  //   Format.fprintf(
  //     ppf,
  //     "Must provide %s when generating docs with `%s` attribute.",
  //     flag,
  //     attr,
  //   )
  | MissingLabeledParamType({name}) =>
    Format.fprintf(
      ppf,
      "Unable to find a matching function parameter for %s. Make sure a parameter exists with this label or use `@param <param_index> %s` for unlabeled parameters.",
      name,
      name,
    )
  | MissingUnlabeledParamType({idx}) =>
    Format.fprintf(
      ppf,
      "Unable to find a type for parameter at index %d. Make sure a parameter exists at this index in the parameter list.",
      idx,
    )
  | ParameterAttributeAppearsMultipleTimes({param_name}) =>
    Format.fprintf(
      ppf,
      "Parameter @%s is only allowed to have one @param attribute.",
      param_name,
    )
  | AttributeAppearsOnNonFunction({attr}) =>
    Format.fprintf(ppf, "Attribute @%s is only allowed on functions.", attr)
  | AttributeAppearsMultipleTimes({attr}) =>
    Format.fprintf(ppf, "Attribute @%s is only allowed to appear once.", attr)
  // | InvalidAttribute({name, attr}) =>
  //   Format.fprintf(ppf, "Invalid attribute @%s on %s", attr, name)
  };
};

// TODO: Figure out the issue here
// let () =
//   Location.register_error_of_exn(
//     fun
//     | Error(loc, err) =>
//       Some(Location.error_of_printer(loc, report_error, err))
//     | _ => None,
//   );

// Comment handling
let saved_comments = Hashtbl.create(64);

/**
 * Retrieves the docblock comment associated with the given location.
 *
 * @param loc The location within the file for which to retrieve comments.
 * @return The comments associated with the file of the given location.
 */
let get_comment_for_loc = (loc: Grain_parsing.Location.t) => {
  // Find the comments for the file associated with the given location.
  let file = loc.loc_start.pos_fname;
  let comments =
    switch (Hashtbl.find_opt(saved_comments, file)) {
    | Some(comments) => comments
    | None =>
      open Grain.Compile;
      let comments =
        switch (compile_file(~hook=stop_after_parse, file)) {
        | exception exn => []
        | {cstate_desc: Parsed(parsed_program)} => parsed_program.comments
        | _ => failwith("Invalid compilation state")
        };
      let ordered = Comments.to_ordered(comments);
      Hashtbl.add(saved_comments, file, ordered);
      ordered;
    };

  switch (
    Comments.Doc.ending_on_including_attribute(
      ~lnum=loc.loc_start.pos_lnum - 1,
      comments,
    )
  ) {
  | Some((_, description, attributes)) => Some((description, attributes))
  | None => None
  };
};

let only_one_attr =
    (attr_value: option('a), attr_name: string, attr_loc: Location.t) => {
  switch (attr_value) {
  | Some(_) =>
    raise(Error(attr_loc, AttributeAppearsMultipleTimes({attr: attr_name})))
  | None => ()
  };
};

let get_func_info = (typ: Types.type_expr) => {
  switch (Ctype.repr(typ).desc) {
  | TTyArrow(args, returns, _) => Some((args, returns))
  | _ => None
  };
};

// Graindoc generation
let from_type_description = (type_desc: Types.type_declaration) => {
  // TODO:
};
let from_value_description =
    (~ident: Ident.t, value_desc: Types.value_description) => {
  // TODO: Look into caching the doc ast

  // Get the docblock comment associated with the value location.
  let comment = get_comment_for_loc(value_desc.val_loc);

  // Get the parameter information
  let (description, attributes) =
    Option.value(comment, ~default=(None, []));
  let function_info =
    switch (get_func_info(value_desc.val_type)) {
    | Some((args, returns)) =>
      let params =
        List.mapi(
          (index, (label, typ: Types.type_expr)) => {
            let (param_name, param_type) =
              switch (label, typ) {
              | (Asttypes.Unlabeled, _) => (string_of_int(index), typ)
              | (Labeled({txt: name}), _) => (name, typ)
              // Default parameters have the type Option<a>; extract the type from the Option
              | (Default({txt: name}), {desc: TTyConstr(_, [typ], _)}) => (
                  "?" ++ name,
                  typ,
                )
              | (Default(_), _) =>
                failwith(
                  "Impossible: Default parameter type is not an Option",
                )
              };
            {
              param_id: label,
              param_name,
              param_type,
              param_msg: None,
            };
          },
          args,
        );
      Some({
        params,
        returns: {
          returns_type: returns,
          returns_msg: None,
        },
        throws: [],
      });
    | None => None
    };
  let value_info: value_info = {
    name: Ident.name(ident),
    type_sig: value_desc.val_type,
    function_info,
    // Docblock info
    description,
    deprecations: [],
    since: None,
    history: [],
    examples: [],
  };
  let value_info =
    List.fold_left(
      (value_info: value_info, {attr, attr_loc}: Comment_attributes.t) => {
        switch (attr) {
        | Deprecated({attr_desc}) => {
            ...value_info,
            deprecations: [
              {
                txt: attr_desc,
                loc: attr_loc,
              },
              ...value_info.deprecations,
            ],
          }
        | Since({attr_version}) =>
          only_one_attr(value_info.since, "since", attr_loc);
          {
            ...value_info,
            since:
              Some({
                txt: attr_version,
                loc: attr_loc,
              }),
          };
        | History({attr_version, attr_desc}) => {
            ...value_info,
            history: [
              {
                txt: {
                  version: attr_version,
                  message: attr_desc,
                },
                loc: attr_loc,
              },
              ...value_info.history,
            ],
          }
        | Example({attr_desc}) => {
            ...value_info,
            examples: [
              {
                txt: attr_desc,
                loc: attr_loc,
              },
              ...value_info.examples,
            ],
          }
        // Function info
        | Param({attr_id, attr_desc}) =>
          switch (value_info.function_info) {
          | Some(func_info) =>
            // TODO: Determine a better way of implementing this
            // TODO: Validate this should be fold_left not fold_right
            let (params, _, found) =
              List.fold_left(
                ((params, index, found), arg) => {
                  let is_match =
                    switch (attr_id) {
                    | PositionalParam(idx, _)
                        when arg.param_id == Unlabeled && index == idx =>
                      true
                    | LabeledParam(name, _) when name == arg.param_name =>
                      true
                    | _ => false
                    };
                  if (!is_match || found) {
                    ([arg, ...params], index + 1, found);
                  } else {
                    switch (arg.param_msg) {
                    | None => (
                        [
                          {
                            ...arg,
                            param_msg:
                              Some({
                                txt: attr_desc,
                                loc: attr_loc,
                              }),
                          },
                          ...params,
                        ],
                        index + 1,
                        true,
                      )
                    | Some(_) =>
                      raise(
                        Error(
                          attr_loc,
                          ParameterAttributeAppearsMultipleTimes({
                            param_name: arg.param_name,
                          }),
                        ),
                      )
                    };
                  };
                },
                ([], 0, false),
                func_info.params,
              );
            if (!found) {
              raise(
                Error(
                  attr_loc,
                  switch (attr_id) {
                  | PositionalParam(idx, _) =>
                    MissingUnlabeledParamType({idx: idx})
                  | LabeledParam(name, _) =>
                    MissingLabeledParamType({name: name})
                  },
                ),
              );
            };
            {
              ...value_info,
              function_info:
                Some({
                  ...func_info,
                  params,
                }),
            };
          | None =>
            raise(
              Error(
                attr_loc,
                AttributeAppearsOnNonFunction({attr: "params"}),
              ),
            )
          }
        | Returns({attr_desc}) =>
          switch (value_info.function_info) {
          | Some(func_info) =>
            only_one_attr(func_info.returns.returns_msg, "returns", attr_loc);
            {
              ...value_info,
              function_info:
                Some({
                  ...func_info,
                  returns: {
                    ...func_info.returns,
                    returns_msg:
                      Some({
                        txt: attr_desc,
                        loc: attr_loc,
                      }),
                  },
                }),
            };
          | None =>
            raise(
              Error(
                attr_loc,
                AttributeAppearsOnNonFunction({attr: "returns"}),
              ),
            )
          }
        | Throws({attr_type, attr_desc}) =>
          switch (value_info.function_info) {
          | Some(func_info) => {
              ...value_info,
              function_info:
                Some({
                  ...func_info,
                  throws: [
                    {
                      txt: {
                        throw_type: attr_type,
                        throw_msg: attr_desc,
                      },
                      loc: attr_loc,
                    },
                    ...func_info.throws,
                  ],
                }),
            }
          | None =>
            raise(
              Error(
                attr_loc,
                AttributeAppearsOnNonFunction({attr: "throws"}),
              ),
            )
          }
        }
      },
      value_info,
      attributes,
    );

  Docir.Value(value_info);
};
// let from_module_description
let from_program = (program: Typedtree.typed_program) => {
  // TODO:
};
