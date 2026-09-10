open Grain_parsing;
open Grain_typed;
open Grain_diagnostics;
open Docir;
open Errors;

module Docir = Docir;
module Errors = Errors;

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                              COMMENT HANDLING                            │
// └──────────────────────────────────────────────────────────────────────────┘

type comment_info = (Comments.description, Comments.attributes);

let saved_comments = Hashtbl.create(64);

/**
 * Retrieves the docblock comment associated with the given location.
 *
 * @param including_attributes Weather to scan past grain attributes to find the comment. (should be false inside of type content)
 * @param loc The location to search for a comment
 * @return The `Some(comment_info)` associated with the given location, or `None` if no comment is found
 */
let get_comment_for_loc =
    (~including_attributes=true, loc: Grain_parsing.Location.t)
    : option(comment_info) => {
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
    including_attributes
      ? Comments.Doc.ending_on_including_attribute(
          ~lnum=loc.loc_start.pos_lnum - 1,
          comments,
        )
      : Comments.Doc.ending_on(~lnum=loc.loc_start.pos_lnum - 1, comments)
  ) {
  | Some((_, description, attributes)) =>
    Some((description, attributes): comment_info)
  | None => None
  };
};

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                             EXTRACTION HELPERS                           │
// └──────────────────────────────────────────────────────────────────────────┘

/**
 * Extracts the base graindoc information from a comment.
 *
 * @param comment_info The comment info to extract from
 * @return `(base_info, comment_info)` where `base_info` is the extracted base information and `comment_info` is the remaining comment info after extraction
 */
let extract_base_info =
    (comment_info: comment_info): (base_info, comment_info) => {
  let (description, attributes: Comments.attributes) = comment_info;
  let base_info: base_info = {
    description,
    deprecations: [],
    since: None,
    history: [],
    examples: [],
  };

  let (base_info, attributes) =
    List.fold_right(
      (attribute, (base_info, attributes)) => {
        let {attr, attr_loc}: Comment_attributes.t = attribute;
        switch (attr) {
        | Deprecated({attr_desc}) => (
            {
              ...base_info,
              deprecations: [
                {
                  txt: attr_desc,
                  loc: attr_loc,
                },
                ...base_info.deprecations,
              ],
            },
            attributes,
          )
        | Since({attr_version}) =>
          only_one_attribute(base_info.since, attribute);
          (
            {
              ...base_info,
              since:
                Some({
                  txt: attr_version,
                  loc: attr_loc,
                }),
            },
            attributes,
          );
        | History({attr_version, attr_desc}) => (
            {
              ...base_info,
              history: [
                {
                  txt: {
                    version: attr_version,
                    message: attr_desc,
                  },
                  loc: attr_loc,
                },
                ...base_info.history,
              ],
            },
            attributes,
          )
        | Example({attr_desc}) => (
            {
              ...base_info,
              examples: [
                {
                  txt: attr_desc,
                  loc: attr_loc,
                },
                ...base_info.examples,
              ],
            },
            attributes,
          )
        | _ => (base_info, [attribute, ...attributes])
        };
      },
      attributes,
      (base_info, []),
    );

  let comment_info = (description, attributes);

  (base_info, comment_info);
};

/**
 * Extracts the function information from a comment and value description.
 *
 * @param value_desc The value description to extract from
 * @param comment_info The comment info to extract from
 * @return `(function_info, comment_info)` where `function_info` is the extracted function information and `comment_info` is the remaining comment info after extraction
 */
let extract_function_info =
    (value_desc: Types.value_description, comment_info: comment_info) => {
  let (description, attributes) = comment_info;
  switch (Ctype.repr(value_desc.val_type).desc) {
  | TTyArrow(arg_types, returns_type, _) =>
    module StringMap = Map.Make(String);

    let params =
      List.mapi(
        (index, (label, typ: Types.type_expr)) => {
          let (label, param_name, typ) =
            switch (label) {
            | Asttypes.Unlabeled => (
                string_of_int(index),
                string_of_int(index),
                typ,
              )
            | Labeled({txt: name}) => (name, name, typ)
            // Default parameters have the type Option<a>; extract the type from the Option
            | Default({txt: name}) =>
              switch (typ) {
              | {desc: TTyConstr(_, [typ], _)} => (name, "?" ++ name, typ)
              | _ =>
                failwith(
                  "Impossible: Default parameter type is not an Option",
                )
              }
            };
          (
            label,
            (
              index,
              {
                param_name,
                param_type: Printtyp.string_of_type_sch(typ),
                param_desc: None,
              },
            ),
          );
        },
        arg_types,
      )
      |> StringMap.of_list;
    let (params, returns_desc, throws, attributes) =
      List.fold_left(
        ((params, returns_desc, throws, attributes), attribute) => {
          let {attr, attr_loc}: Comment_attributes.t = attribute;
          switch (attr) {
          | Param({attr_id, attr_desc}) =>
            let param_label =
              switch (attr_id) {
              | PositionalParam(idx, _) => string_of_int(idx)
              | LabeledParam(name, _) => name
              };
            let params =
              StringMap.update(
                param_label,
                param => {
                  switch (param) {
                  | Some((order: int, param)) =>
                    only_one_attribute(param.param_desc, attribute);
                    Some((
                      order,
                      {
                        ...param,
                        param_desc: Some(attr_desc),
                      },
                    ));
                  | None =>
                    switch (attr_id) {
                    | PositionalParam(idx, _) =>
                      raise(
                        Error(
                          attr_loc,
                          MissingUnlabeledParamType({idx: idx}),
                        ),
                      )
                    | LabeledParam(name, _) =>
                      raise(
                        Error(
                          attr_loc,
                          MissingLabeledParamType({name: name}),
                        ),
                      )
                    }
                  }
                },
                params,
              );
            (params, returns_desc, throws, attributes);
          | Returns({attr_desc}) =>
            only_one_attribute(returns_desc, attribute);
            (params, Some(attr_desc), throws, attributes);
          | Throws({attr_type, attr_desc}) =>
            let throw: throw = {
              txt: {
                throw_type: attr_type,
                throw_msg: attr_desc,
              },
              loc: attr_loc,
            };
            (params, returns_desc, [throw, ...throws], attributes);
          | _ => (params, returns_desc, throws, [attribute, ...attributes])
          };
        },
        (params, None, [], []),
        attributes,
      );
    let params =
      StringMap.to_list(params)
      |> List.sort(((_, (o1, _)), (_, (o2, _))) => o1 - o2)
      |> List.map(((_, (_, param))) => param);
    (
      Some({
        params,
        returns: {
          returns_type: Printtyp.string_of_type_sch(returns_type),
          returns_desc,
        },
        throws,
      }),
      (description, attributes),
    );
  | _ =>
    List.iter(
      (attr: Comment_attributes.t) => {
        switch (attr.attr) {
        | Param(_)
        | Returns(_)
        | Throws(_) => raise(attribute_appears_on_non_function(attr))
        | _ => ()
        }
      },
      attributes,
    );
    (None, comment_info);
  };
};

/** Extracts the record type content from a list of record fields. */
let extract_record_type_content = (content: list(Types.record_field)) => {
  let content =
    List.map(
      (field_info: Types.record_field) => {
        let comment_info =
          get_comment_for_loc(~including_attributes=false, field_info.rf_loc);
        let (field_desc, attributes) =
          Option.value(comment_info, ~default=(None, []));

        let field_name = Ident.name(field_info.rf_name);
        no_attributes_check(~name=field_name, attributes);
        {
          field_name,
          field_desc,
          field_type: Printtyp.string_of_type_sch(field_info.rf_type),
        };
      },
      content,
    );
  content;
};

/** Extracts the ADT type content from a list of constructor declarations. */
let extract_adt_type_content = (content: list(Types.constructor_declaration)) => {
  let content =
    List.map(
      (variant_info: Types.constructor_declaration) => {
        let comment_info =
          get_comment_for_loc(
            ~including_attributes=false,
            variant_info.cd_loc,
          );
        let (variant_desc, attributes) =
          Option.value(comment_info, ~default=(None, []));

        let variant_name = Ident.name(variant_info.cd_id);
        no_attributes_check(~name=variant_name, attributes);

        let variant_record_info =
          switch (variant_info.cd_args) {
          | TConstrRecord(rfs) => Some(extract_record_type_content(rfs))
          | _ => None
          };

        {
          variant_str: Printtyp.string_of_constructor(variant_info),
          variant_record_info,
          variant_desc,
        };
      },
      content,
    );
  content;
};

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                               AST GENERATION                             │
// └──────────────────────────────────────────────────────────────────────────┘

/**
 * Generates a docir from a type description.
 *
 * @param namespace The root namespace of the type
 * @param ident The identifier of the type
 * @param type_desc The type description to generate from
 * @return The docir for the given type description
 */
let from_type_description =
    (
      ~namespace: option(string),
      ~ident: Ident.t,
      type_desc: Types.type_declaration,
    ) => {
  let comment = get_comment_for_loc(type_desc.type_loc);
  let comment_info = Option.value(comment, ~default=(None, []));

  let (base_info, comment_info) = extract_base_info(comment_info);

  let type_content =
    switch (type_desc.type_kind) {
    | TDataVariant(cds) => Adt(extract_adt_type_content(cds))
    | TDataRecord(rfs) => Record(extract_record_type_content(rfs))
    | _ => NoContent
    };

  let (_, attributes) = comment_info;
  no_attributes_check(~name=Ident.name(ident), attributes);

  {
    name: Ident.name(ident),
    namespace,
    type_sig: Printtyp.string_of_type_declaration(~ident, type_desc),
    type_content,
    base_info,
  };
};

/**
 * Generates a docir from a value description.
 *
 * @param namespace The root namespace of the value
 * @param ident The identifier of the value
 * @param value_desc The value description to generate from
 * @return The docir for the given value description
 */
let from_value_description =
    (
      ~namespace: option(string),
      ~ident: Ident.t,
      value_desc: Types.value_description,
    ) => {
  let comment = get_comment_for_loc(value_desc.val_loc);
  let comment_info = Option.value(comment, ~default=(None, []));

  let type_sig = Printtyp.string_of_value_description(~ident, value_desc);

  let (base_info, comment_info) = extract_base_info(comment_info);
  let (function_info, comment_info) =
    extract_function_info(value_desc, comment_info);

  let (_, attributes) = comment_info;
  no_attributes_check(~name=Ident.name(ident), attributes);

  {
    name: Format.asprintf("%a", Printtyp.ident, ident),
    namespace,
    type_sig,
    function_info,
    base_info,
  };
};

/**
 * Generates a docir from a module signature.
 *
 * @param namespace The root namespace of the module
 * @param ident The identifier of the module
 * @param loc The location of the module
 * @param signature The module signature to generate from
 * @return The docir for the given module signature
 */
let rec from_module_signature =
        (
          ~namespace: option(string),
          ~name: string,
          ~loc: Location.t,
          signature: Types.signature,
        ) => {
  let comment = get_comment_for_loc(loc);
  let comment_info = Option.value(comment, ~default=(None, []));

  let (base_info, comment_info) = extract_base_info(comment_info);

  let new_namespace =
    Some(Option.fold(~none=name, ~some=ns => ns ++ "." ++ name, namespace));

  let module_content = {
    provided_types: [],
    provided_values: [],
    provided_modules: [],
  };
  let module_content =
    List.fold_right(
      (sig_item: Types.signature_item, module_content) => {
        switch (sig_item) {
        | TSigType(ident, type_desc, _) =>
          let docblock =
            from_type_description(
              ~namespace=new_namespace,
              ~ident,
              type_desc,
            );
          {
            ...module_content,
            provided_types: [docblock, ...module_content.provided_types],
          };
        | TSigValue(ident, value_desc) =>
          let docblock =
            from_value_description(
              ~namespace=new_namespace,
              ~ident,
              value_desc,
            );
          {
            ...module_content,
            provided_values: [docblock, ...module_content.provided_values],
          };
        | TSigModule(
            ident,
            {md_type: TModSignature(signature_items), md_loc},
            _,
          ) =>
          let docblock =
            from_module_signature(
              ~namespace=new_namespace,
              ~name=Ident.name(ident),
              ~loc=md_loc,
              signature_items,
            );
          {
            ...module_content,
            provided_modules: [docblock, ...module_content.provided_modules],
          };
        | TSigTypeExt(_)
        | TSigModType(_)
        | TSigModule(_) => module_content
        }
      },
      signature,
      module_content,
    );

  {
    name,
    namespace,
    module_content,
    base_info,
  };
};

/**
 * Generates a docir from a program.
 *
 * @param program The program to generate from
 * @return The docir for the given module program
 */
let from_program = (program: Typedtree.typed_program) => {
  from_module_signature(
    ~namespace=None,
    ~name=program.module_name.txt,
    ~loc=program.mod_loc,
    program.signature.cmi_sign,
  );
};
