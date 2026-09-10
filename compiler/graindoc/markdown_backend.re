open Docblock;
open Docblock.Docir;
open Docblock.Errors;
open Grain_formatting;
open Grain_formatting.Doc;

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                             DOCTREE PRINTERS                             │
// └──────────────────────────────────────────────────────────────────────────┘
/** Utilities for generating markdown doctrees. */
module Markdown = {
  /**
   * Generates a frontmatter block for a markdown document.
   * @param entries A list of key-value pairs to include in the frontmatter block.
   * @return A doctree containing the frontmatter block.
   */
  let frontmatter = (entries: list((string, string))) => {
    group(
      string("---")
      ++ concat_map(
           ~lead=next => hardline,
           ~sep=(prev, next) => hardline,
           ~trail=prev => hardline,
           ~f=
             (~final, (key, value)) =>
               group(string(key) ++ string(": ") ++ string(value)),
           entries,
         )
      ++ string("---"),
    )
    ++ hardline
    ++ hardline;
  };

  /**
   * Generates a heading for a markdown document.
   * @param level The level of the heading (1-6)
   * @param doc The content of the heading
   * @return A doctree containing the heading
   */
  let heading = (~level=1, doc) =>
    group(string(String.make(level, '#')) ++ string(" ") ++ doc)
    ++ hardline
    ++ hardline;

  /**
   * Generates a blockquote for a markdown document.
   * @param doc The content of the blockquote
   * @return A doctree containing the blockquote
   */
  let blockquote = doc => group(string("> ") ++ doc) ++ hardline ++ hardline;

  /**
   * Generates bold text for a markdown document.
   * @param str The text to make bold
   * @return A doctree containing the bold text
   */
  let bold = str => {
    let escaped_str =
      Str.global_substitute(
        Str.regexp({|\(^\*+\)\|\(\*\*+\)\|\(\*+$\)|}),
        str => {
          let matched = Str.matched_string(str);
          Str.global_replace(Str.regexp({|\*|}), {|\*|}, matched);
        },
        str,
      );
    group(string("**") ++ string(escaped_str) ++ string("**"));
  };

  /**
   * Generates a paragraph for a markdown document.
   * @param doc The content of the paragraph
   * @return A doctree containing the paragraph
   */
  let paragraph = doc => doc ++ hardline ++ hardline;

  /**
   * Generates a code block for a markdown document.
   * @param syntax The syntax highlighting for the code block
   * @param doc The content of the code block
   * @return A doctree containing the code block
   */
  let code_block = (~syntax="grain", doc) => {
    group(
      string("```")
      ++ string(syntax)
      ++ hardline
      ++ doc
      ++ hardline
      ++ string("```"),
    )
    ++ hardline
    ++ hardline;
  };

  /**
   * Generates inline code for a markdown document.
   * @param doc The content of the inline code
   * @return A doctree containing the inline code
   */
  let code = doc => group(string("`") ++ doc ++ string("`"));

  /**
   * Generates a bullet list for a markdown document.
   * @param items The items in the bullet list
   * @return A doctree containing the bullet list
   */
  let bullet_list = (items: list(Doc.t)) => {
    concat_map(
      ~lead=next => empty,
      ~sep=(prev, next) => hardline,
      ~trail=prev => empty,
      ~f=(~final, doc: Doc.t) => group(string("* ") ++ doc),
      items,
    )
    ++ hardline
    ++ hardline;
  };
};

/** Utilities for generating html doctrees. */
module Html = {
  /**
   * Generates a details element for an html doctree.
   * @param disabled Whether the details element is disabled
   * @param summary The summary of the details element
   * @param msg The message of the details element
   * @return A doctree containing the details element
   */
  let details = (~disabled=false, ~summary, msg) => {
    // The `disabled` html attribute doesn't do anything to <details> but we add it for easier styling
    let details_state = string(disabled ? "<details disabled>" : "<details>");
    let details_summary_state =
      string(disabled ? {|<summary tabindex="-1">|} : "<summary>");
    group(
      details_state
      ++ hardline
      ++ details_summary_state
      ++ summary
      ++ string("</summary>")
      ++ hardline
      ++ msg
      ++ hardline
      ++ string("</details>"),
    )
    ++ hardline
    ++ hardline;
  };
  /**
   * Generates inline code for an html doctree.
   * @param doc The content of the inline code
   * @return A doctree containing the inline code
   */
  let code = doc => group(string("<code>") ++ doc ++ string("</code>"));
};

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                                 HELPERS                                  │
// └──────────────────────────────────────────────────────────────────────────┘
let has_any_record_field_descrs = fields =>
  List.exists(f => Option.is_some(f.field_desc), fields);
let variant_has_desc = variant =>
  Option.is_some(variant.variant_desc)
  // Should also pass through inline record variants that do not have
  // descriptions themselves but have descriptions for some of their fields
  || Option.map(has_any_record_field_descrs, variant.variant_record_info)
  == Some(true);

let get_api_title = (~namespace, name) => {
  let (++) = Stdlib.(++);
  Option.fold(~none=name, ~some=ns => ns ++ "." ++ name, namespace);
};

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                            ATTRIBUTE EMITTERS                            │
// └──────────────────────────────────────────────────────────────────────────┘
let join = (f, lst) => {
  List.fold_left((acc, item) => acc ++ f(item), empty, lst);
};

let emit_item_path = (~heading_level, ~namespace, name) => {
  let path =
    Option.fold(
      ~none=string(name),
      ~some=ns => string(ns) ++ string(".") ++ Markdown.bold(name),
      namespace,
    );
  Markdown.heading(~level=heading_level, path);
};

let emit_deprecations = (deprecations: list(deprecation)) => {
  join(
    ({txt}: deprecation) =>
      Markdown.blockquote(
        Markdown.bold("Deprecated:") ++ string(" ") ++ string(txt),
      ),
    deprecations,
  );
};

let emit_since =
    (~current_version, since: Grain_parsing.Location.loc(string)) => {
  let (<) = Grain_utils.Version.String.less_than;
  let current_version =
    get_current_version(~current_version, ~loc=since.loc, ~attr="@since");
  group(
    string("Added in ")
    ++ Html.code(string(current_version < since.txt ? "next" : since.txt)),
  );
};
let emit_history = (~current_version, history: history) => {
  let (<) = Grain_utils.Version.String.less_than;
  let current_version =
    get_current_version(~current_version, ~loc=history.loc, ~attr="@history");
  [
    Grain_utils.Html.code(
      current_version < history.txt.version ? "next" : history.txt.version,
    ),
    history.txt.message,
  ];
};
let emit_since_and_history = (~current_version, base_info: base_info) => {
  switch (base_info.since, base_info.history) {
  | (None, []) => empty
  | (since, history) =>
    let summary =
      switch (since) {
      | None => string("History")
      | Some(since) => emit_since(~current_version, since)
      };
    let disabled = List.is_empty(history);
    let details =
      switch (history) {
      | [] => string("No other changes yet.")
      | _ =>
        string(
          Grain_utils.Html.table(
            ~headers=["version", "changes"],
            List.map(
              history => emit_history(~current_version, history),
              history,
            ),
          ),
        )
      };
    Html.details(~disabled, ~summary, details);
  };
};

let emit_description = (description: description) => {
  switch (description) {
  | None => empty
  | Some(desc) => Markdown.paragraph(string(desc))
  };
};

let emit_examples = (~module_level, examples: list(example)) => {
  switch (examples) {
  | [] => empty
  | _ =>
    group(
      (module_level ? empty : Markdown.paragraph(string("Examples:")))
      ++ join(
           ({txt}: example) => Markdown.code_block(string(txt)),
           examples,
         ),
    )
  };
};

// Types
let emit_record_info = (record_info: record_info) =>
  if (has_any_record_field_descrs(record_info)) {
    group(
      Markdown.paragraph(string("Fields:"))
      ++ string(
           Grain_utils.Markdown.table(
             ~headers=["name", "type", "description"],
             List.map(
               rf => {
                 [
                   Grain_utils.Markdown.code(rf.field_name),
                   Grain_utils.Markdown.code(rf.field_type),
                   Option.value(rf.field_desc, ~default=""),
                 ]
               },
               record_info,
             ),
           ),
         ),
    );
  } else {
    empty;
  };

let emit_adt_info = (adt_info: adt_info) =>
  switch (adt_info) {
  | [] => empty
  | _
      when
        List.exists(variant_info => variant_has_desc(variant_info), adt_info) =>
    Markdown.paragraph(string("Variants:"))
    ++ join(
         (variant_info: variant_info) =>
           (
             variant_has_desc(variant_info)
               ? Markdown.code_block(string(variant_info.variant_str))
               : empty
           )
           ++ (
             switch (variant_info.variant_desc) {
             | None => empty
             | Some(desc) => Markdown.paragraph(string(desc))
             }
           )
           ++ (
             switch (variant_info.variant_record_info) {
             | None => empty
             | Some(record_info) => emit_record_info(record_info)
             }
           ),
         adt_info,
       )
  | _ => empty
  };
let emit_type_content = (type_content: type_content) => {
  switch (type_content) {
  | Record(record_info) => emit_record_info(record_info)
  | Adt(adt_info) => emit_adt_info(adt_info)
  | NoContent => empty
  };
};

// Functions
let emit_params = params => {
  switch (params) {
  | [] => empty
  | _ when List.exists(param => Option.is_some(param.param_desc), params) =>
    Markdown.paragraph(string("Parameters:"))
    ++ string(
         Grain_utils.Markdown.table(
           ~headers=["param", "type", "description"],
           List.map(
             ({param_name, param_type, param_desc}) => {
               [
                 Grain_utils.Markdown.code(param_name),
                 Grain_utils.Markdown.code(param_type),
                 switch (param_desc) {
                 | None => ""
                 | Some(msg) => msg
                 },
               ]
             },
             params,
           ),
         ),
       )
  | _ => empty
  };
};

let emit_returns = (returns: returns) => {
  switch (returns.returns_desc) {
  | None => empty
  | Some(returns_desc) =>
    let returns_type = Grain_utils.Markdown.code(returns.returns_type);
    Markdown.paragraph(string("Returns:"))
    ++ string(
         Grain_utils.Markdown.table(
           ~headers=["type", "description"],
           // Returns is only 1 item but we want to put it in a table, so we wrap in an outer list
           [[returns_type, returns_desc]],
         ),
       );
  };
};

let emit_throws = (throws: list(throw)) => {
  switch (throws) {
  | [] => empty
  | _ =>
    // Used for joining multiple `@throws` annotations with the exact same type
    module StringMap = Map.Make(String);
    let entries =
      List.fold_left(
        (map, {txt: {throw_type, throw_msg}}: throw) => {
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
      );
    Markdown.paragraph(string("Throws:"))
    ++ join(
         ((throw_type, throw_descs): (string, list(string))) =>
           Markdown.paragraph(Markdown.code(string(throw_type)))
           ++ Markdown.bullet_list(
                List.map(desc => string(desc), throw_descs),
              ),
         StringMap.bindings(entries),
       );
  };
};
let emit_function_info = (function_info: option(function_info)) => {
  switch (function_info) {
  | None => empty
  | Some({params, returns, throws}) =>
    emit_params(params) ++ emit_returns(returns) ++ emit_throws(throws)
  };
};

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                              ITEM EMITTERS                               │
// └──────────────────────────────────────────────────────────────────────────┘

/**
 * Emits a markdown doctree for the given value documentation.
 *
 * @param current_version The current version of the document being documented. Used for `@since` and `@history` attributes.
 * @param heading_level The heading level to use for the value's title.
 * @return The markdown doctree for the given value documentation.
 *
 * @raise Error if the `current_version` is not provided and there are `@since` or `@history` attributes in the documentation.
 */
let emit_value = (~current_version, ~heading_level, value_info: value_info) => {
  group(
    emit_item_path(
      ~heading_level=heading_level + 1,
      ~namespace=value_info.namespace,
      value_info.name,
    )
    ++ emit_deprecations(value_info.base_info.deprecations)
    ++ emit_since_and_history(~current_version, value_info.base_info)
    ++ Markdown.code_block(string(value_info.type_sig))
    ++ emit_description(value_info.base_info.description)
    ++ emit_function_info(value_info.function_info)
    ++ emit_examples(~module_level=false, value_info.base_info.examples),
  );
};

/**
 * Emits a markdown doctree for the given type documentation.
 *
 * @param current_version The current version of the document being documented. Used for `@since` and `@history` attributes.
 * @param heading_level The heading level to use for the type's title.
 * @return The markdown doctree for the given type documentation.
 *
 * @raise Error if the `current_version` is not provided and there are `@since` or `@history` attributes in the documentation.
 */
let emit_type = (~current_version, ~heading_level, type_info: type_info) => {
  group(
    emit_item_path(
      ~heading_level=heading_level + 1,
      ~namespace=type_info.namespace,
      type_info.name,
    )
    ++ emit_deprecations(type_info.base_info.deprecations)
    ++ emit_since_and_history(~current_version, type_info.base_info)
    ++ Markdown.code_block(string(type_info.type_sig))
    ++ emit_description(type_info.base_info.description)
    ++ emit_type_content(type_info.type_content)
    ++ emit_examples(~module_level=false, type_info.base_info.examples),
  );
};

/**
 * Emits a markdown doctree for the given module documentation.
 *
 * @param current_version The current version of the document being documented. Used for `@since` and `@history` attributes.
 * @param top_level Whether or not this module is the top-level module being documented.
 * @param heading_level The heading level to use for the module's title.
 * @return The markdown doctree for the given module documentation.
 *
 * @raise Error if the `current_version` is not provided and there are `@since` or `@history` attributes in the documentation.
 */
let rec emit_module =
        (~current_version, ~top_level, ~heading_level, module_info) => {
  let next_heading_level = heading_level + 1;
  group(
    (
      top_level
        ? empty
        : Markdown.heading(
            ~level=heading_level,
            string(
              get_api_title(
                ~namespace=module_info.namespace,
                module_info.name,
              ),
            ),
          )
    )
    ++ emit_deprecations(module_info.base_info.deprecations)
    ++ emit_description(module_info.base_info.description)
    ++ emit_since_and_history(~current_version, module_info.base_info)
    ++ emit_examples(~module_level=true, module_info.base_info.examples)
    ++ (
      switch (module_info.module_content.provided_types) {
      | [] => empty
      | types =>
        Markdown.heading(~level=next_heading_level, string("Types"))
        ++ Markdown.paragraph(
             string("Type declarations included in the ")
             ++ string(
                  get_api_title(
                    ~namespace=module_info.namespace,
                    module_info.name,
                  ),
                )
             ++ string(" module."),
           )
        ++ join(
             type_info =>
               emit_type(
                 ~current_version,
                 ~heading_level=next_heading_level,
                 type_info,
               ),
             types,
           )
      }
    )
    ++ (
      switch (module_info.module_content.provided_values) {
      | [] => empty
      | values =>
        Markdown.heading(~level=next_heading_level, string("Values"))
        ++ Markdown.paragraph(
             string("Functions and constants included in the ")
             ++ string(
                  get_api_title(
                    ~namespace=module_info.namespace,
                    module_info.name,
                  ),
                )
             ++ string(" module."),
           )
        ++ join(
             value_info =>
               emit_value(
                 ~current_version,
                 ~heading_level=next_heading_level,
                 value_info,
               ),
             values,
           )
      }
    )
    ++ (
      switch (module_info.module_content.provided_modules) {
      | [] => empty
      | modules =>
        join(
          module_info =>
            emit_module(
              ~current_version,
              ~top_level=false,
              ~heading_level=next_heading_level,
              module_info,
            ),
          modules,
        )
      }
    ),
  );
};

/**
 * Emits a markdown doctree for the given module documentation.
 *
 * @param current_version The current version of the document being documented. Used for `@since` and `@history` attributes.
 * @param module_name The name of the module being documented.
 * @param module_info The module documentation to emit.
 * @return The markdown document for the given module documentation.
 *
 * @raise Error if the `current_version` is not provided and there are `@since` or `@history` attributes in the documentation.
 */
let emit_document =
    (~current_version, module_name: string, module_info: module_info) => {
  group(
    Markdown.frontmatter([("title", module_name)])
    ++ emit_module(
         ~current_version,
         ~top_level=true,
         ~heading_level=1,
         module_info,
       ),
  );
};
