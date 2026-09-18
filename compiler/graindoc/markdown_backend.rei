open Docblock.Docir;
open Grain_formatting;
open Grain_formatting.Doc;

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
let emit_value:
  (~current_version: option(string), ~heading_level: int, value_info) => Doc.t;

/**
 * Emits a markdown doctree for the given type documentation.
 *
 * @param current_version The current version of the document being documented. Used for `@since` and `@history` attributes.
 * @param heading_level The heading level to use for the type's title.
 * @return The markdown doctree for the given type documentation.
 *
 * @raise Error if the `current_version` is not provided and there are `@since` or `@history` attributes in the documentation.
 */
let emit_type:
  (~current_version: option(string), ~heading_level: int, type_info) => Doc.t;

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
let emit_module:
  (
    ~current_version: option(string),
    ~top_level: bool,
    ~heading_level: int,
    module_info
  ) =>
  Doc.t;

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
let emit_document:
  (~current_version: option(string), string, module_info) => Doc.t;
