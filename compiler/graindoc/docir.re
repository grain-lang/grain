open Grain_parsing;
open Grain_typed;
open Grain_utils;

// TODO: Enable sexp

// Parameters
type description = option(string);
type deprecation = Location.loc(string);
type since = option(Location.loc(string));
type history_info = {
  version: string,
  message: string,
};
type history = Location.loc(history_info);
type example = Location.loc(string);

type param_info = Location.loc(string);
type param = {
  param_id: Asttypes.argument_label,
  param_name: string,
  param_type: Types.type_expr,
  param_msg: option(param_info),
};
type return_info = Location.loc(string);
type returns = {
  returns_type: Types.type_expr,
  returns_msg: option(return_info),
};
type throw_info = {
  throw_type: string,
  throw_msg: string,
};
type throw = Location.loc(throw_info);

// Structure
type field_info = {
  field_name: string,
  // Docblock info
  field_description: description,
  field_type: Types.type_expr,
};
type record_info = list(field_info);

type variant_info = {
  variant_name: string,
  variant_record_info: option(record_info),
  // Docblock info
  variant_description: description,
};
type adt_info = list(variant_info);

type type_content =
  | Record(record_info)
  | Variant(variant_info)
  | NoContent;

type type_info = {
  name: string,
  type_signature: Types.type_expr,
  type_content,
  // Docblock info
  description,
  deprecations: list(deprecation),
  since,
  history: list(history),
  examples: list(example),
};

type function_info = {
  params: list(param),
  returns,
  throws: list(throw),
};
type value_info = {
  name: string,
  type_sig: Types.type_expr,
  function_info: option(function_info),
  // Docblock info
  description,
  deprecations: list(deprecation),
  since,
  history: list(history),
  examples: list(example),
};

type t =
  | Module(module_info)
  | Type(type_info)
  | Value(value_info)
and module_info = {
  name: string,
  module_content,
  // Docblock info
  description,
  deprecations: list(deprecation),
  since,
  history: list(history),
  examples: list(example),
}
and module_content = {
  provided_types: list(t),
  provided_values: list(t),
  provided_modules: list(t),
};
