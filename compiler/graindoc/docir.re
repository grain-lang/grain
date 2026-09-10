open Grain_parsing;
open Sexplib.Conv;

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                             ATTRIBUTE TYPES                              │
// └──────────────────────────────────────────────────────────────────────────┘

/** The graindoc information for a description attribute. */
[@deriving sexp]
type description = option(string);

/** The graindoc information for a deprecation attribute. */
[@deriving sexp]
type deprecation = Location.loc(string);

/** The graindoc information for a since attribute. */
[@deriving sexp]
type since = option(Location.loc(string));

/** The graindoc information for a historical change. */
[@deriving sexp]
type history_info = {
  version: string,
  message: string,
};

/** The locational graindoc information for a history attribute. */
[@deriving sexp]
type history = Location.loc(history_info);

/** The graindoc information for an example attribute. */
[@deriving sexp]
type example = Location.loc(string);

/** The graindoc information for a parameter attribute. */
[@deriving sexp]
type param = {
  /** The name of the parameter. */
  param_name: string,
  /** The human-readable type of the parameter. */
  param_type: string,
  /** The message describing the parameter. */
  param_desc: description,
};

/** The graindoc information for a return attribute. */
[@deriving sexp]
type returns = {
  /** The human-readable type of the return value. */
  returns_type: string,
  /** The description of the return value. */
  returns_desc: description,
};

/** The graindoc information for a throws attribute. */
[@deriving sexp]
type throw_info = {
  /** The type of the exception. */
  throw_type: string,
  /** The message describing the exception. */
  throw_msg: string,
};

/** The locational graindoc information for a throws attribute. */
[@deriving sexp]
type throw = Location.loc(throw_info);

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                             STRUCTURAL TYPES                             │
// └──────────────────────────────────────────────────────────────────────────┘

/** The base graindoc information for a `type`, `value`, or `module`. */
[@deriving sexp]
type base_info = {
  /** The description of the item. */
  description,
  /** A list of deprecation messages for the item. */
  deprecations: list(deprecation),
  /** Information about when the item was introduced. */
  since,
  /** A list of historical changes to the item. */
  history: list(history),
  /** A list of examples for the item. */
  examples: list(example),
};

/** The graindoc information for a record field. */
[@deriving sexp]
type field_info = {
  /** The name of the field. */
  field_name: string,
  /** The description of the field. */
  field_desc: description,
  /** The human-readable type signature of the field. */
  field_type: string,
};

/** The graindoc information for a record. */
[@deriving sexp]
type record_info = list(field_info);

/** The graindoc information for a variant. */
[@deriving sexp]
type variant_info = {
  /** The string representation of the variant. */
  variant_str: string,
  /** The inline record information for the variant, if it has an inline record. */
  variant_record_info: option(record_info),
  /** The description of the variant. */
  variant_desc: description,
};

/** The graindoc information for an algebraic data type. */
[@deriving sexp]
type adt_info = list(variant_info);

/** The graindoc information for a types content. */
[@deriving sexp]
type type_content =
  | /** The record content of the type. */
    Record(record_info)
  | /** The algebraic data type content of the type. */
    Adt(adt_info)
  | /** The content of the type when it has no content. */
    NoContent;

/** The graindoc information for a type. */
[@deriving sexp]
type type_info = {
  /** The name of the type. */
  name: string,
  /** The root namespace of the type. */
  namespace: option(string),
  /** The human-readable type signature of the type. */
  type_sig: string,
  /** The content of the type. */
  type_content,
  /** The base graindoc information for the type. */
  base_info,
};

/** The graindoc information for a function. */
[@deriving sexp]
type function_info = {
  /** The graindoc parameter information for the function. */
  params: list(param),
  /** The graindoc return information for the function. */
  returns,
  /** The graindoc exception information for the function. */
  throws: list(throw),
};

/** The graindoc information for a value. */
[@deriving sexp]
type value_info = {
  /** The name of the value. */
  name: string,
  /** The root namespace of the value. */
  namespace: option(string),
  /** The human-readable type signature of the value. */
  type_sig: string,
  /** The function information for the value, if it is a function. */
  function_info: option(function_info),
  /** The base graindoc information for the value. */
  base_info,
};

/** The graindoc information for a module. */
[@deriving sexp]
type module_info = {
  /** The name of the module. */
  name: string,
  /** The root namespace of the module. */
  namespace: option(string),
  /** The content of the module. */
  module_content,
  /** The base graindoc information for the module. */
  base_info,
}

/** The content of a module. */
[@deriving sexp]
and module_content = {
  /** The types provided by the module. */
  provided_types: list(type_info),
  /** The values provided by the module. */
  provided_values: list(value_info),
  /** The modules provided by the module. */
  provided_modules: list(module_info),
};
