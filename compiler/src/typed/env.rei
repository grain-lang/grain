open Grain_parsing;
open Parsetree;
open Types;

module PathMap:
  Map.S with type key = Path.t and type t('a) = Map.Make(Path).t('a);

[@deriving sexp]
type summary =
  | Env_empty
  | Env_value(summary, Ident.t, value_description)
  | Env_type(summary, Ident.t, type_declaration)
  | Env_module(summary, Ident.t, module_declaration)
  | Env_modtype(summary, Ident.t, modtype_declaration)
  | Env_open(summary, Path.t)
  | Env_constraints(summary, PathMap.t(type_declaration))
  | Env_copy_types(summary, list(string));

type type_descriptions = (
  list(constructor_description),
  list(label_description),
);
type t;

let empty: t;
let initial_safe_string: t;
let initial_unsafe_string: t;

type dependency_chain = list(Location.loc(string));

type error =
  | Illegal_renaming(string, string, string)
  | Inconsistent_import(string, string, string)
  | Need_recursive_types(string, string)
  | Depend_on_unsafe_string_unit(string, string)
  | Missing_module(Location.t, Path.t, Path.t)
  | Unbound_module(Location.t, string)
  | Unbound_label(Location.t, string)
  | No_module_file(string, option(string))
  | Value_not_found_in_module(Location.t, string, string)
  | Illegal_value_name(Location.t, string)
  | Cyclic_dependencies(string, dependency_chain);

exception Error(error);
let error: error => 'a;

/* For short-paths */
type iter_cont;
let iter_types:
  ((Path.t, (Path.t, (type_declaration, type_descriptions))) => unit, t) =>
  iter_cont;
let run_iter_cont: list(iter_cont) => list((Path.t, iter_cont));
let same_types: (t, t) => bool;
let used_persistent: unit => Concr.t;
let find_shadowed_types: (Path.t, t) => list(Path.t);
/** [without_cmis f arg] applies [f] to [arg], but does not
    allow opening cmis during its execution */

let without_cmis: ('a => 'b, 'a) => 'b;

/* By-path lookups */
let find_value: (Path.t, t) => value_description;
let find_type: (Path.t, t) => type_declaration;
let find_type_descrs: (Path.t, t) => type_descriptions;
let find_module: (Path.t, option(string), t) => module_declaration;
let find_modtype: (Path.t, t) => modtype_declaration;

let find_type_expansion:
  (Path.t, t) => (list(type_expr), type_expr, option(int));
let find_type_expansion_opt:
  (Path.t, t) => (list(type_expr), type_expr, option(int));
/* Find the manifest type information associated to a type for the sake
   of the compiler's type-based optimisations. */
let find_modtype_expansion: (Path.t, t) => module_type;
let normalize_path: (option(Location.t), t, Path.t) => Path.t;
/** Normalize the path to a concrete value or module.
    If the option is None, allow returning dangling paths.
    Otherwise raise a Missing_module error, and may add forgotten
    head as required global. */

let normalize_path_prefix: (option(Location.t), t, Path.t) => Path.t;

let has_local_constraints: t => bool;

/* By-identifier lookups */
/** Looks up the value associated with the given identifier. */

let lookup_value:
  (~mark: bool=?, Identifier.t, t) => (Path.t, value_description);

/** Looks up the value associated with the given identifier. */
/** Looks up the type associated with the given identifier. */

let lookup_type: (~mark: bool=?, Identifier.t, t) => Path.t;

/** Looks up the type associated with the given identifier. */
/** Looks up the constructor associated with the given identifier. */

let lookup_constructor:
  (~mark: bool=?, Identifier.t, t) => constructor_description;

/** Looks up the constructor associated with the given identifier. */

let lookup_all_constructors:
  (~mark: bool=?, Identifier.t, t) =>
  list((constructor_description, unit => unit));
let lookup_all_labels:
  (~mark: bool=?, Identifier.t, t) => list((label_description, unit => unit));
let lookup_module:
  (
    ~load: bool,
    ~loc: Location.t=?,
    ~mark: bool=?,
    Identifier.t,
    option(string),
    t
  ) =>
  Path.t;
let lookup_modtype:
  (~loc: Location.t=?, ~mark: bool=?, Identifier.t, t) =>
  (Path.t, modtype_declaration);

let copy_types: (list(string), t) => t;
/* Used only in Typecore.duplicate_ident_types. */

/* By-identifier insertions */
/** Adds a value identifier with the given name and description. */

let add_value:
  (~check: string => Warnings.t=?, Ident.t, value_description, t) => t;

/** Adds a value identifier with the given name and description. */
/** Adds a type identifier with the given name and declaration. */

let add_type: (~check: bool, Ident.t, type_declaration, t) => t;

/** Adds a type identifier with the given name and declaration. */
/** Adds a constructor with the given name and description. */

let add_constructor: (Ident.t, constructor_description, t) => t;

/** Adds a constructor with the given name and description. */

let add_module: (~arg: bool=?, Ident.t, module_type, option(string), t) => t;
let add_module_declaration:
  (~arg: bool=?, ~check: bool, Ident.t, module_declaration, t) => t;
let add_modtype: (Ident.t, modtype_declaration, t) => t;
let add_local_constraint: (Path.t, type_declaration, int, t) => t;
let add_local_type: (Path.t, type_declaration, t) => t;

/* Insertion of all fields of a signature. */

let add_item: (signature_item, t) => t;
let add_signature: (signature, t) => t;

/* Remember the current compilation unit: modname * filename. */
let set_unit: ((string, string)) => unit;
let get_unit: unit => (string, string);

/* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. Returns None if the path refers to a functor,
   not a structure. */
let open_signature:
  (
    ~used_slot: ref(bool)=?,
    ~toplevel: bool=?,
    Path.t,
    Identifier.t,
    import_declaration,
    t
  ) =>
  option(t);
/* Similar to [open_signature], except that modules from the load path
      have precedence over sub-modules of the opened module.
      For instance, if opening a module [M] with a sub-module [X]:
      - if the load path contains a [x.cmi] file, then resolving [X] in the
        new environment yields the same result as resolving [X] in the
        old environment
      - otherwise, in the new environment [X] resolves to [M.X]
   */
let open_signature_of_initially_opened_module:
  (~loc: Location.t=?, Path.t, option(string), t) => option(t);

/* Read, save a signature to/from a file */

let read_signature: (string, string) => signature;
/* Arguments: module name, file name. Results: signature. */
let build_signature:
  (~deprecated: string=?, signature, string, string) => Cmi_format.cmi_infos;
/* Arguments: signature, module name, file name. */
let build_signature_with_imports:
  (
    ~deprecated: string=?,
    signature,
    string,
    string,
    list((string, option(Digest.t)))
  ) =>
  Cmi_format.cmi_infos;
/* Arguments: signature, module name, file name,
   imported units with their CRCs. */

/* Return the CRC of the interface of the given compilation unit */

let crc_of_unit: (string, option(string)) => Digest.t;

/* Return the set of compilation units imported, with their CRC */

let imports: unit => list((string, option(Digest.t)));

/* [is_imported_opaque md] returns true if [md] is an opaque imported module  */
let is_imported_opaque: string => bool;

/* Direct access to the table of imported compilation units with their CRC */

module Consistbl: (module type of {
  include Grain_utils.Consistbl.Make(Misc.Stdlib.String);
});

let crc_units: Consistbl.t;
let add_import: string => unit;
let clear_imports: unit => unit;

/* By-name insertions */
/** Adds a value identifier with the given name and description.
    The new environment and a generated identifier are returned. */

let enter_value: (string, value_description, t) => (Ident.t, t);

/** Adds a value identifier with the given name and description.
    The new environment and a generated identifier are returned. */
/** Adds a type identifier with the given name and declaration.
    The new environment and a generated identifier are returned. */

let enter_type: (string, type_declaration, t) => (Ident.t, t);

/* Forward declaration to break mutual recursion with Includemod. */
let check_modtype_inclusion:
  ref((~loc: Location.t, t, module_type, Path.t, module_type) => unit);
/* Forward declaration to break mutual recursion with Mtype. */
let strengthen:
  ref((~aliasable: bool, t, module_type, Path.t) => module_type);
/* Forward declaration to break mutual recursion with Typecore. */
let add_delayed_check_forward: ref((unit => unit) => unit);
/* Forward declaration to break mutual recursion with Ctype. */
let same_constr: ref((t, type_expr, type_expr) => bool);
/* Forward declaration to break mutual recursion with Compile.
   Compiles the given input file to the given output file location. */
let compile_module_dependency: ref((string, string) => unit);

/* Analysis functions */

let fold_values:
  (
    (string, Path.t, value_description, 'a) => 'a,
    option(Identifier.t),
    t,
    'a
  ) =>
  'a;
let fold_types:
  (
    (string, Path.t, (type_declaration, type_descriptions), 'a) => 'a,
    option(Identifier.t),
    t,
    'a
  ) =>
  'a;
/** Persistent structures are only traversed if they are already loaded. */

let fold_constructors:
  ((constructor_description, 'a) => 'a, option(Identifier.t), t, 'a) => 'a;

/** Persistent structures are only traversed if they are already loaded. */

let fold_modules:
  (
    (string, Path.t, module_declaration, 'a) => 'a,
    option(Identifier.t),
    t,
    'a
  ) =>
  'a;
let fold_modtypes:
  (
    (string, Path.t, modtype_declaration, 'a) => 'a,
    option(Identifier.t),
    t,
    'a
  ) =>
  'a;

let scrape_alias: (t, module_type) => module_type;
let check_value_name: (string, Location.t) => unit;

module Persistent_signature: {
  type t = {
    /** Name of the file containing the signature. */
    filename: string,
    cmi: Cmi_format.cmi_infos,
  };

  /** Function used to load a persistent signature. The default is to look for
      the .cmi file in the load path. This function can be overridden to load
      it from memory, for instance to build a self-contained toplevel. */

  let load: ref((~loc: Location.t=?, ~unit_name: string) => option(t));
};

/* Summaries -- compact representation of an environment, to be
   exported in debugging information. */

let summary: t => summary;
