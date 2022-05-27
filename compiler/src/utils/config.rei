type profile =
  | Release;

/** The Grain stdlib directory, based on the current configuration */
let stdlib_directory: unit => option(string);

/** The WASI polyfill path, based on the current configuration */
let wasi_polyfill_path: unit => option(string);

/** The list of directories to search for modules in, based on the current configuration */

let module_search_path: unit => list(string);

/** Whether verbose output should be written */

let verbose: ref(bool);

/** Whether locations should be shown in s-expression-converted trees
    (primarily useful with --verbose). */

let sexp_locs_enabled: ref(bool);

/** Whether or not to automatically import Pervasives */

let no_pervasives: ref(bool);

/** Whether to enable garbage collection */

let no_gc: ref(bool);

/** Whether to enable bulk memory operations */

let bulk_memory: ref(bool);

/** Custom WASI implementation */

let wasi_polyfill: ref(option(string));

/** Whether to replace the _start export with a start section during linking */

let use_start_section: ref(bool);

/** Compilation profile, e.g. release for production builds */

let profile: ref(option(profile));

// [NOTE] This default is here because it is used in multiple locations,
//        and it doesn't make sense for it to be "owned" by any of them.
/** The default value for `memory_base` */

let default_memory_base: int;

/** Start address of Grain runtime heap */

let memory_base: ref(option(int));

/** The path to find modules on */

let include_dirs: ref(list(string));

/** The location of the pervasives module. */

let stdlib_dir: ref(option(string));

/** Whether color output should be enabled */

let color_enabled: ref(bool);

/** Whether to use principal types when compiling */

let principal: ref(bool);

/** Initial number of WebAssembly memory pages */

let initial_memory_pages: ref(int);

/** Maximum number of WebAssembly memory pages */

let maximum_memory_pages: ref(option(int));

/** Compilation mode to use when compiling */

let compilation_mode: ref(option(string));

/** Statically link modules after compilation */

let statically_link: ref(bool);

/** Enable tail-call optimizations */

let experimental_tail_call: ref(bool);

/** Whether to allow cyclic types. */

let recursive_types: ref(bool);

/** Whether non-terminal block expressions should be forced to return void */

let strict_sequence: ref(bool);

/** The debugging level to use for the parser. Primarily intended for Grain compiler developers. */

let parser_debug_level: ref(int);

/** Whether debugging information should be included in the compiled output. */

let debug: ref(bool);

/** Also produce a WebAssembly Text (.wat) file. */

let wat: ref(bool);

/** Whether or not to include runtime type information used by toString/print */

let elide_type_info: ref(bool);

/** Whether or not to generate source maps. */

let source_map: ref(bool);

/*** Internal options (no command line flags) */

/** Whether to use safe string representations (always true for now) */

let safe_string: ref(bool);

/** Internal option to disable printing of warnings. */

let print_warnings: ref(bool);

/*** Configuration Saving/Restoring */

/** Type representing a saved set of configuration options */

type config;

/** An empty config */
let empty: config;

/** The current configuration for all programs */

let root_config: ref(config);

/** Set the configuration for all programs */

let set_root_config: unit => unit;

/** Gets a digest of the root configuration */

let get_root_config_digest: unit => string;

/** Saves the current configuration */

let save_config: unit => config;

/** Restores the configuration settings to the given configuration */

let restore_config: config => unit;

/** Reset all configuration items */

let reset_config: unit => unit;

/** Runs the given thunk with the given configuration */

let with_config: (config, unit => 'a) => 'a;

/** Runs the given thunk, making sure that any changes to the configuration
    are contained to its execution. */

let preserve_config: (unit => 'a) => 'a;

/** Runs the given thunk with the given root configuration */
let with_root_config: (config, unit => 'a) => 'a;

/** Runs the given thunk, making sure that any changes to the configuration
    and root configuration are contained to its execution. */

let preserve_all_configs: (unit => 'a) => 'a;

/** Wraps the given thunk with extractors for compiler command-line options */

let with_cli_options: 'a => Cmdliner.Term.t('a);

/** Applies compile flags provided at the start of a file */

let apply_inline_flags:
  (~on_error: [> | `Help | `Message(string)] => unit, string) => unit;

let with_inline_flags:
  (~on_error: [> | `Help | `Message(string)] => unit, string, unit => 'a) =>
  'a;

type implicit_opens =
  | Pervasives_mod
  | Gc_mod;

let get_implicit_opens: unit => list(implicit_opens);
