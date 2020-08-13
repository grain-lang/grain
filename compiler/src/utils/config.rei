/** The Grain stdlib directory, based on the current configuration */
let stdlib_directory: unit => option(string);

/** The list of directories to search for modules in, based on the current configuration */

let module_search_path: unit => list(string);

/** Whether verbose output should be written */

let verbose: ref(bool);

/** Whether locations should be shown in s-expression-converted trees
    (primarily useful with --cdebug). */

let sexp_locs_enabled: ref(bool);

/** Whether optimizations should be run */

let optimizations_enabled: ref(bool);

/** The path to find modules on */

let include_dirs: ref(list(string));

/** The location of the pervasives module. */

let stdlib_dir: ref(option(string));

/** The base path where all Grain files for the program reside */

let base_path: ref(string);

/** Whether color output should be enabled */

let color_enabled: ref(bool);

/** Whether to use principal types when compiling */

let principal: ref(bool);

/** Whether to allow cyclic types. */

let recursive_types: ref(bool);

/** Whether non-terminal block expressions should be forced to return void */

let strict_sequence: ref(bool);

/** The debugging level to use for the parser. Primarily intended for Grain compiler developers. */

let parser_debug_level: ref(int);

/** Whether debugging information should be included in the compiled output. */

let debug: ref(bool);

/** Whether optimizations which could elide runtime errors should be performed. */

let unsound_optimizations: ref(bool);

/*** Internal options (no command line flags) */

/** Whether to use safe string representations (always true for now) */

let safe_string: ref(bool);

/*** Configuration Saving/Restoring */

/** Abstract type representing a saved set of configuration options */

type config;

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

/** Wraps the given thunk with extractors for compiler command-line options */

let with_cli_options: 'a => Cmdliner.Term.t('a);
