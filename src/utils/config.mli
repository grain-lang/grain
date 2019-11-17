val stdlib_directory : unit -> string option
(** The Grain stdlib directory, based on the current configuration *)

val module_search_path : unit -> string list
(** The list of directories to search for modules in, based on the current configuration *)

val grain_root : string option ref
(** Root directory for things like standard library lookups *)

val verbose : bool ref
(** Whether verbose output should be written *)

val sexp_locs_enabled : bool ref
(** Whether locations should be shown in s-expression-converted trees
    (primarily useful with --cdebug). *)

val optimizations_enabled : bool ref
(** Whether optimizations should be run *)

val include_dirs : string list ref
(** The path to find modules on *)

val use_stdlib : bool ref
(** Whether the standard library should be included *)

val base_path : string ref
(** The base path where all Grain files for the program reside *)

val color_enabled : bool ref
(** Whether color output should be enabled *)

val principal : bool ref
(** Whether to use principal types when compiling *)

val recursive_types : bool ref
(** Whether to allow cyclic types. *)

val strict_sequence : bool ref
(** Whether non-terminal block expressions should be forced to return void *)

val parser_debug_level : int ref
(** The debugging level to use for the parser. Primarily intended for Grain compiler developers. *)

val debug : bool ref
(** Whether debugging information should be included in the compiled output. *)

val unsound_optimizations : bool ref
(** Whether optimizations which could elide runtime errors should be performed. *)

(*** Internal options (no command line flags) *)

val safe_string : bool ref
(** Whether to use safe string representations (always true for now) *)

val output_enabled : bool ref
(** Whether to enable file writes. This is useful for testing. *)

(*** Configuration Saving/Restoring *)

type config
(** Abstract type representing a saved set of configuration options *)

val save_config : unit -> config
(** Saves the current configuration *)

val restore_config : config -> unit
(** Restores the configuration settings to the given configuration *)

val reset_config : unit -> unit
(** Reset all configuration items *)

val with_config : config -> (unit -> 'a) -> 'a
(** Runs the given thunk with the given configuration *)

val preserve_config : (unit -> 'a) -> 'a
(** Runs the given thunk, making sure that any changes to the configuration
    are contained to its execution. *)

val with_cli_options : 'a -> 'a Cmdliner.Term.t
(** Wraps the given thunk with extractors for compiler command-line options *)
