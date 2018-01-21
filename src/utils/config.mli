val verbose : bool ref
(** Whether verbose output should be written *)

val optimizations_enabled : bool ref
(** Whether optimizations should be run *)

val include_dirs : string list ref
(** The path to find modules on *)

val use_stdlib : bool ref
(** Whether the standard library should be included *)

val color_enabled : bool ref
(** Whether color output should be enabled *)

val principal : bool ref
(** Whether to use principal types when compiling *)

val recursive_types : bool ref
(** Whether to allow cyclic types. *)

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
