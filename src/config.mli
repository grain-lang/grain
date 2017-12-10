(** Global configuration settings for Grain compiler runtime. *)

(** Gets the Grain root, if set. Default is GRAIN_ROOT environment variable (if set).
    Can be configured by set_grain_root. *)
val get_grain_root : unit -> string option

(** Sets the Grain root. *)
val set_grain_root : string -> unit
