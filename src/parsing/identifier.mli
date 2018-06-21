(** Types for identifiers *)

open Format

(** The type of identifiers. *)
type t =
  | IdentName of string
  (** A simple name. *)
  | IdentExternal of t * string
  (** (module, ident) An external name. It is currently a well-formedness error
      to have a non-name on the LHS. *)
[@@deriving sexp]

val equal : t -> t -> bool
val compare : t -> t -> int

val print : formatter -> t -> unit

val default_printer : formatter -> t -> unit
(** The default {!formatter} implementation for identifiers.
    When formatting {!type:t} instances, one should use {!print} instead. *)
val printer : (formatter -> t -> unit) ref
(** The active {!formatter} implementation for identifiers *)

val string_of_ident : t -> string

val last : t -> string
val unflatten: string list -> t option
val parse: string -> t
val hash: t -> int
val output : out_channel -> t -> unit
val flatten : t -> string list
