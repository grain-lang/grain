(** Source location data definitions *)

open Format

(** The type for source locations. This definition
    is inspired by the OCaml compiler's source. *)
type t = {
  loc_start: Lexing.position; (** The starting position *)
  loc_end: Lexing.position;   (** The ending position *)
  loc_ghost: bool;            (** Whether this location was auto-generated *)
}

val dummy_loc : t
(** A placeholder dummy location *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)

val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. *)
val rhs_loc: int -> t

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

val print_error: formatter -> t -> unit
val print_error_cur_file: formatter -> unit -> unit
val get_pos_info: Lexing.position -> string * int * int (* file, line, char *)
val print_loc: formatter -> t -> unit

val print : formatter -> t -> unit
val print_compact: formatter -> t -> unit
val print_filename: formatter -> string -> unit

val absolute_path: string -> string

val show_filename: string -> string
    (** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. *)

val default_printer : formatter -> t -> unit
(** The default {!formatter} implementation for source locations.
    When formatting {!type:t} instances, one should use {!print} instead. *)
val printer : (formatter -> t -> unit) ref
(** The active {!formatter} implementation for source locations *)

(** The type for location-tagged values. *)
type 'a loc = {
  txt : 'a; (** The tagged value*)
  loc : t;  (** The location of the value *)
}

val mknoloc : 'a -> 'a loc
(** Makes a location-tagged value with the dummy location. *)
val mkloc : 'a -> t -> 'a loc
(** Makes a location-tagged value with the given location. *)
