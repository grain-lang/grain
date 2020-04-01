(** Source location data definitions *)

open Format

(** The type for source locations. This definition
    is inspired by the OCaml compiler's source. *)
type t = Grain_utils.Warnings.loc = {
  loc_start: Lexing.position; (** The starting position *)
  loc_end: Lexing.position;   (** The ending position *)
  loc_ghost: bool;            (** Whether this location was auto-generated *)
} [@@deriving sexp, yojson]

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
val print_warning: t -> formatter -> Grain_utils.Warnings.t -> unit
val formatter_for_warnings : formatter ref
val prerr_warning: t -> Grain_utils.Warnings.t -> unit
val reset: unit -> unit
val echo_eof: unit -> unit
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

(** Support for located errors *)

type error = {
  loc: t;
  msg: string;
  sub: error list;
  if_highlight: string; (* alternative message if locations are highlighted *)
}

exception Already_displayed_error
exception Error of error

val error: ?loc:t -> ?sub:error list -> ?if_highlight:string -> string -> error

val errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, error) format4 -> 'a

val raise_errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val error_of_printer: t -> (formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file: (formatter -> 'a -> unit) -> 'a -> error

val error_of_exn: exn -> [ `Ok of error | `Already_displayed ] option

val register_error_of_exn: (exn -> error option) -> unit
(** Each compiler module which defines a custom type of exception
    which can surface as a user-visible error should register
    a "printer" for this exception using [register_error_of_exn].
    The result of the printer is an [error] value containing
    a location, a message, and optionally sub-messages (each of them
    being located as well). *)

val report_error: formatter -> error -> unit

val error_reporter : (formatter -> error -> unit) ref
(** Hook for intercepting error reports. *)

val default_error_reporter : formatter -> error -> unit
(** Original error reporter for use in hooks. *)

val report_exception: formatter -> exn -> unit
(** Reraise the exception if it is unknown. *)
