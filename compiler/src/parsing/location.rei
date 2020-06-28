/** Source location data definitions */;

open Format;

/** The type for source locations. This definition
    is inspired by the OCaml compiler's source. */

[@deriving (sexp, yojson)]
type t =
  Grain_utils.Warnings.loc = {
    /** The starting position */
    loc_start: Lexing.position,
    /** The ending position */
    loc_end: Lexing.position,
    /** Whether this location was auto-generated */
    loc_ghost: bool,
  };

/** A placeholder dummy location */

let dummy_loc: t;

/** Return an empty ghost range located in a given file. */

let in_file: string => t;

/** Set the file name and line number of the [lexbuf] to be the start
    of the named file. */

let init: (Lexing.lexbuf, string) => unit;

/** Get the location of the current token from the [lexbuf]. */

let curr: Lexing.lexbuf => t;

let symbol_rloc: unit => t;
let symbol_gloc: unit => t;

/** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. */

let rhs_loc: int => t;

let input_name: ref(string);
let input_lexbuf: ref(option(Lexing.lexbuf));

let print_error: (formatter, t) => unit;
let print_error_cur_file: (formatter, unit) => unit;
let print_warning: (t, formatter, Grain_utils.Warnings.t) => unit;
let formatter_for_warnings: ref(formatter);
let prerr_warning: (t, Grain_utils.Warnings.t) => unit;
let reset: unit => unit;
let echo_eof: unit => unit;
let get_pos_info: Lexing.position => (string, int, int); /* file, line, char */
let print_loc: (formatter, t) => unit;

let print: (formatter, t) => unit;
let print_compact: (formatter, t) => unit;
let print_filename: (formatter, string) => unit;

let absolute_path: string => string;

/** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. */

let show_filename: string => string;

/** The default {!formatter} implementation for source locations.
    When formatting {!type:t} instances, one should use {!print} instead. */

let default_printer: (formatter, t) => unit;

/** The default {!formatter} implementation for source locations.
    When formatting {!type:t} instances, one should use {!print} instead. */
/** The active {!formatter} implementation for source locations */

let printer: ref((formatter, t) => unit);

/** The type for location-tagged values. */

type loc('a) = {
  /** The tagged value*/
  txt: 'a,
  /** The location of the value */
  loc: t,
};

/** Makes a location-tagged value with the dummy location. */

let mknoloc: 'a => loc('a);

/** Makes a location-tagged value with the dummy location. */
/** Makes a location-tagged value with the given location. */

let mkloc: ('a, t) => loc('a);

/** Support for located errors */;

type error = {
  loc: t,
  msg: string,
  sub: list(error),
  if_highlight: string /* alternative message if locations are highlighted */
};

exception Already_displayed_error;
exception Error(error);

let error:
  (~loc: t=?, ~sub: list(error)=?, ~if_highlight: string=?, string) => error;

let errorf:
  (
    ~loc: t=?,
    ~sub: list(error)=?,
    ~if_highlight: string=?,
    format4('a, Format.formatter, unit, error)
  ) =>
  'a;

let raise_errorf:
  (
    ~loc: t=?,
    ~sub: list(error)=?,
    ~if_highlight: string=?,
    format4('a, Format.formatter, unit, 'b)
  ) =>
  'a;

let error_of_printer: (t, (formatter, 'a) => unit, 'a) => error;

let error_of_printer_file: ((formatter, 'a) => unit, 'a) => error;

let error_of_exn: exn => option([ | `Ok(error) | `Already_displayed]);

/** Each compiler module which defines a custom type of exception
    which can surface as a user-visible error should register
    a "printer" for this exception using [register_error_of_exn].
    The result of the printer is an [error] value containing
    a location, a message, and optionally sub-messages (each of them
    being located as well). */

let register_error_of_exn: (exn => option(error)) => unit;

let report_error: (formatter, error) => unit;

/** Hook for intercepting error reports. */

let error_reporter: ref((formatter, error) => unit);

/** Original error reporter for use in hooks. */

let default_error_reporter: (formatter, error) => unit;

/** Reraise the exception if it is unknown. */

let report_exception: (formatter, exn) => unit;
