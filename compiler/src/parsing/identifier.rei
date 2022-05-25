/** Types for identifiers */;

open Location;
open Format;

/** The type of identifiers. */

[@deriving (sexp, yojson)]
type t =
  | /** A simple name. */
    IdentName(loc(string))
  | /** (module, ident) An external name. It is currently a well-formedness error
      to have a non-name on the LHS. */
    IdentExternal(
      t,
      loc(string),
    );

let equal: (t, t) => bool;
let compare: (t, t) => int;

let print: (formatter, t) => unit;

/** The default {!formatter} implementation for identifiers.
    When formatting {!type:t} instances, one should use {!print} instead. */

let default_printer: (formatter, t) => unit;

/** The default {!formatter} implementation for identifiers.
    When formatting {!type:t} instances, one should use {!print} instead. */
/** The active {!formatter} implementation for identifiers */

let printer: ref((formatter, t) => unit);

let string_of_ident: t => string;

let last: t => string;
let unflatten: list(string) => option(t);
let parse: string => t;
let hash: t => int;
let output: (out_channel, t) => unit;
let flatten: t => list(string);
