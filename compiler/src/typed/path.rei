/* Modified version of OCaml Path module. */

[@deriving (sexp, yojson)]
type t =
  | PIdent(Ident.t)
  | PExternal(t, string);

let same: (t, t) => bool;
let compare: (t, t) => int;
let find_free_opt: (list(Ident.t), t) => option(Ident.t);
let isfree: (Ident.t, t) => bool;
let binding_time: t => int;
let flatten: t => (Ident.t, list(string));

let nopos: int;

let name: t => string;

let head: t => Ident.t;

let heads: t => list(Ident.t);

let last: t => string;

let stamp: t => int;
