open Grain_parsing;

type t;

let from_comments: list(Parsetree.comment) => t;

let query: (t, Location.t) => list(Parsetree.comment);
