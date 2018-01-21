
open Types

module PathMap : Map.S with type key = Path.t
                        and type 'a t = 'a Map.Make(Path).t

type t

val empty : t

