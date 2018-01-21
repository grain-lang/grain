(* Modified version of OCaml Path module. *)

type t =
  | PIdent of Ident.t
  | PExternal of t * string * int

val same: t -> t -> bool
val compare: t -> t -> int
val isfree: Ident.t -> t -> bool
val binding_time: t -> int
val flatten: Ident.t * string list

val nopos: int

val name: t -> string

val head: t -> Ident.t

val heads: t -> Ident.t list

val last: t -> string
