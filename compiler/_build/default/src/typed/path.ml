(* Modified version of OCaml's Path module *)
open Sexplib.Conv

type t =
  | PIdent of Ident.t
  | PExternal of t * string * int
[@@deriving sexp, yojson]

let nopos = -1

let rec same p1 p2 =
  match (p1, p2) with
  | (PIdent id1, PIdent id2) -> Ident.same id1 id2
  | (PExternal(mod1, s1, _), PExternal(mod2, s2, _)) ->
    s1 = s2 && same mod1 mod2
  | _ -> false

let rec compare p1 p2 =
  match (p1, p2) with
  | (PIdent id1, PIdent id2) -> Ident.compare id1 id2
  | (PExternal(mod1, s1, _), PExternal(mod2, s2, _)) ->
    let s_comp = String.compare s1 s2 in
    if s_comp <> 0 then
      s_comp
    else
    compare mod1 mod2
  | (PIdent _, PExternal _) -> -1
  | (PExternal _, PIdent _) -> 1

let rec isfree id = function
  | PIdent id' -> Ident.same id id'
  | PExternal(m, _, _) -> isfree id m

let rec binding_time = function
  | PIdent id -> Ident.binding_time id
  | PExternal(m, _, _) -> binding_time m

let flatten =
  let rec flatten acc = function
    | PIdent id -> (id, acc)
    | PExternal(m, s, _) -> flatten (s :: acc) m in
  flatten []

let rec name = function
  | PIdent id -> Ident.name id
  | PExternal(m, s, _) ->
    name m ^ "." ^ s

let rec head = function
  | PIdent id -> id
  | PExternal(m, _, _) -> head m

let heads p =
  let rec heads p acc =
    match p with
    | PIdent id -> id :: acc
    | PExternal(m, _, _) -> heads m acc in
  heads p []

let rec last = function
  | PIdent id -> Ident.name id
  | PExternal(_, s, _) -> s

