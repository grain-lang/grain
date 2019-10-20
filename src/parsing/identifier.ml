
open Sexplib.Conv

let sep = "."

type t =
  | IdentName of string
  | IdentExternal of t * string
[@@deriving sexp]

let rec equal i1 i2 =
  match i1, i2 with
  | (IdentName n1), (IdentName n2) -> String.equal n1 n2
  | (IdentExternal(mod1, n1)), (IdentExternal(mod2, n2)) ->
    (equal mod1 mod2) && (String.equal n1 n2)
  | _ -> false

open Format

let rec print_ident ppf = function
  | IdentName n -> fprintf ppf "%s" n
  | IdentExternal(m, n) -> fprintf ppf "%a%s%s" print_ident m sep n

let default_printer ppf i = fprintf ppf "@{<id>%a@}@," print_ident i

let printer = ref default_printer
let print ppf = !printer ppf

let rec string_of_ident i =
  print_ident str_formatter i;
  flush_str_formatter()

let rec compare i1 i2 =
  match i1, i2 with
  | (IdentName n1), (IdentName n2) -> String.compare n1 n2
  | (IdentExternal(mod1, n1)), (IdentExternal(mod2, n2)) ->
    let n_comp = String.compare n1 n2 in
    if n_comp <> 0 then
      n_comp
    else
      compare mod1 mod2
  | (IdentName _), (IdentExternal _)
  | (IdentExternal _), (IdentName _) ->
    String.compare (string_of_ident i1) (string_of_ident i2)

let last = function
  | IdentName s -> s
  | IdentExternal (_, s) -> s

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let flatten n =
  let rec help acc = function
    | IdentName(n) -> List.rev (n::acc)
    | IdentExternal(p, n) -> help (n::acc) p
  in
  help [] n

let unflatten = function
  | [] -> None
  | hd::tl -> Some (List.fold_left (fun p s -> IdentExternal(p, s)) (IdentName hd) tl)

let parse s =
  match unflatten (split_at_dots s 0) with
  | None -> IdentName ""
  | Some v -> v

let hash name = Hashtbl.hash (flatten name)
let output oc name = output_string oc (string_of_ident name)

