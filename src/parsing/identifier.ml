
open Sexplib.Conv

let sep = "::"

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
