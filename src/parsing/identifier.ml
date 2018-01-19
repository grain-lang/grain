
let sep = "::"

type t =
  | IdentName of string
  | IdentExternal of string * t

let rec equal i1 i2 =
  match i1, i2 with
  | (IdentName n1), (IdentName n2) -> String.equal n1 n2
  | (IdentExternal(mod1, n1)), (IdentExternal(mod2, n2)) ->
    (String.equal mod1 mod2) && (equal n1 n2)
  | _ -> false

open Format

let rec print_ident ppf = function
  | IdentName n -> fprintf ppf "%s" n
  | IdentExternal(m, n) -> fprintf ppf "%s%s%a" m sep print_ident n

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
    let m_comp = String.compare mod1 mod2 in
    if m_comp <> 0 then
      m_comp
    else
      compare n1 n2
  | (IdentName _), (IdentExternal _)
  | (IdentExternal _), (IdentName _) ->
    String.compare (string_of_ident i1) (string_of_ident i2)
