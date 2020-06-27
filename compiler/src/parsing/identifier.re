open Sexplib.Conv;

let sep = ".";

[@deriving sexp]
type t =
  | IdentName(string)
  | IdentExternal(t, string);

let rec equal = (i1, i2) =>
  switch (i1, i2) {
  | (IdentName(n1), IdentName(n2)) => String.equal(n1, n2)
  | (
      [@implicit_arity] IdentExternal(mod1, n1),
      [@implicit_arity] IdentExternal(mod2, n2),
    ) =>
    equal(mod1, mod2) && String.equal(n1, n2)
  | _ => false
  };

open Format;

let rec print_ident = ppf =>
  fun
  | IdentName(n) => fprintf(ppf, "%s", n)
  | [@implicit_arity] IdentExternal(m, n) =>
    fprintf(ppf, "%a%s%s", print_ident, m, sep, n);

let default_printer = (ppf, i) =>
  fprintf(ppf, "@{<id>%a@}@,", print_ident, i);

let printer = ref(default_printer);
let print = ppf => printer^(ppf);

let rec string_of_ident = i => {
  print_ident(str_formatter, i);
  flush_str_formatter();
};

let rec compare = (i1, i2) =>
  switch (i1, i2) {
  | (IdentName(n1), IdentName(n2)) => String.compare(n1, n2)
  | (
      [@implicit_arity] IdentExternal(mod1, n1),
      [@implicit_arity] IdentExternal(mod2, n2),
    ) =>
    let n_comp = String.compare(n1, n2);
    if (n_comp != 0) {
      n_comp;
    } else {
      compare(mod1, mod2);
    };
  | (IdentName(_), IdentExternal(_))
  | (IdentExternal(_), IdentName(_)) =>
    String.compare(string_of_ident(i1), string_of_ident(i2))
  };

let last =
  fun
  | IdentName(s) => s
  | [@implicit_arity] IdentExternal(_, s) => s;

let rec split_at_dots = (s, pos) =>
  try({
    let dot = String.index_from(s, pos, '.');
    [String.sub(s, pos, dot - pos), ...split_at_dots(s, dot + 1)];
  }) {
  | Not_found => [String.sub(s, pos, String.length(s) - pos)]
  };

let flatten = n => {
  let rec help = acc =>
    fun
    | IdentName(n) => List.rev([n, ...acc])
    | [@implicit_arity] IdentExternal(p, n) => help([n, ...acc], p);

  help([], n);
};

let unflatten =
  fun
  | [] => None
  | [hd, ...tl] =>
    Some(
      List.fold_left(
        (p, s) => [@implicit_arity] IdentExternal(p, s),
        IdentName(hd),
        tl,
      ),
    );

let parse = s =>
  switch (unflatten(split_at_dots(s, 0))) {
  | None => IdentName("")
  | Some(v) => v
  };

let hash = name => Hashtbl.hash(flatten(name));
let output = (oc, name) => output_string(oc, string_of_ident(name));
