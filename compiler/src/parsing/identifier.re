open Location;
open Sexplib.Conv;

let sep = ".";

[@deriving (sexp, yojson)]
type t =
  | IdentName(loc(string))
  | IdentExternal(t, loc(string));

let rec equal = (i1, i2) =>
  switch (i1, i2) {
  | (IdentName(n1), IdentName(n2)) => String.equal(n1.txt, n2.txt)
  | (IdentExternal(mod1, n1), IdentExternal(mod2, n2)) =>
    equal(mod1, mod2) && String.equal(n1.txt, n2.txt)
  | _ => false
  };

open Format;

let rec print_ident = ppf =>
  fun
  | IdentName(n) => fprintf(ppf, "%s", n.txt)
  | IdentExternal(m, n) =>
    fprintf(ppf, "%a%s%s", print_ident, m, sep, n.txt);

let default_printer = (ppf, i) => fprintf(ppf, "%a@,", print_ident, i);

let printer = ref(default_printer);
let print = ppf => printer^(ppf);

let rec string_of_ident = i => {
  print_ident(str_formatter, i);
  flush_str_formatter();
};

let rec compare = (i1, i2) =>
  switch (i1, i2) {
  | (IdentName(n1), IdentName(n2)) => String.compare(n1.txt, n2.txt)
  | (IdentExternal(mod1, n1), IdentExternal(mod2, n2)) =>
    let n_comp = String.compare(n1.txt, n2.txt);
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
  | IdentName(s) => s.txt
  | IdentExternal(_, s) => s.txt;

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
    | IdentName(n) => List.rev([n.txt, ...acc])
    | IdentExternal(p, n) => help([n.txt, ...acc], p);

  help([], n);
};

let unflatten =
  fun
  | [] => None
  | [hd, ...tl] =>
    Some(
      List.fold_left(
        (p, s) => IdentExternal(p, mknoloc(s)),
        IdentName(mknoloc(hd)),
        tl,
      ),
    );

let parse = s =>
  switch (unflatten(split_at_dots(s, 0))) {
  | None => IdentName(mknoloc(""))
  | Some(v) => v
  };

let hash = name => Hashtbl.hash(flatten(name));
let output = (oc, name) => output_string(oc, string_of_ident(name));
