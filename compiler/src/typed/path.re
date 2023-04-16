/* Modified version of OCaml's Path module */
open Sexplib.Conv;

[@deriving (sexp, yojson)]
type t =
  | PIdent(Ident.t)
  | PExternal(t, string);

let nopos = (-1);

let rec same = (p1, p2) =>
  switch (p1, p2) {
  | (PIdent(id1), PIdent(id2)) => Ident.same(id1, id2)
  | (PExternal(mod1, s1), PExternal(mod2, s2)) =>
    s1 == s2 && same(mod1, mod2)
  | _ => false
  };

let rec compare = (p1, p2) =>
  switch (p1, p2) {
  | (PIdent(id1), PIdent(id2)) => Ident.compare(id1, id2)
  | (PExternal(mod1, s1), PExternal(mod2, s2)) =>
    let s_comp = String.compare(s1, s2);
    if (s_comp != 0) {
      s_comp;
    } else {
      compare(mod1, mod2);
    };
  | (PIdent(_), PExternal(_)) => (-1)
  | (PExternal(_), PIdent(_)) => 1
  };

let rec find_free_opt = ids =>
  fun
  | PIdent(id) => List.find_opt(Ident.same(id), ids)
  | PExternal(p, _) => find_free_opt(ids, p);

let rec isfree = id =>
  fun
  | PIdent(id') => Ident.same(id, id')
  | PExternal(m, _) => isfree(id, m);

let rec binding_time =
  fun
  | PIdent(id) => Ident.binding_time(id)
  | PExternal(m, _) => binding_time(m);

let flatten = {
  let rec flatten = acc =>
    fun
    | PIdent(id) => (id, acc)
    | PExternal(m, s) => flatten([s, ...acc], m);
  flatten([]);
};

let rec name =
  fun
  | PIdent(id) => Ident.name(id)
  | PExternal(m, s) => name(m) ++ "." ++ s;

let rec head =
  fun
  | PIdent(id) => id
  | PExternal(m, _) => head(m);

let heads = p => {
  let rec heads = (p, acc) =>
    switch (p) {
    | PIdent(id) => [id, ...acc]
    | PExternal(m, _) => heads(m, acc)
    };
  heads(p, []);
};

let rec last =
  fun
  | PIdent(id) => Ident.name(id)
  | PExternal(_, s) => s;

let rec stamp =
  fun
  | PIdent(id) => id.stamp
  | PExternal(p, _) => stamp(p);
