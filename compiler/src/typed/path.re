/* Modified version of OCaml's Path module */
open Sexplib.Conv;

[@deriving (sexp, yojson)]
type t =
  | PIdent(Ident.t)
  | PExternal(t, string, int);

let nopos = (-1);

let rec same = (p1, p2) =>
  switch (p1, p2) {
  | (PIdent(id1), PIdent(id2)) => Ident.same(id1, id2)
  | (
      [@implicit_arity] PExternal(mod1, s1, _),
      [@implicit_arity] PExternal(mod2, s2, _),
    ) =>
    s1 == s2 && same(mod1, mod2)
  | _ => false
  };

let rec compare = (p1, p2) =>
  switch (p1, p2) {
  | (PIdent(id1), PIdent(id2)) => Ident.compare(id1, id2)
  | (
      [@implicit_arity] PExternal(mod1, s1, _),
      [@implicit_arity] PExternal(mod2, s2, _),
    ) =>
    let s_comp = String.compare(s1, s2);
    if (s_comp != 0) {
      s_comp;
    } else {
      compare(mod1, mod2);
    };
  | (PIdent(_), PExternal(_)) => (-1)
  | (PExternal(_), PIdent(_)) => 1
  };

let rec isfree = id =>
  fun
  | PIdent(id') => Ident.same(id, id')
  | [@implicit_arity] PExternal(m, _, _) => isfree(id, m);

let rec binding_time =
  fun
  | PIdent(id) => Ident.binding_time(id)
  | [@implicit_arity] PExternal(m, _, _) => binding_time(m);

let flatten = {
  let rec flatten = acc =>
    fun
    | PIdent(id) => (id, acc)
    | [@implicit_arity] PExternal(m, s, _) => flatten([s, ...acc], m);
  flatten([]);
};

let rec name =
  fun
  | PIdent(id) => Ident.name(id)
  | [@implicit_arity] PExternal(m, s, _) => name(m) ++ "." ++ s;

let rec head =
  fun
  | PIdent(id) => id
  | [@implicit_arity] PExternal(m, _, _) => head(m);

let heads = p => {
  let rec heads = (p, acc) =>
    switch (p) {
    | PIdent(id) => [id, ...acc]
    | [@implicit_arity] PExternal(m, _, _) => heads(m, acc)
    };
  heads(p, []);
};

let rec last =
  fun
  | PIdent(id) => Ident.name(id)
  | [@implicit_arity] PExternal(_, s, _) => s;

let rec stamp =
  fun
  | PIdent(id) => id.stamp
  | [@implicit_arity] PExternal(p, _, _) => stamp(p);
