include Hashtbl.Make({
  type t = Ident.t;
  let equal = (i, j) => Ident.same(i, j);
  let hash = i => Hashtbl.hash(i);
});
