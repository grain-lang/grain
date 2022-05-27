include Hashtbl.Make({
  type t = Path.t;
  let hash = x => Hashtbl.hash(Path.name(x));
  let equal = (a, b) => Path.compare(a, b) === 0;
});

let sexp_of_t = (conv, m) => {
  let sexp_of_path = Path.sexp_of_t;
  open Sexplib;
  let pairs =
    Seq.map(
      ((key, v)) => Sexp.List([sexp_of_path(key), conv(v)]),
      to_seq(m),
    );
  Sexp.List(List.of_seq(pairs));
};

let t_of_sexp = (conv, sexp) => {
  let path_of_sexp = Path.t_of_sexp;
  Sexplib.Conv.(
    Sexplib.Sexp.(
      switch (sexp) {
      | Atom(str) => of_sexp_error("t_of_sexp: list needed", sexp)
      | List(sexprs) =>
        let fields =
          List.map(
            fun
            | List([key, value]) => (path_of_sexp(key), conv(value))
            | _ => of_sexp_error("t_of_sexp: invalid field", sexp),
            sexprs,
          );
        let map = create(16);
        List.iter(((k, v)) => add(map, k, v), fields);
        map;
      }
    )
  );
};
