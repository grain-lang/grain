include Hashtbl.S with type key = Path.t;

let sexp_of_t: ('a => Sexplib.Sexp.t, t('a)) => Sexplib.Sexp.t;
let t_of_sexp: (Sexplib.Sexp.t => 'a, Sexplib.Sexp.t) => t('a);
