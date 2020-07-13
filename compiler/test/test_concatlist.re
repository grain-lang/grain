/* Port of Pyret's concatlist tests */
open Grain_codegen;
open Concatlist;

open OUnit2;

let l1 = append(snoc(cons(1, empty), 2), cons(3, snoc(empty, 4)));

let sexp_printer = (to_sexp, x) => Sexplib.Sexp.to_string_hum(to_sexp(x));
let list_printer = (p, l) =>
  "[" ++ String.concat("; ", List.map(p, l)) ++ "]";

let str_printer = x => x;
let num_printer = string_of_int;
let cl_printer = elt_printer =>
  sexp_printer(Concatlist.sexp_of_t(elt_printer));

let cl_num_printer = cl_printer(Sexplib.Conv.sexp_of_int);

let printer = str_printer;

let test_foldl = _ => {
  let res = fold_left((acc, e) => acc ++ string_of_int(e * e), "B", l1);
  assert_equal(~printer, "B14916", res);
};

let test_foldr = _ => {
  let res = fold_right((e, acc) => string_of_int(e * e) ++ acc, l1, "B");
  assert_equal(~printer, "14916B", res);
};

let test_is_empty = _ =>
  assert_bool("is_empty", is_empty(append(empty, empty)));

let test_hd = _ => {
  let res = hd(append(empty, cons(1, empty)));
  assert_equal(~printer=num_printer, 1, res);
};

let test_tl = _ => {
  let res = tl(cons(1, empty));
  assert_equal(~printer=cl_num_printer, empty, res);
};

let test_last = _ => {
  let res = last @@ append(snoc(empty, 1), empty);
  assert_equal(~printer=num_printer, 1, res);
};

let test_map_to_list = _ => {
  let aux = ref("");
  let func = e => {
    let as_str = string_of_int(e);
    aux := aux^ ++ as_str;
    as_str;
  };
  assert_equal(
    ~printer=list_printer(str_printer),
    ["1", "2", "3", "4"],
    mapped_list_of_t(func, l1),
  );
  assert_equal(~printer, "3421", aux^);
};

let test_left_map_to_list = _ => {
  let aux = ref("");
  let func = e => {
    let as_str = string_of_int(e);
    aux := aux^ ++ as_str;
    as_str;
  };
  assert_equal(
    ~printer=list_printer(str_printer),
    ["1", "2", "3", "4"],
    mapped_list_of_t(func, l1),
  );
  assert_equal(~printer, "1234", aux^);
};

let tests =
  "Concatlist"
  >::: [
    "is_empty" >:: test_is_empty,
    "foldl" >:: test_foldl,
    "foldr" >:: test_foldr,
    "hd" >:: test_hd,
    "tl" >:: test_tl,
    "last" >:: test_last,
    "map_to_list" >:: test_map_to_list,
  ];
