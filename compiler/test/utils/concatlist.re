/* Port of Pyret's concatlist tests */
open Grain_codegen;
open Concatlist;

open TestFramework;
open Runner;

let l1 = append(snoc(cons(1, empty), 2), cons(3, snoc(empty, 4)));

describe("aux/concatlist", ({test}) => {
  test("foldl", ({expect}) => {
    let res = fold_left((acc, e) => acc ++ string_of_int(e * e), "B", l1);
    expect.string(res).toEqual("B14916");
  });

  test("foldr", ({expect}) => {
    let res = fold_right((e, acc) => string_of_int(e * e) ++ acc, l1, "B");
    expect.string(res).toEqual("14916B");
  });

  test("is_empty", ({expect}) =>
    expect.bool(is_empty(append(empty, empty))).toBeTrue()
  );

  test("hd", ({expect}) => {
    let res = hd(append(empty, cons(1, empty)));
    expect.int(res).toBe(1);
  });

  test("tl", ({expect}) => {
    let res = tl(cons(1, empty));
    expect.equal(res, empty);
  });

  test("last", ({expect}) => {
    let res = last @@ append(snoc(empty, 1), empty);
    expect.int(res).toBe(1);
  });

  test("map_to_list", ({expect}) => {
    let aux = ref("");
    let func = e => {
      let as_str = string_of_int(e);
      aux := aux^ ++ as_str;
      as_str;
    };
    expect.equal(mapped_list_of_t(func, l1), ["1", "2", "3", "4"]);
    expect.string(aux^).toEqual("3421");
  });

  test("left_map_to_list", ({expect}) => {
    let aux = ref("");
    let func = e => {
      let as_str = string_of_int(e);
      aux := aux^ ++ as_str;
      as_str;
    };
    expect.equal(left_mapped_list_of_t(func, l1), ["1", "2", "3", "4"]);
    expect.string(aux^).toEqual("1234");
  });
});
