open Grain_utils;

open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("utils/mini_bigint", ({test}) => {
  test("mini_bigint_add1", ({expect}) => {
    let res = Mini_bigint.unsigned_add_i64(Mini_bigint.zero(), 4L);
    expect.int(Array.length(res.limbs)).toBe(1);
    expect.equal(~equals=Int64.equal, res.limbs[0], 4L);
  });

  test("mini_bigint_add2", ({expect}) => {
    let res =
      Mini_bigint.unsigned_add_i64(
        Mini_bigint.unsigned_add_i64(Mini_bigint.zero(), 4L),
        8L,
      );
    expect.equal(~equals=Int64.equal, res.limbs[0], 12L);
  });

  test("mini_bigint_add3", ({expect}) => {
    let res =
      Mini_bigint.unsigned_add_i64(
        Mini_bigint.unsigned_add_i64(Mini_bigint.zero(), Int64.max_int),
        1L,
      );
    expect.int(Array.length(res.limbs)).toBe(1);
    expect.equal(
      ~equals=Int64.equal,
      res.limbs[0],
      Int64.add(Int64.max_int, 1L),
    );
  });

  test("mini_bigint_add4", ({expect}) => {
    let x = Mini_bigint.zero();
    x.limbs = [|0xFFFFFFFFFFFFFFFFL|];
    let res = Mini_bigint.unsigned_add_i64(x, 1L);
    expect.int(Array.length(res.limbs)).toBe(2);
    expect.equal(~equals=Int64.equal, res.limbs[0], 0L);
    expect.equal(~equals=Int64.equal, res.limbs[1], 1L);
  });

  test("mini_bigint_add5", ({expect}) => {
    let x = Mini_bigint.zero();
    x.limbs = [|0xFFFFFFFFFFFFFFFFL|];
    let res = Mini_bigint.unsigned_add_i64(x, 3L);
    expect.int(Array.length(res.limbs)).toBe(2);
    expect.equal(~equals=Int64.equal, res.limbs[0], 2L);
    expect.equal(~equals=Int64.equal, res.limbs[1], 1L);
  });

  test("mini_bigint_mul1", ({expect}) => {
    let res = Mini_bigint.unsigned_mul_i32(Mini_bigint.of_int(5), 10l);
    expect.int(Array.length(res.limbs)).toBe(1);
    expect.equal(~equals=Int64.equal, res.limbs[0], 50L);
  });

  test("mini_bigint_mul2", ({expect}) => {
    let x = Mini_bigint.zero();
    x.limbs = [|0xFFFFFFFFFFFFFFFFL|];
    let res = Mini_bigint.unsigned_mul_i32(x, 5l);
    expect.int(Array.length(res.limbs)).toBe(2);
    expect.equal(~equals=Int64.equal, res.limbs[0], 0xFFFFFFFFFFFFFFFBL);
    expect.equal(~equals=Int64.equal, res.limbs[1], 0x4L);
  });
});
