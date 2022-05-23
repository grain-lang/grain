open Grain_utils;

open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("utils/literals", ({test}) => {
  test("literals_conv_bigintBad", ({expect}) => {
    let res = Literals.conv_bigint("");
    expect.option(res).toBeNone();
  });

  test("literals_conv_bigint_dec_simple1", ({expect}) => {
    let res = Literals.conv_bigint("15");
    expect.option(res).toBeSome();
    let (_, res) = Option.get(res);
    expect.array(res).toEqual(~equals=Int64.equal, [|15L|]);
  });

  test("literals_conv_bigint_dec_simple2", ({expect}) => {
    let res = Literals.conv_bigint("0u15");
    expect.option(res).toBeSome();
    let (_, res) = Option.get(res);
    expect.array(res).toEqual(~equals=Int64.equal, [|15L|]);
  });

  test("literals_conv_bigint_dec_simple3", ({expect}) => {
    let res = Literals.conv_bigint("0U15");
    expect.option(res).toBeSome();
    let (_, res) = Option.get(res);
    expect.array(res).toEqual(~equals=Int64.equal, [|15L|]);
  });

  test("literals_conv_bigint_hex_simple1", ({expect}) => {
    let res = Literals.conv_bigint("0x15");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(~equals=Int64.equal, [|21L|]);
  });

  test("literals_conv_bigint_hex_simple2", ({expect}) => {
    let res = Literals.conv_bigint("0X15");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(~equals=Int64.equal, [|21L|]);
  });

  test("literals_conv_bigint_oct_simple1", ({expect}) => {
    let res = Literals.conv_bigint("0o15");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(~equals=Int64.equal, [|13L|]);
  });

  test("literals_conv_bigint_oct_simple2", ({expect}) => {
    let res = Literals.conv_bigint("0O15");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(~equals=Int64.equal, [|13L|]);
  });

  test("literals_conv_bigint_bin_simple1", ({expect}) => {
    let res = Literals.conv_bigint("0b10101010101");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(~equals=Int64.equal, [|1365L|]);
  });

  test("literals_conv_bigint_bin_simple2", ({expect}) => {
    let res = Literals.conv_bigint("0B10101010101");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(~equals=Int64.equal, [|1365L|]);
  });

  test("literals_conv_bigint3", ({expect}) => {
    let res = Literals.conv_bigint("1773345651416625031020");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    // 60221d5a601f9fa76c
    expect.array(res).toEqual(
      ~equals=Int64.equal,
      [|0x221d5a601f9fa76cL, 0x60L|],
    );
  });

  test("literals_conv_bigint4", ({expect}) => {
    let res = Literals.conv_bigint("0xffffffffffffffff");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(~equals=Int64.equal, [|0xffffffffffffffffL|]);
  });

  test("literals_conv_bigint5", ({expect}) => {
    let res = Literals.conv_bigint("0xffffffffffffffffffff");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(
      ~equals=Int64.equal,
      [|0xffffffffffffffffL, 0xffffL|],
    );
  });

  test("literals_conv_bigint6", ({expect}) => {
    let res = Literals.conv_bigint("1208925819614629174706175");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(
      ~equals=Int64.equal,
      [|0xffffffffffffffffL, 0xffffL|],
    );
  });

  test("literals_conv_bigint7", ({expect}) => {
    let res =
      Literals.conv_bigint(
        "120892581961462917470617512089258196146291747061751208925819614629174706175",
      );
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeFalse();
    expect.array(res).toEqual(
      ~equals=Int64.equal,
      [|
        0x3487feb7b5ffffffL,
        0x6ee1c22d3bf3856dL,
        0x87d2c3c90e57b57eL,
        0x446c3b15f99266L,
      |],
    );
  });

  test("literals_conv_bigint_neg1", ({expect}) => {
    let res =
      Literals.conv_bigint(
        "-120892581961462917470617512089258196146291747061751208925819614629174706175",
      );
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeTrue();
    expect.array(res).toEqual(
      ~equals=Int64.equal,
      [|
        0x3487feb7b5ffffffL,
        0x6ee1c22d3bf3856dL,
        0x87d2c3c90e57b57eL,
        0x446c3b15f99266L,
      |],
    );
  });

  test("literals_conv_bigint_neg2", ({expect}) => {
    let res = Literals.conv_bigint("-0xffffffffffffffffffff");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeTrue();
    expect.array(res).toEqual(
      ~equals=Int64.equal,
      [|0xffffffffffffffffL, 0xffffL|],
    );
  });

  test("literals_conv_bigint_neg3", ({expect}) => {
    let res = Literals.conv_bigint("-0O15");
    expect.option(res).toBeSome();
    let (neg, res) = Option.get(res);
    expect.bool(neg).toBeTrue();
    expect.array(res).toEqual(~equals=Int64.equal, [|13L|]);
  });
});
