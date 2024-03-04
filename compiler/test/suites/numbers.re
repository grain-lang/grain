open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_utils;

describe("numbers", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertRun = makeRunner(test_or_skip);
  let assertRunError = makeErrorRunner(test_or_skip);
  let assertCompileError = makeCompileErrorRunner(test);

  assertRun("numbers1", "print(0.3333 + (1 / 3))", "0.6666333333333333\n");
  assertRun(
    "numbers2",
    "print(0.3333 - (1 / 3))",
    "-0.00003333333333332966\n",
  );
  assertRun(
    "numbers3",
    "print(0.0 + ((1 / 3) * (1 / 3)))",
    "0.1111111111111111\n",
  );
  assertRun("numbers4", "print(1 / 3)", "1/3\n");
  assertRun("numbers5", "print(1.0 / 3)", "0.3333333333333333\n");
  assertRun("numbers6", "print(2 / 6)", "1/3\n");
  assertRun("numbers7", "print((1 / 3) + (1 / 6))", "1/2\n");
  assertRun("numbers8", "print((1 / 3) * (1 / 3))", "1/9\n");
  assertRun("numbers9", "print((1 / 3) / (1 / 3))", "1\n");
  assertRun("numbers10", "print(-2 / 4)", "-1/2\n");
  assertRun("numbers11", "print(2 / -4)", "-1/2\n");
  assertRun("numbers12", "print(-2 / -4)", "1/2\n");
  assertRun("numbers13", "print(1e3)", "1000.0\n");
  assertCompileError("numbers14", "9 / 0", "denominator of zero");
  assertCompileError("numbers14", "9 / 000", "denominator of zero");
  assertCompileError("numbers14", "9 / 0_0_0", "denominator of zero");
  // basic syntax tests
  assertRun("number_syntax1", "print(1.2)", "1.2\n");
  assertRun("number_syntax2", "print(1.0)", "1.0\n");
  assertCompileError("number_syntax2_parse_err", "print(1.)", "Syntax error");
  assertRun("number_syntax3", "print(0.2)", "0.2\n");
  assertCompileError(
    "number_syntax3_parse_err",
    "print(.2)",
    "Floats must contain a leading zero. Use 0.2 instead.",
  );
  assertCompileError("number_syntax4", ".", "Syntax error");
  assertRun("number_syntax5", "print(1.2d)", "1.2\n");
  assertCompileError(
    "number_syntax5_parse_err_trailing",
    "print(1.d)",
    "Unbound record label d",
  );
  assertCompileError(
    "number_syntax5_parse_err_leading",
    "print(.1d)",
    "Floats must contain a leading zero. Use 0.1d instead.",
  );
  assertRun("number_syntax6", "print(1.2f)", "1.2000000476837159\n");
  assertCompileError(
    "number_syntax6_parse_err_trailing",
    "print(1.f)",
    "Unbound record label f",
  );
  assertCompileError(
    "number_syntax6_parse_err_leading",
    "print(.1f)",
    "Floats must contain a leading zero. Use 0.1f instead.",
  );
  assertRun("number_syntax7", "print(1e2)", "100.0\n");
  assertRun("number_syntax8", "print(1.2e2)", "120.0\n");
  assertRun("number_syntax9", "print(1l)", "1\n");
  assertRun("number_syntax10", "print(1L)", "1\n");
  assertRun(
    "number_syntax11",
    "print(9_223_372_036_854_775_808)",
    "9223372036854775808\n",
  );
  assertRun(
    "number_shift_promote",
    "print(5 << 64)",
    "92233720368547758080\n",
  );
  assertRun(
    "number_syntax11",
    "print(987654321987654321987654321)",
    "987654321987654321987654321\n",
  );
  assertRun(
    "number_syntax12",
    "print(987654321987654321987654321t)",
    "987654321987654321987654321\n",
  );
  assertRun("number_syntax13", "print(17179869184 - 1024)", "17179868160\n");
  // equality checks
  assertRun("nan_equality1", {|print(NaNf == NaNf)|}, "false\n");
  assertRun(
    "nan_equality2",
    {|from "float64" include Float64; use Float64.{ (/) }; print((0.0d / 0.0d) == (0.0d / 0.0d))|},
    "false\n",
  );
  assertRun("nan_equality3", {|print(0.0 / 0.0 == 0.0 / 0.0)|}, "false\n");
  assertRun("number_equality", {|print(5.0 == 5)|}, "true\n");
  assertRun("number_equality2", {|print(5 == 5.0)|}, "true\n");
  assertRun("shortnum_parse_equality1", {|print(0xffs == -1s)|}, "true\n");
  assertRun("shortnum_parse_equality2", {|print(0xffffS == -1S)|}, "true\n");
  // comparison checks
  assertRun(
    "number_compare1",
    {|print(9007199254740992 < 9007199254740993)|},
    "true\n",
  );
  // syntax errors
  assertCompileError(
    "number_syntax_err2",
    "987654321987654321987654321l",
    "representable 32-bit",
  );
  assertCompileError(
    "number_syntax_err3",
    "987654321987654321987654321L",
    "representable 64-bit",
  );
  assertCompileError(
    "negative_unsigned_err1",
    "-1ul",
    "Uint32 literal -1ul contains a sign but should be unsigned; consider using 0xfffffffful instead.",
  );
  assertCompileError(
    "negative_unsigned_err2",
    "-1uL",
    "Uint64 literal -1uL contains a sign but should be unsigned; consider using 0xffffffffffffffffuL instead.",
  );
  assertCompileError(
    "negative_unsigned_err3",
    "-9999999999999999999ul",
    "Uint32 literal -9999999999999999999ul contains a sign but should be unsigned.",
  );
  assertCompileError(
    "negative_unsigned_err4",
    "-99999999999999999999999uL",
    "Uint64 literal -99999999999999999999999uL contains a sign but should be unsigned.",
  );
  assertCompileError(
    "numbers_rational_zero_denom",
    "1/0r",
    "Rational numbers may not have a denominator of zero.",
  );
  assertCompileError(
    "numbers_rational_multiple_zero_denom",
    "1/0000r",
    "Rational numbers may not have a denominator of zero.",
  );
  assertCompileError(
    "numbers_rational_multiple_zero_denom",
    "1/0_0_0_0r",
    "Rational numbers may not have a denominator of zero.",
  );
  // runtime errors
  assertRunError(
    "unsigned_overflow_err1",
    {|from "uint32" include Uint32; let n = -1; print(Uint32.fromNumber(n))|},
    "Overflow: Number overflow",
  );
  assertRunError(
    "unsigned_overflow_err2",
    {|from "uint32" include Uint32; let n = 0x1ffffffff; print(Uint32.fromNumber(n))|},
    "Overflow: Number overflow",
  );
  assertRunError(
    "unsigned_overflow_err3",
    {|from "uint64" include Uint64; let n = -1; print(Uint64.fromNumber(n))|},
    "Overflow: Number overflow",
  );
  assertRunError(
    "unsigned_overflow_err4",
    {|from "uint64" include Uint64; let n = 0x1ffffffffffffffff; print(Uint64.fromNumber(n))|},
    "Overflow: Number overflow",
  );
  assertRunError(
    "shortnum_err1",
    {|
    from "uint8" include Uint8;
    // Suppress warnings about using `fromNumber` on constants, since that's what we want to test.
    let fromNumber = Uint8.fromNumber
    print(fromNumber(-1))
    |},
    "Overflow: Number overflow",
  );
  assertRunError(
    "shortnum_err2",
    {|
    from "uint8" include Uint8;
    // Suppress warnings about using `fromNumber` on constants, since that's what we want to test.
    let fromNumber = Uint8.fromNumber
    print(fromNumber(256))
    |},
    "Overflow: Number overflow",
  );
  assertCompileError(
    "shortnum_err3",
    "-1us",
    "Uint8 literal -1us contains a sign but should be unsigned; consider using 0xffus instead.",
  );
  assertCompileError(
    "shortnum_err4",
    "-99999us",
    "Uint8 literal -99999us contains a sign but should be unsigned.",
  );

  // well-formedness warnings
  test("short_fromNumber_warn1", ({expect}) => {
    expect.string(Warnings.message(FromNumberLiteral(Uint8, "Uint8", "2"))).
      toMatch(
      "2us",
    )
  });
  test("short_fromNumber_warn2", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Uint16, "Uint16", "2")),
    ).
      toMatch(
      "2uS",
    )
  });
  test("short_fromNumber_warn3", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Uint32, "Uint32", "2")),
    ).
      toMatch(
      "2ul",
    )
  });
  test("short_fromNumber_warn4", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Uint64, "Uint64", "2")),
    ).
      toMatch(
      "2uL",
    )
  });
  test("short_fromNumber_warn5", ({expect}) => {
    expect.string(Warnings.message(FromNumberLiteral(Int8, "Int8", "2"))).
      toMatch(
      "2s",
    )
  });
  test("short_fromNumber_warn6", ({expect}) => {
    expect.string(Warnings.message(FromNumberLiteral(Int16, "Int16", "2"))).
      toMatch(
      "2S",
    )
  });
  test("short_fromNumber_warn7", ({expect}) => {
    expect.string(Warnings.message(FromNumberLiteral(Int32, "Int32", "2"))).
      toMatch(
      "2l",
    )
  });
  test("short_fromNumber_warn8", ({expect}) => {
    expect.string(Warnings.message(FromNumberLiteral(Int64, "Int64", "2"))).
      toMatch(
      "2L",
    )
  });
  test("float32_fromNumber_warn9", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Float32, "Float32", "5")),
    ).
      toMatch(
      "5.f",
    )
  });
  test("float32_fromNumber_warn2", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Float32, "Float32", "5.")),
    ).
      toMatch(
      "5.f",
    )
  });
  test("float32_fromNumber_warn3", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Float32, "Float32", "5.5")),
    ).
      toMatch(
      "5.5f",
    )
  });
  test("float64_fromNumber_warn1", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Float64, "Float64", "5")),
    ).
      toMatch(
      "5.d",
    )
  });
  test("float64_fromNumber_warn2", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Float64, "Float64", "5.")),
    ).
      toMatch(
      "5.d",
    )
  });
  test("float64_fromNumber_warn3", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Float64, "Float64", "5.5")),
    ).
      toMatch(
      "5.5d",
    )
  });
  test("rational_fromNumber_warn1", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Rational, "Rational", "2")),
    ).
      toMatch(
      "2/1r",
    )
  });
  test("rational_fromNumber_warn2", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(Rational, "Rational", "2/4")),
    ).
      toMatch(
      "2/4r",
    )
  });
  test("bigint_fromNumber_warn1", ({expect}) => {
    expect.string(
      Warnings.message(FromNumberLiteral(BigInt, "Bigint", "2")),
    ).
      toMatch(
      "2t",
    )
  });
});
