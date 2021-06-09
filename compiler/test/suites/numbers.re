open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("numbers", ({test}) => {
  let assertRun = makeRunner(test);
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
  assertCompileError("numbers13", "9 / 0", "denominator of zero");
  // basic syntax tests
  assertRun("number_syntax1", "print(1.2)", "1.2\n");
  assertRun("number_syntax2", "print(1.)", "1.0\n");
  assertRun("number_syntax3", "print(.2)", "0.2\n");
  assertCompileError("number_syntax4", ".", "Syntax error");
  assertRun("number_syntax5", "print(1.2d)", "1.2\n");
  assertRun("number_syntax6", "print(1.2f)", "1.2000000476837159\n");
  assertRun("number_syntax7", "print(1e2)", "100.0\n");
  assertRun("number_syntax8", "print(1.2e2)", "120.0\n");
  assertRun("number_syntax9", "print(1l)", "1\n");
  assertRun("number_syntax10", "print(1L)", "1\n");
  // equality checks
  assertRun(
    "nan_equality1",
    {|import Float32 from "float32"; print(Float32.div(0.0f, 0.0f) == Float32.div(0.0f, 0.0f))|},
    "false\n",
  );
  assertRun(
    "nan_equality2",
    {|import Float64 from "float64"; print(Float64.div(0.0d, 0.0d) == Float64.div(0.0d, 0.0d))|},
    "false\n",
  );
  assertRun("nan_equality3", {|print(0.0 / 0.0 == 0.0 / 0.0)|}, "false\n");
  // syntax errors
  assertCompileError(
    "number_syntax_err1",
    "987654321987654321987654321",
    "outside of the integer range",
  );
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
});
