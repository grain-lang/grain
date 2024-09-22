open Grain_tests.TestFramework;
open Grain_tests.Runner;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

describe("expressions", ({test, testSkip}) => {
  let assertNoWarning = makeNoWarningRunner(test);
  let assertWarning = makeWarningRunner(test);

  assertWarning(
    "non_void_non_returning_expression1",
    {|
    1
    print(2)
    |},
    Grain_utils.Warnings.StatementType,
  );

  assertWarning(
    "non_void_non_returning_expression2",
    {|
    let f = () => {
      1
      print(2)
    }
    |},
    Grain_utils.Warnings.StatementType,
  );

  assertNoWarning(
    "non_void_non_returning_expression3",
    {|
    ignore(1)
    print(2)
    |},
  );

  assertNoWarning(
    "non_void_non_returning_expression4",
    {|
    let f = () => {
      ignore(1)
      print(2)
    }
    |},
  );

  assertNoWarning(
    "non_void_non_returning_expression5",
    {|
    let rec f = () => {
      if (true) {
        f()
        void
      }
    }
    |},
  );
});
