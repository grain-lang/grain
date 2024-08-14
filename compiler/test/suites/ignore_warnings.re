open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_utils;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

describe("ignore warnings", ({test, testSkip}) => {
  let assertWarning = makeWarningRunner(test);
  let assertNoWarning = makeNoWarningRunner(test);

  let assertWarningFlag = (name, code, config_warning, expected_warning) => {
    assertWarning(
      ~config_fn=() => {Config.ignore_warnings := []},
      name,
      code,
      expected_warning,
    );

    assertNoWarning(
      ~config_fn=() => {Config.ignore_warnings := [config_warning]},
      name,
      code,
    );
  };

  assertWarningFlag(
    "warning_match",
    {|
    match (true) {
      true => void
    }
    |},
    Config.PartialMatch,
    Warnings.PartialMatch("false"),
  );

  assertWarningFlag(
    "warning_match_all_ignored",
    {|
    match (true) {
      true => void
    }
    |},
    Config.IgnoreAll,
    Warnings.PartialMatch("false"),
  );

  assertWarningFlag(
    "warning_useless_record_spread",
    {|
    record R { x: Number }
    let r = { x: 1 }
    let r2 = { ...r, x: 2 }
    |},
    Config.UselessRecordSpread,
    Warnings.UselessRecordSpread,
  );

  assertWarningFlag(
    "warning_print_unsafe",
    {|
    @unsafe
    let f = () => {
      let a = 1n
      print(a)
    }
    |},
    Config.PrintUnsafe,
    Warnings.PrintUnsafe("I32"),
  );
});
