open Grain_tests.TestFramework;
open Grain_tests.Runner;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

describe("graindoc", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertGrianDocOutput = makeGrainDocRunner(test_or_skip);
  ();
  assertGrianDocOutput("noDoc", "noDoc");
});
