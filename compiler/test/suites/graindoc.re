open Grain_tests.TestFramework;
open Grain_tests.Runner;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

describe("graindoc", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertGrianDocOutput = makeGrainDocRunner(test_or_skip);
  let assertGrainDocError = makeGrainDocErrorRunner(test_or_skip);
  ();
  assertGrianDocOutput("noDoc", "noDoc", [||]);
  assertGrianDocOutput(
    "descriptions",
    "descriptions",
    [|"--current-version=v0.2.0"|],
  );
  assertGrianDocOutput("since", "since", [|"--current-version=v0.2.0"|]);
  assertGrianDocOutput("example", "example", [|"--current-version=v0.2.0"|]);
  assertGrianDocOutput(
    "functionDoc",
    "functionDoc",
    [|"--current-version=v0.2.0"|],
  );
  assertGrianDocOutput("types", "types", [|"--current-version=v0.2.0"|]);
  assertGrainDocError(
    "singleSince",
    "singleSince",
    "Attribute @since is only allowed to appear once.",
    [|"--current-version=v0.2.0"|],
  );
  assertGrainDocError(
    "singleReturn",
    "singleReturn",
    "Attribute @returns is only allowed to appear once.",
    [|"--current-version=v0.2.0"|],
  );
});
