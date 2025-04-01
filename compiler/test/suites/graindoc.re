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
    "Attribute `@since` is only allowed to appear once on `SingleSince.test`.",
    [|"--current-version=v0.2.0"|],
  );
  assertGrainDocError(
    "singleReturn",
    "singleReturn",
    "Attribute `@returns` is only allowed to appear once on `SingleReturn.test`.",
    [|"--current-version=v0.2.0"|],
  );
  assertGrainDocError(
    "missingLabeledParam",
    "missingLabeledParamType",
    "Unable to find a matching function parameter for `value` on `MissingLabeledParam.missing`. Make sure a parameter exists with this label or use `@param <param_index> value` for unlabeled parameters.",
    [|"--current-version=v0.2.0"|],
  );
  assertGrainDocError(
    "missingUnlabeledParam",
    "missingUnlabeledParamType",
    "Unable to find a type for parameter at index `0` on `MissingUnlabeledParamType.missing`. Make sure a parameter exists at this index in the parameter list.",
    [|"--current-version=v0.2.0"|],
  );
  assertGrainDocError(
    "invalidAttr",
    "invalidAttr",
    "Invalid attribute `@param` on `InvalidAttr.Invalid.InvalidVar`",
    [|"--current-version=v0.2.0"|],
  );
});
