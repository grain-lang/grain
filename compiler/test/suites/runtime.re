open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("runtime", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertRunError = makeErrorRunner(test_or_skip);
  let assertRuntime = makeRuntimeRunner(test_or_skip);

  assertRunError(
    ~config_fn=() => {Grain_utils.Config.compilation_mode := Runtime},
    "runtime_mode_panics",
    {|
      let arr = [> 1, 2, 3]
      provide let x = arr[10]
    |},
    "RuntimeError: unreachable",
  );

  assertRuntime("numbers.test");

  assertRuntime("unsafe/wasmf32.test");
  assertRuntime("unsafe/wasmf64.test");
  assertRuntime("unsafe/wasmi32.test");
  assertRuntime("unsafe/wasmi64.test");
});
