open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("runtime", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertRuntime = makeRuntimeRunner(test_or_skip);
  assertRuntime("numbers.test");

  assertRuntime("unsafe/wasmf32.test");
  assertRuntime("unsafe/wasmf64.test");
  assertRuntime("unsafe/wasmi32.test");
  assertRuntime("unsafe/wasmi64.test");
});
