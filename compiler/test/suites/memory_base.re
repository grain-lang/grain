open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("memory_base", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertFileRun = makeFileRunner(test_or_skip);
  // Non-snapshots since we want to track functional issues.
  assertFileRun("basecase", "memoryBase/basecase", "HelloWorld\n");
  assertFileRun(
    "same_as_default",
    "memoryBase/sameAsDefault",
    "HelloWorld\n",
  );
  assertFileRun("zero", "memoryBase/zero", "HelloWorld\n");
  assertFileRun(
    "slightly_higher",
    "memoryBase/slightlyHigher",
    "HelloWorld\n",
  );
  assertFileRun("very_large", "memoryBase/veryLarge", "HelloWorld\n");
  assertFileRun("asserts", "memoryBase/asserts", "");
});
