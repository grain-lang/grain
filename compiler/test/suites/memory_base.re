open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("memory_base", ({test}) => {
  let assertFileRun = makeFileRunner(test);
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
