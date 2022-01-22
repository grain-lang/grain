open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("memory_base", ({test}) => {
  let assertFileRun = makeFileRunner(test);
  // Non-snapshots for now, since we want to track functional issues.
  // Maybe these can be migrated to snapshots once we're confident in
  // everything being stable
  assertFileRun("basecase", "memoryBase/basecase", "HelloWorld\n");
  assertFileRun("no_op", "memoryBase/noop", "HelloWorld\n");
  assertFileRun("zero", "memoryBase/zero", "HelloWorld\n");
  assertFileRun(
    "slightly_higher",
    "memoryBase/slightlyHigher",
    "HelloWorld\n",
  );
  assertFileRun("very_large", "memoryBase/veryLarge", "HelloWorld\n");
});
