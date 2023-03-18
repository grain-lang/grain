open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("cli_options", ({test, testSkip}) => {
  let assertFileSnapshot = makeSnapshotFileRunner(test);

  assertFileSnapshot("call_start", "cliOptions/callstart");
});
