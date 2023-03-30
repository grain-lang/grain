open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("memory_base", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertFileRun = makeFileRunner(test_or_skip);
  // Non-snapshots since we want to track functional issues.
  assertFileRun("basecase", "memoryBase/basecase", "HelloWorld\n");
  assertFileRun(
    ~config_fn=() => {Grain_utils.Config.memory_base := Some(0x400)},
    "same_as_default",
    "memoryBase/sameAsDefault",
    "HelloWorld\n",
  );
  assertFileRun(
    ~config_fn=() => {Grain_utils.Config.memory_base := Some(0x0)},
    "zero",
    "memoryBase/zero",
    "HelloWorld\n",
  );
  assertFileRun(
    ~config_fn=() => {Grain_utils.Config.memory_base := Some(0x800)},
    "slightly_higher",
    "memoryBase/slightlyHigher",
    "HelloWorld\n",
  );
  assertFileRun(
    ~config_fn=() => {Grain_utils.Config.memory_base := Some(0x110000)},
    "very_large",
    "memoryBase/veryLarge",
    "HelloWorld\n",
  );
  assertFileRun(
    ~config_fn=() => {Grain_utils.Config.memory_base := Some(0x110000)},
    "asserts",
    "memoryBase/asserts",
    "",
  );
});
