open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("lists", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);

  assertRun("list1", "print([1, 2, 3])", "[1, 2, 3]\n");
  assertRun("list2", "print([])", "[]\n");
  assertCompileError("list_heterogeneous", "[1, false, 2]", "type");
  assertSnapshot("list_spread", "let a = [3, 4]; [1, 2, ...a]");
  assertCompileError(
    "invalid_list_no_comma_before_spread",
    "let a = [3, 4]; [1, 2 ...a]",
    "Error: Syntax error",
  );
  // trailing commas
  assertSnapshot("list1_trailing", "[1, 2, 3,]");
  assertSnapshot("list1_trailing_space", "[1, 2, 3, ]");
  assertCompileError("invalid_empty_trailing", "[,]", "Error: Syntax error");
});
