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

  assertSnapshot(
    "list_large1",
    "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
      33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,
      49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
      65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
      81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96]",
  );
});
