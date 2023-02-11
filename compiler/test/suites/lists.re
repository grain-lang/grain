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
  // arbitrary-position spread
  assertRun(
    "list_spread_anywhere1",
    "let a = [1, 2]; print([...a, 3, 4])",
    "[1, 2, 3, 4]\n",
  );
  assertRun(
    "list_spread_anywhere2",
    "let a = [2]; let b = [5, 6]; print([1, ...a, 3, 4, ...b])",
    "[1, 2, 3, 4, 5, 6]\n",
  );
  assertRun(
    "list_spread_anywhere3",
    "let a = [1, 2, 3]; print([...a])",
    "[1, 2, 3]\n",
  );
  assertCompileError(
    "list_spread_anywhere4",
    "let a = [1, 2]; [...a, \"a\"]",
    "has type String but",
  );
});
