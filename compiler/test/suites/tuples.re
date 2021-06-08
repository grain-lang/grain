open TestFramework;
open Runner;

describe("tuples", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);

  assertRun("print_tup", "print((1, 2))", "(1, 2)\n");
  assertRun("big_tup", "print((1, 2, 3, 4))", "(1, 2, 3, 4)\n");
  assertSnapshot("big_tup_access", "let (a, b, c, d) = (1, 2, 3, 4); c");
  assertSnapshot("nested_tup_1", "let (a, b) = ((1, 2), (3, 4)); a");
  assertSnapshot(
    "nested_tup_2",
    "let (a, b) = ((1, 2), (3, 4)); let (c, d) = b; d",
  );
  assertSnapshot(
    "nested_tup_3",
    "let (x, y) = ((1, 2), (3, 4)); let (a, b) = y; a",
  );
  assertSnapshot("no_singleton_tup", "(1)");
  // trailing commas
  assertSnapshot("tup1_trailing", "(1, 2, 3,)");
  assertSnapshot("tup1_trailing_space", "(1, 2, 3, )");
  assertCompileError("invalid_empty_trailing", "(,)", "Error: Syntax error");
});
