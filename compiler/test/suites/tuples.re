open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("tuples", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);

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
  // Single-argument tuples are not supported at the parser, but `(1)` is parsed as a grouped value
  assertSnapshot("no_non_trailing_comma_singleton_tup", "(1)");
  assertCompileError("singleton_tup_err", "(1,)", "Expected a pattern");
  assertCompileError(
    "singleton_tup_annotation_err",
    "(1, 1): (Number,)",
    "Expected a type",
  );
  // trailing commas
  assertSnapshot("tup1_trailing", "(1, 2, 3,)");
  assertSnapshot("tup1_destruct_trailing", "let (a, b, c,) =(1, 2, 3,)");
  assertSnapshot("tup1_trailing_space", "(1, 2, 3, )");
  assertCompileError("invalid_empty_trailing", "(,)", "Error: Syntax error");
});
