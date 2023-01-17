open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("early return", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);
  let assertSnapshot = makeSnapshotRunner(test);

  assertRun("early_return1", "let fun = () => return 5; print(fun())", "5\n");
  assertFileRun("early_return2", "earlyReturn", "Some(4)\nNone\n");
  assertSnapshot(
    "early_return3",
    "provide let foo = () => { if (1 == 0) return true; return false }",
  );

  assertCompileError(
    "early_return_err1",
    "return",
    "`return` statement used outside of a function",
  );
  assertCompileError(
    "early_return_err2",
    "if (false) return 5",
    "`return` statement used outside of a function",
  );
  assertCompileError(
    "mismatched_return_err1",
    {|
      () => {
        if (false) return 5
        6
      }
    |},
    "All returned values must use the `return` keyword if the function returns early",
  );
  assertCompileError(
    "mismatched_return_err2",
    {|
      () => {
        if (false) return 5
        if (false) {
          6
        } else {
          7
        }
      }
    |},
    "line 4",
  );
  assertCompileError(
    "mismatched_return_err3",
    {|
      () => {
        if (false) return 5
        if (false) {
          return 6
        } else {
          7
        }
      }
    |},
    "line 7",
  );
  assertRun(
    "early_return3",
    {|
      print((() => {
        if (false) return 5
        if (false) {
          fail "failure"
        } else if (false) {
          throw Failure("failure")
        } else {
          return 7
        }
      })())
    |},
    "7\n",
  );
  assertCompileError(
    "mismatched_return_err4",
    {|
      () => {
        if (false) return 5
        match (false) {
          true => return 6,
          _ => 7
        }
      }
    |},
    "line 6",
  );
  assertRun(
    "early_return4",
    {|
      print((() => {
        if (false) return 5
        match ("foo") {
          "bar" => fail "failure",
          "baz" => throw Failure("failure"),
          _ => return 7
        }
      })())
    |},
    "7\n",
  );
});
