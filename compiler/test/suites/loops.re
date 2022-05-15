open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("loops", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);

  assertRun(
    "loop1",
    "let b = box(3);\n            {\n              while (unbox(b) > 0) {\n                b := unbox(b) - 1;\n              };\n              print(unbox(b))\n            }",
    "0\n",
  );
  assertSnapshot(
    "loop2",
    "let b = box(12);\n             let count = box(0);\n            {\n              while (unbox(b) > 0) {\n                b := unbox(b) - 1;\n                count := unbox(count) + 1;\n              };\n              unbox(count)\n            }",
  );
  assertSnapshot("loop3", "let mut b = 3; while (b > 0) { b = b - 1 }; b ");
  assertSnapshot(
    "loop4",
    "let mut b = 12; let mut count = 0; while (b > 0) { b = b - 1; count = count + 1 }; count",
  );
  assertSnapshot(
    "loop5",
    "let mut b = 12; let mut count = 0; while ({b -= 1; b >= 0}) { count += 1 }; count",
  );
  assertRun(
    "for1",
    "let mut x = 0; for (;;) { print(x); x += 1; if (x > 3) break }",
    "0\n1\n2\n3\n",
  );
  assertRun(
    "for2",
    "for (let mut x = 0;;) { print(x); x += 1; if (x > 3) break }",
    "0\n1\n2\n3\n",
  );
  assertRun(
    "for3",
    "for (let mut x = 0; x <= 3;) { print(x); x += 1 }",
    "0\n1\n2\n3\n",
  );
  assertRun(
    "for4",
    "for (let mut x = 0; x <= 3; x += 1) { print(x) }",
    "0\n1\n2\n3\n",
  );
  assertRun(
    "for5",
    "for (let mut x = 0; x <= 3; x += 1) { if (x == 2) continue; print(x) }",
    "0\n1\n3\n",
  );
  assertRun(
    "for6",
    "for (let mut i = 0; i <= 3; i += 1) { for (let mut j = 0; j <= 3; j += 1) { if (j == 2) continue; print(i * j) } }",
    "0\n0\n0\n0\n1\n3\n0\n2\n6\n0\n3\n9\n",
  );
  assertCompileError(
    "loop_err1",
    "break",
    "`break` statement used outside of a loop",
  );
  assertCompileError(
    "loop_err2",
    "continue",
    "`continue` statement used outside of a loop",
  );
  assertCompileError(
    "loop_err3",
    "for (let mut x = 0; x <= 3; x += 1) { void }; continue",
    "`continue` statement used outside of a loop",
  );
  assertCompileError(
    "loop_err4",
    "for (let mut x = 0; x <= 3; x += 1) { () => { continue } }",
    "`continue` statement used outside of a loop",
  );
  assertCompileError(
    "loop_err5",
    "for (let mut x = 0; x <= 3; x += 1) { void }; x",
    "Unbound value x",
  );
  assertCompileError(
    "loop_err6",
    "let x = while (false) { 5 }; x + 3",
    "has type Number but",
  );
  assertCompileError(
    "loop_err7",
    "let x = for (;false;) { 5 }; x + 3",
    "has type Number but",
  );
});
