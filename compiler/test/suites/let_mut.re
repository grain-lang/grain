open TestFramework;
open Runner;

describe("let mut", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);
  let assertFileRun = makeFileRunner(test);

  assertSnapshot("let-mut1", "let mut b = 4;b");
  assertSnapshot("let-mut2", "let mut b = (4, (5, 6));b");
  assertSnapshot("let-mut3", "let mut b = box(4); unbox(b)");
  assertSnapshot("let-mut4", "let mut b = 4;b = 3;b");
  assertSnapshot("let-mut5", "let mut b = 4;b = b - 1;b");
  assertFileRun("counter-mut", "counter-mut", "1\n2\n3\n");
  assertCompileError(
    "test_mut_typing",
    "let mut a = false; a + 4",
    "expression has type Bool but",
  );
  // let mut destructure tests
  assertRun(
    "let-mut_destructure1",
    "let mut (x, y, z) = (5, false, \"foo\"); x = 6; y = true; z = \"bar\"; print(x); print(y); print(z)",
    "6\ntrue\nbar\n",
  );
  assertRun(
    "let-mut_destructure2",
    "{let mut (x, y, z) = (5, false, \"foo\"); x = 6; y = true; z = \"bar\"; print(x); print(y); print(z)}",
    "6\ntrue\nbar\n",
  );
  assertRun(
    "let-mut_destructure3",
    "record Rec {foo: Number, bar: Bool}; let mut {foo, bar} = {foo: 5, bar: false}; foo = 6; bar = true; print(foo); print(bar)",
    "6\ntrue\n",
  );
  assertRun(
    "let-mut_destructure4",
    "record Rec {foo: Number, bar: Bool}; {let mut {foo, bar} = {foo: 5, bar: false}; foo = 6; bar = true; print(foo); print(bar)}",
    "6\ntrue\n",
  );
  // not-mut let errors
  assertCompileError(
    "let-mut_err1",
    "let x = 5; x = 6",
    "The identifier x was not declared mutable",
  );
  assertCompileError(
    "let-mut_err2",
    "let (x, y) = (1, 2); x = 6",
    "The identifier x was not declared mutable",
  );
  assertCompileError(
    "let-mut_err3",
    "let (x, y) = (1, 2); y = 6",
    "The identifier y was not declared mutable",
  );
  assertCompileError(
    "let-mut_err4",
    "record Rec {foo: Number, bar: Bool}; let {foo, bar} = {foo: 1, bar: false}; foo = 6",
    "The identifier foo was not declared mutable",
  );
  /* Operations on mutable `Number`s */
  assertSnapshot("let-mut_addition1", "let mut b = 4; b = b + 19");
  assertSnapshot("let-mut_addition2", "let mut b = 4; b = b + 19; b");
  assertSnapshot("let-mut_addition3", "let mut b = 4; b += 19; b");
  assertSnapshot("let-mut_subtraction1", "let mut b = 4; b = b - 19");
  assertSnapshot("let-mut_subtraction2", "let mut b = 4; b = b - 19; b");
  assertSnapshot("let-mut_subtraction3", "let mut b = 4; b -= 19; b");
  assertSnapshot("let-mut_multiplication1", "let mut b = 4; b = b * 19");
  assertSnapshot("let-mut_multiplication2", "let mut b = 4; b = b * 19; b");
  assertSnapshot("let-mut_multiplication3", "let mut b = 4; b *= 19; b");
  assertSnapshot("let-mut_division1", "let mut b = 76; b = b / 19");
  assertSnapshot("let-mut_division2", "let mut b = 76; b = b / 19; b");
  assertSnapshot("let-mut_division3", "let mut b = 76; b /= 19; b");
  /* Exported let mut */
  assertRun(
    "let-mut_export1",
    "import { x } from \"letMutExport\"; x = 5; x = 6; print(x)",
    "6\n",
  );
  /* unsafe let mut in a loop */
  assertFileRun("let-mut_loop", "letMutForLoop", "0N\n1N\n2N\n3N\n4N\n");
});
