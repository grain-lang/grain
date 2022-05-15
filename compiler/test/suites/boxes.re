open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("boxes", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);

  assertRun(
    "box1",
    "let b = box(4);\n            {\n              print(unbox(b))\n            }",
    "4\n",
  );
  assertRun(
    "box2",
    "let b = box((4, (5, 6)));\n            {\n              print(unbox(b))\n            }",
    "(4, (5, 6))\n",
  );
  assertRun(
    "box3",
    "let b = box(box(4));\n            {\n              print(unbox(unbox(b)))\n            }",
    "4\n",
  );
  assertRun(
    "box4",
    "let b = box(4);\n            {\n              b := 3;\n              print(unbox(b))\n            }",
    "3\n",
  );
  assertRun(
    "box5",
    "let b = box(4);\n            {\n              b := unbox(b) - 1;\n              print(unbox(b))\n            }",
    "3\n",
  );
  assertSnapshot("test_set_extra1", "box(1) := 2");
  assertFileRun("counter-box", "counter-box", "1\n2\n3\n");
  assertCompileError("test_unbox_err", "unbox(5)", "Box");
  assertCompileError(
    "test_box_typing",
    "unbox(box(false)) + 4",
    "expression has type Bool but",
  );
  /* Operations on Box<Number> */
  assertSnapshot("box_addition1", "let b = box(4); b := unbox(b) + 19");
  assertSnapshot(
    "box_addition2",
    "let b = box(4); b := unbox(b) + 19; unbox(b)",
  );
  assertSnapshot("box_subtraction1", "let b = box(4); b := unbox(b) - 19");
  assertSnapshot(
    "box_subtraction2",
    "let b = box(4); b := unbox(b) - 19; unbox(b)",
  );
  assertSnapshot("box_multiplication1", "let b = box(4); b := unbox(b) * 19");
  assertSnapshot(
    "box_multiplication2",
    "let b = box(4); b := unbox(b) * 19; unbox(b)",
  );
  assertSnapshot("box_division1", "let b = box(76); b := unbox(b) / 19");
  assertSnapshot(
    "box_division2",
    "let b = box(76); b := unbox(b) / 19; unbox(b)",
  );
});
