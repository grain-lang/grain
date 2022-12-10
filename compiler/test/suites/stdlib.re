open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("stdlib", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);
  let assertStdlib = makeStdlibRunner(test_or_skip);

  assertSnapshot("stdlib_cons", "[1, 2, 3]");
  assertSnapshot(
    "stdlib_equal_1",
    "import * from \"list\"; (1, 2) is (1, 2)",
  );
  assertSnapshot(
    "stdlib_equal_2",
    "import * from \"pervasives\"; (1, 2) == (1, 2)",
  );
  assertSnapshot(
    "stdlib_equal_3",
    "import * from \"list\"; [1, 2, 3] == [1, 2, 3]",
  );
  assertSnapshot("stdlib_equal_4", "import * from \"list\"; 1 == 1");
  assertSnapshot("stdlib_equal_5", "import * from \"list\"; 1 == 2");
  assertSnapshot("stdlib_equal_6", "import * from \"list\"; true == true");
  assertSnapshot("stdlib_equal_7", "import * from \"list\"; true == false");
  assertSnapshot("stdlib_equal_8", "import * from \"list\"; [>] == [>]");
  assertSnapshot("stdlib_equal_9", "import * from \"list\"; [>] == [> 1]");
  assertSnapshot("stdlib_equal_10", "import * from \"list\"; [> 1] == [> 1]");
  assertSnapshot(
    "stdlib_equal_11",
    "import * from \"list\"; [> 1, 2] == [> 1]",
  );
  assertSnapshot(
    "stdlib_equal_12",
    "import * from \"list\"; [> 1, 2, 3, 4] == [> 1, 2, 3, 4]",
  );
  assertSnapshot("stdlib_equal_13", "import * from \"list\"; \"\" == \"\"");
  assertSnapshot("stdlib_equal_14", "import * from \"list\"; \" \" == \"\"");
  assertSnapshot("stdlib_equal_15", "import * from \"list\"; \"f\" == \"\"");
  assertSnapshot(
    "stdlib_equal_16",
    "import * from \"list\"; \"foo\" == \"foo\"",
  );
  assertSnapshot(
    "stdlib_equal_17",
    "import * from \"list\"; \"foo 😂\" == \"foo 😂\"",
  );
  assertSnapshot(
    "stdlib_equal_18",
    "import * from \"list\"; \"foo 😂\" == \"foo 🙄\"",
  );
  assertSnapshot(
    "stdlib_equal_19",
    "record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true} == {foo: 4, bar: \"boo\", baz: true}",
  );
  assertSnapshot(
    "stdlib_equal_20",
    "record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true} == {foo: 4, bar: \"bar\", baz: true}",
  );
  assertSnapshot(
    "stdlib_equal_21",
    "record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true} == {foo: 78, bar: \"boo\", baz: true}",
  );
  assertSnapshot(
    "stdlib_equal_22",
    "record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true} == {foo: 4, bar: \"boo\", baz: false}",
  );
  assertFileRun("recursive_equal_box", "recursive-equal-box", "OK\n");
  assertFileRun("recursive_equal_mut", "recursive-equal-mut", "OK\n");
  assertCompileError(
    "stdlib_length_err",
    "import * from \"list\"; length(true)",
    "This expression has type Bool but",
  );
  assertCompileError(
    "stdlib_reverse_err",
    "import * from \"list\"; reverse(1)",
    "This expression has type Number but",
  );
  // logging to the stdout file descriptor
  assertRun(
    "stdlib_file_stdout",
    {|import File from "sys/file"; ignore(File.fdWrite(File.stdout, "enterthe")); print(void)|},
    "enterthevoid\n",
  );
  assertStdlib("array.test");
  assertStdlib("immutablearray.test");
  assertStdlib("bigint.test");
  assertStdlib("bytes.test");
  assertStdlib("buffer.test");
  assertStdlib("char.test");
  assertStdlib("float32.test");
  assertStdlib("float64.test");
  assertStdlib("hash.test");
  assertStdlib("int32.test");
  assertStdlib("int64.test");
  assertStdlib("list.test");
  assertStdlib("map.test");
  assertStdlib("immutablemap.test");
  assertStdlib("marshal.test");
  assertStdlib("number.test");
  assertStdlib("option.test");
  assertStdlib("path.test");
  assertStdlib("pervasives.test");
  assertStdlib("queue.test");
  assertStdlib("range.test");
  assertStdlib("result.test");
  assertStdlib("set.test");
  assertStdlib("immutableset.test");
  assertStdlib("regex.test");
  assertStdlib("stack.test");
  assertStdlib("priorityqueue.test");
  assertStdlib("immutablepriorityqueue.test");
  assertStdlib("string.test");
  assertStdlib("sys.file.test");
  assertStdlib(~code=5, "sys.process.test");
  assertStdlib("sys.random.test");
  assertStdlib("sys.time.test");
  assertStdlib("wasmf32.test");
  assertStdlib("wasmf64.test");
  assertStdlib("wasmi32.test");
  assertStdlib("wasmi64.test");
});
