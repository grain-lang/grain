open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("types", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);
  let assertRunError = makeErrorRunner(test);

  assertRun("type_alias_1", "type Foo = List<Number>; print([1, 2, 3] : Foo)", "[1, 2, 3]\n");
  assertCompileError(
    "type_alias_2",
    "type Foo = List<String>; [1, 2, 3] : Foo",
    "Type Number is not compatible with type String",
  )
  assertRun(
    "type_alias_3",
    "type Foo<a> = (String, List<a>); print((\"foo\", [1, 2, 3]) : Foo<Number>)",
    "(\"foo\", [1, 2, 3])\n",
  )
  // Fully abstract types
  assertCompileError("type_abstract_1", "type Foo; 3 : Foo", "expected of type
         Foo")
});
