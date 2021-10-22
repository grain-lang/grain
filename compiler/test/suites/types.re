open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("aliased_types", ({test}) => {
  // let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);
  // let assertRunError = makeErrorRunner(test);

  assertRun(
    "type_alias_1",
    {|
      type Foo = List<Number>
      print([1, 2, 3] : Foo)
    |},
    "[1, 2, 3]\n",
  );
  assertRun(
    "type_alias_2",
    {|
      type Foo<a> = List<a>
      print([1, 2, 3] : Foo<Number>)
    |},
    "[1, 2, 3]\n",
  );
  assertRun(
    "type_alias_3",
    {|
      type Foo<a> = (String, List<a>)
      print(("foo", [1, 2, 3]) : Foo<Number>)
    |},
    "(\"foo\", [1, 2, 3])\n",
  );
  assertRun(
    "type_alias_4",
    {|
      type Foo<a> = (String, List<a>)
      let foo = ("foo", [1, 2, 3])
      let bar: Foo<Number> = foo
      print(bar)
    |},
    "(\"foo\", [1, 2, 3])\n",
  );
  assertRun(
    "type_alias_5",
    {|
      type Foo<a> = (String, List<a>)
      let foo = (x: Foo<Number>) => {
        x: (String, List<Number>)
      }
      print(foo(("foo", [1, 2, 3])))
    |},
    "(\"foo\", [1, 2, 3])\n",
  );
  assertRun(
    "type_alias_6",
    {|
      type Foo<a> = (String, List<a>)
      let foo = (x: (String, List<Number>)) => {
        x: Foo<Number>
      }
      print(foo(("foo", [1, 2, 3])))
    |},
    "(\"foo\", [1, 2, 3])\n",
  );
  assertRun(
    "type_alias_7",
    {|
      type Bar = Number
      type Foo = Bar
      let foo = (x: Foo) => {
        x: Bar
      }
      let x: Number = foo(1)
      print(x)
    |},
    "1\n",
  );
  assertCompileError(
    "err_type_alias_1",
    {|
      type Foo = List<String>
      [1, 2, 3] : Foo
    |},
    "Type Number is not compatible with type String",
  );
  assertCompileError(
    "err_type_alias_2",
    {|
      type Foo = List<String>
      let a: Foo = [1, 2, 3]
    |},
    "Type Number is not compatible with type String",
  );
  assertCompileError(
    "err_type_alias_3",
    {|
      type Foo<a> = List<a>
      let a: Foo<String> = [1, 2, 3]
    |},
    "Type Number is not compatible with type String",
  );
  assertCompileError(
    "err_type_alias_4",
    {|
      type Foo<a> = List<a>
      let mut a = [1, 2, 3]: Foo<Number>
      a = ["foo", "bar"]
    |},
    "Type String is not compatible with type Number",
  );
  assertRun(
    "import_type_alias_1",
    {|
      import * from "aliases"
      let foo = 123 : Foo
      print(foo)
    |},
    "123\n",
  );
});

describe("abstract_types", ({test}) => {
  let assertCompileError = makeCompileErrorRunner(test);
  // let assertRun = makeRunner(test);

  assertCompileError(
    "type_abstract_1",
    {|
      type Foo
      3 : Foo
    |},
    "expected of type
         Foo",
  );
});
