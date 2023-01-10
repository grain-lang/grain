open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("aliased types", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);

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
        (x): (String, List<Number>)
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
        (x): Foo<Number>
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
        (x): Bar
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
    "This expression has type Number but",
  );
  assertCompileError(
    "err_type_alias_2",
    {|
      type Foo = List<String>
      let a: Foo = [1, 2, 3]
    |},
    "This expression has type Number but",
  );
  assertCompileError(
    "err_type_alias_3",
    {|
      type Foo<a> = List<a>
      let a: Foo<String> = [1, 2, 3]
    |},
    "This expression has type Number but",
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
  assertCompileError(
    "err_type_alias_5",
    {|
      type Foo = List<a>
    |},
    "Unbound type parameter 'a'",
  );
  assertRun(
    "import_type_alias_1",
    {|
      import * from "aliases"
      let foo1 = 123 : Foo
      let foo2: Foo = 234
      print(foo1)
      print(foo2)
    |},
    "123\n234\n",
  );
  assertRun(
    "import_type_alias_2",
    {|
      import * from "aliases"
      let foo1 = [234] : (Bar<Foo>)
      let foo2: Bar<Number> = [123, ...foo1]
      print(foo2)
    |},
    "[123, 234]\n",
  );
  assertRun(
    "import_type_alias_3",
    {|
      import * from "aliases"
      let foo: Baz = baz
      print(foo: Baz)
    |},
    "5\n",
  );
  assertRun(
    "import_type_alias_4",
    {|
      import { Foo } from "aliases"
      let foo: Foo = 5
      print(foo)
    |},
    "5\n",
  );
  assertRun(
    "import_type_alias_5",
    {|
      import * from "aliases"
      let foo: Qux<Number> = qux
      print(foo: Qux<Foo>)
    |},
    "7\n",
  );
  assertCompileError(
    "err_import_type_alias_1",
    {|
      import * from "aliases"
      let bar = 5: Baz
    |},
    "expected of type
         %Aliases.Baz",
  );
  assertCompileError(
    "err_import_type_alias_2",
    {|
      import * from "aliases"
      let bar: Qux<Number> = 5
    |},
    "expected of type
         %Aliases.Qux<Number>",
  );
  assertCompileError(
    "err_import_type_alias_3",
    {|
      import { Foo, baz } from "aliases"
      let foo: Foo = baz
    |},
    "expression was expected of type %Aliases.Foo = Number",
  );
  assertRun(
    "regression_annotated_type_func_1",
    {|
      type AddPrinter = (Number, Number) -> Void
      export let add: AddPrinter = (x, y) => print(x + y)
      add(4, 4)
    |},
    "8\n",
  );
  assertRun(
    "regression_annotated_type_func_2",
    {|
      type AddPrinter<a> = (a, a) -> Void
      export let add: AddPrinter<Number> = (x, y) => print(x + y)
      add(4, 4)
    |},
    "8\n",
  );
});

describe("abstract types", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);

  assertCompileError(
    "type_abstract_1",
    {|
      type Foo
      3 : Foo
    |},
    "Syntax error",
    // TODO: This will be a type error when we support fully abstract types
    // "expected of type
    //      Foo",
  );

  assertRun(
    "regression_annotated_func_export",
    {|
      import A from "funcAliasExport"
      print(A.function())
    |},
    "abc\n",
  );
});
