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
      from "aliases" include Aliases
      use Aliases.*
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
      from "aliases" include Aliases
      use Aliases.*
      let foo1 = [234] : (Bar<Foo>)
      let foo2: Bar<Number> = [123, ...foo1]
      print(foo2)
    |},
    "[123, 234]\n",
  );
  assertRun(
    "import_type_alias_3",
    {|
      from "aliases" include Aliases
      use Aliases.*
      let foo: Baz = baz
      print(foo: Baz)
    |},
    "5\n",
  );
  assertRun(
    "import_type_alias_4",
    {|
      from "aliases" include Aliases
      use Aliases.{ type Foo }
      let foo: Foo = 5
      print(foo)
    |},
    "5\n",
  );
  assertRun(
    "import_type_alias_5",
    {|
      from "aliases" include Aliases
      use Aliases.*
      let foo: Qux<Number> = qux
      print(foo: Qux<Foo>)
    |},
    "7\n",
  );
  assertCompileError(
    "err_import_type_alias_1",
    {|
      from "aliases" include Aliases
      use Aliases.*
      let bar = 5: Baz
    |},
    "expected of type
         Aliases.Baz",
  );
  assertCompileError(
    "err_import_type_alias_2",
    {|
      from "aliases" include Aliases
      use Aliases.*
      let bar: Qux<Number> = 5
    |},
    "expected of type
         Aliases.Qux<Number>",
  );
  assertCompileError(
    "err_import_type_alias_3",
    {|
      from "aliases" include Aliases
      use Aliases.{ type Foo, baz }
      let foo: Foo = baz
    |},
    "expression was expected of type Aliases.Foo = Number",
  );
  assertRun(
    "disambiguation_enum_1",
    {|
      enum A { V }
      enum B { V }
      let f = x => match (x) { V: A => print(true) }
      f(V)
    |},
    "true\n",
  );
  assertRun(
    "disambiguation_record_1",
    {|
      record A { v: Number }
      record B { v: Number }
      let f = x => match (x) { { v }: A => print(v) }
      f({v: 5})
    |},
    "5\n",
  );
  assertRun(
    "regression_annotated_type_func_1",
    {|
      abstract type AddPrinter = (Number, Number) => Void
      provide let add: AddPrinter = (x, y) => print(x + y)
      add(4, 4)
    |},
    "8\n",
  );
  assertRun(
    "regression_annotated_type_func_2",
    {|
      abstract type AddPrinter<a> = (a, a) => Void
      provide let add: AddPrinter<Number> = (x, y) => print(x + y)
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

  assertCompileError(
    "type_provided_1",
    {|
      type Foo = Number
      provide let three: Foo = 3
    |},
    "value is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_2",
    {|
      enum Foo { Foo }
      provide let foo = Foo
    |},
    "value is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_3",
    {|
      record Foo { foo: String }
      provide let foo = { foo: "" }
    |},
    "value is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_4",
    {|
      type Foo = Number
      provide type Bar = Foo
    |},
    "type is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_5",
    {|
      type Foo = Number
      provide enum Bar { Bar(Foo) }
    |},
    "enum is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_6",
    {|
      type Foo = Number
      provide record Bar { bar: Foo }
    |},
    "record is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_7",
    {|
      type Foo = Number
      provide module Nested {
        provide let foo: Foo = 5
      }
    |},
    "module is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_8",
    {|
      module Nested {
        type Foo = Number
        provide let three: Foo = 3
      }
    |},
    "value is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_9",
    {|
      module Nested {
        enum Foo { Foo }
        provide let foo = Foo
      }
    |},
    "value is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_10",
    {|
      module Nested {
        record Foo { foo: String }
        provide let foo = { foo: "" }
      }
    |},
    "value is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_11",
    {|
      module Nested {
        type Foo = Number
        provide type Bar = Foo
      }
    |},
    "type is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_12",
    {|
      module Nested {
        type Foo = Number
        provide enum Bar { Bar(Foo) }
      }
    |},
    "enum is provided but contains type Foo",
  );
  assertCompileError(
    "type_provided_13",
    {|
      module Nested {
        type Foo = Number
        provide record Bar { bar: Foo }
      }
    |},
    "record is provided but contains type Foo",
  );

  assertRun(
    "regression_annotated_func_export",
    {|
      from "funcAliasProvide" include FuncAliasProvide as A
      print(A.function())
    |},
    "abc\n",
  );
});

describe("recursive types", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);

  assertCompileError(
    "type_rec_incorrect_1",
    {|
      record Oops {
        x: Oops
      }
    |},
    "Unbound type constructor Oops. Are you missing the `rec` keyword on this type?",
  );
  assertCompileError(
    "type_rec_incorrect_2",
    {|
      record T {
        x: Number
      }
      and enum T2 {
        T2(Number)
      }
    |},
    "Mutually recursive type groups must include `rec` on the first type in the group.",
  );
  assertCompileError(
    "type_rec_incorrect_3",
    {|
      record rec T {
        x: Number
      }
      and enum rec T2 {
        T2(Number)
      }
    |},
    "The `rec` keyword should only appear on the first type in the mutually recursive type group.",
  );
  assertRun(
    "type_rec_correct_1",
    {|
      record rec T {
        x: T
      }
      and enum T2 {
        T2(T2),
        Val(Number)
      }
      print(T2(T2(Val(1))))
    |},
    "T2(T2(Val(1)))\n",
  );
  assertRun(
    "type_rec_correct_2",
    {|
      record T {
        x: Number
      }
      type T = T
      print({ x: 1 }: T)
    |},
    "{\n  x: 1\n}\n",
  );
});

describe("function types", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);

  assertRun(
    "type_fn_1",
    {|
      type Foo = Number => String
      let f: Foo = num => "a"
      print(f(1))
    |},
    "a\n",
  );
  assertRun(
    "type_fn_2",
    {|
      type Foo = (Number, Number) => Number
      let f: Foo = (a, b) => a + b
      print(f(1, 2))
    |},
    "3\n",
  );
  assertRun(
    "type_fn_3",
    {|
      type Foo<a> = a => a
      let f: Foo<Number> = a => a
      print(f(1))
    |},
    "1\n",
  );
  assertRun(
    "type_fn_4",
    {|
      from "map" include Map
      type Foo = Map.Map<List<Number>, Map.Map<Number, Number>> => Map.Map<Number, Number>
      let f: Foo = a => Map.make()
      print(f(Map.make()) == Map.make())
    |},
    "true\n",
  );
  assertRun(
    "type_fn_5",
    {|
      from "map" include Map
      type Foo = Map.Map<Number, Number> => Map.Map<Number, Number> => Number
      let f: Foo = a => b => 1
      print(f(Map.make())(Map.make()))
    |},
    "1\n",
  );
  assertRun(
    "type_fn_6",
    {|
      let f: List<Number> => List<Number> => Number = a => b => 1
      print(f([])([]))
    |},
    "1\n",
  );
  assertCompileError(
    "type_fn_6",
    {|
      let badIdentity: x => x
      print(badIdentity(1))
    |},
    "Syntax error after 'x' and before ' '.\nExpected a type annotation or `=`.",
  );
  assertCompileError(
    "type_fn_7",
    {|
      let badFn: Number =>
    |},
    "Syntax error after ' ' and before ''.\nExpected a type for the result of the function type.",
  );
  assertCompileError(
    "type_fn_8",
    {|
      let badFn: Number =>
      print(badFn(1))
    |},
    "Syntax error after 'print' and before '\\('.\nExpected a type annotation or `=`.",
  );
  assertCompileError(
    "type_fn_9",
    {|
      let badFn: (Number => )
      print(badFn(1))
    |},
    "Syntax error after '=>' and before '\\)'.\nExpected a type for the result of the function type.",
  );
});
