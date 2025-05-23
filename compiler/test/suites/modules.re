open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_codegen;
open Grain_typed;

describe("modules", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);

  assertSnapshot(
    "smallest_submodule",
    {|
      module Foo {
        void
      }
    |},
  );
  assertRun(
    "module_side_effect",
    {|
      module Foo {
        print("foo")
      }
    |},
    "foo\n",
  );
  assertRun(
    "module_provide_value",
    {|
      module Foo {
        provide let foo = "foo"
      }
      print(Foo.foo)
    |},
    "foo\n",
  );
  assertRun(
    "module_provide_function",
    {|
      module Foo {
        provide let foo = () => "foo"
      }
      print(Foo.foo())
    |},
    "foo\n",
  );
  assertRun(
    "module_provide_type",
    {|
      module Foo {
        provide enum Foo { Foo }
      }
      print(Foo.Foo)
    |},
    "Foo\n",
  );
  assertRun(
    "module_provide_module",
    {|
      module Foo {
        provide module Foo {
          provide let foo = () => "foo"
        }
      }
      print(Foo.Foo.foo())
    |},
    "foo\n",
  );
  assertRun(
    "module_shadow",
    {|
      module Foo {
        provide let foo = "foo"
      }
      module Foo {
        provide let foo = "bar"
      }
      print(Foo.foo)
    |},
    "bar\n",
  );
  assertRun(
    "local_module_use",
    {|
      module Foo {
        provide let foo = "foo"
        provide enum Foo { Foo }
        provide module Foo {
          provide let foo = "foo2"
        }
      }
      use Foo.{ foo, type Foo, module Foo }
      print(foo)
      print(Foo.foo)
      print(Foo)
    |},
    "foo\nfoo2\nFoo\n",
  );
  assertCompileError(
    "local_module_include",
    {|
      module Foo {
        from "list" include List
      }
    |},
    "`include` statements may only appear at the file level",
  );
  assertRun(
    "local_module_scoping",
    {|
      let foo = "foo"

      module Foo {
        provide let foo = foo
      }

      print(Foo.foo)
    |},
    "foo\n",
  );
  assertFileRun(
    "nested_and_reprovided_modules",
    "nestedModules",
    "hello from foo\nhello from bar\n[2, 3, 4]\n9\n[> 2, 3, 4]\nfalse\nfoo\n",
  );
  assertSnapshot(
    "reprovided_module",
    {|
      from "simpleModule" include Simple
      provide { module Simple }
    |},
  );
});
