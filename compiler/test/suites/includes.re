open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("includes", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);
  let assertFileCompileError = makeFileCompileErrorRunner(test_or_skip);
  let assertFileSnapshot = makeSnapshotFileRunner(test);

  /* use * tests */
  assertRun(
    "include_all",
    "from \"provideAll\" include ProvideAll; use ProvideAll.*; {print(x); print(y(4)); print(z)}",
    "5\n4\nfoo\n",
  );
  assertSnapshot(
    "include_all_constructor",
    "from \"tlists\" include TLists; use TLists.*; Cons(2, Empty)",
  );
  /* use {} tests */
  assertSnapshot(
    "include_some",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{x}; x",
  );
  assertSnapshot(
    "include_some_multiple",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{x, y}; y(x)",
  );
  assertSnapshot(
    "include_some_multiple_trailing",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{x, y,}; y(x)",
  );
  assertSnapshot(
    "include_some_multiple_trailing2",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{
      x,
      y,
    }; y(x)",
  );
  assertSnapshot(
    "include_some_constructor",
    "from \"tlists\" include TLists; use TLists.{type TList}; Cons(5, Empty)",
  );
  assertSnapshot(
    "include_some_mixed",
    "from \"tlists\" include TLists; use TLists.{type TList, sum}; sum(Cons(5, Empty))",
  );
  assertSnapshot(
    "include_alias",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{x as y}; y",
  );
  assertSnapshot(
    "include_alias_multiple",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{x as y, y as x}; x(y)",
  );
  /* use {} errors */
  assertCompileError(
    "include_some_error",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{a}; a",
    "Unbound value a in module ProvideAll",
  );
  assertCompileError(
    "include_some_error2",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{x, a}; a",
    "Unbound value a in module ProvideAll",
  );
  assertCompileError(
    "include_some_error3",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{module Foo}",
    "Unbound module Foo in module ProvideAll",
  );
  assertCompileError(
    "include_some_error4",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{x, module Foo}",
    "Unbound module Foo in module ProvideAll",
  );
  assertCompileError(
    "include_some_error5",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{Foo}",
    "Expected a lowercase identifier to use a value, the keyword `module` followed by an uppercase identifier to use a module, or the keyword `type` followed by an uppercase identifier to use a type.",
  );
  assertCompileError(
    "include_some_error6",
    "from \"provideAll\" include ProvideAll; use ProvideAll.{a, Foo}",
    "Expected a lowercase identifier to use a value, the keyword `module` followed by an uppercase identifier to use a module, the keyword `type` followed by an uppercase identifier to use a type, or `}` to end the use statement.",
  );
  /* include module tests */
  assertSnapshot(
    "include_module",
    "from \"provideAll\" include ProvideAll as Foo; Foo.x",
  );
  assertSnapshot(
    "include_module2",
    "from \"provideAll\" include ProvideAll as Foo; Foo.y(Foo.x)",
  );
  /* include module errors */
  assertCompileError(
    "include_module_error",
    "from \"provideAll\" include ProvideAll as Foo; Foo.foo",
    "Unbound value foo in module Foo",
  );
  assertCompileError(
    "include_module_error",
    "from \"provideAll\" include Foo; Foo.foo",
    "This statement includes module Foo, but the file at the path defines module ProvideAll. Did you mean `include ProvideAll as Foo`?",
  );
  /* use well-formedness errors */
  assertCompileError(
    "include_alias_illegal_renaming",
    "from \"list\" include List; use List.{module Cons as cons, module Empty}; cons(3, Empty)",
    "Expected an uppercase module alias",
  );
  assertCompileError(
    "include_alias_illegal_renaming2",
    "from \"list\" include List; use List.{sum as Sum, module Empty}; sum(Empty)",
    "Expected a lowercase alias",
  );
  assertCompileError(
    "include_module_illegal_name",
    "from \"list\" include List as foo;",
    "Expected an uppercase module identifier",
  );
  assertCompileError(
    "include_module_not_external",
    "from \"list\" include List as Foo.Foo;",
    "A module alias cannot contain `.` as that would reference a binding within another module.",
  );
  assertCompileError(
    "include_value_not_external",
    "from \"list\" include List; use List.{foo as Foo.foo};",
    "Expected a lowercase alias",
  );
  /* include multiple modules tests */
  assertSnapshot(
    "include_muliple_modules",
    "from \"tlists\" include TLists; use TLists.*; from \"provideAll\" include ProvideAll; use ProvideAll.*; Cons(x, Empty)",
  );
  /* include same module tests */
  assertSnapshot(
    "include_same_module_unify",
    "from \"tlists\" include TLists; use TLists.*; Cons(5, TLists.Empty)",
  );
  /* include filepath tests */
  assertFileSnapshot("include_relative_path1", "relativeInclude1");
  assertFileSnapshot("include_relative_path2", "relativeInclude2");
  assertFileSnapshot("include_relative_path3", "relativeInclude3");
  assertFileSnapshot("include_relative_path4", "relativeIncludes/foo");
  assertCompileError(
    "include_missing_file",
    "from \"foo\" include Foo; 2",
    "Missing file for module \"foo.gr\"",
  );
  /* Unbound module tests */
  assertCompileError(
    "test_unbound_module",
    "String.concat(\"hello \", \"world\")",
    "Unbound module String",
  );
  /* Misc include tests */
  assertCompileError(
    "test_bad_import",
    "{let x = (1, 2); from \"tlists\" include TLists; x}",
    "error",
  );
  assertFileRun("test_file_same_name", "list", "OK\n");
  assertSnapshot(
    "annotation_across_import",
    "from \"tlists\" include TLists; use TLists.{ type TList }; let foo : TLists.TList<String> = Empty; foo",
  );
  assertFileRun(
    "relative_include_linking",
    "relativeIncludeLinking",
    "2\n2\n",
  );
  assertFileCompileError(
    "include_broken",
    "brokenIncludes/main",
    "./broken.gr\", line 4, characters 5-15",
  );
  assertCompileError(
    "include_extension1",
    "from \"list.gr\" include List",
    "Missing file for module \"list.gr\": did you mean \"list\"?",
  );
  assertCompileError(
    "include_extension2",
    "from \"brokenRelativeInclude\" include BrokenRelativeInclude",
    "Missing file for module \"./data\": did you mean \"./data.gr\"?",
  );
  assertRun(
    "reprovide_values",
    "from \"reprovideContents\" include ReprovideContents; use ReprovideContents.{ type Type, module Mod }; print(A); print(Mod.val)",
    "A\n123\n",
  );
  assertRun(
    "reprovide_type2",
    "from \"reprovideContents\" include ReprovideContents; use ReprovideContents.{ type OtherT as TT, val }; print(val); print({ x: 2 })",
    "{\n  x: 1\n}\n{\n  x: 2\n}\n",
  );
  assertRun(
    "reprovide_type3",
    "from \"reprovideContents\" include ReprovideContents; use ReprovideContents.{ type OtherT as Other }; print({ x: 1 }: Other)",
    "{\n  x: 1\n}\n",
  );
});
