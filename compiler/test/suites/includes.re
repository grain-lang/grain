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
    "include \"provideAll\" as ProvideAll; from ProvideAll use *; {print(x); print(y(4)); print(z)}",
    "5\n4\nfoo\n",
  );
  assertSnapshot(
    "include_all_constructor",
    "include \"tlists\" as TLists; from TLists use *; Cons(2, Empty)",
  );
  /* use {} tests */
  assertSnapshot(
    "include_some",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {x}; x",
  );
  assertSnapshot(
    "include_some_multiple",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {x, y}; y(x)",
  );
  assertSnapshot(
    "include_some_multiple_trailing",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {x, y,}; y(x)",
  );
  assertSnapshot(
    "include_some_multiple_trailing2",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {
      x,
      y,
    }; y(x)",
  );
  assertSnapshot(
    "include_some_constructor",
    "include \"tlists\" as TLists; from TLists use {type TList}; Cons(5, Empty)",
  );
  assertSnapshot(
    "include_some_mixed",
    "include \"tlists\" as TLists; from TLists use {type TList, sum}; sum(Cons(5, Empty))",
  );
  assertSnapshot(
    "include_alias",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {x as y}; y",
  );
  assertSnapshot(
    "include_alias_multiple",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {x as y, y as x}; x(y)",
  );
  /* use {} errors */
  assertCompileError(
    "include_some_error",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {a}; a",
    "Unbound value a in module ProvideAll",
  );
  assertCompileError(
    "include_some_error2",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {x, a}; a",
    "Unbound value a in module ProvideAll",
  );
  assertCompileError(
    "include_some_error3",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {module Foo}",
    "Unbound module Foo in module ProvideAll",
  );
  assertCompileError(
    "include_some_error4",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {x, module Foo}",
    "Unbound module Foo in module ProvideAll",
  );
  assertCompileError(
    "include_some_error5",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {Foo}",
    "Expected a lowercase identifier to use a value, the keyword `module` followed by an uppercase identifier to use a module, or the keyword `type` followed by an uppercase identifier to use a type.",
  );
  assertCompileError(
    "include_some_error6",
    "include \"provideAll\" as ProvideAll; from ProvideAll use {a, Foo}",
    "Expected a lowercase identifier to use a value, the keyword `module` followed by an uppercase identifier to use a module, the keyword `type` followed by an uppercase identifier to use a type, or `}` to end the use statement.",
  );
  /* include module tests */
  assertSnapshot("include_module", "include \"provideAll\" as Foo; Foo.x");
  assertSnapshot(
    "include_module2",
    "include \"provideAll\" as Foo; Foo.y(Foo.x)",
  );
  /* include module errors */
  assertCompileError(
    "include_module_error",
    "include \"provideAll\" as Foo; Foo.foo",
    "Unbound value foo in module Foo",
  );
  /* use well-formedness errors */
  assertCompileError(
    "include_alias_illegal_renaming",
    "include \"list\" as List; from List use {module Cons as cons, module Empty}; cons(3, Empty)",
    "Expected an uppercase module alias",
  );
  assertCompileError(
    "include_alias_illegal_renaming2",
    "include \"list\" as List; from List use {sum as Sum, module Empty}; sum(Empty)",
    "Expected a lowercase alias",
  );
  assertCompileError(
    "include_module_illegal_name",
    "include \"list\" as foo;",
    "Expected an uppercase module identifier",
  );
  assertCompileError(
    "include_module_not_external",
    "include \"list\" as Foo.Foo;",
    "A module alias cannot contain `.` as that would reference a binding within another module.",
  );
  assertCompileError(
    "include_value_not_external",
    "include \"list\" as List; from List use {foo as Foo.foo};",
    "Expected a lowercase alias",
  );
  /* include multiple modules tests */
  assertSnapshot(
    "include_muliple_modules",
    "include \"tlists\" as TLists; from TLists use *; include \"provideAll\" as ProvideAll; from ProvideAll use *; Cons(x, Empty)",
  );
  /* include same module tests */
  assertSnapshot(
    "include_same_module_unify",
    "include \"tlists\" as TLists; from TLists use *; Cons(5, TLists.Empty)",
  );
  /* include filepath tests */
  assertFileSnapshot("include_relative_path1", "relativeInclude1");
  assertFileSnapshot("include_relative_path2", "relativeInclude2");
  assertFileSnapshot("include_relative_path3", "relativeInclude3");
  assertFileSnapshot("include_relative_path4", "relativeIncludes/foo");
  assertCompileError(
    "include_missing_file",
    "include \"foo\" as Foo; 2",
    "Missing file for module foo",
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
    "{let x = (1, 2); include \"tlists\" as TLists; x}",
    "error",
  );
  assertFileRun("test_file_same_name", "list", "OK\n");
  assertSnapshot(
    "annotation_across_import",
    "include \"tlists\" as TLists; from TLists use { type TList }; let foo : TLists.TList<String> = Empty; foo",
  );
  assertFileRun(
    "relative_include_linking",
    "relativeIncludeLinking",
    "2\n2\n",
  );
  assertFileCompileError(
    "include_broken",
    "brokenIncludes/main",
    "./broken.gr\", line 4, characters 8-15",
  );
  assertRun(
    "reprovide_values",
    "include \"reprovideContents\"; from ReprovideContents use { type Type, module Mod }; print(A); print(Mod.val)",
    "A\n123\n",
  );
  assertRun(
    "reprovide_type2",
    "include \"reprovideContents\"; from ReprovideContents use { type OtherT as TT, val }; print(val); print({ x: 2 })",
    "{\n  x: 1\n}\n{\n  x: 2\n}\n",
  );
  assertRun(
    "reprovide_type3",
    "include \"reprovideContents\"; from ReprovideContents use { type OtherT as Other }; print({ x: 1 }: Other)",
    "{\n  x: 1\n}\n",
  );
});
