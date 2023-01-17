open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("imports", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);
  let assertFileCompileError = makeFileCompileErrorRunner(test_or_skip);
  let assertFileSnapshot = makeSnapshotFileRunner(test);

  /* import * tests */
  assertRun(
    "import_all",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use *; {print(x); print(y(4)); print(z)}",
    "5\n4\nfoo\n",
  );
  assertSnapshot(
    "import_all_constructor",
    "include \"tlists\" as TLists; from TLists use *; Cons(2, Empty)",
  );
  assertSnapshot(
    "import_with_export_multiple",
    "include \"sameExport\" as SameExport; from SameExport use *; foo()",
  );
  /* import {} tests */
  assertSnapshot(
    "import_some",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {x}; x",
  );
  assertSnapshot(
    "import_some_multiple",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {x, y}; y(x)",
  );
  assertSnapshot(
    "import_some_multiple_trailing",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {x, y,}; y(x)",
  );
  assertSnapshot(
    "import_some_multiple_trailing2",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {
      x,
      y,
    }; y(x)",
  );
  assertSnapshot(
    "import_some_constructor",
    "include \"tlists\" as TLists; from TLists use {type TList}; Cons(5, Empty)",
  );
  assertSnapshot(
    "import_some_mixed",
    "include \"tlists\" as TLists; from TLists use {type TList, sum}; sum(Cons(5, Empty))",
  );
  assertSnapshot(
    "import_alias",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {x as y}; y",
  );
  assertSnapshot(
    "import_alias_multiple",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {x as y, y as x}; x(y)",
  );
  /* import {} errors */
  assertCompileError(
    "import_some_error",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {a}; a",
    "Unbound value a in module ExposeStar",
  );
  assertCompileError(
    "import_some_error2",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {x, a}; a",
    "Unbound value a in module ExposeStar",
  );
  assertCompileError(
    "import_some_error3",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {Foo}; a",
    "Unbound module Foo in module ExposeStar",
  );
  assertCompileError(
    "import_some_error3",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use {x, Foo}; a",
    "Unbound module Foo in module ExposeStar",
  );
  /* import module tests */
  assertSnapshot("import_module", "include \"exposeStar\" as Foo; Foo.x");
  assertSnapshot(
    "import_module2",
    "include \"exposeStar\" as Foo; Foo.y(Foo.x)",
  );
  /* import module errors */
  assertCompileError(
    "import_module_error",
    "include \"exposeStar\" as Foo; Foo.foo",
    "Unbound value foo in module Foo",
  );
  /* import well-formedness errors */
  assertCompileError(
    "import_alias_illegal_renaming",
    "include \"list\" as List; from List use {Cons as cons, Empty}; cons(3, Empty)",
    "Expected an uppercase module alias",
  );
  assertCompileError(
    "import_alias_illegal_renaming2",
    "include \"list\" as List; from List use {sum as Sum, Empty}; sum(Empty)",
    "Expected a lowercase alias",
  );
  assertCompileError(
    "import_module_illegal_name",
    "include \"list\" as foo;",
    "Expected an uppercase module identifier",
  );
  assertCompileError(
    "import_module_not_external",
    "include \"list\" as Foo.Foo;",
    "Expected a newline character to terminate the statement",
  );
  assertCompileError(
    "import_value_not_external",
    "include \"list\" as List; from List use {foo as Foo.foo};",
    "Expected a lowercase alias",
  );
  /* import multiple modules tests */
  assertSnapshot(
    "import_muliple_modules",
    "include \"tlists\" as TLists; from TLists use *; include \"exposeStar\" as ExposeStar; from ExposeStar use *; Cons(x, Empty)",
  );
  /* import same module tests */
  assertSnapshot(
    "import_same_module_unify",
    "include \"tlists\" as TLists; from TLists use *; Cons(5, TLists.Empty)",
  );
  /* import filepath tests */
  assertFileSnapshot("import_relative_path1", "relativeImport1");
  assertFileSnapshot("import_relative_path2", "relativeImport2");
  assertFileSnapshot("import_relative_path3", "relativeImport3");
  assertFileSnapshot("import_relative_path4", "relativeImports/foo");
  assertCompileError(
    "import_missing_file",
    "include \"foo\" as Foo; 2",
    "Missing file for module foo",
  );
  /* Unbound module tests */
  assertCompileError(
    "test_unbound_module",
    "String.concat(\"hello \", \"world\")",
    "Unbound module String",
  );
  /* Misc import tests */
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
  assertFileRun("relative_import_linking", "relativeImportLinking", "2\n2\n");
  assertFileCompileError(
    "import_broken",
    "brokenImports/main",
    "./broken.gr\", line 4, characters 8-15",
  );
});
