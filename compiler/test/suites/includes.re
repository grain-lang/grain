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
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.*; {print(x); print(y(4)); print(z)}",
    "5\n4\nfoo\n",
  );
  assertSnapshot(
    "include_all_constructor",
    "from \"test-libs/tlists\" include TLists; use TLists.*; Cons(2, Empty)",
  );
  /* use {} tests */
  assertSnapshot(
    "include_some",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{x}; x",
  );
  assertSnapshot(
    "include_some_multiple",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{x, y}; y(x)",
  );
  assertSnapshot(
    "include_some_multiple_trailing",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{x, y,}; y(x)",
  );
  assertSnapshot(
    "include_some_multiple_trailing2",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{
      x,
      y,
    }; y(x)",
  );
  assertSnapshot(
    "include_some_constructor",
    "from \"test-libs/tlists\" include TLists; use TLists.{type TList}; Cons(5, Empty)",
  );
  assertSnapshot(
    "include_some_mixed",
    "from \"test-libs/tlists\" include TLists; use TLists.{type TList, sum}; sum(Cons(5, Empty))",
  );
  assertSnapshot(
    "include_alias",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{x as y}; y",
  );
  assertSnapshot(
    "include_alias_multiple",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{x as y, y as x}; x(y)",
  );
  /* use {} errors */
  assertCompileError(
    "include_some_error",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{a}; a",
    "Unbound value a in module ProvideAll",
  );
  assertCompileError(
    "include_some_error2",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{x, a}; a",
    "Unbound value a in module ProvideAll",
  );
  assertCompileError(
    "include_some_error3",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{module Foo}",
    "Unbound module Foo in module ProvideAll",
  );
  assertCompileError(
    "include_some_error4",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{x, module Foo}",
    "Unbound module Foo in module ProvideAll",
  );
  assertCompileError(
    "include_some_error5",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{Foo}",
    "Expected a lowercase identifier to use a value, the keyword `module` followed by an uppercase identifier to use a module, or the keyword `type` followed by an uppercase identifier to use a type.",
  );
  assertCompileError(
    "include_some_error6",
    "from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.{a, Foo}",
    "Expected a lowercase identifier to use a value, the keyword `module` followed by an uppercase identifier to use a module, the keyword `type` followed by an uppercase identifier to use a type, or `}` to end the use statement.",
  );
  /* include module tests */
  assertSnapshot(
    "include_module",
    "from \"test-libs/provideAll\" include ProvideAll as Foo; Foo.x",
  );
  assertSnapshot(
    "include_module2",
    "from \"test-libs/provideAll\" include ProvideAll as Foo; Foo.y(Foo.x)",
  );
  /* include module errors */
  assertCompileError(
    "include_module_error",
    "from \"test-libs/provideAll\" include ProvideAll as Foo; Foo.foo",
    "Unbound value foo in module Foo",
  );
  assertCompileError(
    "include_module_error",
    {|from "test-libs/provideAll" include Foo; Foo.foo|},
    {|This statement includes module Foo, but the file at the path defines module ProvideAll. Did you mean `from "test-libs/provideAll" include ProvideAll as Foo`?|},
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
    "from \"test-libs/tlists\" include TLists; use TLists.*; from \"test-libs/provideAll\" include ProvideAll; use ProvideAll.*; Cons(x, Empty)",
  );
  /* include same module tests */
  assertSnapshot(
    "include_same_module_unify",
    "from \"test-libs/tlists\" include TLists; use TLists.*; Cons(5, TLists.Empty)",
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
    "{let x = (1, 2); from \"test-libs/tlists\" include TLists; x}",
    "error",
  );
  assertFileRun("test_file_same_name", "list", "OK\n");
  assertSnapshot(
    "annotation_across_import",
    "from \"test-libs/tlists\" include TLists; use TLists.{ type TList }; let foo : TLists.TList<String> = Empty; foo",
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
    "from \"test-libs/brokenRelativeInclude\" include BrokenRelativeInclude",
    "Missing file for module \"./data\": did you mean \"./data.gr\"?",
  );
  assertRun(
    "reprovide_values",
    "from \"test-libs/reprovideContents\" include ReprovideContents; use ReprovideContents.{ type Type, module Mod }; print(A); print(Mod.val)",
    "A\n123\n",
  );
  assertRun(
    "reprovide_type2",
    "from \"test-libs/reprovideContents\" include ReprovideContents; use ReprovideContents.{ type OtherT as TT, val }; print(val); print({ x: 2 })",
    "{\n  x: 1\n}\n{\n  x: 2\n}\n",
  );
  assertRun(
    "reprovide_type3",
    "from \"test-libs/reprovideContents\" include ReprovideContents; use ReprovideContents.{ type OtherT as Other }; print({ x: 1 }: Other)",
    "{\n  x: 1\n}\n",
  );
  test("only_include_dirs", ({expect}) => {
    let prog = {|
      module OnlyIncludeDirs
      from "test-libs/reprovideException" include ReprovideException
      void
    |};
    ignore @@ compile("setup_artifacts", prog);

    let deps_dir = Fp.At.(test_target_dir / "debug" / "deps");
    ignore @@
    compile(
      ~link=true,
      ~config_fn=
        () => {
          Grain_utils.Config.libraries := [];
          Grain_utils.Config.include_dirs := [deps_dir];
          assert(
            List.assoc_opt("test-libs", Grain_utils.Config.libraries^) == None,
          );
        },
      "only_include_dirs",
      prog,
    );
    let (result, _) = run(wasmfile("only_include_dirs"));
    expect.string(result).toEqual("");
  });

  /* Duplicate imports */
  test("dedupe_includes", ({expect}) => {
    let name = "dedupe_includes";
    let outfile = wasmfile(name);
    ignore @@
    compile(
      ~link=true,
      name,
      {|
      module DeDupeIncludes
      // Ensures test is only included once
      foreign wasm test: WasmI32 => WasmI32 from "env"
      let test2 = test
      foreign wasm test: WasmI32 => WasmI32 from "env"
      @unsafe
      let _ = {
        test(1n)
        test2(1n)
      }
      |},
    );
    let ic = open_in_bin(outfile);
    let sections = Grain_utils.Wasm_utils.get_wasm_sections(ic);
    close_in(ic);
    let import_section =
      List.find_map(
        (sec: Grain_utils.Wasm_utils.wasm_bin_section) =>
          switch (sec) {
          | {sec_type: Import(imports)} => Some(imports)
          | _ => None
          },
        sections,
      );
    expect.option(import_section).toBeSome();
    expect.int(List.length(Option.get(import_section))).toBe(2);
    // Runtime printing import
    expect.list(Option.get(import_section)).toContainEqual((
      WasmFunction,
      "wasi_snapshot_preview1",
      "fd_write",
    ));
    // Test import
    expect.list(Option.get(import_section)).toContainEqual((
      WasmFunction,
      "env",
      "test",
    ));
  });

  test("stdlib_preceeds_library", ({expect}) => {
    let library_dir = Fp.At.(test_libs_dir / "runtime");
    ignore @@
    compile(
      ~config_fn=
        () => {
          Grain_utils.Config.libraries :=
            [("runtime", library_dir), ...Grain_utils.Config.libraries^]
        },
      "stdlib_preceeds_library",
      {|
        module StdlibPreceedsLibrary
        from "runtime/string" include String
        void
      |},
    );
    let obj_path =
      Grain_typed.Module_resolution.locate_unit_object_file(
        ~base_dir=Grain_utils.Filepath.to_string(Fp.At.(test_input_dir)),
        "runtime/string",
      );
    expect.string(obj_path).toMatch("deps/stdlib/");
  });
});
