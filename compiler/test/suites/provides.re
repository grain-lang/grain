open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("provides", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertStartSectionSnapshot =
    makeSnapshotRunner(
      ~config_fn=() => {Grain_utils.Config.use_start_section := true},
      test,
    );
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertRunError = makeErrorRunner(test_or_skip);
  let assertHasWasmExport = (name, prog, expectedExports) => {
    test(
      name,
      ({expect}) => {
        let state =
          compile(
            ~hook=Grain.Compile.stop_after_object_file_emitted,
            name,
            prog,
          );
        ();
        switch (state.Grain.Compile.cstate_desc) {
        | ObjectFileEmitted({asm}) =>
          let num_exports = Binaryen.Export.get_num_exports(asm);
          let exports =
            List.init(
              num_exports,
              i => {
                let export = Binaryen.Export.get_export_by_index(asm, i);
                (
                  Binaryen.Export.get_name(export),
                  Binaryen.Export.export_get_kind(export),
                );
              },
            );
          List.iter(expect.list(exports).toContainEqual, expectedExports);
        | _ => assert(false)
        };
      },
    );
  };

  assertCompileError(
    "provide1",
    "include \"noProvides\" as NoProvides; from NoProvides use *; x",
    "Unbound value x",
  );
  assertCompileError(
    "provide2",
    "include \"noProvides\" as NoProvides; from NoProvides use *; y",
    "Unbound value y",
  );
  assertCompileError(
    "provide3",
    "include \"noProvides\" as NoProvides; from NoProvides use *; z",
    "Unbound value z",
  );
  assertSnapshot(
    "provide4",
    "include \"onlyXProvided\" as OnlyXProvided; from OnlyXProvided use *; x",
  );
  assertCompileError(
    "provide5",
    "include \"onlyXProvided\" as OnlyXProvided; from OnlyXProvided use *; y",
    "Unbound value y",
  );
  assertCompileError(
    "provide6",
    "include \"onlyXProvided\" as OnlyXProvided; from OnlyXProvided use *; z",
    "Unbound value z",
  );
  assertSnapshot(
    "provide7",
    "include \"provideAll\" as ProvideAll; from ProvideAll use *; x",
  );
  assertSnapshot(
    "provide8",
    "include \"provideAll\" as ProvideAll; from ProvideAll use *; x + y(4)",
  );
  assertSnapshot(
    "provide9",
    "include \"provideAll\" as ProvideAll; from ProvideAll use *; y(z)",
  );
  assertCompileError(
    "provide10",
    "include \"provideAll\" as ProvideAll; from ProvideAll use *; y(secret)",
    "Unbound value secret",
  );
  assertCompileError(
    "provide11",
    "enum Foo { Bar }; provide { module Bar }",
    "Unbound module Bar",
  );
  assertSnapshot(
    "provide12",
    {|
      include "providedType"
      ProvidedType.apply((arg) => print("ok"))
    |},
  );
  assertCompileError(
    "provide13",
    "module Nested { let val = 1 }; provide { Nested }",
    "Expected a lowercase identifier to provide a value, the keyword `module` followed by an uppercase identifier to provide a module, or the keyword `type` followed by an uppercase identifier to provide a type.",
  );
  assertCompileError(
    "provide13",
    "let a = 1; module Nested { let val = 1 }; provide { a, Nested }",
    "Expected a lowercase identifier to provide a value, the keyword `module` followed by an uppercase identifier to provide a module, the keyword `type` followed by an uppercase identifier to provide a type, or `}` to end the provide statement.",
  );
  assertCompileError(
    "regression_issue_1489",
    "provide { foo }",
    "Unbound value foo",
  );
  assertCompileError(
    "multiple_provides_1",
    "provide let foo = 1; provide let foo = 2",
    "provided multiple times",
  );
  assertCompileError(
    "multiple_provides_2",
    "provide let foo = 1; provide {foo}",
    "provided multiple times",
  );
  assertCompileError(
    "multiple_provides_3",
    "provide enum Foo {Foo}; provide enum Foo {Foo}",
    "provided multiple times",
  );
  assertCompileError(
    "multiple_provides_4",
    "provide enum Foo {Foo}; provide {type Foo}",
    "provided multiple times",
  );
  assertCompileError(
    "multiple_provides_5",
    "provide module Foo {void}; provide module Foo {void}",
    "provided multiple times",
  );
  assertCompileError(
    "multiple_provides_6",
    "provide module Foo {void}; provide {module Foo}",
    "provided multiple times",
  );
  assertCompileError(
    "multiple_provides_7",
    "let foo = 1; provide {foo, foo}",
    "provided multiple times",
  );
  assertSnapshot(
    "multiple_provides_8",
    "let foo = 1; provide {foo, foo as bar}",
  );
  assertCompileError(
    "multiple_provides_9",
    "let foo = 1; let bar = 2; provide {foo, foo as bar, bar as foo}",
    "provided multiple times",
  );
  assertRunError(
    "provide_exceptions1",
    "include \"provideException\"; let f = () => if (true) { throw ProvideException.MyException }; f()",
    "OriginalException",
  );
  assertRunError(
    "provide_exceptions2",
    "include \"reprovideException\"; from ReprovideException use *; let f = () => if (true) { throw MyException }; f()",
    "OriginalException",
  );
  assertRunError(
    "provide_exceptions3",
    "include \"reprovideException\"; from ReprovideException use { exception MyException as E }; let f = () => if (true) { throw E }; f()",
    "OriginalException",
  );
  assertRun(
    "provide_exceptions4",
    {|
      include "reprovideException"
      from ReprovideException use { exception MyException, excVal1, excVal2 }
      match (excVal1) {
        MyException => print("good1"),
        _ => assert false,
      }
      match (excVal2) {
        MyException => print("good2"),
        _ => assert false,
      }
      match (MyException) {
        ReprovideException.MyException => print("good3"),
        _ => assert false,
      }
    |},
    "good1\ngood2\ngood3\n",
  );

  assertSnapshot("let_rec_provide", "provide let rec foo = () => 5");

  assertStartSectionSnapshot(
    "provide_start_function",
    {|
      print("init")
      provide let _start = () => {
        print("starting up")
      }
    |},
  );

  assertHasWasmExport(
    "issue_918_annotated_func_provide",
    "module Test; provide let foo: () => Number = () => 5",
    [("foo", Binaryen.Export.external_function)],
  );
  assertHasWasmExport(
    "issue_918_annotated_func_provide2",
    "module Test; provide let rec foo: () => Number = () => 5",
    [("foo", Binaryen.Export.external_function)],
  );
  assertHasWasmExport(
    "issue_1872_reprovide_from_submodule",
    "module Test; module M { provide let x = 1; provide let y = 2 }; from M use *; provide { x, y }",
    [
      ("GRAIN$EXPORT$x", Binaryen.Export.external_global),
      ("GRAIN$EXPORT$y", Binaryen.Export.external_global),
    ],
  );
  assertHasWasmExport(
    "issue_1884_type_provided_later1",
    "module Test; enum T { A }; let a = A; provide { type T }; provide { a }",
    [("GRAIN$EXPORT$a", Binaryen.Export.external_global)],
  );
  assertHasWasmExport(
    "issue_1884_type_provided_later2",
    "module Test; enum T { A }; let a = A; provide { a, type T }",
    [("GRAIN$EXPORT$a", Binaryen.Export.external_global)],
  );
  assertHasWasmExport(
    "issue_1884_type_provided_later3",
    "module Test; enum T { A }; let a = A; provide { a }; provide { type T }",
    [("GRAIN$EXPORT$a", Binaryen.Export.external_global)],
  );
  assertHasWasmExport(
    "issue_1884_type_provided_later4",
    "module Test; enum T { A }; provide let a = A; provide { type T }",
    [("GRAIN$EXPORT$a", Binaryen.Export.external_global)],
  );
});
