open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("provides", ({test, testSkip}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertStartSectionSnapshot =
    makeSnapshotRunner(
      ~config_fn=() => {Grain_utils.Config.use_start_section := true},
      test,
    );
  let assertCompileError = makeCompileErrorRunner(test);
  let assertHasWasmExport = (name, prog, export) => {
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
          expect.list(exports).toContainEqual(export);
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
    "enum Foo { Bar }; provide { Bar }",
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
    "provide module Foo {void}; provide {Foo}",
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
    "module Test; provide let foo: () -> Number = () => 5",
    ("foo", Binaryen.Export.external_function),
  );
  assertHasWasmExport(
    "issue_918_annotated_func_provide2",
    "module Test; provide let rec foo: () -> Number = () => 5",
    ("foo", Binaryen.Export.external_function),
  );
});
