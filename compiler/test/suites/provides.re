open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_typed;
open Grain_codegen;

describe("provides", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);
  let assertRunError = makeErrorRunner(test_or_skip);
  let assertHasWasmExport =
      (~use_start_section=false, name, prog, expectedExports) => {
    test(
      name,
      ({expect}) => {
        ignore(
          compile(
            ~link=true,
            ~config_fn=
              () => {
                Grain_utils.Config.use_start_section := use_start_section
              },
            name,
            prog,
          ),
        );
        let bytes = {
          let ic = open_in_bin(wasmfile(name));
          let chan_len = in_channel_length(ic);
          let bytes = Bytes.create(chan_len);
          really_input(ic, bytes, 0, chan_len);
          close_in(ic);
          bytes;
        };
        let asm = Binaryen.Module.read(bytes);
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
      },
    );
  };

  assertCompileError(
    "provide1",
    "from \"noProvides\" include NoProvides; use NoProvides.*; x",
    "Unbound value x",
  );
  assertCompileError(
    "provide2",
    "from \"noProvides\" include NoProvides; use NoProvides.*; y",
    "Unbound value y",
  );
  assertCompileError(
    "provide3",
    "from \"noProvides\" include NoProvides; use NoProvides.*; z",
    "Unbound value z",
  );
  assertSnapshot(
    "provide4",
    "from \"onlyXProvided\" include OnlyXProvided; use OnlyXProvided.*; x",
  );
  assertCompileError(
    "provide5",
    "from \"onlyXProvided\" include OnlyXProvided; use OnlyXProvided.*; y",
    "Unbound value y",
  );
  assertCompileError(
    "provide6",
    "from \"onlyXProvided\" include OnlyXProvided; use OnlyXProvided.*; z",
    "Unbound value z",
  );
  assertSnapshot(
    "provide7",
    "from \"provideAll\" include ProvideAll; use ProvideAll.*; x",
  );
  assertSnapshot(
    "provide8",
    "from \"provideAll\" include ProvideAll; use ProvideAll.*; x + y(4)",
  );
  assertSnapshot(
    "provide9",
    "from \"provideAll\" include ProvideAll; use ProvideAll.*; y(z)",
  );
  assertCompileError(
    "provide10",
    "from \"provideAll\" include ProvideAll; use ProvideAll.*; y(secret)",
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
      from "providedType" include ProvidedType
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
    "from \"provideException\" include ProvideException; let f = () => if (true) { throw ProvideException.MyException }; f()",
    "OriginalException",
  );
  assertRunError(
    "provide_exceptions2",
    "from \"reprovideException\" include ReprovideException; use ReprovideException.*; let f = () => if (true) { throw MyException }; f()",
    "OriginalException",
  );
  assertRunError(
    "provide_exceptions3",
    "from \"reprovideException\" include ReprovideException; use ReprovideException.{ exception MyException as E }; let f = () => if (true) { throw E }; f()",
    "OriginalException",
  );
  assertRun(
    "provide_exceptions4",
    {|
      from "reprovideException" include ReprovideException
      use ReprovideException.{ exception MyException, excVal1, excVal2 }
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

  assertHasWasmExport(
    ~use_start_section=true,
    "provide_start_function",
    {|
      module Start
      print("init")
      provide let _start = () => {
        print("starting up")
      }
    |},
    [("_start", Binaryen.Export.external_function)],
  );

  assertCompileError(
    ~link=true,
    "provide_start_function_invalid",
    {|
      provide let _start = () => void
    |},
    "The export `_start` is only allowed when compiling with `--use-start-section`.",
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
    "module Test; module M { provide let x = () => 1; provide let y = () => 2 }; use M.*; provide { x, y }",
    [
      ("x", Binaryen.Export.external_function),
      ("y", Binaryen.Export.external_function),
    ],
  );
  assertHasWasmExport(
    "provide_from_import",
    "module Test; from \"provideAll\" include ProvideAll; use ProvideAll.*; provide { y }",
    [("y", Binaryen.Export.external_function)],
  );
  assertHasWasmExport(
    "provide_from_import_with_rebind",
    "module Test; from \"provideAll\" include ProvideAll; use ProvideAll.*; provide let z = y",
    [("z", Binaryen.Export.external_function)],
  );
  assertFileRun(
    "issue_1886_type_reprovided_unify",
    "reprovideTypeUnifyA",
    "true\n",
  );
  assertRun(
    "use_multilevel_alias",
    {|
      module A {
        provide module B {
          provide let b = 1
        }
      }

      use A.{ module B as C }
      use C.{ b }

      print(b)
    |},
    "1\n",
  );
});
