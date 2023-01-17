open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("exports", ({test, testSkip}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertStartSectionSnapshot =
    makeSnapshotRunner(
      ~config_fn=() => {Grain_utils.Config.use_start_section := true},
      test,
    );
  let assertCompileError = makeCompileErrorRunner(test);
  let assertHasExport = (name, prog, export) => {
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
    "export1",
    "include \"noExports\" as NoExports; from NoExports use *; x",
    "Unbound value x",
  );
  assertCompileError(
    "export2",
    "include \"noExports\" as NoExports; from NoExports use *; y",
    "Unbound value y",
  );
  assertCompileError(
    "export3",
    "include \"noExports\" as NoExports; from NoExports use *; z",
    "Unbound value z",
  );
  assertSnapshot(
    "export4",
    "include \"onlyXExported\" as OnlyXExported; from OnlyXExported use *; x",
  );
  assertCompileError(
    "export5",
    "include \"onlyXExported\" as OnlyXExported; from OnlyXExported use *; y",
    "Unbound value y",
  );
  assertCompileError(
    "export6",
    "include \"onlyXExported\" as OnlyXExported; from OnlyXExported use *; z",
    "Unbound value z",
  );
  assertSnapshot(
    "export7",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use *; x",
  );
  assertSnapshot(
    "export8",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use *; x + y(4)",
  );
  assertSnapshot(
    "export9",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use *; y(z)",
  );
  assertCompileError(
    "export10",
    "include \"exposeStar\" as ExposeStar; from ExposeStar use *; y(secret)",
    "Unbound value secret",
  );
  assertCompileError(
    "export11",
    "enum Foo { Bar }; expose { Bar }",
    "Unbound module Bar",
  );
  assertSnapshot(
    "export12",
    {|
      include "exposedType"
      ExposedType.apply((arg) => print("ok"))
    |},
  );
  assertCompileError(
    "regression_issue_1489",
    "expose { foo }",
    "Unbound value foo",
  );

  assertSnapshot("let_rec_export", "expose let rec foo = () => 5");

  assertStartSectionSnapshot(
    "export_start_function",
    {|
      print("init")
      expose let _start = () => {
        print("starting up")
      }
    |},
  );

  assertHasExport(
    "issue_918_annotated_func_export",
    "module Test; expose let foo: () -> Number = () => 5",
    ("foo", Binaryen.Export.external_function),
  );
  assertHasExport(
    "issue_918_annotated_func_export2",
    "module Test; expose let rec foo: () -> Number = () => 5",
    ("foo", Binaryen.Export.external_function),
  );
});
