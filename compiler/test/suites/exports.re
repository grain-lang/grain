open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("exports", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
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
    "import * from \"noExports\"; x",
    "Unbound value x",
  );
  assertCompileError(
    "export2",
    "import * from \"noExports\"; y",
    "Unbound value y",
  );
  assertCompileError(
    "export3",
    "import * from \"noExports\"; z",
    "Unbound value z",
  );
  assertSnapshot("export4", "import * from \"onlyXExported\"; x");
  assertCompileError(
    "export5",
    "import * from \"onlyXExported\"; y",
    "Unbound value y",
  );
  assertCompileError(
    "export6",
    "import * from \"onlyXExported\"; z",
    "Unbound value z",
  );
  assertSnapshot("export7", "import * from \"exportStar\"; x");
  assertSnapshot("export8", "import * from \"exportStar\"; x + y(4)");
  assertSnapshot("export9", "import * from \"exportStar\"; y(z)");
  assertCompileError(
    "export10",
    "import * from \"exportStar\"; y(secret)",
    "Unbound value secret",
  );
  assertCompileError(
    "export10",
    "enum Foo { Bar }; export Bar",
    "Unbound type constructor",
  );

  assertSnapshot("let_rec_export", "export let rec foo = () => 5");
  assertHasExport(
    "issue_918_annotated_func_export",
    "export let foo: () -> Number = () => 5",
    ("foo", Binaryen.Export.external_function),
  );
  assertHasExport(
    "issue_918_annotated_func_export2",
    "export let rec foo: () -> Number = () => 5",
    ("foo", Binaryen.Export.external_function),
  );
});
