open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("foreigns", ({test}) => {
  test("external_name", ({expect}) => {
    let name = "external_name";
    let outfile = wasmfile(name);
    ignore @@
    compile(
      name,
      {|
      module Test
      @externalName("env.foo")
      foreign wasm foo: Number => Number from "env"

      @externalName("__foo%%!")
      provide let bar = () => foo(1)
      |},
    );
    let ic = open_in_bin(outfile);
    let sections = Grain_utils.Wasm_utils.get_wasm_sections(ic);
    close_in(ic);
    let import_sections =
      List.find_map(
        (sec: Grain_utils.Wasm_utils.wasm_bin_section) =>
          switch (sec) {
          | {sec_type: Import(exports)} => Some(exports)
          | _ => None
          },
        sections,
      );
    let export_sections =
      List.find_map(
        (sec: Grain_utils.Wasm_utils.wasm_bin_section) =>
          switch (sec) {
          | {sec_type: Export(exports)} => Some(exports)
          | _ => None
          },
        sections,
      );
    expect.option(import_sections).toBeSome();
    expect.list(Option.get(import_sections)).toContainEqual((
      WasmFunction,
      "env",
      "env.foo",
    ));
    expect.option(export_sections).toBeSome();
    expect.list(Option.get(export_sections)).toContainEqual((
      WasmFunction,
      "__foo%%!",
    ));
  })
});
