open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("linking", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;
  let assertRun = makeRunner(test_or_skip);
  let assertRunError = makeErrorRunner(test_or_skip);
  let assertWasiPolyfillRun = file =>
    makeRunner(
      ~config_fn=() => {Grain_utils.Config.wasi_polyfill := Some(file)},
      test_or_skip,
    );

  assertRun("link_simple", {|print("Hello, world!")|}, "Hello, world!\n");
  assertRunError(
    "link_exception",
    {|exception BadError(Number, String); let _ = throw BadError(5, "foo")|},
    {|BadError\(5, "foo"\)|},
  );
  assertRun(
    "link_import",
    {|from "list" include List; print(List.map(n => n + 1, [1, 2, 3]))|},
    "[2, 3, 4]\n",
  );
  assertRun("link_issue_994_no_generated_code", {|0|}, "");
  assertRun(
    "link_issue_994_unexported_type",
    {|record Foo { foo: String }|},
    "",
  );
  assertRun(
    "link_issue_994_exported_type",
    {|provide record Foo { foo: String }|},
    "",
  );
  // --wasi-polyfill
  assertWasiPolyfillRun(
    "test/input/wasiPolyfill.gr",
    "wasi_polyfill",
    {|print("foo")|},
    "foo\nfoo\nfoo\n",
  );
  assertWasiPolyfillRun(
    "test/input/wasiPolyfillNoop.gr",
    "wasi_polyfill_noop",
    {|print("foo")|},
    "",
  );

  let tuple_equal = ((a1, a2), (b1, b2)) => a1 == b1 && a2 == b2;
  let triple_equal = ((a1, a2, a3), (b1, b2, b3)) =>
    a1 == b1 && a2 == b2 && a3 == b3;
  test("no_start_section", ({expect}) => {
    let name = "no_start_section";
    let outfile = wasmfile(name);
    ignore @@ compile(name, {|module Test; print("Hello, world!")|});
    let ic = open_in_bin(outfile);
    let sections = Grain_utils.Wasm_utils.get_wasm_sections(ic);
    close_in(ic);
    let export_sections =
      List.find_map(
        (sec: Grain_utils.Wasm_utils.wasm_bin_section) =>
          switch (sec) {
          | {sec_type: Export(exports)} => Some(exports)
          | _ => None
          },
        sections,
      );
    let start_section =
      List.find_opt(
        (sec: Grain_utils.Wasm_utils.wasm_bin_section) =>
          switch (sec) {
          | {sec_type: Start} => true
          | _ => false
          },
        sections,
      );
    expect.option(start_section).toBeNone();
    expect.option(export_sections).toBeSome();
    expect.list(Option.get(export_sections)).toContainEqual(
      ~equals=tuple_equal,
      (WasmFunction, "_start"),
    );
  });

  test("use_start_section", ({expect}) => {
    let name = "use_start_section";
    let outfile = wasmfile(name);
    ignore @@
    compile(
      ~config_fn=() => {Grain_utils.Config.use_start_section := true},
      name,
      {|module Test; print("Hello, world!")|},
    );
    let ic = open_in_bin(outfile);
    let sections = Grain_utils.Wasm_utils.get_wasm_sections(ic);
    close_in(ic);
    let start_section =
      List.find_opt(
        (sec: Grain_utils.Wasm_utils.wasm_bin_section) =>
          switch (sec) {
          | {sec_type: Start} => true
          | _ => false
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
    expect.option(start_section).toBeSome();
    expect.option(export_sections).toBeSome();
    expect.list(Option.get(export_sections)).not.toContainEqual(
      ~equals=tuple_equal,
      (WasmFunction, "_start"),
    );
  });

  test("import_memory", ({expect}) => {
    let name = "import_memory";
    let outfile = wasmfile(name);
    ignore @@
    compile(
      ~config_fn=() => {Grain_utils.Config.import_memory := true},
      name,
      {|module Test; print("Hello, world!")|},
    );
    let ic = open_in_bin(outfile);
    let sections = Grain_utils.Wasm_utils.get_wasm_sections(ic);
    close_in(ic);
    let imports =
      List.find_map(
        (sec: Grain_utils.Wasm_utils.wasm_bin_section) =>
          switch (sec) {
          | {sec_type: Import(imports)} => Some(imports)
          | _ => None
          },
        sections,
      );
    expect.list(Option.get(imports)).toContainEqual(
      ~equals=triple_equal,
      (WasmMemory, "env", "memory"),
    );
  });
});
