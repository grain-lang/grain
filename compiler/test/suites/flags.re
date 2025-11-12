open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_codegen;
open Binaryen;

let load_module = fullpath => {
  let ic = open_in_bin(fullpath);
  let length = in_channel_length(ic);
  let module_bytes = Bytes.create(length);
  really_input(ic, module_bytes, 0, length);
  close_in(ic);
  Module.read(module_bytes);
};

describe("flags", ({test, testSkip}) => {
  // Test --no-bulk-memory polyfill
  test("no_bulk_memory_calls", ({expect}) => {
    let name = "no_bulk_memory_calls";
    let outfile = wasmfile(name);
    ignore @@
    compile(
      ~link=true,
      ~config_fn=() => {Grain_utils.Config.bulk_memory := false},
      name,
      {|
      module NoBulkMemoryCalls

      from "runtime/unsafe/memory" include Memory
      @unsafe
      provide let foo = Memory.fill
      @unsafe
      provide let bar = Memory.copy
      |},
    );
    let wasm_mod = load_module(outfile);
    Module.set_features(
      wasm_mod,
      List.filter(
        f =>
          f != Module.Feature.bulk_memory
          && f != Module.Feature.bulk_memory_opt,
        Compcore.features,
      ),
    );
    expect.bool(
      List.mem(Module.Feature.bulk_memory, Module.get_features(wasm_mod)),
    ).
      toBe(
      false,
    );
    // The module will fail validation if bulk memory opcodes are still present
    expect.int(Module.validate(wasm_mod)).toBe(1);
  })
});
