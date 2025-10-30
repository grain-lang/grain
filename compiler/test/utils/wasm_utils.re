open Grain_utils;
open Wasm_utils;

open Grain_tests.TestFramework;
open Grain_tests.Runner;

let iter_bytes = bytes => {
  let bytes = ref(bytes);
  () =>
    switch (bytes^) {
    | [] => raise(Not_found)
    | [hd, ...tl] =>
      bytes := tl;
      hd;
    };
};

describe("aux/wasm_utils", ({describe}) => {
  describe("LEB128", ({test}) => {
    test("test_leb128_u32", ({expect}) => {
      expect.equal(
        ~equals=Int32.equal,
        read_leb128_u32(iter_bytes([0xE5, 0x8E, 0x26])),
        624485l,
      )
    });

    test("test_leb128_i32", ({expect}) => {
      expect.equal(
        ~equals=Int32.equal,
        read_leb128_i32(iter_bytes([0x9B, 0xF1, 0x59])),
        -624485l,
      )
    });
  });

  describe("WebAssembly Binary", ({describe}) => {
    describe("read sections", ({test}) => {
      test("test_get_wasm_sections", ({expect}) => {
        let inchan = open_in_bin("test/test-data/testmod.wasm");
        let sections = get_wasm_sections(inchan);
        close_in(inchan);
        expect.equal(
          [
            {
              sec_type: Type,
              offset: 10,
              size: 8,
            },
            {
              sec_type: Import([(WasmFunction, "imports", "imported_func")]),
              offset: 21,
              size: 24,
            },
            {
              sec_type: Function,
              offset: 47,
              size: 2,
            },
            {
              sec_type: Export([(WasmFunction, "exported_func")]),
              offset: 52,
              size: 16,
            },
            {
              sec_type: Code,
              offset: 70,
              size: 8,
            },
            {
              sec_type: Custom("name"),
              offset: 85,
              size: 28,
            },
          ],
          sections,
        );
      });
      test("test_get_wasm_sections2", ({expect}) => {
        let inchan = open_in_bin("test/test-data/testmod_multi_exports.wasm");
        let sections = get_wasm_sections(inchan);
        close_in(inchan);
        expect.equal(
          [
            {
              sec_type: Type,
              offset: 10,
              size: 8,
            },
            {
              sec_type:
                Import([
                  (WasmFunction, "imports", "imported_func"),
                  (WasmMemory, "imports", "mem"),
                ]),
              offset: 21,
              size: 39,
            },
            {
              sec_type: Function,
              offset: 62,
              size: 3,
            },
            {
              sec_type: Global,
              offset: 67,
              size: 11,
            },
            {
              sec_type:
                Export([
                  (WasmFunction, "exported_func"),
                  (WasmFunction, "exported_func2"),
                  (WasmGlobal, "exported_glob"),
                  (WasmGlobal, "exported_glob2"),
                  (WasmMemory, "memory"),
                ]),
              offset: 81,
              size: 75,
            },
            {
              sec_type: Code,
              offset: 158,
              size: 15,
            },
          ],
          sections,
        );
      });
    })
  });
});
