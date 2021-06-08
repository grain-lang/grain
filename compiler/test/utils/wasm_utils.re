open Grain_utils;
open Wasm_utils;

open TestFramework;
open Runner;

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
          sections,
          [
            {sec_type: Type, offset: 10, size: 8},
            {sec_type: Import, offset: 20, size: 25},
            {sec_type: Function, offset: 47, size: 2},
            {sec_type: Export, offset: 51, size: 17},
            {sec_type: Code, offset: 70, size: 8},
            {sec_type: Custom("name"), offset: 85, size: 28},
          ],
        );
      })
    })
  });
});
