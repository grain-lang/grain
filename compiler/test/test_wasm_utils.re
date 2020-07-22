open Grain_utils;
open Wasm_utils;

open OUnit2;

let assert_equal_i32 = assert_equal(~printer=Int32.to_string);
let assert_equal_i64 = assert_equal(~printer=Int64.to_string);

let print_list = (printer, lst) =>
  "[" ++ String.concat("; ", List.map(printer, lst)) ++ "]";

let print_bytes = print_list(Printf.sprintf("0x%02x"));

let assert_equal_bytes = assert_equal(~printer=print_bytes);

let print_sexp = Sexplib.Sexp.to_string_hum;
let print_section = s =>
  Sexplib.Sexp.to_string_hum(sexp_of_wasm_bin_section(s));
let print_sections = s =>
  print_sexp(Sexplib.Conv.sexp_of_list(sexp_of_wasm_bin_section, s));

let assert_equal_section = assert_equal(~printer=print_section);
let assert_equal_sections = assert_equal(~printer=print_sections);

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

let collect_bytes = f => {
  let bytes = ref([]);
  let func = x => bytes := [x, ...bytes^];
  f(func);
  List.rev(bytes^);
};

let test_leb128_u32 = ctxt => {
  assert_equal_i32(
    ~ctxt,
    Int32.of_int(624485),
    read_leb128_u32(iter_bytes([0xE5, 0x8E, 0x26])),
  );
};

let test_leb128_i32 = ctxt => {
  assert_equal_i32(
    ~ctxt,
    Int32.of_int(-624485),
    read_leb128_i32(iter_bytes([0x9B, 0xF1, 0x59])),
  );
};

let test_get_wasm_sections = ctxt => {
  let inchan = open_in_bin("test-data/testmod.wasm");
  let sections = get_wasm_sections(inchan);
  close_in(inchan);
  assert_equal_sections(
    ~ctxt,
    [
      {sec_type: Type, offset: 10, size: 8},
      {sec_type: Import, offset: 20, size: 25},
      {sec_type: Function, offset: 47, size: 2},
      {sec_type: Export, offset: 51, size: 17},
      {sec_type: Code, offset: 70, size: 8},
      {sec_type: Custom("name"), offset: 85, size: 28},
    ],
    sections,
  );
};

let leb128_tests =
  "LEB128 Tests"
  >::: ["LEB128 U32" >:: test_leb128_u32, "LEB128 I32" >:: test_leb128_i32];

let wasm_binary_tests =
  "WebAssembly Binary Tests" >::: ["read sections" >:: test_get_wasm_sections];

let tests = "WASM Utils" >::: [leb128_tests, wasm_binary_tests];
