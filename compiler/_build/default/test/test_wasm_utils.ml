open Grain_utils
open Wasm_utils

open OUnit2
open Stdint

let assert_equal_i32 = assert_equal ~printer:Int32.to_string
let assert_equal_u32 = assert_equal ~printer:Uint32.to_string
let assert_equal_i64 = assert_equal ~printer:Int64.to_string
let assert_equal_u64 = assert_equal ~printer:Uint64.to_string

let print_list printer lst =
  "[" ^ (ExtString.String.join "; " (List.map printer lst)) ^ "]"

let print_bytes = print_list (Printf.sprintf "0x%02x")

let assert_equal_bytes = assert_equal ~printer:print_bytes

let print_sexp = Sexplib.Sexp.to_string_hum
let print_section s = Sexplib.Sexp.to_string_hum (sexp_of_wasm_bin_section s)
let print_sections s = print_sexp (Sexplib.Conv.sexp_of_list sexp_of_wasm_bin_section s)

let assert_equal_section = assert_equal ~printer:print_section
let assert_equal_sections = assert_equal ~printer:print_sections

let iter_bytes bytes =
  let bytes = ref bytes in
  (fun () ->
     match !bytes with
     | [] -> raise Not_found
     | hd::tl -> bytes := tl; hd)

let collect_bytes f =
  let bytes = ref [] in
  let func = fun x -> bytes := x::(!bytes) in
  f func;
  List.rev (!bytes)


let compile_wasm_file filename =
  let testdata_dir = "test-data" in
  let filename = testdata_dir ^ "/" ^ filename in
  Runner.assemble_object_file (filename ^ ".wast") false (filename ^ ".wasm")


let test_leb128_u32 ctxt =
  assert_equal_u32 ~ctxt (Uint32.of_int 624485) (read_leb128_u32 (iter_bytes [0xE5; 0x8E; 0x26]));
  assert_equal_bytes ~ctxt [0xE5; 0x8E; 0x26] (collect_bytes (fun sink -> write_leb128_u32 sink (Uint32.of_int 624485)))

let test_leb128_i32 ctxt =
  assert_equal_i32 ~ctxt (Int32.of_int (-624485)) (read_leb128_i32 (iter_bytes [0x9B; 0xF1; 0x59]));
  assert_equal_bytes ~ctxt [0x9B; 0xF1; 0x59] (collect_bytes (fun sink -> write_leb128_i32 sink (Int32.of_int (-624485))))

let test_get_wasm_sections ctxt =
  ignore(compile_wasm_file "testmod");
  let inchan = open_in_bin "test-data/testmod.wasm" in
  let sections = get_wasm_sections inchan in
  close_in inchan;
  assert_equal_sections ~ctxt [
    {
      sec_type=Type;
      offset=14;
      size=8;
    };
    {
      sec_type=Import;
      offset=28;
      size=25;
    };
    {
      sec_type=Function;
      offset=59;
      size=2;
    };
    {
      sec_type=Export;
      offset=67;
      size=17;
    };
    {
      sec_type=Code;
      offset=90;
      size=12;
    };
  ] sections


let leb128_tests = "LEB128 Tests">::: [
    "LEB128 U32">::test_leb128_u32;
    "LEB128 I32">::test_leb128_i32;
  ]

let wasm_binary_tests = "WebAssembly Binary Tests">::: [
    "read sections">::test_get_wasm_sections;
  ]

let tests = "WASM Utils">::: [
    leb128_tests;
    wasm_binary_tests;
  ]
