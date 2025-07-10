/** Utilities for interfacing with WebAssembly */;

[@deriving sexp]
type wasm_bin_type =
  | WasmFunction
  | WasmTable
  | WasmMemory
  | WasmGlobal;

[@deriving sexp]
type wasm_bin_section_type =
  | Custom(string)
  | Type
  | Import(list((wasm_bin_type, string, string)))
  | Function
  | Table
  | Memory
  | Global
  | Export(list((wasm_bin_type, string)))
  | Start
  | Element
  | Code
  | Data
  | DataCount;

[@deriving sexp]
type wasm_bin_section = {
  sec_type: wasm_bin_section_type,
  offset: int,
  size: int,
};

let read_leb128_i32: (unit => int) => int32;
let read_leb128_i32_input: in_channel => int32;

let read_leb128_u32: (unit => int) => int32;
let read_leb128_u32_input: in_channel => int32;

let read_leb128_i64: (unit => int) => int64;
let read_leb128_i64_input: in_channel => int64;

let read_leb128_u64: (unit => int) => int64;
let read_leb128_u64_input: in_channel => int64;

let get_wasm_sections: (~reset: bool=?, in_channel) => list(wasm_bin_section);
