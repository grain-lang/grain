/** Utilities for interfacing with WebAssembly */;

[@deriving sexp]
type wasm_bin_section_type =
  | Custom(string)
  | Type
  | Import
  | Function
  | Table
  | Memory
  | Global
  | Export
  | Start
  | Element
  | Code
  | Data;

[@deriving sexp]
type wasm_bin_section = {
  sec_type: wasm_bin_section_type,
  offset: int,
  size: int,
};

[@deriving sexp]
type abi_version = {
  major: int,
  minor: int,
  patch: int,
};

let latest_abi: abi_version;

let read_leb128_i32: (unit => int) => int32;
let read_leb128_i32_input: in_channel => int32;
let read_leb128_i32_stream: Stream.t(int) => int32;

let read_leb128_u32: (unit => int) => int32;
let read_leb128_u32_input: in_channel => int32;
let read_leb128_u32_stream: Stream.t(int) => int32;

let read_leb128_i64: (unit => int) => int64;
let read_leb128_i64_input: in_channel => int64;
let read_leb128_i64_stream: Stream.t(int) => int64;

let read_leb128_u64: (unit => int) => int64;
let read_leb128_u64_input: in_channel => int64;
let read_leb128_u64_stream: Stream.t(int) => int64;

let get_wasm_sections: (~reset: bool=?, in_channel) => list(wasm_bin_section);

module type BinarySectionSpec = {
  type t;

  let name: string;
  let deserialize: bytes => t;
  let accepts_version: abi_version => bool;
  let serialize: t => bytes;
};

module type BinarySectionSig = {
  type t;

  /** Loads the first instance of this section from the WASM module
      loaded at the given [in_channel]. */
  /** Serializes this section at the current position in the given [out_channel]. */

  let load: (~preserve: bool=?, in_channel) => option(t);

  /** Serializes this section at the current position in the given [out_channel]. */

  let serialize: t => bytes;
};

module BinarySection:
  (Spec: BinarySectionSpec) => BinarySectionSig with type t = Spec.t;
