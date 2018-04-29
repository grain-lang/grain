(** Utilities for interfacing with WebAssembly *)

type wasm_bin_section_type =
  | Custom of string
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
  | Data
[@@deriving sexp]

type wasm_bin_section = {
  sec_type: wasm_bin_section_type;
  offset: int;
  size: int;
} [@@deriving sexp]

type abi_version = {
  major: int;
  minor: int;
  patch: int;
} [@@deriving sexp]

val latest_abi : abi_version

val read_leb128_i32 : (unit -> int) -> int32
val read_leb128_i32_input : in_channel -> int32
val read_leb128_i32_stream : (int Stream.t) -> int32

val read_leb128_u32 : (unit -> int) -> Stdint.uint32
val read_leb128_u32_input : in_channel -> Stdint.uint32
val read_leb128_u32_stream : (int Stream.t) -> Stdint.uint32

val read_leb128_i64 : (unit -> int) -> int64
val read_leb128_i64_input : in_channel -> int64
val read_leb128_i64_stream : (int Stream.t) -> int64

val read_leb128_u64 : (unit -> int) -> Stdint.uint64
val read_leb128_u64_input : in_channel -> Stdint.uint64
val read_leb128_u64_stream : (int Stream.t) -> Stdint.uint64

val write_leb128_i32 : (int -> unit) -> int32 -> unit
val write_leb128_u32 : (int -> unit) -> Stdint.uint32 -> unit
val write_leb128_i64 : (int -> unit) -> int64 -> unit
val write_leb128_u64 : (int -> unit) -> Stdint.uint64 -> unit

val get_wasm_sections : ?reset:bool -> in_channel -> wasm_bin_section list

module type BinarySectionSpec = sig
  type t

  val name : string
  val deserialize : bytes -> t
  val accepts_version : abi_version -> bool
  val serialize : t -> bytes
end

module type BinarySectionSig = sig
  type t

  (** Loads the first instance of this section from the WASM module
      loaded at the given [in_channel]. *)
  val load : ?preserve:bool -> in_channel -> t option
  (** Serializes this section at the current position in the given [out_channel]. *)
  val write : t -> out_channel -> unit
end

module BinarySection : functor(Spec : BinarySectionSpec) ->  BinarySectionSig with type t = Spec.t
