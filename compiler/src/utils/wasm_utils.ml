(** Utilities for interfacing with WebAssembly *)
open Sexplib.Conv

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

let grain_magic = [0x53; 0x77; 0x13; 0x00] (* punny, I know [16 April 2018] <Philip>
                                              Took me like 5min to figure out the pun here...
                                                if a bad pun is hidden in the code and no one is
                                                there to explain it, is it still a joke? #showerthoughts [21 October 2019] <Philip> *)
let latest_abi = {major=1; minor=0; patch=0}


let identity : 'a. 'a -> 'a = fun x -> x
let i64_of_u64 = Stdint.Int64.of_uint64
let i32_of_u64 = Stdint.Int32.of_uint64
let u32_of_u64 = Stdint.Uint32.of_uint64


exception MalformedLEB of bool * int
exception MalformedSectionType of int * (int option)

(** Reads an LEB128-encoded integer (which WebAssembly uses)
    from the given input channel.
    @see <https://webassembly.github.io/spec/core/binary/values.html#integers> WebAssembly documentation
*)
let read_leb128 : 'a. ?signed:bool -> ?maxbits:int -> conv:(Stdint.uint64 -> 'a) -> (unit -> int) -> 'a = fun ?signed:(signed=false) ?maxbits:(maxbits=32) ~conv next_byte ->
  let rec read_int maxbits =
    let zero, of_int, add, mul =
      let open Stdint.Uint64 in
      zero, of_int, add, mul in
    if maxbits <= 0 then
      zero
    else begin
      match (signed, (next_byte())) with
      (* Check for invalid input *)
      | (false, n) when (n >= (1 lsl maxbits)) -> raise (MalformedLEB(false, maxbits))
      | (true, n) when (n >= (1 lsl (maxbits - 1))) -> raise (MalformedLEB(true, maxbits))
      (* Unsigned case: zero MSB => last byte *)
      | (false, n) when (n < (1 lsl (min maxbits 7))) -> of_int n
      (* Signed case: zero MSB(s) => last byte *)
      | (true, n) when (n < (1 lsl (min (maxbits - 1) 6))) -> of_int n
      (* In the signed case, getting here means n >= 2^6 *)
      | (true, n) when (n < (1 lsl 7)) && (n >= ((1 lsl 7) - (1 lsl (maxbits - 1)))) -> of_int (n - (1 lsl 7))
      (* Nonzero MSB: we need to recur: *)
      | (false, n)
      | (true, n)-> add (mul (of_int @@ 1 lsl 7) (read_int (maxbits - 7))) (of_int (n - (1 lsl 7)))
    end
  in
  conv (read_int maxbits)

(* https://graphics.stanford.edu/~seander/bithacks.html#IntegerLog *)
let log2_u64 x =
  let open Stdint.Uint64 in
  let v = ref zero in
  let b = [| of_int 0x2; of_int 0xC; of_int 0xF0; of_int 0xFF00; of_int 0xFFFF0000; shift_left (of_int 0xFFFFFFFF) 8 |] in
  let s = [| 1; 2; 4; 8; 16; 32 |] in
  let r = ref 0 in
  for i = 5 downto 0 do
    if (zero <> (logand (!v) (b.(i)))) then begin
      v := shift_right (!v) (s.(i));
      r := (!r) lor (s.(i))
    end
  done;
  !r

let log2_i64 x =
  let open Stdint.Int64 in
  let v = ref zero in
  let b = [| of_int 0x2; of_int 0xC; of_int 0xF0; of_int 0xFF00; of_int 0xFFFF0000; of_uint64 (Stdint.Uint64.shift_left (Stdint.Uint64.of_int 0xFFFFFFFF) 8) |] in
  let s = [| 1; 2; 4; 8; 16; 32 |] in
  let r = ref 0 in
  for i = 5 downto 0 do
    if (zero <> (logand (!v) (b.(i)))) then begin
      v := shift_right (!v) (s.(i));
      r := (!r) lor (s.(i))
    end
  done;
  !r

(* Encoding algorithms taken from https://en.wikipedia.org/wiki/LEB128 *)
let write_leb128_unsigned : sink:(int -> unit) -> Stdint.uint64 -> unit = fun ~sink num ->
  let open Stdint.Uint64 in
  let last_byte_low n = Stdint.Uint8.of_uint64 (logand (of_int 0x7F) n) in (* last 7 bits *)
  let set_high_bit = Stdint.Uint8.(logor (of_int 0x80)) in
  let rec process num =
    let byte = last_byte_low num in
    let num = shift_right num 7 in
    if num = zero then
      sink (Stdint.Uint8.to_int byte)
    else begin
      sink (Stdint.Uint8.to_int (set_high_bit byte));
      process num
    end in
  process num

let write_leb128_signed : sink:(int -> unit) -> Stdint.int64 -> unit = fun ~sink num ->
  let open Stdint.Int64 in
  let last_byte_low n = Stdint.Uint8.of_int64 (logand (of_int 0x7F) n) in (* last 7 bits *)
  let set_high_bit = Stdint.Uint8.(logor (of_int 0x80)) in
  let get_sign_bit = Stdint.Uint8.(logand (of_int 0x40)) in
  let nonneg_finished b n = (n = zero) && ((get_sign_bit b) = Stdint.Uint8.zero) in
  let neg_finished b n = (n = (of_int (-1))) && ((get_sign_bit b) <> Stdint.Uint8.zero) in
  let rec process num =
    let byte = last_byte_low num in
    let num = shift_right num 7 in (* Arithmetic shift *)
    if (nonneg_finished byte num) || (neg_finished byte num) then
      sink (Stdint.Uint8.to_int byte)
    else begin
      sink (Stdint.Uint8.to_int (set_high_bit byte));
      process num
    end in
  process num

let read_leb128_i32 bytesrc : Stdint.int32 = read_leb128 ~signed:true ~maxbits:32 ~conv:i32_of_u64 bytesrc
let read_leb128_i32_input inchan = read_leb128_i32 (fun () -> input_byte inchan)
let read_leb128_i32_stream stream = read_leb128_i32 (fun () -> Stream.next stream)
let read_leb128_u32 bytesrc : Stdint.uint32 = read_leb128 ~signed:false ~maxbits:32 ~conv:u32_of_u64 bytesrc
let read_leb128_u32_input inchan = read_leb128_u32 (fun () -> input_byte inchan)
let read_leb128_u32_stream stream = read_leb128_u32 (fun () -> Stream.next stream)
let read_leb128_i64 bytesrc : Stdint.int64 = read_leb128 ~signed:true ~maxbits:64 ~conv:i64_of_u64 bytesrc
let read_leb128_i64_input inchan = read_leb128_i64 (fun () -> input_byte inchan)
let read_leb128_i64_stream stream = read_leb128_i64 (fun () -> Stream.next stream)
let read_leb128_u64 bytesrc : Stdint.uint64 = read_leb128 ~signed:false ~maxbits:64 ~conv:identity bytesrc
let read_leb128_u64_input inchan = read_leb128_u64 (fun () -> input_byte inchan)
let read_leb128_u64_stream stream = read_leb128_u64 (fun () -> Stream.next stream)


let write_leb128_i32 bytesink n : unit = write_leb128_signed bytesink (Stdint.Int64.of_int32 n)
let write_leb128_u32 bytesink n : unit = write_leb128_unsigned bytesink (Stdint.Uint64.of_uint32 n)
let write_leb128_i64 bytesink n : unit = write_leb128_signed bytesink n
let write_leb128_u64 bytesink n : unit = write_leb128_unsigned bytesink n

let read_int32 inchan =
  let bytes = Bytes.create 4 in
  if (input inchan bytes 0 4) <> 4 then
    raise End_of_file;
  let open Stdint.Int32 in
  to_int (of_bytes_little_endian bytes 0)

let read_abi_version inchan =
  let num_bytes = 4 * 3 in
  let bytes = Bytes.create num_bytes in
  if (input inchan bytes 0 num_bytes) <> num_bytes then
    raise End_of_file;
  let open Stdint.Uint32 in
  let major = to_int (of_bytes_little_endian bytes 0) in
  let minor = to_int (of_bytes_little_endian bytes 4) in
  let patch = to_int (of_bytes_little_endian bytes 8) in
  {
    major;
    minor;
    patch;
  }

let serialize_int32 i =
  let bytes = Bytes.create 4 in
  let open Stdint.Uint32 in
  to_bytes_little_endian (of_int i) bytes 0;
  bytes

let serialize_abi_version {major; minor; patch} =
  let num_bytes = 4 * 3 in
  let bytes = Bytes.create num_bytes in
  let open Stdint.Uint32 in
  to_bytes_little_endian (of_int major) bytes 0;
  to_bytes_little_endian (of_int minor) bytes 4;
  to_bytes_little_endian (of_int patch) bytes 8;
  bytes

let section_type_of_int ?pos ?name = function
  | 0 -> Custom (Option.default "" name)
  | 1 -> Type
  | 2 -> Import
  | 3 -> Function
  | 4 -> Table
  | 5 -> Memory
  | 6 -> Global
  | 7 -> Export
  | 8 -> Start
  | 9 -> Element
  | 10 -> Code
  | 11 -> Data
  | n -> raise (MalformedSectionType(n, pos))

let int_of_section_type = function
  | Custom _ -> 0
  | Type -> 1
  | Import -> 2
  | Function -> 3
  | Table -> 4
  | Memory -> 5
  | Global -> 6
  | Export -> 7
  | Start -> 8
  | Element -> 9
  | Code -> 10
  | Data -> 11

let get_wasm_sections ?reset:(reset=false) inchan =
  let orig_pos = pos_in inchan in
  let read_boilerplate() =
    let magic = [0x00; 0x61; 0x73; 0x6D] in
    let version = [0x01; 0x00; 0x00; 0x00] in
    let expect strbuilder b =
      let actual = input_byte inchan in
      if actual <> b then
        raise (Invalid_argument (strbuilder b actual))
    in
    let magic_strbuilder b actual =
      Printf.sprintf "Error reading WebAssembly magic number. Expected byte 0x%02x; found 0x%02x" b actual in
    let version_strbuilder b actual =
      (* TODO: This should probably warn, not fail *)
      Printf.sprintf "Error reading WebAssembly version. Expected byte 0x%02x; found 0x%02x" b actual in
    List.iter (expect magic_strbuilder) magic;
    List.iter (expect version_strbuilder) version in

  let next_section() =
    try
      let sec_type = section_type_of_int (input_byte inchan) in
      let size = Stdint.Uint32.to_int (read_leb128_u32_input inchan) in
      let offset = pos_in inchan in
      let sec_type, true_offset, true_size = match sec_type with
        | Custom _ ->
          let name_len = Stdint.Uint32.to_int (read_leb128_u32_input inchan) in
          let name = really_input_string inchan name_len in
          (*let bstr bs = "[" ^ (ExtString.String.join ", " (List.map (Printf.sprintf "0x%02x") bs)) ^ "]" in
          Printf.eprintf "read: size: %d; name_len: %d; name: %s\n"
            size name_len (bstr (List.map int_of_char @@ ExtString.String.explode name));*)
          let name = Wasm.Utf8.encode (List.map int_of_char @@ ExtString.String.explode name) in
          let true_offset = pos_in inchan in
          (Custom name), true_offset, size - (true_offset - offset)
        | s -> s, offset, size
      in
      seek_in inchan (offset + size);
      Some {
        sec_type;
        offset=true_offset;
        size=true_size;
      }
    with End_of_file -> None
  in
  let rec collect_sections acc =
    match next_section() with
    | Some(sec) -> collect_sections (sec::acc)
    | None -> acc in

  read_boilerplate();

  let ret = List.rev (collect_sections []) in
  if reset then begin
    seek_in inchan orig_pos
  end;
  ret

let write_wasm_section_header {sec_type; size} ochan =
  output_byte ochan (int_of_section_type sec_type);
  begin match sec_type with
    | Custom name ->
      let bytes = ref [] in
      let push = (fun i -> bytes := i::(!bytes)) in
      let name_bytes = Wasm.Utf8.decode name in
      write_leb128_u32 push (Stdint.Uint32.of_int (List.length name_bytes));
      bytes := List.rev !bytes;
      let full_size = size + (List.length !bytes) + (List.length name_bytes) in
      (*let bstr bs = "[" ^ (ExtString.String.join ", " (List.map (Printf.sprintf "0x%02x") bs)) ^ "]" in
      Printf.eprintf "write: size: %d; name: %s; bytes: %s; name_bytes: %s; full_size: %d\n"
        size name (bstr !bytes) (bstr name_bytes) full_size;*)
      write_leb128_u32 (output_byte ochan) (Stdint.Uint32.of_int full_size);
      List.iter (output_byte ochan) !bytes;
      List.iter (output_byte ochan) name_bytes;
    | _ ->
      write_leb128_u32 (output_byte ochan) (Stdint.Uint32.of_int size);
  end

let write_custom_wasm_section name bs ochan =
  let size = Bytes.length bs in
  (* Build a header just to validate that we write a complete one *)
  let hdr = {
    sec_type=Custom name;
    size;
    offset=(-1);
  } in
  write_wasm_section_header hdr ochan;
  output_bytes ochan bs

let get_grain_custom_info inchan =
  try
    let rec check_magic remaining =
      match remaining with
      | [] -> true
      | hd::tl ->
        ((input_byte inchan) = hd) && (check_magic tl) in
    if not (check_magic grain_magic) then
      None
    else begin
      let version = read_abi_version inchan in
      let section_name_length = read_int32 inchan in
      let section_name_bytes = Bytes.create section_name_length in
      if (input inchan section_name_bytes 0 section_name_length) <> section_name_length then
        raise End_of_file;
      let section_name = Bytes.to_string section_name_bytes in
      Some(version, section_name)
    end
  with End_of_file -> None

let serialize_grain_custom_info sec_name abi_version =
  let sec_bytes = Bytes.of_string sec_name in
  let buf = BatBuffer.create ((Bytes.length sec_bytes) + 4 + (4 * 3) + 4) in
  List.iter (fun b -> BatBuffer.add_char buf (char_of_int b)) grain_magic;
  BatBuffer.add_bytes buf (serialize_abi_version abi_version);
  BatBuffer.add_bytes buf (serialize_int32 (Bytes.length sec_bytes));
  BatBuffer.add_bytes buf sec_bytes;
  BatBuffer.to_bytes buf


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

module BinarySection(Spec : BinarySectionSpec) : BinarySectionSig with type t = Spec.t = struct
  type t = Spec.t

  let load ?preserve:(preserve=false) inchan =
    let orig_pos = pos_in inchan in
    let sections = List.filter (fun {sec_type} -> sec_type = Custom Spec.name)
        (get_wasm_sections inchan) in
    let rec process sections =
      match sections with
      | [] -> None
      | {offset; size}::tl ->
        seek_in inchan offset;
        begin match get_grain_custom_info inchan with
          | Some(abi_version, name) when name = Spec.name && Spec.accepts_version abi_version ->
            (* Now we're at the start of the section. Time to read *)
            let realsize = size - ((pos_in inchan) - offset) in
            let bytes = Bytes.create realsize in
            if (input inchan bytes 0 realsize) = realsize then
              Some(Spec.deserialize bytes)
            else
              process tl
          | _ -> process tl
        end
    in
    let ret = process sections in
    if preserve then
      seek_in inchan orig_pos;
    ret

  let write value outchan =
    let val_bytes = Spec.serialize value in
    let header_bytes = serialize_grain_custom_info Spec.name latest_abi in
    let sep = Bytes.empty in
    write_custom_wasm_section Spec.name (Bytes.concat sep [header_bytes; val_bytes]) outchan

end



let () =
  Printexc.register_printer (fun exc ->
      match exc with
      | MalformedLEB(signed, maxbits) ->
        let sstr = if signed then "(signed)" else "(unsigned)" in
        Some(Printf.sprintf "Malformed LEB-encoded %s number (expected at most %d bits)" sstr maxbits)
      | MalformedSectionType(tag, Some(pos)) ->
        Some(Printf.sprintf "Malformed WASM section tag at position %d: %d" pos tag)
      | MalformedSectionType(tag, None) ->
        Some(Printf.sprintf "Malformed WASM section tag: %d" tag)
      | Wasm.Script.Syntax(src, msg) ->
        Some(Printf.sprintf "WebAssembly Parse Error at %s: %s"
               (Wasm.Source.string_of_region src)
               msg)
      | _ -> None)
