/** Utilities for interfacing with WebAssembly */
open Sexplib.Conv;

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

let grain_magic = [0x53, 0x77, 0x13, 0x00]; /* punny, I know [16 April 2018] <Philip>
                                              Took me like 5min to figure out the pun here...
                                                if a bad pun is hidden in the code and no one is
                                                there to explain it, is it still a joke? #showerthoughts [21 October 2019] <Philip> */

let latest_abi = {major: 1, minor: 0, patch: 0};

let identity: 'a. 'a => 'a = x => x;
let i32_of_u64 = Int64.to_int32;

exception MalformedLEB(bool, int);
exception MalformedSectionType(int, option(int));

/** Reads an LEB128-encoded integer (which WebAssembly uses)
    from the given input channel.
    @see <https://webassembly.github.io/spec/core/binary/values.html#integers> WebAssembly documentation
*/

let read_leb128:
  'a.
  (~signed: bool=?, ~maxbits: int=?, ~conv: int64 => 'a, unit => int) => 'a
 =
  (~signed=false, ~maxbits=32, ~conv, next_byte) => {
    let rec read_int = maxbits => {
      let (zero, of_int, add, mul) = Int64.(zero, of_int, add, mul);
      if (maxbits <= 0) {
        zero;
      } else {
        switch (signed, next_byte()) {
        /* Check for invalid input */
        | (false, n) when n >= 1 lsl maxbits =>
          raise([@implicit_arity] MalformedLEB(false, maxbits))
        | (true, n) when n >= 1 lsl (maxbits - 1) =>
          raise([@implicit_arity] MalformedLEB(true, maxbits))
        /* Unsigned case: zero MSB => last byte */
        | (false, n) when n < 1 lsl min(maxbits, 7) => of_int(n)
        /* Signed case: zero MSB(s) => last byte */
        | (true, n) when n < 1 lsl min(maxbits - 1, 6) => of_int(n)
        /* In the signed case, getting here means n >= 2^6 */
        | (true, n) when n < 1 lsl 7 && n >= 1 lsl 7 - 1 lsl (maxbits - 1) =>
          of_int(n - 1 lsl 7)
        /* Nonzero MSB: we need to recur: */
        | (false, n)
        | (true, n) =>
          add(
            mul(of_int @@ 1 lsl 7, read_int(maxbits - 7)),
            of_int(n - 1 lsl 7),
          )
        };
      };
    };

    conv(read_int(maxbits));
  };

/* https://graphics.stanford.edu/~seander/bithacks.html#IntegerLog */
let log2_u64 = x => {
  open Int64;
  let v = ref(zero);
  let b = [|
    of_int(0x2),
    of_int(0xC),
    of_int(0xF0),
    of_int(0xFF00),
    of_int(0xFFFF0000),
    shift_left(of_int(0xFFFFFFFF), 8),
  |];
  let s = [|1, 2, 4, 8, 16, 32|];
  let r = ref(0);
  for (i in 5 downto 0) {
    if (zero != logand(v^, b[i])) {
      v := shift_right(v^, s[i]);
      r := r^ lor s[i];
    };
  };
  r^;
};

let log2_i64 = x => {
  open Int64;
  let v = ref(zero);
  let b = [|
    of_int(0x2),
    of_int(0xC),
    of_int(0xF0),
    of_int(0xFF00),
    of_int(0xFFFF0000),
    Int64.shift_left(Int64.of_int(0xFFFFFFFF), 8),
  |];
  let s = [|1, 2, 4, 8, 16, 32|];
  let r = ref(0);
  for (i in 5 downto 0) {
    if (zero != logand(v^, b[i])) {
      v := shift_right(v^, s[i]);
      r := r^ lor s[i];
    };
  };
  r^;
};

let read_leb128_i32 = (bytesrc): int32 =>
  read_leb128(~signed=true, ~maxbits=32, ~conv=i32_of_u64, bytesrc);
let read_leb128_i32_input = inchan =>
  read_leb128_i32(() => input_byte(inchan));
let read_leb128_i32_stream = stream =>
  read_leb128_i32(() => Stream.next(stream));
let read_leb128_u32 = (bytesrc): int32 =>
  read_leb128(~signed=false, ~maxbits=32, ~conv=i32_of_u64, bytesrc);
let read_leb128_u32_input = inchan =>
  read_leb128_u32(() => input_byte(inchan));
let read_leb128_u32_stream = stream =>
  read_leb128_u32(() => Stream.next(stream));
let read_leb128_i64 = (bytesrc): int64 =>
  read_leb128(~signed=true, ~maxbits=64, ~conv=identity, bytesrc);
let read_leb128_i64_input = inchan =>
  read_leb128_i64(() => input_byte(inchan));
let read_leb128_i64_stream = stream =>
  read_leb128_i64(() => Stream.next(stream));
let read_leb128_u64 = (bytesrc): int64 =>
  read_leb128(~signed=false, ~maxbits=64, ~conv=identity, bytesrc);
let read_leb128_u64_input = inchan =>
  read_leb128_u64(() => input_byte(inchan));
let read_leb128_u64_stream = stream =>
  read_leb128_u64(() => Stream.next(stream));

let read_int32 = inchan => {
  let bytes = Bytes.create(4);
  if (input(inchan, bytes, 0, 4) != 4) {
    raise(End_of_file);
  };
  Bytes.get_int32_le(bytes, 0);
};

let read_abi_version = inchan => {
  let num_bytes = 4 * 3;
  let bytes = Bytes.create(num_bytes);
  if (input(inchan, bytes, 0, num_bytes) != num_bytes) {
    raise(End_of_file);
  };
  open Int32;
  let major = to_int(Bytes.get_int32_le(bytes, 0));
  let minor = to_int(Bytes.get_int32_le(bytes, 4));
  let patch = to_int(Bytes.get_int32_le(bytes, 8));
  {major, minor, patch};
};

let serialize_int32 = i => {
  let bytes = Bytes.create(4);
  open Int32;
  Bytes.set_int32_le(bytes, 0, of_int(i));
  bytes;
};

let serialize_abi_version = ({major, minor, patch}) => {
  let num_bytes = 4 * 3;
  let bytes = Bytes.create(num_bytes);
  open Int32;
  Bytes.set_int32_le(bytes, 0, of_int(major));
  Bytes.set_int32_le(bytes, 4, of_int(minor));
  Bytes.set_int32_le(bytes, 8, of_int(patch));
  bytes;
};

let utf8_encode = ints => {
  let buf = Buffer.create(14);
  List.iter(i => {Buffer.add_utf_8_uchar(buf, Uchar.of_int(i))}, ints);
  Buffer.contents(buf);
};

let utf8_decode = str => {
  List.init(String.length(str), i => {
    Uchar.to_int(Uchar.of_char(str.[i]))
  });
};

let section_type_of_int = (~pos=?, ~name=?) =>
  fun
  | 0 => Custom(Option.value(~default="", name))
  | 1 => Type
  | 2 => Import
  | 3 => Function
  | 4 => Table
  | 5 => Memory
  | 6 => Global
  | 7 => Export
  | 8 => Start
  | 9 => Element
  | 10 => Code
  | 11 => Data
  | n => raise([@implicit_arity] MalformedSectionType(n, pos));

let int_of_section_type =
  fun
  | Custom(_) => 0
  | Type => 1
  | Import => 2
  | Function => 3
  | Table => 4
  | Memory => 5
  | Global => 6
  | Export => 7
  | Start => 8
  | Element => 9
  | Code => 10
  | Data => 11;

let get_wasm_sections = (~reset=false, inchan) => {
  let orig_pos = pos_in(inchan);
  let read_boilerplate = () => {
    let magic = [0x00, 0x61, 0x73, 0x6D];
    let version = [0x01, 0x00, 0x00, 0x00];
    let expect = (strbuilder, b) => {
      let actual = input_byte(inchan);
      if (actual != b) {
        raise(Invalid_argument(strbuilder(b, actual)));
      };
    };

    let magic_strbuilder = (b, actual) =>
      Printf.sprintf(
        "Error reading WebAssembly magic number. Expected byte 0x%02x; found 0x%02x",
        b,
        actual,
      );
    let version_strbuilder = (b, actual) =>
      /* TODO: This should probably warn, not fail */
      Printf.sprintf(
        "Error reading WebAssembly version. Expected byte 0x%02x; found 0x%02x",
        b,
        actual,
      );
    List.iter(expect(magic_strbuilder), magic);
    List.iter(expect(version_strbuilder), version);
  };

  let next_section = () =>
    try({
      let sec_type = section_type_of_int(input_byte(inchan));
      let size = Int32.to_int(read_leb128_u32_input(inchan));
      let offset = pos_in(inchan);
      let (sec_type, true_offset, true_size) =
        switch (sec_type) {
        | Custom(_) =>
          let name_len = Int32.to_int(read_leb128_u32_input(inchan));
          let name = really_input_string(inchan, name_len);
          /*let bstr bs = "[" ^ (String.concat ", " (List.map (Printf.sprintf "0x%02x") bs)) ^ "]" in
            Printf.eprintf "read: size: %d; name_len: %d; name: %s\n"
              size name_len (bstr (List.map int_of_char @@ String_util.explode name));*/
          let name =
            utf8_encode(
              List.map(int_of_char) @@ String_utils.explode(name),
            );
          let true_offset = pos_in(inchan);
          (Custom(name), true_offset, size - (true_offset - offset));
        | s => (s, offset, size)
        };

      seek_in(inchan, offset + size);
      Some({sec_type, offset: true_offset, size: true_size});
    }) {
    | End_of_file => None
    };

  let rec collect_sections = acc =>
    switch (next_section()) {
    | Some(sec) => collect_sections([sec, ...acc])
    | None => acc
    };

  read_boilerplate();

  let ret = List.rev(collect_sections([]));
  if (reset) {
    seek_in(inchan, orig_pos);
  };
  ret;
};

let get_grain_custom_info = inchan =>
  try({
    let rec check_magic = remaining =>
      switch (remaining) {
      | [] => true
      | [hd, ...tl] => input_byte(inchan) == hd && check_magic(tl)
      };
    if (!check_magic(grain_magic)) {
      None;
    } else {
      let version = read_abi_version(inchan);
      let section_name_length = Int32.to_int(read_int32(inchan));
      let section_name_bytes = Bytes.create(section_name_length);
      if (input(inchan, section_name_bytes, 0, section_name_length)
          != section_name_length) {
        raise(End_of_file);
      };
      let section_name = Bytes.to_string(section_name_bytes);
      Some((version, section_name));
    };
  }) {
  | End_of_file => None
  };

let serialize_grain_custom_info = (sec_name, abi_version) => {
  let sec_bytes = Bytes.of_string(sec_name);
  let buf = Buffer.create(Bytes.length(sec_bytes) + 4 + 4 * 3 + 4);
  List.iter(b => Buffer.add_char(buf, char_of_int(b)), grain_magic);
  Buffer.add_bytes(buf, serialize_abi_version(abi_version));
  Buffer.add_bytes(buf, serialize_int32(Bytes.length(sec_bytes)));
  Buffer.add_bytes(buf, sec_bytes);
  Buffer.to_bytes(buf);
};

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

module BinarySection =
       (Spec: BinarySectionSpec)
       : (BinarySectionSig with type t = Spec.t) => {
  type t = Spec.t;

  let load = (~preserve=false, inchan) => {
    let orig_pos = pos_in(inchan);
    let sections =
      List.filter(
        ({sec_type}) => sec_type == Custom(Spec.name),
        get_wasm_sections(inchan),
      );
    let rec process = sections =>
      switch (sections) {
      | [] => None
      | [{offset, size}, ...tl] =>
        seek_in(inchan, offset);
        switch (get_grain_custom_info(inchan)) {
        | Some((abi_version, name))
            when name == Spec.name && Spec.accepts_version(abi_version) =>
          /* Now we're at the start of the section. Time to read */
          let realsize = size - (pos_in(inchan) - offset);
          let bytes = Bytes.create(realsize);
          if (input(inchan, bytes, 0, realsize) == realsize) {
            Some(Spec.deserialize(bytes));
          } else {
            process(tl);
          };
        | _ => process(tl)
        };
      };

    let ret = process(sections);
    if (preserve) {
      seek_in(inchan, orig_pos);
    };
    ret;
  };

  let serialize = value => {
    let val_bytes = Spec.serialize(value);
    let header_bytes = serialize_grain_custom_info(Spec.name, latest_abi);
    let sep = Bytes.empty;
    Bytes.concat(sep, [header_bytes, val_bytes]);
  };
};

let () =
  Printexc.register_printer(exc =>
    switch (exc) {
    | [@implicit_arity] MalformedLEB(signed, maxbits) =>
      let sstr = if (signed) {"(signed)"} else {"(unsigned)"};
      Some(
        Printf.sprintf(
          "Malformed LEB-encoded %s number (expected at most %d bits)",
          sstr,
          maxbits,
        ),
      );
    | [@implicit_arity] MalformedSectionType(tag, Some(pos)) =>
      Some(
        Printf.sprintf(
          "Malformed WASM section tag at position %d: %d",
          pos,
          tag,
        ),
      )
    | [@implicit_arity] MalformedSectionType(tag, None) =>
      Some(Printf.sprintf("Malformed WASM section tag: %d", tag))
    | _ => None
    }
  );
