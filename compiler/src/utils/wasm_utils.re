/** Utilities for interfacing with WebAssembly */
open Sexplib.Conv;

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

let identity: 'a. 'a => 'a = x => x;
let i32_of_u64 = Int64.to_int32;

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
        let byte = next_byte();
        /* Check for invalid input */
        assert(byte >= 0 && byte < 256);

        switch (signed, byte) {
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

let read_leb128_i32 = (bytesrc): int32 =>
  read_leb128(~signed=true, ~maxbits=32, ~conv=i32_of_u64, bytesrc);
let read_leb128_i32_input = inchan =>
  read_leb128_i32(() => input_byte(inchan));
let read_leb128_u32 = (bytesrc): int32 =>
  read_leb128(~signed=false, ~maxbits=32, ~conv=i32_of_u64, bytesrc);
let read_leb128_u32_input = inchan =>
  read_leb128_u32(() => input_byte(inchan));
let read_leb128_i64 = (bytesrc): int64 =>
  read_leb128(~signed=true, ~maxbits=64, ~conv=identity, bytesrc);
let read_leb128_i64_input = inchan =>
  read_leb128_i64(() => input_byte(inchan));
let read_leb128_u64 = (bytesrc): int64 =>
  read_leb128(~signed=false, ~maxbits=64, ~conv=identity, bytesrc);
let read_leb128_u64_input = inchan =>
  read_leb128_u64(() => input_byte(inchan));

let utf8_encode = ints => {
  let buf = Buffer.create(14);
  List.iter(i => {Buffer.add_utf_8_uchar(buf, Uchar.of_int(i))}, ints);
  Buffer.contents(buf);
};

let section_type_of_int = (~pos=?, ~name=?) =>
  fun
  | 0 => Custom(Option.value(~default="", name))
  | 1 => Type
  | 2 => Import([])
  | 3 => Function
  | 4 => Table
  | 5 => Memory
  | 6 => Global
  | 7 => Export([])
  | 8 => Start
  | 9 => Element
  | 10 => Code
  | 11 => Data
  | 12 => DataCount
  | n => raise(MalformedSectionType(n, pos));

let get_wasm_sections = (~reset=false, inchan) => {
  let orig_pos = pos_in(inchan);
  let read_boilerplate = () => {
    let magic = [0x00, 0x61, 0x73, 0x6D];
    let version = [0x01, 0x00, 0x00, 0x00];
    let expect = (strbuilder, b) => {
      // TODO: This can raise End_of_file. Should we really be letting that bubble all the way up?
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
      // TODO: This should probably warn, not fail
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
        | Import(_) =>
          let num_imports = Int32.to_int(read_leb128_u32_input(inchan));
          let rec read_import = () => {
            let mod_len = Int32.to_int(read_leb128_u32_input(inchan));
            let mod_ = really_input_string(inchan, mod_len);
            let name_len = Int32.to_int(read_leb128_u32_input(inchan));
            let name = really_input_string(inchan, name_len);
            let import_type =
              switch (input_byte(inchan)) {
              | 0 =>
                let _import_id = read_leb128_u32_input(inchan);
                WasmFunction;
              | 1 =>
                let _reftype = input_byte(inchan);
                switch (input_byte(inchan)) {
                | 0 =>
                  let _min = read_leb128_u32_input(inchan);
                  ();
                | 1 =>
                  let _min = read_leb128_u32_input(inchan);
                  let _max = read_leb128_u32_input(inchan);
                  ();
                | _ => failwith("Unknown limit type")
                };
                WasmTable;
              | 2 =>
                switch (input_byte(inchan)) {
                | 0 =>
                  let _min = read_leb128_u32_input(inchan);
                  ();
                | 1 =>
                  let _min = read_leb128_u32_input(inchan);
                  let _max = read_leb128_u32_input(inchan);
                  ();
                | _ => failwith("Unknown limit type")
                };
                WasmMemory;
              | 3 =>
                let _valtype = input_byte(inchan);
                let _mut = input_byte(inchan);
                WasmGlobal;
              | _ => failwith("Unknown import type")
              };
            (import_type, mod_, name);
          };
          let true_offset = pos_in(inchan);
          (
            Import(List.init(num_imports, _ => read_import())),
            true_offset,
            size - (true_offset - offset),
          );
        | Export(_) =>
          let num_exports = Int32.to_int(read_leb128_u32_input(inchan));
          let rec read_export = () => {
            let name_len = Int32.to_int(read_leb128_u32_input(inchan));
            let name = really_input_string(inchan, name_len);
            let export_type =
              switch (input_byte(inchan)) {
              | 0 => WasmFunction
              | 1 => WasmTable
              | 2 => WasmMemory
              | 3 => WasmGlobal
              | _ => failwith("Unknown export type")
              };
            let _export_idx = read_leb128_u32_input(inchan);
            (export_type, name);
          };
          let true_offset = pos_in(inchan);
          (
            Export(List.init(num_exports, _ => read_export())),
            true_offset,
            size - (true_offset - offset),
          );
        | s => (s, offset, size)
        };

      seek_in(inchan, offset + size);
      Some({
        sec_type,
        offset: true_offset,
        size: true_size,
      });
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

let () =
  Printexc.register_printer(exc =>
    switch (exc) {
    | MalformedSectionType(tag, Some(pos)) =>
      Some(
        Printf.sprintf(
          "Malformed WASM section tag at position %d: %d",
          pos,
          tag,
        ),
      )
    | MalformedSectionType(tag, None) =>
      Some(Printf.sprintf("Malformed WASM section tag: %d", tag))
    | _ => None
    }
  );
