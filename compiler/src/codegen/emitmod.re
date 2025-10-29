open Grain_typed;
open Grain_utils;
open Mashtree;

/*
 Grain object files follow this layout:

 | magic               | version length | version | cmi length | cmi   | mash_code   |
 | 0xF0 0x9F 0x8C 0xBE | 5              | 0.6.3   | 1024       | <cmi> | <mash_code> |
 */

type bad_object =
  | InvalidMagic
  | InvalidVersion
  | Corrupted;

exception BadObject(bad_object);

let emit_object = (mashed: mash_program, outfile) => {
  Grain_utils.Fs_access.ensure_parent_directory_exists(outfile);
  if (Config.debug^) {
    let mash_string =
      Sexplib.Sexp.to_string_hum @@ Mashtree.sexp_of_mash_program(mashed);
    let mashfile = Filepath.String.replace_extension(outfile, "mashtree");
    let oc = open_out(mashfile);
    output_string(oc, mash_string);
    close_out(oc);
  };

  let oc = open_out_bin(outfile);

  output_bytes(oc, Cmi_format.magic);
  let version_length = String.length(Config.version);
  output_binary_int(oc, version_length);
  output_string(oc, Config.version);
  let cmi = Marshal.to_bytes(mashed.signature, []);
  output_binary_int(oc, Bytes.length(cmi));
  output_bytes(oc, cmi);
  Marshal.to_channel(oc, mashed.mash_code, []);

  close_out(oc);
};

let load_object = (ic): mash_program => {
  let read_magic = Bytes.create(4);
  really_input(ic, read_magic, 0, 4);
  if (read_magic != Cmi_format.magic) {
    raise(BadObject(InvalidMagic));
  };
  let version_length = input_binary_int(ic);
  let read_version = really_input_string(ic, version_length);
  if (read_version != Config.version) {
    raise(BadObject(InvalidVersion));
  };
  let cmi_length = input_binary_int(ic);
  // JSOO messes up positioning when unmarshaling from a channel, so we read
  // the CMI fully into memory to preserve the correct position for the code
  let cmi_bytes = Bytes.create(cmi_length);
  really_input(ic, cmi_bytes, 0, cmi_length);
  let signature: Cmi_format.cmi_infos =
    try(Marshal.from_bytes(cmi_bytes, 0)) {
    | _ => raise(BadObject(Corrupted))
    };
  let mash_code: mash_code =
    try(input_value(ic)) {
    | _ => raise(BadObject(Corrupted))
    };

  {
    signature,
    mash_code,
  };
};
let load_object = object_file => {
  let ic = open_in_bin(object_file);
  let program =
    try(load_object(ic)) {
    | BadObject(_) as exn =>
      close_in(ic);
      raise(exn);
    | Failure(_)
    | End_of_file =>
      close_in(ic);
      raise(BadObject(Corrupted));
    };
  close_in(ic);
  program;
};

let emit_binary = (asm, signature, outfile) => {
  Fs_access.ensure_parent_directory_exists(outfile);
  if (Config.debug^) {
    let sig_string =
      Sexplib.Sexp.to_string_hum(Cmi_format.sexp_of_cmi_infos(signature));
    let sig_file = Filepath.String.replace_extension(outfile, "modsig");
    let oc = open_out(sig_file);
    output_string(oc, sig_string);
    close_out(oc);
  };
  if (Config.wat^) {
    Binaryen.Settings.set_colors_enabled(Grain_utils.Config.color_enabled^);
    let asm_string = Binaryen.Module.write_text(asm);
    let wat_file = Filepath.String.replace_extension(outfile, "wat");
    let oc = open_out(wat_file);
    output_string(oc, asm_string);
    close_out(oc);
  };
  switch (Config.profile^) {
  | Some(Release) => Binaryen.Settings.set_debug_info(false)
  | _ => Binaryen.Settings.set_debug_info(true)
  };
  let source_map_name =
    if (Config.source_map^) {
      Some(Filepath.String.basename(outfile) ++ ".map");
    } else {
      None;
    };
  let (encoded, map) = Binaryen.Module.write(asm, source_map_name);
  let oc = Fs_access.open_file_for_writing(outfile);
  output_bytes(oc, encoded);
  close_out(oc);
  switch (map) {
  | Some(map) =>
    let oc = Fs_access.open_file_for_writing(outfile ++ ".map");
    output_string(oc, map);
    close_out(oc);
  | None => ()
  };
};
