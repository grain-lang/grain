open Grain_typed;
open Grain_utils;
open Compmod;

let emit_module = ({asm, signature}, outfile) => {
  Files.ensure_parent_directory_exists(outfile);
  if (Config.debug^) {
    let sig_string =
      Sexplib.Sexp.to_string_hum(Cmi_format.sexp_of_cmi_infos(signature));
    let sig_file = Files.replace_extension(outfile, "modsig");
    let oc = open_out(sig_file);
    output_string(oc, sig_string);
    close_out(oc);
  };
  if (Config.wat^) {
    if (Grain_utils.Config.color_enabled^) {
      Binaryen.Settings.set_colors_enabled(true);
    } else {
      Binaryen.Settings.set_colors_enabled(false);
    };
    let asm_string = Binaryen.Module.write_text(asm);
    let wat_file = Files.replace_extension(outfile, "wat");
    let oc = open_out(wat_file);
    output_string(oc, asm_string);
    close_out(oc);
  };
  let source_map_name =
    if (Config.source_map^) {
      Some(Filename.basename(outfile) ++ ".map");
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
