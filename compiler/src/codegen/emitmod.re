open Grain_typed;
open Grain_utils;
open Compmod;

let emit_module = ({asm, signature}, outfile) => {
  Files.ensure_parent_directory_exists(outfile);
  if (Config.debug^) {
    let asm_string = Binaryen.Module.write_text(asm);
    let sig_string =
      Sexplib.Sexp.to_string_hum(Cmi_format.sexp_of_cmi_infos(signature));
    let wast_file = outfile ++ ".wast";
    let sig_file = outfile ++ ".modsig";
    let oc = open_out(wast_file);
    output_string(oc, asm_string);
    close_out(oc);
    let oc = open_out(sig_file);
    output_string(oc, sig_string);
    close_out(oc);
  };
  let source_map_name =
    if (Config.source_map^) {
      Some(Filename.basename(outfile) ++ ".map");
    } else {
      None;
    };
  let (encoded, map) = Binaryen.Module.write(asm, source_map_name);
  let oc = open_out_bin(outfile);
  output_bytes(oc, encoded);
  close_out(oc);
  switch (map) {
  | Some(map) =>
    let oc = open_out_bin(outfile ++ ".map");
    output_string(oc, map);
    close_out(oc);
  | None => ()
  };
};
