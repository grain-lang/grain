open Grain_typed;
open Grain_utils;
open Compmod;

let emit_module = ({asm, signature}, outfile) => {
  Files.ensure_parent_directory_exists(outfile);
  if (Config.debug^) {
    let asm_string = Wasm.Sexpr.to_string(80, Wasm.Arrange.module_(asm));
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
  let encoded = Wasm.Encode.encode(asm);
  let oc = open_out_bin(outfile);
  output_string(oc, encoded);
  Cmi_format.output_cmi(outfile, oc, signature);
  close_out(oc);
};
