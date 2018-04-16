open Grain_typed
open Grain_utils
open Compmod


let emit_module {asm; signature} outfile =
  Files.ensure_parent_directory_exists outfile;
  if !Config.debug then begin
    let asm_string = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ asm) in
    let sig_string = Sexplib.Sexp.to_string_hum (Cmi_format.sexp_of_cmi_infos signature) in
    let wast_file = outfile ^ ".wast" in
    let sig_file = outfile ^ ".modsig" in
    let oc = open_out wast_file in
    output_string oc asm_string;
    close_out oc;
    let oc = open_out sig_file in
    output_string oc sig_string;
    close_out oc
  end;
  let encoded = Wasm.Encode.encode asm in
  let oc = open_out_bin outfile in
  output_string oc encoded;
  Cmi_format.output_cmi outfile oc signature;
  close_out oc

