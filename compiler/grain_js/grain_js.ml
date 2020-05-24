open Grain
open Compile
open Printf
open Lexing
open Filename

let () =
  Printexc.register_printer (fun exc ->
      match Grain_parsing.Location.error_of_exn exc with
      | None -> None
      | Some `Already_displayed -> None
      | Some (`Ok err) ->
        let buf = Buffer.create 512 in
        let formatter = Format.formatter_of_buffer buf in
        Format.fprintf formatter "@[%a@]@." Grain_parsing.Location.report_error err;
        Format.pp_print_flush formatter ();
        let s = Buffer.contents buf in
        Buffer.reset buf;
        Some (s))


(** `remove_extension` new enough that we should just use this *)
let safe_remove_extension name =
  try
    Filename.chop_extension name
  with
  | Invalid_argument _ -> name

let default_output_filename name = safe_remove_extension name ^ ".wasm"

let default_assembly_filename name = safe_remove_extension name ^ ".wast"

let compile_file name outfile_arg =
  Grain_utils.Config.base_path := dirname name;
  if not (Printexc.backtrace_status()) && !Grain_utils.Config.verbose then
    Printexc.record_backtrace true;
  begin
    try
      let outfile = Option.default (default_output_filename name) outfile_arg in
      ignore (Compile.compile_file ~outfile name)
    with exn ->
      let bt = if Printexc.backtrace_status() then Some(Printexc.get_backtrace()) else None in
      Grain_parsing.Location.report_exception Format.err_formatter exn;
      Option.may (fun s -> prerr_string "Backtrace:\n"; prerr_string s; prerr_string "\n") bt;
      exit 2
  end;
  `Ok ()
;;
