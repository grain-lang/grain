open Grain
open Compile
open Printf
open Lexing
open Filename
open Cmdliner

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

(** Converter which checks that the given output filename is valid *)
let output_file_conv =
  let parse s =
    let s_dir = dirname s in
    match Sys.file_exists s_dir with
    | true -> if Sys.is_directory s_dir then `Ok s else `Error (sprintf "`%s' is not a directory" s_dir)
    | false -> `Error (sprintf "no `%s' directory" s_dir) in
  parse, Format.pp_print_string

let input_filename =
  let doc = sprintf "Grain source file to compile" in
  let docv = "FILE" in
  Arg.(required & pos ~rev:true 0 (some non_dir_file) None & info [] ~docv ~doc)
;;

let output_filename =
  let doc = sprintf "Output filename" in
  let docv = "FILE" in
  Arg.(value & opt (some output_file_conv) None & info ["o"] ~docv ~doc)
;;

let help_flag =
  let doc = "Show this help message" in
  Arg.(value & flag & info ["h"] ~doc)

let help_cmd =
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ help_flag)),
  Term.info "help"



let cmd =
  let doc = sprintf "Compile Grain programs" in
  Term.(ret (Grain_utils.Config.with_cli_options compile_file $ input_filename $ output_filename)),
  Term.info (Sys.argv.(0)) ~version:"1.0.0" ~doc

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
