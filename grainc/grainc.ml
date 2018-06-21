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
        Format.pp_flush_formatter formatter;
        let s = Buffer.contents buf in
        Buffer.reset buf;
        Some (s))

open BatPathGen.OfString

let make_absolute d =
  if is_absolute d then
    d
  else
    normalize @@ concat (of_string (Unix.getcwd())) d

let could_be_grain_root d =
  let open Infix in
  List.for_all (fun x -> Sys.file_exists (to_string @@ (of_string d) /: x))
    ["bin"; "lib"]

let try_infer_grain_root grainc_path =
  if Sys.file_exists grainc_path then begin
    try
      let grainc_dir = parent @@ of_string grainc_path in
      let as_abs = make_absolute grainc_dir in
      let parent = to_string @@ parent as_abs in
      let could_be = could_be_grain_root parent in
      if could_be then
        Grain_utils.Config.grain_root := Some(parent);
      could_be
    with _ -> false
  end
  else
    false

let path_var_sep = if Sys.os_type = "Win32" then ';' else ':'

let infer_root_from_argv() =
  match Sys.argv.(0) with
  | x when not(BatString.ends_with x "grainc") -> false
  | exec_path when exec_path <> "grainc" ->
    (* Ends with 'grainc' and isn't 'grainc'. Likely an absolute or relative path *)
    try_infer_grain_root exec_path
  | _ -> (* argv[0] is exactly 'grainc'. Look for it on $PATH *)
    let path_var = Sys.getenv_opt "PATH" in
    match path_var with
    | None ->
      prerr_string "WARNING: When locating grain root, we found that there \
                    is no PATH environment variable. This seems strange!\n";
      false
    | Some(path) ->
      (* Got the PATH variable. Check all directories *)
      let grainc_path dir =
        let open Infix in
        to_string @@ (of_string dir) /: "grainc" in
      let path_dirs = String.split_on_char path_var_sep path in
      (List.exists try_infer_grain_root (List.map grainc_path path_dirs)) || begin
        (* Last resort. Check if 'grainc' is in the cwd. *)
        let open Infix in
        let cwd = Sys.getcwd() in
        let grainc_path = to_string @@ (of_string cwd) /: "grainc" in
        try_infer_grain_root grainc_path
      end


let infer_root_from_running_grainc() =
  try
    let pid = Unix.getpid() in
    let grainc_path = Unix.readlink (sprintf "/proc/%d/exe" pid) in
    try_infer_grain_root grainc_path
  with _ ->
    false

let infer_root() =
  let found = infer_root_from_argv() || infer_root_from_running_grainc() in
  if not found then
    prerr_string "Failed to find Grain installation root. \
                  Please set the GRAIN_ROOT environment variable \
                  in order to use the standard library.\n"

let infer_root_if_needed() =
  match !Grain_utils.Config.grain_root with
  (* Prefer environment variable over inferred path *)
  | Some(_) -> ()
  | None ->
    infer_root()

(** `remove_extension` new enough that we should just use this *)
let safe_remove_extension name =
  try
    Filename.chop_extension name
  with
  | Invalid_argument _ -> name

let default_output_filename name = safe_remove_extension name ^ ".wasm"

let default_assembly_filename name = safe_remove_extension name ^ ".wast"

let compile_file name outfile_arg =
  if not (Printexc.backtrace_status()) && !Grain_utils.Config.verbose then
    Printexc.record_backtrace true;
  infer_root_if_needed();
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
