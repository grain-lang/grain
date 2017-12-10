open Compile
open Runner
open Printf
open Lexing
open Types
open Filename
open Cmdliner

let language_name = "Grain"

(** `remove_extension` new enough that we should just use this *)
let safe_remove_extension name =
  try
    Filename.chop_extension name
  with
  | Invalid_argument _ -> name

let default_output_filename name = safe_remove_extension name ^ ".wasm"

let default_assembly_filename name = safe_remove_extension name ^ ".wast"

let compile_file debug cdebug unsound_opts name outfile no_stdlib extra_includes =
  let input_file = open_in name in
  let runnable = true in
  let opts = {Compile.default_compile_options with
              sound_optimizations=(not unsound_opts);
              optimizations_enabled=(not debug);
              verbose=cdebug;
              use_stdlib=(not no_stdlib);
              include_dirs=extra_includes
             } in
  let output = if runnable then
      compile_file_to_binary name opts debug input_file (Option.default (default_output_filename name) outfile)
    else
      match outfile with
      | Some(out) ->
        compile_file_to_assembly name opts input_file out
      | None ->
        match compile_file_to_string name opts input_file with
        | Left errs ->
          Left(ExtString.String.join "\n" (print_errors errs))
        | Right program -> (printf "%s\n" program; Right(program))  in
  match output with
  | Left (err) ->
    `Error (false, err)
  | Right prog -> `Ok ()
;;

(** Converter which checks that the given output filename is valid *)
let output_file_conv =
  let parse s =
    let s_dir = dirname s in
    match Sys.file_exists s_dir with
    | true -> if Sys.is_directory s_dir then `Ok s else `Error (sprintf "`%s' is not a directory" s_dir)
    | false -> `Error (sprintf "no `%s' directory" s_dir) in
  parse, Format.pp_print_string

let runnable =
  let doc = "Emit runnable program (deprecated)" in
  Arg.(value & flag & info ["r"; "runnable"] ~doc)

let debug =
  let doc = "Compile with debug information" in
  Arg.(value & flag & info ["g"] ~doc)
;;

let compiler_debug =
  let doc = "Print internal debug messages" in
  Arg.(value & flag & info ["cdebug"] ~doc)
;;

let unsound_opts =
  let doc = "Compile with optimizations which may remove errors" in
  Arg.(value & flag & info ["Ounsound"; "big-kid"] ~doc)
;;

let no_stdlib =
  let doc = sprintf "Disable the %s standard library." language_name in
  Arg.(value & flag & info ["no-stdlib"] ~doc)
;;

let input_filename =
  let doc = sprintf "%s source file to compile" language_name in
  let docv = "FILE" in
  Arg.(required & pos ~rev:true 0 (some non_dir_file) None & info [] ~docv ~doc)
;;

let output_filename =
  let doc = sprintf "Output filename" in
  let docv = "FILE" in
  Arg.(value & opt (some output_file_conv) None & info ["o"] ~docv ~doc)
;;

let extra_includes =
  let doc = "Extra library include directories" in
  let docv = "DIR" in
  Arg.(value & opt (list dir) [] & info ["I"] ~docv ~doc)
;;

let help_flag =
  let doc = "Show this help message" in
  Arg.(value & flag & info ["h"] ~doc)

let help_cmd =
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ help_flag)),
  Term.info "help"

let cmd =
  let doc = sprintf "Compile %s programs" language_name in
  Term.(ret (const compile_file $ debug $ compiler_debug $ unsound_opts $ input_filename $ output_filename $ no_stdlib $ extra_includes)),
  Term.info (Sys.argv.(0)) ~version:"1.0.0" ~doc

let () =
  let open BatPathGen.OfString in
  match Config.get_grain_root() with
  (* Prefer environment variable over inferred path *)
  | Some(_) -> ()
  | None ->
    begin
      let grainc_dir = parent @@ of_string Sys.argv.(0) in
      let as_abs =
        if is_absolute grainc_dir then
          grainc_dir
        else
          normalize @@ concat (of_string @@ Unix.getcwd()) grainc_dir in
      Config.set_grain_root @@ to_string @@ parent as_abs
    end;
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
