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

let compile_file debug cdebug unsound_opts name outfile =
  let input_file = open_in name in
  let runnable = true in
  let opts = {Compile.default_compile_options with
              sound_optimizations=(not unsound_opts);
              optimizations_enabled=(not debug);
              verbose=cdebug
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

let help_flag =
  let doc = "Show this help message" in
  Arg.(value & flag & info ["h"] ~doc)

let help_cmd =
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ help_flag)),
  Term.info "help"

let cmd =
  let doc = sprintf "Compile %s programs" language_name in
  Term.(ret (const compile_file $ debug $ compiler_debug $ unsound_opts $ input_filename $ output_filename)),
  Term.info (Sys.argv.(0)) ~version:"1.0.0" ~doc

(*let () =
  let name = Sys.argv.(1) in
  let input_file = open_in name in
  match compile_file_to_string name true input_file with
  | Left errs ->
     printf "Errors:\n%s\n" (ExtString.String.join "\n" (print_errors errs))
  | Right program -> printf "%s\n" program;;*)

let () = match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
