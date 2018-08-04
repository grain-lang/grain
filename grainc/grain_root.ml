open Cmdliner
open Printf
open Grain_root_utils.Root_utils

let print_root_and_exit _ =
  if not (Printexc.backtrace_status()) && !Grain_utils.Config.verbose then
    Printexc.record_backtrace true;
  infer_root_if_needed();
  printf "%s\n" (Option.get !Grain_utils.Config.grain_root);
  `Ok ()
;;

let help_flag =
  let doc = "Show this help message" in
  Arg.(value & flag & info ["h"] ~doc)

(* A hack to make this work. FIXME: find a better way to do this *)
let print_root_cmd =
  Term.(ret (const print_root_and_exit $ help_flag)),
  Term.info "print-root"

let cmd =
  let doc = sprintf "Compile Grain programs" in
  Term.(ret (Grain_utils.Config.with_cli_options print_root_and_exit $ help_flag)),
  Term.info (Sys.argv.(0)) ~version:"1.0.0" ~doc

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0

