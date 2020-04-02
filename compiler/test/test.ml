open OUnit2
open Printf
open Extlib

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

let compile_stdlib stdlib_dir =
  Array.iter (fun file ->
    if Filename.check_suffix file ".gr" then begin
      let infile = Printf.sprintf "%s/%s" stdlib_dir file in
      let outfile = Printf.sprintf "%s/%s" stdlib_dir (Filename.remove_extension file ^ ".wasm") in
      ignore @@ Grain.Compile.compile_file ~outfile infile
    end
  ) (Sys.readdir stdlib_dir)

let all_tests = [
  Test_concatlist.tests;
  Test_end_to_end.tests;
  Test_wasm_utils.tests;
]

let () =
  (** Override default stdlib location to use development version of stdlib *)
  let stdlib_dir = Unix.getenv "GRAIN_STDLIB" in
  let stdlib_dir = Grain_utils.Files.derelativize stdlib_dir in
  Grain_utils.Config.stdlib_dir := Some(stdlib_dir);
  compile_stdlib stdlib_dir;
  Grain_utils.Config.debug := true;
  Printexc.record_backtrace true;
  run_test_tt_main ("All Tests" >::: all_tests)
;;
