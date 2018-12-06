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
        Format.pp_flush_formatter formatter;
        let s = Buffer.contents buf in
        Buffer.reset buf;
        Some (s))

let all_tests = [
  (* Test_concatlist.tests; *)
  Test_end_to_end.tests;
  (* Test_wasm_utils.tests; *)
]

let () =
  (** Override default stdlib location to use development version of stdlib *)
  let grain_root = BatFile.with_file_in "grain-root.txt" BatInnerIO.read_all in
  let grain_root = Grain_utils.Files.derelativize grain_root in
  Grain_utils.Config.grain_root := Some(grain_root);
  let stdlib = Grain_utils.Config.stdlib_directory() in
  Option.may (fun x ->
      ignore(Grain.Compile.compile_file ~outfile:(x ^ "/" ^ "lists.wasm") (x ^ "/" ^ "lists.grlib"))
    )
    stdlib;
  Grain_utils.Config.debug := true;
  Printexc.record_backtrace true;
  run_test_tt_main ("All Tests" >::: all_tests)
;;
