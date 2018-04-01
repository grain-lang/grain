open OUnit2
open Printf
open ExtLib

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
  Test_concatlist.tests;
  Test_end_to_end.tests;
]

let () =
  (** Override default stdlib location to use development version of stdlib *)
  Grain.Config.set_grain_root (BatFile.with_file_in "grain-root.txt" BatInnerIO.read_all);
  Grain_utils.Config.grain_root := Some(BatFile.with_file_in "grain-root.txt" BatInnerIO.read_all);
  run_test_tt_main ("All Tests" >::: all_tests)
;;
