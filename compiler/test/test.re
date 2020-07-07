open OUnit2;
open Printf;
open Extlib;

let () =
  Printexc.register_printer(exc =>
    switch (Grain_parsing.Location.error_of_exn(exc)) {
    | None => None
    | Some(`Already_displayed) => None
    | Some(`Ok(err)) =>
      let buf = Buffer.create(512);
      let formatter = Format.formatter_of_buffer(buf);
      Format.fprintf(
        formatter,
        "@[%a@]@.",
        Grain_parsing.Location.report_error,
        err,
      );
      Format.pp_print_flush(formatter, ());
      let s = Buffer.contents(buf);
      Buffer.reset(buf);
      Some(s);
    }
  );

// Recursive readdir
let rec readdir = (dir, excludes) => {
  Sys.readdir(dir)
  |> Array.fold_left(
       (results, filename) => {
         let filepath = Filename.concat(dir, filename);
         (Sys.is_directory(filepath) && List.for_all((exclude) => !BatString.starts_with(filename, exclude), excludes))
           ? Array.append(results, readdir(filepath, excludes))
           : Array.append(results, [|filepath|]);
       },
       [||],
     );
};

let clean_stdlib = stdlib_dir =>
  Array.iter(
    (file) =>
      if (Filename.check_suffix(file, ".wasm")) {
        Sys.remove(file);
      },
    readdir(stdlib_dir, ["stdlib-external"]),
  );

let all_tests = [
  Test_concatlist.tests,
  Test_end_to_end.tests,
  Test_wasm_utils.tests,
];

let () = {
  /*** Override default stdlib location to use development version of stdlib */
  let stdlib_dir = Unix.getenv("GRAIN_STDLIB");
  let stdlib_dir = Grain_utils.Files.derelativize(stdlib_dir);
  Grain_utils.Config.stdlib_dir := Some(stdlib_dir);
  clean_stdlib(stdlib_dir);
  Grain_utils.Config.debug := true;
  Printexc.record_backtrace(true);
  run_test_tt_main("All Tests" >::: all_tests);
};
