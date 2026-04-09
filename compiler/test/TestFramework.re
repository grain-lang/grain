open Grain_utils;

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

let test_dir = Fp.At.(Filepath.get_cwd() / "test");
let test_libs_dir = Fp.At.(test_dir / "test-libs");
let test_data_dir = Fp.At.(test_dir / "test-data");
let test_input_dir = Fp.At.(test_dir / "input");
let test_output_dir = Fp.At.(test_dir / "output");
let test_stdlib_dir = Fp.At.(test_dir / "stdlib");
let test_runtime_dir = Fp.At.(test_dir / "runtime");
let test_snapshots_dir = Fp.At.(test_dir / "__snapshots__");
let test_target_dir = Fp.At.(test_dir / "target");

let test_grainfmt_dir = Fp.At.(test_dir / "grainfmt");

let test_graindoc_dir = Fp.At.(test_dir / "graindoc");

let clean_dir = dir =>
  if (Sys.file_exists(Filepath.to_string(dir))) {
    Array.iter(Fs.rmExn, Fs_access.readdir(dir));
  };

let () = {
  let github_actions =
    try(Unix.getenv("GITHUB_ACTIONS") == "true") {
    | Not_found => false
    };
  if (github_actions) {
    Pastel.setMode(Pastel.Terminal);
  };
  let stdlib_dir = Unix.getenv("GRAIN_STDLIB");
  let stdlib_dir = Filepath.String.derelativize(stdlib_dir);
  Config.stdlib_dir := Some(stdlib_dir);
  Config.target_dir := test_target_dir;
  Config.project_root := test_dir;
  Config.libraries := [("test-libs", test_libs_dir)];
  clean_dir(test_target_dir);
  clean_dir(test_output_dir);
  Config.debug := true;
  Config.wat := true;
  Config.color_enabled := false;
  Printexc.record_backtrace(true);
};

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: Filepath.to_string(test_snapshots_dir),
      projectDir: Filepath.to_string(test_dir),
    });
});
