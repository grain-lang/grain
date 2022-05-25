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

let test_dir = Filepath.At.(Filepath.cwd() / "test");
let test_libs_dir = Filepath.At.(test_dir / "test-libs");
let test_input_dir = Filepath.At.(test_dir / "input");
let test_output_dir = Filepath.At.(test_dir / "output");
let test_stdlib_dir = Filepath.At.(test_dir / "stdlib");
let test_snapshots_dir = Filepath.At.(test_dir / "__snapshots__");

let test_formatter_in_dir = Filepath.At.(test_dir / "formatter_inputs");
let test_formatter_out_dir = Filepath.At.(test_dir / "formatter_outputs");

let clean_grain_output = stdlib_dir =>
  Array.iter(
    file => {
      let filename = Filepath.to_string(file);
      if (Filename.check_suffix(filename, ".gr.wasm")
          || Filename.check_suffix(filename, ".gr.wat")
          || Filename.check_suffix(filename, ".gr.modsig")) {
        Fs_access.remove(file);
      };
    },
    Fs_access.readdir(stdlib_dir),
  );

let clean_output = output =>
  if (Sys.file_exists(Filepath.to_string(output))) {
    Array.iter(Fs_access.remove, Fs_access.readdir(output));
  };

let () = {
  /*** Override default stdlib location to use development version of stdlib */
  let stdlib_dir = Unix.getenv("GRAIN_STDLIB");
  let stdlib_dir = Option.get(Filepath.from_string(stdlib_dir));
  Config.stdlib_dir := Some(Filepath.to_string(stdlib_dir));
  clean_grain_output(test_input_dir);
  clean_grain_output(stdlib_dir);
  clean_grain_output(test_libs_dir);
  clean_output(test_output_dir);
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
