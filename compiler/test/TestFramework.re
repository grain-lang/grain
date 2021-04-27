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

let starts_with = (string, prefix) => {
  let prefixLength = String.length(prefix);
  let stringLength = String.length(string);
  if (stringLength < prefixLength) {
    false;
  } else {
    String.sub(string, 0, prefixLength) == prefix;
  };
};

// Recursive readdir
let rec readdir = (dir, excludes) => {
  Sys.readdir(dir)
  |> Array.fold_left(
       (results, filename) => {
         let filepath = Filename.concat(dir, filename);
         Sys.is_directory(filepath)
         && List.for_all(
              exclude => !starts_with(filename, exclude),
              excludes,
            )
           ? Array.append(results, readdir(filepath, excludes))
           : Array.append(results, [|filepath|]);
       },
       [||],
     );
};

let clean_stdlib = stdlib_dir =>
  Array.iter(
    file =>
      if (Filename.check_suffix(file, ".gr.wasm")) {
        Sys.remove(file);
      },
    readdir(stdlib_dir, []),
  );

let () = {
  /*** Override default stdlib location to use development version of stdlib */
  let stdlib_dir = Unix.getenv("GRAIN_STDLIB");
  let stdlib_dir = Grain_utils.Files.derelativize(stdlib_dir);
  Grain_utils.Config.stdlib_dir := Some(stdlib_dir);
  clean_stdlib(stdlib_dir);
  Grain_utils.Config.debug := true;
  Grain_utils.Config.wat := true;
  Grain_utils.Config.color_enabled := false;
  Printexc.record_backtrace(true);
};

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir:
        Filename.concat(Sys.getenv("GRAIN_TEST_ROOT"), "__snapshots__"),
      projectDir: "./",
    });
});

let cli = () => {
  // This hack uses the Dune profile value to pass along a test filter.
  let filter =
    switch (Sys.getenv("GRAIN_TEST_FILTER")) {
    | "release" => None
    | filter => Some(filter)
    };
  let overrideSnapshots =
    switch (Sys.getenv_opt("GRAIN_TEST_UPDATE_SNAPSHOTS")) {
    | Some("true") => true
    | _ => false
    };
  let config =
    Rely.RunConfig.(
      initialize()
      |> updateSnapshots(overrideSnapshots)
      |> withTestNamePattern(filter)
    );
  run(config);
};
