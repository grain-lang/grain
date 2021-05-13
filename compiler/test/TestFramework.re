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

let clean_output = output =>
  if (Sys.file_exists(output)) {
    Array.iter(Sys.remove, readdir(output, []));
  };

let () = {
  /*** Override default stdlib location to use development version of stdlib */
  let stdlib_dir = Unix.getenv("GRAIN_STDLIB");
  let stdlib_dir = Grain_utils.Files.derelativize(stdlib_dir);
  Grain_utils.Config.stdlib_dir := Some(stdlib_dir);
  clean_stdlib(stdlib_dir);
  clean_output("test/output");
  Grain_utils.Config.debug := true;
  Grain_utils.Config.wat := true;
  Grain_utils.Config.color_enabled := false;
  Printexc.record_backtrace(true);
};

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "test/__snapshots__",
      projectDir: "test",
    });
});
