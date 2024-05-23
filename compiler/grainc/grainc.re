open Grain;
open Grain_depgraph;
open Grain_typed;
open Compile;
open Printf;
open Lexing;
open Filename;
open Cmdliner;

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

let error_wrapped = f =>
  try(f()) {
  | exn =>
    let bt =
      if (Printexc.backtrace_status()) {
        Some(Printexc.get_backtrace());
      } else {
        None;
      };
    Grain_parsing.Location.report_exception(Format.err_formatter, exn);
    Option.iter(
      s =>
        if (Grain_utils.Config.debug^) {
          prerr_string("Backtrace:\n");
          prerr_string(s);
          prerr_string("\n");
        },
      bt,
    );
    exit(2);
  };

let compile_file = (~outfile=?, filename) => {
  let outfile =
    Option.value(
      ~default=Compile.default_object_filename(filename),
      outfile,
    );
  ignore(Compile.compile_file(~outfile, filename));
};
let compile_file = (~outfile=?, filename) =>
  error_wrapped(() => compile_file(~outfile?, filename));

let grainc = (single_file_mode, name, outfile) => {
  Grain_utils.Config.set_root_config();

  if (!Printexc.backtrace_status() && Grain_utils.Config.verbose^) {
    Printexc.record_backtrace(true);
  };

  if (single_file_mode) {
    compile_file(~outfile?, name);
  } else {
    switch (Grain_utils.Config.wasi_polyfill^) {
    | Some(name) =>
      Grain_utils.Config.preserve_config(() => {
        Grain_utils.Config.compilation_mode := Grain_utils.Config.Runtime;
        Dependencies.load_dependency_graph(name);
        let to_compile = Dependencies.get_out_of_date_dependencies();
        List.iter(compile_file, to_compile);
        compile_file(name);
      })
    | None => ()
    };

    Dependencies.load_dependency_graph(name);
    let to_compile = Dependencies.get_out_of_date_dependencies();
    List.iter(compile_file, to_compile);
    compile_file(name);

    if (Grain_utils.Config.statically_link^) {
      let main_object = Compile.default_object_filename(name);
      let outfile =
        Option.value(~default=Compile.default_wasm_filename(name), outfile);
      let dependencies = Dependencies.get_dependencies();

      Link.link(~main_object, ~outfile, dependencies);
    };
  };

  `Ok();
};

/** Converter which checks that the given output filename is valid */

let output_file_conv = {
  let parse = s => {
    let s_dir = dirname(s);
    Sys.file_exists(s_dir)
      ? if (Sys.is_directory(s_dir)) {
          `Ok(s);
        } else {
          `Error(sprintf("`%s' is not a directory", s_dir));
        }
      : `Error(sprintf("no `%s' directory", s_dir));
  };
  (parse, Format.pp_print_string);
};

let input_file_conv = {
  open Arg;
  let (prsr, prntr) = non_dir_file;

  (filename => prsr(filename), prntr);
};

let input_filename = {
  let doc = sprintf("Grain source file to compile");
  let docv = "FILE";
  Arg.(
    required
    & pos(~rev=true, 0, some(input_file_conv), None)
    & info([], ~docv, ~doc)
  );
};

let output_filename = {
  let doc = sprintf("Output filename");
  let docv = "FILE";
  Arg.(
    value & opt(some(output_file_conv), None) & info(["o"], ~docv, ~doc)
  );
};

let single_file_mode = {
  let doc = sprintf("Compile a single file without compiling dependencies");
  Arg.(value & vflag(false, [(true, info(["single-file"], ~doc))]));
};

let cmd = {
  let doc = sprintf("Compile Grain programs");
  let version =
    switch (Build_info.V1.version()) {
    | None => "unknown"
    | Some(v) => Build_info.V1.Version.to_string(v)
    };
  Cmd.v(
    Cmd.info(Sys.argv[0], ~version, ~doc),
    Term.(
      ret(
        Grain_utils.Config.with_cli_options(grainc)
        $ single_file_mode
        $ input_filename
        $ output_filename,
      )
    ),
  );
};

let () =
  switch (Cmd.eval_value(cmd)) {
  | Error(_) => exit(1)
  | _ => ()
  };
