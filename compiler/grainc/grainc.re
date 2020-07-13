open Grain;
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

/** `remove_extension` new enough that we should just use this */

let safe_remove_extension = name =>
  try(Filename.chop_extension(name)) {
  | Invalid_argument(_) => name
  };

let default_output_filename = name => safe_remove_extension(name) ++ ".wasm";

let default_assembly_filename = name =>
  safe_remove_extension(name) ++ ".wast";

let default_mashtree_filename = name =>
  safe_remove_extension(name) ++ ".mashtree";

let compile_file = (name, outfile_arg) => {
  Grain_utils.Config.base_path := dirname(name);
  if (!Printexc.backtrace_status() && Grain_utils.Config.verbose^) {
    Printexc.record_backtrace(true);
  };
  try({
    let outfile =
      Option.value(~default=default_output_filename(name), outfile_arg);
    if (Grain_utils.Config.debug^) {
      Compile.save_mashed(name, default_mashtree_filename(outfile));
    };
    ignore(Compile.compile_file(~outfile, name));
  }) {
  | exn =>
    let bt =
      if (Printexc.backtrace_status()) {
        Some(Printexc.get_backtrace());
      } else {
        None;
      };
    Grain_parsing.Location.report_exception(Format.err_formatter, exn);
    Option.iter(
      s => {
        prerr_string("Backtrace:\n");
        prerr_string(s);
        prerr_string("\n");
      },
      bt,
    );
    exit(2);
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

let input_filename = {
  let doc = sprintf("Grain source file to compile");
  let docv = "FILE";
  Arg.(
    required
    & pos(~rev=true, 0, some(non_dir_file), None)
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

let help_flag = {
  let doc = "Show this help message";
  Arg.(value & flag & info(["h"], ~doc));
};

let help_cmd = (
  Term.(ret(const(_ => `Help((`Pager, None))) $ help_flag)),
  Term.info("help"),
);

let cmd = {
  let doc = sprintf("Compile Grain programs");
  let version =
    switch (Build_info.V1.version()) {
    | None => "unknown"
    | Some(v) => Build_info.V1.Version.to_string(v)
    };
  (
    Term.(
      ret(
        Grain_utils.Config.with_cli_options(compile_file)
        $ input_filename
        $ output_filename,
      )
    ),
    Term.info(Sys.argv[0], ~version, ~doc),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
