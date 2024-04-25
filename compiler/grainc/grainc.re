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

let compile_file = (name, outfile_arg) => {
  if (!Printexc.backtrace_status() && Grain_utils.Config.verbose^) {
    Printexc.record_backtrace(true);
  };
  try({
    let outfile =
      Option.value(
        ~default=Compile.default_output_filename(name),
        outfile_arg,
      );
    let hook =
      if (Grain_utils.Config.statically_link^) {
        Compile.stop_after_assembled;
      } else {
        Compile.stop_after_object_file_emitted;
      };
    ignore(Compile.compile_file(~is_root_file=true, ~hook, ~outfile, name));
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
        Grain_utils.Config.with_cli_options(compile_file)
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
