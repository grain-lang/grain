open Cmdliner;
open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Filename;

let compile_parsed = filename => {
  Grain_utils.Config.base_path := dirname(filename);

  switch (Compile.compile_file(~hook=stop_after_parse, filename)) {
  | exception exn =>
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
  | {cstate_desc: Parsed(parsed_program)} => `Ok(parsed_program)
  | _ => `Error((false, "Invalid compilation state"))
  };
};

let format_code = (outfile, program: Parsetree.parsed_program) => {
  Reformat.reformat_ast(program);
  `Ok();
};

let grainformat = (outfile, program) =>
  try(format_code(outfile, program)) {
  | e => `Error((false, Printexc.to_string(e)))
  };

let input_file_conv = {
  open Arg;
  let (prsr, prntr) = non_dir_file;

  (
    filename => prsr(Grain_utils.Files.normalize_separators(filename)),
    prntr,
  );
};

let input_filename = {
  let doc = "Grain source file to format";
  let docv = "FILE";
  Arg.(
    required
    & pos(~rev=true, 0, some(input_file_conv), None)
    & info([], ~docv, ~doc)
  );
};

/** Converter which checks that the given output filename is valid */
let output_file_conv = {
  let parse = s => {
    let s_dir = dirname(s);
    Sys.file_exists(s_dir)
      ? if (Sys.is_directory(s_dir)) {
          `Ok(s);
        } else {
          `Error(Format.sprintf("`%s' is not a directory", s_dir));
        }
      : `Error(Format.sprintf("no `%s' directory", s_dir));
  };
  (parse, Format.pp_print_string);
};

let output_filename = {
  let doc = "Output filename";
  let docv = "FILE";
  Arg.(
    value & opt(some(output_file_conv), None) & info(["o"], ~docv, ~doc)
  );
};

let cmd = {
  open Term;

  let doc = "Reformat Grain source";
  let version =
    switch (Build_info.V1.version()) {
    | None => "unknown"
    | Some(v) => Build_info.V1.Version.to_string(v)
    };

  (
    Term.(
      ret(
        const(grainformat)
        $ output_filename
        $ ret(
            Grain_utils.Config.with_cli_options(compile_parsed)
            $ input_filename,
          ),
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
