open Cmdliner;
open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_formatting;
open Filename;

let get_program_string = filename => {
  switch (filename) {
  | None =>
    let source_buffer = Buffer.create(1024);
    set_binary_mode_in(stdin, true);
    /* read from stdin until we get end of buffer */
    try(
      while (true) {
        let c = input_char(stdin);
        Buffer.add_char(source_buffer, c);
      }
    ) {
    | exn => ()
    };
    Buffer.contents(source_buffer);
  | Some(filename) =>
    let ic = open_in_bin(filename);
    let n = in_channel_length(ic);
    let source_buffer = Buffer.create(n);
    Buffer.add_channel(source_buffer, ic, n);
    close_in(ic);
    Buffer.contents(source_buffer);
  };
};

let compile_parsed = (filename: option(string)) => {
  let program_str = get_program_string(filename);
  switch (Format.parse_source(program_str)) {
  | Error(ParseError(exn)) =>
    let bt =
      if (Printexc.backtrace_status()) {
        Some(Printexc.get_backtrace());
      } else {
        None;
      };
    Grain_parsing.Location.report_exception(Stdlib.Format.err_formatter, exn);
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
  | Ok((parsed_program, lines, eol)) => `Ok((parsed_program, lines, eol))
  | Error(InvalidCompilationState) =>
    `Error((false, "Invalid compilation state"))
  };
};

let format_code =
    (
      ~eol,
      srcfile: option(string),
      program: Parsetree.parsed_program,
      outfile,
      original_source: array(string),
      format_in_place: bool,
    ) => {
  let formatted_code =
    Grain_formatting.Format.format_ast(~original_source, ~eol, program);

  // return the file to its format

  let buf = Buffer.create(0);
  Buffer.add_string(buf, formatted_code);

  let contents = Buffer.to_bytes(buf);
  switch (outfile) {
  | Some(outfile) =>
    let oc = Fs_access.open_file_for_writing(outfile);
    output_bytes(oc, contents);
    close_out(oc);
  | None =>
    switch (srcfile, format_in_place) {
    | (Some(src), true) =>
      let oc = Fs_access.open_file_for_writing(src);
      output_bytes(oc, contents);
      close_out(oc);
    | _ =>
      set_binary_mode_out(stdout, true);
      print_bytes(contents);
    }
  };

  `Ok();
};

let grainformat =
    (
      srcfile: option(string),
      outfile,
      format_in_place: bool,
      (program, lines: array(string), eol),
    ) =>
  try(format_code(~eol, srcfile, program, outfile, lines, format_in_place)) {
  | e => `Error((false, Printexc.to_string(e)))
  };

let input_file_conv = {
  open Arg;
  let (prsr, prntr) = non_dir_file;
  (filename => prsr(filename), prntr);
};

/** Converter which checks that the given output filename is valid */
let output_file_conv = {
  let parse = s => {
    let s_dir = dirname(s);
    Sys.file_exists(s_dir)
      ? if (Sys.is_directory(s_dir)) {
          `Ok(s);
        } else {
          `Error(Stdlib.Format.sprintf("`%s' is not a directory", s_dir));
        }
      : `Error(Stdlib.Format.sprintf("no `%s' directory", s_dir));
  };
  (parse, Stdlib.Format.pp_print_string);
};

let output_filename = {
  let doc = "Output filename";
  let docv = "FILE";
  Arg.(
    value & opt(some(output_file_conv), None) & info(["o"], ~docv, ~doc)
  );
};

let format_in_place = {
  let doc = "Format in place";
  let docv = "";
  Arg.(value & flag & info(["in-place"], ~docv, ~doc));
};

let input_filename = {
  let doc = "Grain source file to format";
  let docv = "FILE";
  Arg.(
    value
    & pos(~rev=true, 0, some(~none="", input_file_conv), None)
    & info([], ~docv, ~doc)
  );
};

let cmd = {
  open Term;

  let doc = "Format Grain source";
  let version =
    switch (Build_info.V1.version()) {
    | None => "unknown"
    | Some(v) => Build_info.V1.Version.to_string(v)
    };

  Cmd.v(
    Cmd.info(Sys.argv[0], ~version, ~doc),
    Term.(
      ret(
        const(grainformat)
        $ input_filename
        $ output_filename
        $ format_in_place
        $ ret(
            Grain_utils.Config.with_cli_options(compile_parsed)
            $ input_filename,
          ),
      )
    ),
  );
};

let () =
  switch (Cmd.eval_value(cmd)) {
  | Error(_) => exit(1)
  | _ => ()
  };
