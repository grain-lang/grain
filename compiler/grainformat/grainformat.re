open Cmdliner;
open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Filename;

let compile_parsed = (filename: option(string)) => {
  let program_str = ref("");
  let linesList = ref([]);

  switch (
    switch (filename) {
    | None =>
      // force from stdin for now

      /* read from stdin until we get end of buffer */
      try(
        while (true) {
          let line = read_line();
          linesList := linesList^ @ [line];
        }
      ) {
      | exn => ()
      };

      program_str := String.concat("\n", linesList^);

      Compile.compile_string(
        ~is_root_file=true,
        ~hook=stop_after_parse,
        program_str^,
      );
    | Some(filenm) =>
      // need to read the source file in case we want to use the content
      // for formatter-ignore or decision making
      program_str := "";

      let ic = open_in(filenm);

      try(
        while (true) {
          let line = input_line(ic);

          linesList := linesList^ @ [line];
        }
      ) {
      | exn => ()
      };

      program_str := String.concat("\n", linesList^);

      // add a new line to the end for where it's in CRLF more
      // TODO(#940): Handle CRLF properly
      program_str := program_str^ ++ "\n";

      Grain_utils.Config.base_path := dirname(filenm);
      Compile.compile_string(
        ~is_root_file=true,
        ~hook=stop_after_parse,
        ~name=filenm,
        program_str^,
      );
    }
  ) {
  | exception exn =>
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
  | {cstate_desc: Parsed(parsed_program)} =>
    `Ok((parsed_program, Array.of_list(linesList^)))
  | _ => `Error((false, "Invalid compilation state"))
  };
};

let format_code =
    (
      srcfile: option(string),
      program: Parsetree.parsed_program,
      outfile,
      original_source: array(string),
      format_in_place: bool,
    ) => {
  let formatted_code = Format.format_ast(~original_source, program);

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
    | _ => print_bytes(contents)
    }
  };

  `Ok();
};

let grainformat =
    (
      srcfile: option(string),
      outfile,
      format_in_place: bool,
      (program, source: array(string)),
    ) =>
  try(format_code(srcfile, program, outfile, source, format_in_place)) {
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

  (
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
    Term.info(Sys.argv[0], ~version, ~doc),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
