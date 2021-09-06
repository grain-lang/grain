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

      Compile.compile_string(~hook=stop_after_parse, ~name="", program_str^);
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

      Grain_utils.Config.base_path := dirname(filenm);
      Compile.compile_string(
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
  | {cstate_desc: Parsed(parsed_program)} =>
    `Ok((parsed_program, Array.of_list(linesList^)))
  | _ => `Error((false, "Invalid compilation state"))
  };
};

let format_code =
    (program: Parsetree.parsed_program, original_source: array(string)) => {
  // HELP NEEDED
  // I need to get this value from the command line and

  let check_format = false;

  if (check_format) {
    Grain_utils.Config.formatter_maintain_ast := true;
    let reformatted_code = Reformat.reformat_ast(program, original_source);

    switch (
      Compile.compile_string(
        ~hook=stop_after_parse,
        ~name="",
        reformatted_code,
      )
    ) {
    | exception exn =>
      Grain_parsing.Location.report_exception(Format.err_formatter, exn);
      `Error((false, "Compilation exception from formatted code"));
    | {cstate_desc: Parsed(parsed_formatted_program)} =>
      // validate it against the original
      let valid_reformat =
        Reformat.validate_reformat(program, parsed_formatted_program);

      if (valid_reformat) {
        reformatted_code |> print_endline;
        `Ok();
      } else {
        `Error((false, "Reformatted code had a different AST"));
      };

    | _ => `Error((false, "Invalid compilation state from formatted code"))
    };
  } else {
    Grain_utils.Config.formatter_maintain_ast := false;
    let reformatted_code = Reformat.reformat_ast(program, original_source);

    print_endline(reformatted_code);
    `Ok();
  };
};

let grainformat = ((program, source: array(string))) => {
  // FIX needed here too
  let check_format = false;
  if (check_format) {
    Grain_utils.Config.formatter_maintain_ast := true;
  };
  try(format_code(program, source)) {
  | e => `Error((false, Printexc.to_string(e)))
  };
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
    value
    & pos(~rev=true, 0, some(~none="", input_file_conv), None)
    & info([], ~docv, ~doc)
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
