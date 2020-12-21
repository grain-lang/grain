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

let default_output_filename = name =>
  safe_remove_extension(name) ++ ".gr.wasm";

let default_assembly_filename = name =>
  safe_remove_extension(name) ++ ".wast";

let default_mashtree_filename = name =>
  safe_remove_extension(name) ++ ".mashtree";

/* Diagnostic mode - read the file to compile from stdin and return nothing or compile errors on stdout */
let compile_string = name => {
  let program_str = ref("");
  /* read from stdin until we get end of buffer */
  try(
    while (true) {
      program_str := program_str^ ++ read_line() ++ "\n";
    }
  ) {
  | exn => ()
  };

  try(
    ignore(
      {
        let compile_state =
          Compile.compile_string(~hook=stop_after_typed, ~name, program_str^);

        switch (compile_state.cstate_desc) {
        | TypeChecked(typed_program) =>
          let values: list(Grain_diagnostics.Lenses.lens_t) =
            Grain_diagnostics.Lenses.get_lenses_values(typed_program);
          let json =
            Grain_diagnostics.Output.result_to_json(~errors=[], ~values);
          print_endline(json);
        | _ => ()
        };
      },
    )
  ) {
  | exn =>
    let error = Grain_diagnostics.Output.exn_to_lsp_error(exn);
    let errors =
      switch (error) {
      | None => []
      | Some(err) => [err]
      };

    let json = Grain_diagnostics.Output.result_to_json(~errors, ~values=[]);
    print_endline(json);
  };

  /* as the compiler throws an exception on an error, we always just return OK */
  `Ok();
};

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

/* add a wrapper so we can switch to LSP mode based on cli config */

let compile_wrapper = (name, outfile_arg) =>
  if (Grain_utils.Config.lsp_mode^) {
    compile_string(name);
  } else {
    compile_file(name, outfile_arg);
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
        Grain_utils.Config.with_cli_options(compile_wrapper)
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
