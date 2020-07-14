open Grain;
open Compile;
open Printf;
open Lexing;
open Filename;

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

Js_of_ocaml.Js.export("compile", compile_file);
