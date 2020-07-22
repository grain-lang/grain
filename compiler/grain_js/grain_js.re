open Grain;
open Compile;
open Printf;
open Js_of_ocaml;

let p = {|
export *

export data List<a> = [] | [...](a, List<a>)

# Maybe some data, maybe not!
data Option<a> = Some(a) | None
|};

Sys_js.mount(~path="", (~prefix, ~path) =>
  if (path == "pervasives.wasm") {
    None;
  } else {
    Some(p);
  }
);

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

let compile = str => {
  if (!Printexc.backtrace_status()) {
    Printexc.record_backtrace(true);
  };
  try(Compile.compile_string(~name="sandbox.gr", Js.to_string(str))) {
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
};

Js.export("compile", compile);
