open Cmdliner;
open Grain_utils;
open Grain_language_server;

[@deriving cmdliner]
type params = {
  [@name "debuglsp"] [@docv "DEBUG"]
  debug: bool,
};

let run = (opts: params) => {
  set_binary_mode_in(stdin, true);
  set_binary_mode_out(stdout, true);

  let rec read_stdin = () => {
    switch (Protocol.request()) {
    | Error(err) => Trace.log("Failed to read message: " ++ err)
    | Ok(msg) =>
      switch (Driver.process(msg)) {
      | Exit(code) => exit(code)
      | Break => ()
      | Reading => read_stdin()
      }
    };
  };
  read_stdin();
};

let lsp = (opts: params) =>
  try(run(opts)) {
  | exn =>
    Format.eprintf("@[%s@]@.", Printexc.to_string(exn));
    exit(2);
  };

let cmd = {
  open Term;

  let doc = "Grain LSP";
  let version =
    switch (Build_info.V1.version()) {
    | None => "unknown"
    | Some(v) => Build_info.V1.Version.to_string(v)
    };

  Cmd.v(
    Cmd.info(Sys.argv[0], ~version, ~doc),
    Grain_utils.Config.with_cli_options(lsp) $ params_cmdliner_term(),
  );
};

let () =
  switch (Cmd.eval_value(cmd)) {
  | Error(_) => exit(1)
  | _ => ()
  };
