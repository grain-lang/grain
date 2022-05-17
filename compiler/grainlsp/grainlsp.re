open Cmdliner;
open Grain_utils;
open Grain_language_server;

[@deriving cmdliner]
type params = {
  [@name "debuglsp"] [@docv "DEBUG"]
  debug: bool,
};

let run = (opts: params) => {
  if (opts.debug) {
    Logfile.open_("lsp.log");
    Logfile.log("LSP is starting up");
  };

  Logfile.log(Sys.os_type);

  let rec read_stdin = () => {
    switch (Rpc.read_message(stdin)) {
    | exception exn => Logfile.log("exception on read message")
    | msg =>
      let status = Message.process(msg);
      if (status == Message.Reading) {
        read_stdin();
      };
    };
  };
  read_stdin();

  Logfile.close();
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
