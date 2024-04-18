open Silo;
open Ansi;

let _ =
  try(Cli.run()) {
  | exn =>
    prerr_string(ansi(~color=RedBright, "Error: "));
    prerr_endline(Printexc.to_string(exn));
    exit(1);
  };
