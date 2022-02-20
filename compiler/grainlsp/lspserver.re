let rootPath = ".";

let documents = Hashtbl.create(128);
let compiled_code = Hashtbl.create(128);
let cached_code = Hashtbl.create(128); // we keep the last successful compile to help with completion and definitions

/* Will wait up to 100ms */
let can_read = desc => {
  let (r, _, _) = Unix.select([desc], [], [], 0.1);
  r != [];
};

let break = ref(false);
let is_shutting_down = ref(false);
let stdin_descr = Unix.descr_of_in_channel(stdin);

let loop = log =>
  while (! break^) {
    if (can_read(stdin_descr)) {
      switch (Rpc.read_message(log, stdin)) {
      | Message(id, action, json) =>
        log("received message " ++ action);
        switch (action) {
        | "textDocument/hover" =>
          Hover.get_hover(~log, ~id, ~compiled_code, ~documents, json)
        | "textDocument/codeLens" =>
          Lenses.process_get_lenses(~log, ~id, ~compiled_code, json)
        // Disabled until we can get locations for external values back with locations
        // | "textDocument/definition" =>
        //   Definitions.goto_definition(
        //     ~log,
        //     ~id,
        //     ~compiled_code,
        //     ~cached_code,
        //     json,
        //   )
        | "textDocument/completion" =>
          Completion.process_completion(
            ~log,
            ~id,
            ~compiled_code,
            ~cached_code,
            ~documents,
            json,
          )
        | "completionItem/resolve" =>
          Completion.process_resolution(
            ~log,
            ~id,
            ~compiled_code,
            ~cached_code,
            ~documents,
            json,
          )
        | "shutdown" =>
          Rpc.send_null_message(log, stdout, id);
          is_shutting_down := true;
        | _ => ()
        };

      | Notification("exit", _) =>
        if (is_shutting_down^) {
          log("Got exit! Terminating loop");
          break := true;
        } else {
          log("Got exit without shutdown. Erroring out");
          exit(1);
        }
      | Notification(method, json) =>
        log("received notification " ++ method);
        switch (method) {
        | "textDocument/didOpen"
        | "textDocument/didChange" =>
          Processcode.textDocument_didOpenOrChange(
            ~log,
            ~documents,
            ~compiled_code,
            ~cached_code,
            json,
          )
        | _ => ()
        };
      | Error(_) =>
        log("Received Error, shutting down");
        is_shutting_down := true;
      };
    };
  };

let run = () => {
  Log.set_location(rootPath ++ "/.lsp_debug.log");
  Log.log("LSP starting" ++ Sys.executable_name);
  let log = Log.log;

  let initialize = () =>
    switch (Rpc.read_message(log, stdin)) {
    | Message(id, "initialize", _) =>
      log("initialize");
      Rpc.send_capabilities(log, stdout, id);
      loop(log);
    | _ => failwith("Client must send 'initialize' as first event")
    };

  initialize();
};
