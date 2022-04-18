let rootPath = ".";

let documents = Hashtbl.create(128);
let compiled_code = Hashtbl.create(128);
let cached_code = Hashtbl.create(128); // we keep the last successful compile to help with completion and definitions

// /* Will wait up to 100ms */
// let can_read = desc => {
//   let (r, _, _) = Unix.select([desc], [], [], 0.1);
//   r != [];
// };

let break = ref(false);
let is_shutting_down = ref(false);
//let stdin_descr = Unix.descr_of_in_channel(stdin);

let loop = () =>
  while (! break^) {
    //  if (can_read(stdin_descr)) {
    switch (Rpc.read_message(stdin)) {
    | Message(id, action, json) =>
      switch (action) {
      | "textDocument/hover" =>
        Hover.get_hover(~id, ~compiled_code, ~documents, json)
      | "textDocument/codeLens" =>
        Lenses.send_lenses_to_client(~id, ~compiled_code, json)
      | "textDocument/completion" =>
        Completion.process_completion(
          ~id,
          ~compiled_code,
          ~cached_code,
          ~documents,
          json,
        )
      | "completionItem/resolve" =>
        Completion.process_resolution(
          ~id,
          ~compiled_code,
          ~cached_code,
          ~documents,
          json,
        )
      | "shutdown" =>
        Rpc.send_null_message(stdout, id);
        is_shutting_down := true;
      | _ => ()
      }

    | Notification("exit", _) when is_shutting_down^ => break := true
    | Notification("exit", _) => exit(1)
    | Notification("textDocument/didOpen", json)
    | Notification("textDocument/didChange", json) =>
      Processcode.textDocument_didOpenOrChange(
        ~documents,
        ~compiled_code,
        ~cached_code,
        json,
      )
    | Notification(_, json) => ()
    | Error(_) => is_shutting_down := true
    //  };
    };
  };

let run = (debug: bool) => {
  if (debug) {
    Log.set_level(DebugLog);
    Log.log("LSP is starting up");
  };

  let initialize = () =>
    switch (Rpc.read_message(stdin)) {
    | Message(id, "initialize", _) =>
      Rpc.send_capabilities(stdout, id);
      loop();
      `Ok();
    | _ =>
      Log.log("Client must send 'initialize' as first event");
      `Error((false, "Client must send 'initialize' as first event"));
    };

  let run_result = initialize();
  Log.close_log();
  run_result;
};
