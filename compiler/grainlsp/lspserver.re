let rootPath = ".";

let documents = Hashtbl.create(128);
let compiled_code = Hashtbl.create(128);
let cached_code = Hashtbl.create(128); // we keep the last successful compile to help with completion and definitions

let break = ref(false);
let is_shutting_down = ref(false);

let process_message = (msg: Rpc.protocol_msg) => {
  switch (msg) {
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
      break := true;
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
  };
};

let loop = () =>
  while (! break^) {
    Log.log("in the loop");
    switch (Rpc.read_message(stdin)) {
    | exception exn => Log.log("exception on read message")
    | msg => process_message(msg)
    };
  };

let run = (debug: bool) => {
  if (debug) {
    Log.set_level(DebugLog);
    Log.log("LSP is starting up");
  };

  Log.log(Sys.os_type);

  let initialize = () =>
    switch (Rpc.read_message(stdin)) {
    | Message(id, "initialize", _) =>
      Rpc.send_capabilities(stdout, id);
      loop();

    | _ => Log.log("Client must send 'initialize' as first event")
    };

  initialize();
  Log.close_log();
};

// convenience functions for the JS version
// return true to shut server down
let process_js_message = (raw, first) => {
  let message = Rpc.parse_message(raw);
  if (first) {
    switch (message) {
    | Message(id, "initialize", _) =>
      Rpc.send_capabilities(stdout, id);
      false;
    | _ =>
      Log.log("Client must send 'initialize' as first event");
      true;
    };
  } else {
    process_message(message);
    is_shutting_down^;
  };
};

let enable_logging = () => {
  Log.set_level(DebugLog);
  Log.log("JS LSP is starting up");
};
let js_log = entry => Log.log(entry);
