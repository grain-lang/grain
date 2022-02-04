let rootPath = ".";

/* Will wait up to 100ms */
let canRead = desc => {
  let (r, _, _) = Unix.select([desc], [], [], 0.1);
  r != [];
};

let run = () => {
  Log.set_location(rootPath ++ "/lsp_debug.log");
  Log.log("Hello - from " ++ Sys.executable_name);
  let log = Log.log;

  let stdin_descr = Unix.descr_of_in_channel(stdin);

  log("Starting main loop");
  let rec loop = (~isShuttingDown, ~documents, ~compiledCode) =>
    // let state = tick(state);
    if (canRead(stdin_descr)) {
      switch (Rpc.read_message(log, stdin)) {
      | Message(id, action, json) =>
        log("received message " ++ action);
        switch (action) {
        //     | "textDocument/hover" =>
        //       Messages.get_hover(log, id, json, compiledCode)
        | "textDocument/codeLens" =>
          Lenses.process_get_lenses(log, id, json, compiledCode)
        //     | "textDocument/codeAction" =>
        //       Codeaction.process_request(log, id, json, compiledCode, documents)
        //     | "textDocument/definition" =>
        //       Messages.goto_definition(log, id, json, compiledCode)
        //     | "textDocument/completion" =>
        //       Completion.process_completion(
        //         log,
        //         id,
        //         json,
        //         compiledCode,
        //         documents,
        //       )
        //     | "textDocument/signatureHelp" =>
        //       Messages.signature_help(log, id, json, compiledCode, documents)
        | _ => ()
        };
        loop(~isShuttingDown, ~documents, ~compiledCode);

      | Notification(method, json) =>
        log("received notification " ++ method);
        switch (method) {
        | "textDocument/didOpen"
        | "textDocument/didChange" =>
          Processcode.textDocument_didOpenOrChange(
            log,
            json,
            documents,
            compiledCode,
          )
        | _ => ()
        };
        loop(~isShuttingDown, ~documents, ~compiledCode);
      | Error(_) =>
        log("!!!Received Error");
        loop(~isShuttingDown=true, ~documents, ~compiledCode);
      };
    } else {
      loop(~isShuttingDown, ~documents, ~compiledCode);
    };

  let initialize = (~documents, ~compiledCode) =>
    switch (Rpc.read_message(log, stdin)) {
    | Message(id, "initialize", _) =>
      log("initialize");
      Rpc.send_capabilities(log, stdout, id);
      loop(~isShuttingDown=false, ~documents, ~compiledCode);

    | _ => failwith("Client must send 'initialize' as first event")
    };

  let documents = Hashtbl.create(128);
  let compiledCode = Hashtbl.create(128);

  initialize(~documents, ~compiledCode);
};
