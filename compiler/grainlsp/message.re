let documents = Hashtbl.create(128);
let compiled_code = Hashtbl.create(128);
let cached_code = Hashtbl.create(128); // we keep the last successful compile to help with completion and definitions

type status =
  | Reading
  | Break;

let is_initialized = ref(true);
let is_shutting_down = ref(false);

let process = (msg: Rpc.protocol_msg) => {
  switch (msg) {
  | Message(id, "initialize", _) =>
    is_initialized := true;
    Rpc.send_capabilities(stdout, id);
    Reading;
  | Message(id, "textDocument/hover", json) when is_initialized^ =>
    Hover.get_hover(~id, ~compiled_code, ~documents, json);
    Reading;
  | Message(id, "textDocument/codeLens", json) when is_initialized^ =>
    Lenses.send_lenses_to_client(~id, ~compiled_code, json);
    Reading;
  | Message(id, "textDocument/completion", json) when is_initialized^ =>
    Completion.process_completion(
      ~id,
      ~compiled_code,
      ~cached_code,
      ~documents,
      json,
    );
    Reading;
  | Message(id, "completionItem/resolve", json) when is_initialized^ =>
    Completion.process_resolution(
      ~id,
      ~compiled_code,
      ~cached_code,
      ~documents,
      json,
    );
    Reading;
  | Message(id, "shutdown", json) when is_initialized^ =>
    Rpc.send_null_message(stdout, id);
    is_shutting_down := true;
    Reading;
  | Notification("exit", _) when is_shutting_down^ => Break
  | Notification("exit", _) =>
    // TODO: Why is this doing an exit(1) ?
    exit(1)
  | Notification("textDocument/didOpen", json)
  | Notification("textDocument/didChange", json) when is_initialized^ =>
    Processcode.textDocument_didOpenOrChange(
      ~documents,
      ~compiled_code,
      ~cached_code,
      json,
    );
    Reading;
  | Notification(_)
  | Message(_) =>
    /* TODO: What should happen here? */
    if (is_initialized^ == false) {
      Log.log("Client must send 'initialize' as first event");
      Break;
    } else {
      Reading;
    }
  | Error(_) =>
    is_shutting_down := true;
    Break;
  };
};
