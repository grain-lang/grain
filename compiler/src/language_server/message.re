open Grain_utils;

let documents = Hashtbl.create(128);
let compiled_code = Hashtbl.create(128);
let cached_code = Hashtbl.create(128); // we keep the last successful compile to help with completion and definitions

type status =
  | Reading
  | Break;

let is_initialized = ref(false);
let is_shutting_down = ref(false);

let process = (msg: Rpc.protocol_msg) => {
  switch (msg) {
  | Message(id, "initialize", json) =>
    is_initialized := true;
    Capabilities.process(~id, ~compiled_code, ~cached_code, ~documents, json);
    Reading;
  | Message(id, "textDocument/hover", json) when is_initialized^ =>
    Hover.process(~id, ~compiled_code, ~cached_code, ~documents, json);
    Reading;
  | Message(id, "textDocument/codeLens", json) when is_initialized^ =>
    Lenses.process(~id, ~compiled_code, ~cached_code, ~documents, json);
    Reading;
  | Message(id, "textDocument/completion", json) when is_initialized^ =>
    Completion.process(~id, ~compiled_code, ~cached_code, ~documents, json);
    Reading;
  | Message(id, "completionItem/resolve", json) when is_initialized^ =>
    Completion.Resolution.process(
      ~id,
      ~compiled_code,
      ~cached_code,
      ~documents,
      json,
    );
    Reading;
  | Message(id, "shutdown", json) when is_initialized^ =>
    Shutdown.process(~id, ~compiled_code, ~cached_code, ~documents, json);
    is_shutting_down := true;
    Reading;
  | Notification("exit", _) when is_shutting_down^ => Break
  | Notification("exit", _) =>
    // TODO: Why is this doing an exit(1) ?
    exit(1)
  | Notification("textDocument/didOpen", json)
  | Notification("textDocument/didChange", json) when is_initialized^ =>
    Code_file.process(~compiled_code, ~cached_code, ~documents, json);
    Reading;
  | Notification(_)
  | Message(_) =>
    /* TODO: What should happen here? */
    if (is_initialized^ == false) {
      Logfile.log("Client must send 'initialize' as first event");
      Break;
    } else {
      Reading;
    }
  | Error(_) =>
    is_shutting_down := true;
    Break;
  };
};
