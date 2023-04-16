open Lsp_types;
open Grain_utils;
open Grain_typed;

type status =
  | Reading
  | Break
  | Exit(int);

let documents: Hashtbl.t(Protocol.uri, string) = Hashtbl.create(128);

/* we keep the last successful compile to help with completion and definition*/
let compiled_code: Hashtbl.t(Protocol.uri, code) = Hashtbl.create(128);

let is_initialized = ref(false);
let is_shutting_down = ref(false);

let process = msg => {
  switch (Message.of_request(msg)) {
  | Initialize(id, params) =>
    Trace.log("initializing");
    is_initialized := true;
    Initialize.process(~id, ~compiled_code, ~documents, params);
    Reading;
  | TextDocumentHover(id, params) when is_initialized^ =>
    Hover.process(~id, ~compiled_code, ~documents, params);
    Reading;
  | TextDocumentInlayHint(id, params) when is_initialized^ =>
    Inlayhint.process(~id, ~compiled_code, ~documents, params);
    Reading;
  | TextDocumentCodeLens(id, params) when is_initialized^ =>
    Lenses.process(~id, ~compiled_code, ~documents, params);
    Reading;
  | Shutdown(id, params) when is_initialized^ =>
    Shutdown.process(~id, ~compiled_code, ~documents, params);
    is_shutting_down := true;
    Reading;
  | Exit(id, _) when is_shutting_down^ => Break
  | Exit(id, _) => Exit(1)
  | TextDocumentDidOpen(uri, params) when is_initialized^ =>
    Code_file.DidOpen.process(~uri, ~compiled_code, ~documents, params);
    Reading;
  | TextDocumentDidChange(uri, params) when is_initialized^ =>
    Code_file.DidChange.process(~uri, ~compiled_code, ~documents, params);
    Reading;
  | Formatting(id, params) when is_initialized^ =>
    Formatting.process(~id, ~compiled_code, ~documents, params);
    Reading;
  | Definition(id, params) when is_initialized^ =>
    Definition.process(~id, ~compiled_code, ~documents, params);
    Reading;
  | SetTrace(trace_value) =>
    Trace.set_level(trace_value);
    Reading;
  | Error(msg) =>
    Protocol.error({code: InvalidParams, message: msg});
    Reading;
  | _ =>
    // If we don't get initialize as the first event we stop the server
    // this signals to the client to restart
    if (is_initialized^ == false) {
      Protocol.error({
        code: ServerNotInitialized,
        message: "Client must send 'initialize' as first event",
      });
      Break;
    } else {
      Reading;
    }
  };
};
