open Grain_utils;
open Grain_typed;

type status =
  | Reading
  | Break;
let documents: Hashtbl.t(Protocol.uri, string) = Hashtbl.create(128);
let compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program) =
  Hashtbl.create(128);
let cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program) =
  Hashtbl.create(128) /* we keep the last successful compile to help with completion and definition*/;

let is_initialized = ref(false);
let is_shutting_down = ref(false);

let process = msg => {
  switch (Message.of_request(msg)) {
  | Initialize(id, params) =>
    is_initialized := true;
    Initialize.process(~id, ~compiled_code, ~cached_code, ~documents, params);
    Reading;
  | TextDocumentHover(id, params) when is_initialized^ =>
    Hover.process(~id, ~compiled_code, ~cached_code, ~documents, params);
    Reading;
  | TextDocumentCodeLens(id, params) when is_initialized^ =>
    Lenses.process(~id, ~compiled_code, ~cached_code, ~documents, params);
    Reading;
  | TextDocumentCompletion(id, params) when is_initialized^ =>
    Completion.process(~id, ~compiled_code, ~cached_code, ~documents, params);
    Reading;
  | CompletionItemResolve(id, params) when is_initialized^ =>
    Completion.Resolution.process(
      ~id,
      ~compiled_code,
      ~cached_code,
      ~documents,
      params,
    );
    Reading;
  | Shutdown(id, params) when is_initialized^ =>
    Shutdown.process(~id, ~compiled_code, ~cached_code, ~documents, params);
    is_shutting_down := true;
    Reading;
  | Exit(id, _) when is_shutting_down^ => Break
  | Exit(id, _) =>
    // TODO: Why is this doing an exit(1) ?
    exit(1)
  | TextDocumentDidOpen(uri, params) when is_initialized^ =>
    Code_file.DidOpen.process(
      ~uri,
      ~compiled_code,
      ~cached_code,
      ~documents,
      params,
    );
    Reading;
  | TextDocumentDidChange(uri, params) when is_initialized^ =>
    Code_file.DidChange.process(
      ~uri,
      ~compiled_code,
      ~cached_code,
      ~documents,
      params,
    );
    Reading;
  | Error(msg) =>
    prerr_endline(msg);
    is_shutting_down := true;
    Break;
  | _ =>
    /* TODO: What should happen here? */
    if (is_initialized^ == false) {
      Logfile.log("Client must send 'initialize' as first event");
      Break;
    } else {
      Reading;
    }
  };
};
