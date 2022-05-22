open Grain_typed;

type t =
  | Initialize(Protocol.message_id, Initialize.RequestParams.t)
  | TextDocumentHover(Protocol.message_id, Hover.RequestParams.t)
  | TextDocumentCodeLens(Protocol.message_id, Lenses.RequestParams.t)
  | TextDocumentCompletion(Protocol.message_id, Completion.RequestParams.t)
  | CompletionItemResolve(
      Protocol.message_id,
      Completion.Resolution.RequestParams.t,
    )
  | Shutdown(Protocol.message_id, Shutdown.RequestParams.t)
  | Exit(Protocol.message_id, Exit.RequestParams.t)
  | TextDocumentDidOpen(Protocol.uri, Code_file.DidOpen.RequestParams.t)
  | TextDocumentDidChange(Protocol.uri, Code_file.DidChange.RequestParams.t)
  | SetTrace(Protocol.trace_value)
  | Unsupported
  | Error(string);

let of_request = (msg: Protocol.request_message): t => {
  switch (msg) {
  | {method: "initialize", id: Some(id), params} =>
    switch (Initialize.RequestParams.of_yojson(params)) {
    | Ok(params) => Initialize(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/hover", id: Some(id), params} =>
    switch (Hover.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentHover(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/codeLens", id: Some(id), params} =>
    switch (Lenses.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentCodeLens(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/completion", id: Some(id), params} =>
    switch (Completion.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentCompletion(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "completionItem/resolve", id: Some(id), params} =>
    switch (Completion.Resolution.RequestParams.of_yojson(params)) {
    | Ok(params) => CompletionItemResolve(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "shutdown", id: Some(id), params} =>
    switch (Shutdown.RequestParams.of_yojson(params)) {
    | Ok(params) => Shutdown(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "exit", id: Some(id), params} =>
    switch (Exit.RequestParams.of_yojson(params)) {
    | Ok(params) => Exit(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/didOpen", id, params} =>
    switch (Code_file.DidOpen.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentDidOpen(params.text_document.uri, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/didChange", id, params} =>
    switch (Code_file.DidChange.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentDidChange(params.text_document.uri, params)
    | Error(msg) => Error(msg)
    }
  | {method: "$/setTrace", params} =>
    switch (Trace.RequestParams.of_yojson(params)) {
    | Ok(params) => SetTrace(params.value)
    | Error(msg) => Error(msg)
    }
  | _ => Unsupported
  };
};
