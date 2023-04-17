open Grain_typed;

type t =
  | Initialize(Protocol.message_id, Initialize.RequestParams.t)
  | TextDocumentHover(Protocol.message_id, Hover.RequestParams.t)
  | TextDocumentCodeLens(Protocol.message_id, Lenses.RequestParams.t)
  | Shutdown(Protocol.message_id, Shutdown.RequestParams.t)
  | Exit(Protocol.message_id, Exit.RequestParams.t)
  | TextDocumentDidOpen(Protocol.uri, Code_file.DidOpen.RequestParams.t)
  | TextDocumentDidChange(Protocol.uri, Code_file.DidChange.RequestParams.t)
  | TextDocumentInlayHint(Protocol.message_id, Inlayhint.RequestParams.t)
  | Formatting(Protocol.message_id, Formatting.RequestParams.t)
  | Definition(Protocol.message_id, Definition.RequestParams.t)
  | SetTrace(Protocol.trace_value)
  | Unsupported
  | Error(string);

let of_request = (msg: Protocol.request_message): t => {
  switch (msg) {
  | {method: "initialize", id: Some(id), params: Some(params)} =>
    switch (Initialize.RequestParams.of_yojson(params)) {
    | Ok(params) => Initialize(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/hover", id: Some(id), params: Some(params)} =>
    switch (Hover.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentHover(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/inlayHint", id: Some(id), params: Some(params)} =>
    switch (Inlayhint.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentInlayHint(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/codeLens", id: Some(id), params: Some(params)} =>
    switch (Lenses.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentCodeLens(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "shutdown", id: Some(id), params: None} =>
    switch (Shutdown.RequestParams.of_yojson(`Null)) {
    | Ok(params) => Shutdown(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "exit", id: Some(id), params: None} =>
    switch (Exit.RequestParams.of_yojson(`Null)) {
    | Ok(params) => Exit(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/didOpen", id, params: Some(params)} =>
    switch (Code_file.DidOpen.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentDidOpen(params.text_document.uri, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/didChange", id, params: Some(params)} =>
    switch (Code_file.DidChange.RequestParams.of_yojson(params)) {
    | Ok(params) => TextDocumentDidChange(params.text_document.uri, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/formatting", id: Some(id), params: Some(params)} =>
    switch (Formatting.RequestParams.of_yojson(params)) {
    | Ok(params) => Formatting(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "textDocument/definition", id: Some(id), params: Some(params)} =>
    switch (Definition.RequestParams.of_yojson(params)) {
    | Ok(params) => Definition(id, params)
    | Error(msg) => Error(msg)
    }
  | {method: "$/setTrace", params: Some(params)} =>
    switch (Trace.RequestParams.of_yojson(params)) {
    | Ok(params) => SetTrace(params.value)
    | Error(msg) => Error(msg)
    }
  | _ => Unsupported
  };
};
