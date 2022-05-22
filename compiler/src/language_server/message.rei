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

let of_request: Protocol.request_message => t;
