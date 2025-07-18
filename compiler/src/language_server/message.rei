type t =
  | Initialize(Protocol.message_id, Initialize.RequestParams.t)
  | TextDocumentHover(Protocol.message_id, Hover.RequestParams.t)
  | TextDocumentCodeLens(Protocol.message_id, Lenses.RequestParams.t)
  | Shutdown(Protocol.message_id, Shutdown.RequestParams.t)
  | Exit(Protocol.message_id, Exit.RequestParams.t)
  | TextDocumentDidOpen(Protocol.uri, Code_file.DidOpen.RequestParams.t)
  | TextDocumentDidChange(Protocol.uri, Code_file.DidChange.RequestParams.t)
  | TextDocumentInlayHint(Protocol.message_id, Inlayhint.RequestParams.t)
  | TextDocumentSymbol(Protocol.message_id, Symbol.RequestParams.t)
  | Formatting(Protocol.message_id, Formatting.RequestParams.t)
  | Goto(Protocol.message_id, Goto.goto_request_type, Goto.RequestParams.t)
  | CodeAction(Protocol.message_id, Code_action.RequestParams.t)
  | SetTrace(Protocol.trace_value)
  | Unsupported
  | Error(string);

let of_request: Protocol.request_message => t;
