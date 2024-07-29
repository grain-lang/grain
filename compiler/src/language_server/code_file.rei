open Grain_typed;

module DidOpen: {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didOpenTextDocumentParams
  module RequestParams: {
    [@deriving yojson({strict: false})]
    type t = {
      // Not abstract so it can pluck the URI
      text_document: Protocol.text_document_item,
    };
  };

  let process:
    (
      ~uri: Protocol.uri,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      RequestParams.t
    ) =>
    unit;
};

module DidChange: {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didChangeTextDocumentParams
  module RequestParams: {
    [@deriving yojson({strict: false})]
    type text_document_content_change_event;

    [@deriving yojson({strict: false})]
    type t = {
      // Not abstract so it can pluck the URI
      text_document: Protocol.versioned_text_document_identifier,
      content_changes: list(text_document_content_change_event),
    };
  };

  let process:
    (
      ~uri: Protocol.uri,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      RequestParams.t
    ) =>
    unit;
};
