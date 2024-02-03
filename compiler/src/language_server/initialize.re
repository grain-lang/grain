open Grain_typed;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type client_info = {
    name: string,
    [@default None]
    version: option(string),
  };

  // TODO: Implement the rest of the fields
  [@deriving yojson({strict: false})]
  type t = {
    [@key "processId"]
    process_id: option(int),
    [@key "clientInfo"] [@default None]
    client_info: option(client_info),
    [@default None]
    locale: option(string),
    [@key "rootUri"]
    root_uri: option(Protocol.uri),
    [@default "off"]
    trace: Protocol.trace_value,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeResult
module ResponseResult = {
  [@deriving yojson]
  type code_values = {
    [@key "resolveProvider"]
    resolve_provider: bool,
  };

  [@deriving yojson]
  type lsp_capabilities = {
    [@key "documentFormattingProvider"]
    document_formatting_provider: bool,
    [@key "textDocumentSync"]
    text_document_sync: Protocol.text_document_sync_kind,
    [@key "hoverProvider"]
    hover_provider: bool,
    [@key "definitionProvider"]
    definition_provider: Protocol.definition_client_capabilities,
    [@key "typeDefinitionProvider"]
    type_definition_provider: bool,
    [@key "referencesProvider"]
    references_provider: bool,
    [@key "documentSymbolProvider"]
    document_symbol_provider: bool,
    [@key "codeActionProvider"]
    code_action_provider: bool,
    [@key "codeLensProvider"]
    code_lens_provider: code_values,
    [@key "documentHighlightProvider"]
    document_highlight_provider: bool,
    [@key "documentRangeFormattingProvider"]
    document_range_formatting_provider: bool,
    [@key "renameProvider"]
    rename_provider: bool,
    [@key "inlayHintProvider"]
    inlay_hint_provider: Protocol.inlay_hint_options,
  };
  [@deriving yojson]
  type t = {capabilities: lsp_capabilities};

  let capabilities = {
    document_formatting_provider: true,
    text_document_sync: Full,
    hover_provider: true,
    definition_provider: {
      link_support: true,
    },
    type_definition_provider: false,
    references_provider: false,
    document_symbol_provider: false,
    code_action_provider: false,
    code_lens_provider: {
      resolve_provider: true,
    },
    document_highlight_provider: false,
    document_range_formatting_provider: false,
    rename_provider: false,
    inlay_hint_provider: {
      resolve_provider: false,
    },
  };
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  // The initialize request can set up the initial trace level
  Trace.set_level(params.trace);
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({capabilities: ResponseResult.capabilities}),
  );
};
