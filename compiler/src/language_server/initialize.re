open Grain_typed;

[@deriving yojson({strict: false})]
type link_support_capability = {
  [@key "linkSupport"] [@default false]
  link_support: bool,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_definition
// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_typeDefinition
[@deriving yojson({strict: false})]
type text_document_client_capability = {
  [@key "definition"] [@default None]
  definition: option(link_support_capability),
  [@key "typeDefinition"] [@default None]
  type_definition: option(link_support_capability),
};

[@deriving yojson({strict: false})]
type client_capabilities = {
  [@key "textDocument"] [@default None]
  text_document: option(text_document_client_capability),
};

let client_definition_link_support = ref(false);
let client_type_definition_link_support = ref(false);

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
    [@key "capabilities"] [@default None]
    capabilities: option(client_capabilities),
  };
};

let take_definition_link_support = (params: RequestParams.t) =>
  switch (params.capabilities) {
  | None => false
  | Some({text_document: None}) => false
  | Some({text_document: Some({definition: None})}) => false
  | Some({text_document: Some({definition: Some({link_support})})}) => link_support
  };

let take_type_definition_link_support = (params: RequestParams.t) =>
  switch (params.capabilities) {
  | None => false
  | Some({text_document: None}) => false
  | Some({text_document: Some({type_definition: None})}) => false
  | Some({text_document: Some({type_definition: Some({link_support})})}) => link_support
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
    definition_provider: bool,
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
    definition_provider: true,
    type_definition_provider: true,
    references_provider: false,
    document_symbol_provider: true,
    code_action_provider: true,
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
  client_definition_link_support := take_definition_link_support(params);
  client_type_definition_link_support :=
    take_type_definition_link_support(params);
  // The initialize request can set up the initial trace level
  Trace.set_level(params.trace);
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({capabilities: ResponseResult.capabilities}),
  );
};
