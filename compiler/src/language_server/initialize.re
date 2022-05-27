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
  type completion_values = {
    resolveProvider: bool,
    triggerCharacters: list(string),
  };

  [@deriving yojson]
  type code_values = {resolveProvider: bool};

  [@deriving yojson]
  type lsp_capabilities = {
    documentFormattingProvider: bool,
    textDocumentSync: int,
    hoverProvider: bool,
    completionProvider: completion_values,
    definitionProvider: bool,
    typeDefinitionProvider: bool,
    referencesProvider: bool,
    documentSymbolProvider: bool,
    codeActionProvider: bool,
    codeLensProvider: code_values,
    documentHighlightProvider: bool,
    documentRangeFormattingProvider: bool,
    renameProvider: bool,
  };
  [@deriving yojson]
  type t = {capabilities: lsp_capabilities};

  let capabilities = {
    documentFormattingProvider: true,
    textDocumentSync: 1,
    hoverProvider: true,
    completionProvider: {
      resolveProvider: true,
      triggerCharacters: ["."],
    },
    definitionProvider: false, // disabled until we can resolve the external module location
    typeDefinitionProvider: false,
    referencesProvider: false,
    documentSymbolProvider: false,
    codeActionProvider: false,
    codeLensProvider: {
      resolveProvider: true,
    },
    documentHighlightProvider: false,
    documentRangeFormattingProvider: false,
    renameProvider: false,
  };
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
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
