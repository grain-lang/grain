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
type capabilities_result = {capabilities: lsp_capabilities};

[@deriving yojson]
type capabilities_response = {
  jsonrpc: Rpc.version,
  id: Rpc.message_id,
  result: capabilities_result,
};

let capabilities = {
  documentFormattingProvider: false,
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

let process = (~id: Rpc.message_id, ()) => {
  let response = {
    jsonrpc: Rpc.version,
    id,
    result: {
      capabilities: capabilities,
    },
  };

  let res = capabilities_response_to_yojson(response);
  let str_json = Yojson.Safe.to_string(res);

  Rpc.send(stdout, str_json);
};
