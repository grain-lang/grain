type msg_id;

type protocol_msg =
  | Message(msg_id, string, Yojson.Safe.t)
  | Error(string)
  | Notification(string, Yojson.Safe.t);

[@deriving yojson]
type completion_values = {
  resolveProvider: bool,
  triggerCharacters: list(string),
};

[@deriving yojson]
type lsp_error = {
  file: string,
  line: int,
  startchar: int,
  endline: int,
  endchar: int,
  lsp_message: string,
};

[@deriving yojson]
type lsp_warning = {
  file: string,
  line: int,
  startchar: int,
  endline: int,
  endchar: int,
  number: int,
  lsp_message: string,
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
type position = {
  line: int,
  character: int,
};

[@deriving yojson]
type lens_t = {
  line: int,
  signature: string,
};

[@deriving yojson]
type range_t = {
  start_line: int,
  start_char: int,
  end_line: int,
  end_char: int,
};

[@deriving yojson]
type range = {
  start: position,
  [@key "end"]
  range_end: position,
};

[@deriving yojson]
type capabilities_result = {capabilities: lsp_capabilities};
[@deriving yojson]
type capabilities_response = {
  jsonrpc: string,
  id: int,
  result: capabilities_result,
};

[@deriving yojson]
type command_t = {
  title: string,
  // command: string,
};

[@deriving yojson]
type lsp_lens_t = {
  range,
  command: command_t,
};

[@deriving yojson]
type lens_response = {
  jsonrpc: string,
  id: int,
  result: list(lsp_lens_t),
};

[@deriving yojson]
type diagnostic_t = {
  range,
  severity: int,
  message: string,
};

[@deriving yojson]
type document_diagnostics = {
  uri: string,
  diagnostics: list(diagnostic_t),
};

[@deriving yojson]
type diagnostics_message = {
  jsonrpc: string,
  method: string,
  params: document_diagnostics,
};

[@deriving yojson]
type markup_content = {
  kind: string,
  value: string,
};

[@deriving yojson]
type marked_string = {
  language: string,
  value: string,
};

[@deriving yojson]
type hover_result = {
  contents: markup_content,
  range,
};
[@deriving yojson]
type null_response = {
  jsonrpc: string,
  id: int,
  result: option(string),
};

[@deriving yojson]
type hover_response = {
  jsonrpc: string,
  id: int,
  result: hover_result,
};

[@deriving yojson]
type definition_result = {
  uri: string,
  range,
};

[@deriving yojson]
type definition_response = {
  jsonrpc: string,
  id: int,
  result: definition_result,
};

// This is the full enumeration of all CompletionItemKind as declared by the language server
// protocol (https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind),
// but not all will be used by Grain LSP
[@deriving (enum, yojson)]
type completion_item_kind =
  // Since these are using ppx_deriving enum, order matters
  | [@value 1] CompletionItemKindText
  | CompletionItemKindMethod
  | CompletionItemKindFunction
  | CompletionItemKindConstructor
  | CompletionItemKindField
  | CompletionItemKindVariable
  | CompletionItemKindClass
  | CompletionItemKindInterface
  | CompletionItemKindModule
  | CompletionItemKindProperty
  | CompletionItemKindUnit
  | CompletionItemKindValue
  | CompletionItemKindEnum
  | CompletionItemKindKeyword
  | CompletionItemKindSnippet
  | CompletionItemKindColor
  | CompletionItemKindFile
  | CompletionItemKindReference
  | CompletionItemKindFolder
  | CompletionItemKindEnumMember
  | CompletionItemKindConstant
  | CompletionItemKindStruct
  | CompletionItemKindEvent
  | CompletionItemKindOperator
  | CompletionItemKindTypeParameter;

[@deriving yojson]
type completion_item = {
  label: string,
  kind: completion_item_kind,
  detail: string,
  documentation: string,
};

[@deriving yojson]
type completion_result = {
  isIncomplete: bool,
  items: list(completion_item),
};

[@deriving yojson]
type completion_response = {
  jsonrpc: string,
  id: int,
  result: completion_result,
};

let read_message: in_channel => protocol_msg;

let send: (out_channel, string) => unit;

let send_null_message: (out_channel, msg_id) => unit;

let send_capabilities: (out_channel, msg_id) => unit;

let send_lenses: (~output: out_channel, ~id: msg_id, list(lens_t)) => unit;

let send_hover:
  (~output: out_channel, ~id: msg_id, ~range: range_t, string) => unit;

let send_diagnostics:
  (
    ~output: out_channel,
    ~uri: string,
    list(lsp_warning),
    option(lsp_error)
  ) =>
  unit;

let clear_diagnostics: (~output: out_channel, string) => unit;

let send_completion:
  (~output: out_channel, ~id: msg_id, list(completion_item)) => unit;
