open Grain_utils;

[@deriving yojson]
type version;

[@deriving yojson]
type message_id;

[@deriving yojson]
type uri;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position
[@deriving yojson({strict: false})]
type position = {
  line: int,
  character: int,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range
[@deriving yojson]
type range = {
  [@key "start"]
  range_start: position,
  [@key "end"]
  range_end: position,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
[@deriving (enum, yojson)]
type diagnostic_severity =
  | [@value 1] Error
  | Warning
  | Information
  | Hint;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
[@deriving yojson]
type diagnostic = {
  range,
  severity: diagnostic_severity,
  message: string,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#command
[@deriving yojson]
type command = {
  title: string,
  command: string,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#traceValue
[@deriving yojson]
type trace_value = string; // 'off' | 'messages' | 'verbose';

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentIdentifier
[@deriving yojson]
type text_document_identifier = {uri};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
[@deriving (enum, yojson)]
type text_document_sync_kind =
  | No
  | Full
  | Incremental;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentItem
[@deriving yojson({strict: false})]
type text_document_item = {
  uri,
  languageId: string,
  version: int,
  text: string,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#versionedTextDocumentIdentifier
[@deriving yojson({strict: false})]
type versioned_text_document_identifier = {
  uri,
  version: int,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#errorCodes
[@deriving (enum, yojson)]
type error_code =
  | ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerNotInitialized
  | UnknownErrorCode
  | RequestFailed
  | ServerCancelled
  | ContentModified
  | RequestCancelled;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseError
[@deriving yojson({strict: false})]
type response_error = {
  code: error_code,
  message: string,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage
[@deriving yojson({strict: false})]
type request_message = {
  jsonrpc: version,
  id: option(message_id),
  method: string,
  params: option(Yojson.Safe.t),
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseMessage
[@deriving yojson({strict: false})]
type response_message = {
  jsonrpc: version,
  id: option(message_id),
  result: option(Yojson.Safe.t),
  error: option(response_error),
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage
[@deriving yojson({strict: false})]
type notification_message = {
  jsonrpc: version,
  method: string,
  params: Yojson.Safe.t,
};

let request: unit => result(request_message, string);

let response: (~id: message_id=?, Yojson.Safe.t) => unit;

let empty_response: message_id => unit;

let error: (~id: message_id=?, response_error) => unit;

let notification: (~method: string, Yojson.Safe.t) => unit;

let uri_to_filename: uri => string;
