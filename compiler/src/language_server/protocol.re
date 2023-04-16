open Grain_utils;

let uri_to_yojson = (uri: Uri.t): Yojson.Safe.t =>
  Uri.to_string(uri) |> [%to_yojson: string];
let uri_of_yojson = (json: Yojson.Safe.t) =>
  json |> [%of_yojson: string] |> Result.map(Uri.of_string);

//https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#inlayHintOptions
[@deriving yojson]
type inlay_hint_options = {
  [@key "resolveProvider"]
  resolve_provider: bool,
};

type uri = [@to_yojson uri_to_yojson] [@of_yojson uri_of_yojson] Uri.t;

[@deriving yojson]
type version = string;

[@deriving yojson]
type message_id = int;

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

let diagnostic_severity_to_yojson = severity =>
  diagnostic_severity_to_enum(severity) |> [%to_yojson: int];
let diagnostic_severity_of_yojson = json =>
  Result.bind(json |> [%of_yojson: int], value => {
    switch (diagnostic_severity_of_enum(value)) {
    | Some(severity) => Ok(severity)
    | None => Result.Error("Invalid enum value")
    }
  });

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location
[@deriving yojson]
type location = {
  uri,
  range,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#locationLink
[@deriving yojson]
type location_link = {
  [@key "originSelectionRange"]
  origin_selection_range: range,
  [@key "targetUri"]
  target_uri: uri,
  [@key "targetRange"]
  target_range: range,
  [@key "targetSelectionRange"]
  target_selection_range: range,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticRelatedInformation
[@deriving yojson]
type diagnostic_related_information = {
  location,
  message: string,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
[@deriving yojson]
type diagnostic = {
  // TODO: Implement the rest of the fields
  range,
  severity: diagnostic_severity,
  message: string,
  [@key "relatedInformation"]
  related_information: list(diagnostic_related_information),
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#command
[@deriving yojson]
type command = {
  title: string,
  command: string,
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#traceValue
// TODO: Implement custom yojson serde functions to convert to an enum
[@deriving yojson]
type trace_value = string; // 'off' | 'messages' | 'verbose';

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentIdentifier
[@deriving yojson]
type text_document_identifier = {uri};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
[@deriving (enum, yojson)]
type text_document_sync_kind =
  | [@value 0] No // This is `None` via the spec, but we don't want to collide with Option's `None`
  | Full
  | Incremental;

//https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#definitionClientCapabilities
[@deriving yojson({strict: false})]
type definition_client_capabilities = {
  [@key "linkSupport"]
  link_support: bool,
};

let text_document_sync_kind_to_yojson = kind =>
  text_document_sync_kind_to_enum(kind) |> [%to_yojson: int];
let text_document_sync_kind_of_yojson = json =>
  Result.bind(json |> [%of_yojson: int], value => {
    switch (text_document_sync_kind_of_enum(value)) {
    | Some(kind) => Ok(kind)
    | None => Result.Error("Invalid enum value")
    }
  });

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
  | [@value (-32700)] ParseError
  | [@value (-32600)] InvalidRequest
  | [@value (-32601)] MethodNotFound
  | [@value (-32602)] InvalidParams
  | [@value (-32603)] InternalError
  | [@value (-32002)] ServerNotInitialized
  | [@value (-32001)] UnknownErrorCode
  | [@value (-32803)] RequestFailed
  | [@value (-32802)] ServerCancelled
  | [@value (-32801)] ContentModified
  | [@value (-32800)] RequestCancelled;

let error_code_to_yojson = kind =>
  error_code_to_enum(kind) |> [%to_yojson: int];
let error_code_of_yojson = json =>
  Result.bind(json |> [%of_yojson: int], value => {
    switch (error_code_of_enum(value)) {
    | Some(kind) => Ok(kind)
    | None => Result.Error("Invalid enum value")
    }
  });

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
  [@default None]
  id: option(message_id),
  method: string,
  [@default None]
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

let version: version = "2.0";

let header_prefix = "Content-Length: ";

let sep = "\r\n\r\n";

let request = (): result(request_message, string) => {
  switch (input_line(stdin)) {
  | exception exn => Error("Failed to read input")
  | header when String_utils.starts_with(header, header_prefix) =>
    let len =
      String_utils.slice(
        ~first=String.length(header_prefix),
        ~last=-1 /* don't include the CR of the CRLF */,
        header,
      );

    switch (int_of_string_opt(len)) {
    | None => Error("Invalid Content-Length in header")
    | Some(len) =>
      // The LSP specification uses Windows CRLF separators
      // and there's a CRLF between the header and the content we need to consume
      // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#headerPart

      switch (really_input_string(stdin, len + 2)) {
      | exception exn => Error("Failed to read message")
      | raw => request_message_of_yojson(Yojson.Safe.from_string(raw))
      }
    };
  | _ => Error("Invalid header")
  };
};

let response = (~id=?, result) => {
  let response_message = {
    jsonrpc: version,
    id,
    result: Some(result),
    error: None,
  };
  let content =
    Yojson.Safe.to_string(
      ~std=true,
      response_message_to_yojson(response_message),
    );
  let length = String.length(content);

  let len = string_of_int(length);

  let msg = header_prefix ++ len ++ sep ++ content;

  output_string(stdout, msg);

  flush(stdout);
};

let empty_response = id => {
  let response_message = {
    jsonrpc: version,
    id: Some(id),
    result: None,
    error: None,
  };
  let content =
    Yojson.Safe.to_string(
      ~std=true,
      response_message_to_yojson(response_message),
    );
  let length = String.length(content);

  let len = string_of_int(length);
  let msg = header_prefix ++ len ++ sep ++ content;
  output_string(stdout, msg);
  flush(stdout);
};

let error = (~id=?, error) => {
  let response_message = {
    jsonrpc: version,
    id,
    result: None,
    error: Some(error),
  };
  let content =
    Yojson.Safe.to_string(
      ~std=true,
      response_message_to_yojson(response_message),
    );
  let length = String.length(content);
  let len = string_of_int(length);
  let msg = header_prefix ++ len ++ sep ++ content;

  output_string(stdout, msg);

  flush(stdout);
};

let notification = (~method, params) => {
  let notification_message = {jsonrpc: version, method, params};
  let content =
    Yojson.Safe.to_string(
      ~std=true,
      notification_message_to_yojson(notification_message),
    );
  let length = String.length(content);

  let len = string_of_int(length);

  let msg = header_prefix ++ len ++ sep ++ content;

  output_string(stdout, msg);

  flush(stdout);
};
