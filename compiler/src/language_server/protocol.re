open Grain_utils;

[@deriving yojson]
type version = string;

[@deriving yojson]
type message_id = int;

[@deriving yojson]
type uri = string;

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
  // TODO: Implement the rest of the fields
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
// TODO: Implement custom yojson serde functions to convert to an enum
[@deriving yojson]
type trace_value = string; // 'off' | 'messages' | 'verbose';

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentIdentifier
[@deriving yojson]
type text_document_identifier = {uri};

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
  result: Yojson.Safe.t,
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

let request = (): result(request_message, string) => {
  switch (input_line(stdin)) {
  | exception exn => Error("Failed to read input")
  | header when String_utils.starts_with(header, header_prefix) =>
    let len =
      String_utils.slice(
        ~first=String.length(header_prefix),
        ~last=-1 /* newline? */,
        header,
      );

    switch (int_of_string_opt(len)) {
    | None => Error("Invalid Content-Length in header")
    | Some(len) =>
      // TODO: Comment about this +2
      switch (really_input_string(stdin, len + 2)) {
      | exception exn => Error("Failed to read message")
      | raw => request_message_of_yojson(Yojson.Safe.from_string(raw))
      }
    };
  | _ => Error("Invalid header")
  };
};

let response = (~id=?, result) => {
  let response_message = {jsonrpc: version, id, result};
  let content =
    Yojson.Safe.to_string(
      ~std=true,
      response_message_to_yojson(response_message),
    );
  let length = String.length(content);

  let sep = "\r\n\r\n";

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

  let sep = "\r\n\r\n";

  let len = string_of_int(length);

  let msg = header_prefix ++ len ++ sep ++ content;

  output_string(stdout, msg);

  flush(stdout);
};

let uri_to_filename = (uri: uri): string => {
  let filetype = "file://";
  let typelen = String.length(filetype);
  if (String_utils.starts_with(String.lowercase_ascii(uri), filetype)) {
    String_utils.slice(~first=typelen, uri);
  } else {
    uri;
  };
};
