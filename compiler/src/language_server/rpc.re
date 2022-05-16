let windows_mode = ref(false);

[@deriving yojson]
type msg_id = int;

type protocol_msg =
  | Message(msg_id, string, Yojson.Safe.t)
  | Error(string)
  | Notification(string, Yojson.Safe.t);

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

let jsonrpc = "2.0";

let convert_range = range => {
  let r_start: position = {
    line: range.start_line - 1,
    character: range.start_char,
  };
  let r_end: position = {line: range.end_line - 1, character: range.end_char};
  {start: r_start, range_end: r_end};
};

let parse_message = raw => {
  let json = Yojson.Safe.from_string(raw);

  let action =
    Yojson.Safe.Util.member("method", json) |> Yojson.Safe.Util.to_string;

  let id_opt =
    Yojson.Safe.Util.member("id", json) |> Yojson.Safe.Util.to_int_option;

  switch (id_opt) {
  | None => Notification(action, json)
  | Some(id) => Message(id, action, json)
  };
};

let read_message = (input): protocol_msg => {
  Log.log("read_message");

  // TODO: Catch error?
  let clength = input_line(input);

  // This feeld a bit backwards, but on a Windows machine the \r will have been stripped out automatically
  // and so bizarrley if the Windows separator is there, then this is not a Windows machine!
  // if (!String.contains(clength, '\r')) {
  //   Log.log("Enabling Windows mode");
  //   windows_mode := true;
  // };

  let cl = "Content-Length: ";
  let cll = String.length(cl);
  if (String.sub(clength, 0, cll) == cl) {
    /* if on windows, dont need the extra -1 */

    let offset = windows_mode^ ? 0 : (-1); /* -1 for trailing \r */

    // Can't use from_channel as we need to read the number of characters specified only

    let num =
      String.sub(clength, cll, String.length(clength) - cll + offset);
    let num = (num |> int_of_string) + (windows_mode^ ? 1 : 2);
    let buffer = Buffer.create(num);
    Buffer.add_channel(buffer, input, num);
    let raw = Buffer.contents(buffer);

    parse_message(raw);
  } else {
    Error("Invalid header");
  };
};

let send = (output, content) => {
  let length = String.length(content);

  let sep = windows_mode^ ? "\n\n" : "\r\n\r\n";

  let len = string_of_int(length);

  let msg = "Content-Length: " ++ len ++ sep ++ content;

  Log.log(msg);

  output_string(output, msg);

  flush(output);
};

let send_null_message = (output, id) => {
  let empty_response: null_response = {jsonrpc: "2.0", id, result: None};
  let res = null_response_to_yojson(empty_response);
  let str_json = Yojson.Safe.to_string(res);
  send(output, str_json);
};

let send_lenses = (~output, ~id: int, lenses: list(lens_t)) => {
  let converted_lenses =
    List.map(
      (l: lens_t) => {
        let rstart: position = {line: l.line - 1, character: 1};
        let rend: position = {line: l.line - 1, character: 1};
        let range = {
          {start: rstart, range_end: rend};
        };

        //TODO: Why was this "command string"
        let command = {title: l.signature};
        let lsp_lens: lsp_lens_t = {range, command};
        lsp_lens;
      },
      lenses,
    );

  let response: lens_response = {jsonrpc, id, result: converted_lenses};
  let res = lens_response_to_yojson(response);
  let str_json = Yojson.Safe.to_string(res);
  send(output, str_json);
};

let send_hover = (~output, ~id: int, ~range: range_t, signature) => {
  let range_ext = convert_range(range);
  let hover_info: hover_result = {
    contents: {
      kind: "markdown",
      value: signature,
    },
    range: range_ext,
  };
  let response: hover_response = {jsonrpc, id, result: hover_info};
  let res = hover_response_to_yojson(response);
  let str_json = Yojson.Safe.to_string(res);
  send(output, str_json);
};

let send_diagnostics =
    (~output, ~uri, warnings: list(lsp_warning), error: option(lsp_error)) => {
  let error_diags =
    switch (error) {
    | None => []
    | Some(err) =>
      if (err.line < 0 || err.startchar < 0) {
        // dummy location so set to zero
        let rstart: position = {line: 0, character: 0};
        let rend: position = {line: 0, character: 0};
        let range = {start: rstart, range_end: rend};
        [{range, severity: 1, message: err.lsp_message}];
      } else {
        let rstart: position = {line: err.line - 1, character: err.startchar};
        let rend: position = {line: err.endline - 1, character: err.endchar};
        let range = {start: rstart, range_end: rend};

        [{range, severity: 1, message: err.lsp_message}];
      }
    };

  let with_warnings =
    switch (warnings) {
    | [] => error_diags
    | _ =>
      let warnings_diags =
        List.map(
          (w: lsp_warning) =>
            if (w.line < 0 || w.startchar < 0) {
              // dummy location so set to zero
              let rstart: position = {line: 0, character: 0};
              let rend: position = {line: 0, character: 0};
              let range = {start: rstart, range_end: rend};
              {range, severity: 2, message: w.lsp_message};
            } else {
              let rstart: position = {
                line: w.line - 1,
                character: w.startchar,
              };
              let rend: position = {
                line: w.endline - 1,
                character: w.endchar,
              };
              let range = {start: rstart, range_end: rend};
              {range, severity: 2, message: w.lsp_message};
            },
          warnings,
        );
      List.append(error_diags, warnings_diags);
    };

  let message: diagnostics_message = {
    jsonrpc,
    method: "textDocument/publishDiagnostics",
    params: {
      uri,
      diagnostics: with_warnings,
    },
  };

  let jsonMessage =
    Yojson.Safe.to_string(diagnostics_message_to_yojson(message));

  send(output, jsonMessage);
};

let clear_diagnostics = (~output, uri) => {
  let message: diagnostics_message = {
    jsonrpc,
    method: "textDocument/publishDiagnostics",
    params: {
      uri,
      diagnostics: [],
    },
  };

  let jsonMessage =
    Yojson.Safe.to_string(diagnostics_message_to_yojson(message));

  send(output, jsonMessage);
};

let send_completion = (~output, ~id: int, completions: list(completion_item)) => {
  let completion_info: completion_result = {
    isIncomplete: false,
    items: completions,
  };
  let response: completion_response = {jsonrpc, id, result: completion_info};

  let res = completion_response_to_yojson(response);
  let str_json = Yojson.Safe.to_string(res);

  send(output, str_json);
};
