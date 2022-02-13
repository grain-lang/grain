let jsonrpc = "2.0";

type protocol_msg =
  | Message(int, string, Yojson.Safe.t)
  | Error(string)
  | Notification(string, Yojson.Safe.t);

[@deriving yojson]
type completion_values = {
  resolveProvider: bool,
  triggerCharacters: list(string),
};

// [@deriving yojson]
// type signature_helpers = {
//   triggerCharacters: list(string),
//   retriggerCharacters: list(string),
// };

[@deriving yojson]
type code_values = {resolveProvider: bool};

[@deriving yojson]
type lsp_capabilities = {
  documentFormattingProvider: bool,
  textDocumentSync: int,
  hoverProvider: bool,
  completionProvider: completion_values,
  // signatureHelpProvider: signature_helpers,
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
  command: string,
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
  contents: marked_string,
  range,
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

[@deriving yojson]
type completion_item = {
  label: string,
  kind: int,
  detail: string,
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

let convert_range = range => {
  let r_start: position = {
    line: range.start_line - 1,
    character: range.start_char,
  };
  let r_end: position = {line: range.end_line - 1, character: range.end_char};
  {start: r_start, range_end: r_end};
};

let read_message = (log, input): protocol_msg => {
  let clength = input_line(input);
  let cl = "Content-Length: ";
  let cll = String.length(cl);
  if (String.sub(clength, 0, cll) == cl) {
    /* if on windows, dont need the extra -1 */
    let offset = Sys.os_type == "Win32" ? 0 : (-1); /* -1 for trailing \r */

    let num =
      String.sub(clength, cll, String.length(clength) - cll + offset);
    let num = (num |> int_of_string) + (Sys.os_type == "Win32" ? 1 : 2);
    let buffer = Buffer.create(num);
    Buffer.add_channel(buffer, input, num);
    let raw = Buffer.contents(buffer);
    let json = Yojson.Safe.from_string(raw);

    let action =
      Yojson.Safe.Util.member("method", json) |> Yojson.Safe.Util.to_string;

    let idOpt =
      Yojson.Safe.Util.member("id", json) |> Yojson.Safe.Util.to_int_option;

    switch (idOpt) {
    | None => Notification(action, json)
    | Some(id) => Message(id, action, json)
    };
  } else {
    failwith("Invalid header");
  };
};

let send = (output, content) => {
  let length = String.length(content);
  let sep = Sys.os_type == "Unix" ? "\r\n\r\n" : "\n\n";
  let len = string_of_int(length);

  output_string(output, "Content-Length: " ++ len ++ sep ++ content);
  flush(output);
};

let send_null_message = (log, output, id) => {
  let res =
    `Assoc([
      ("jsonrpc", `String("2.0")),
      ("id", `Int(id)),
      ("result", `Null),
    ]);

  let strJson = Yojson.Safe.pretty_to_string(res);
  log(strJson);
  send(output, strJson);
};

let send_capabilities = (log, output, id: int) => {
  //   let sig_helpers: signature_helpers = {
  //     triggerCharacters: ["("],
  //     retriggerCharacters: [","],
  //   };

  let completion_vals: completion_values = {
    resolveProvider: true,
    triggerCharacters: ["."],
  };

  let codeVals: code_values = {resolveProvider: true};

  let capabilities: lsp_capabilities = {
    documentFormattingProvider: false,
    textDocumentSync: 1,
    hoverProvider: true,
    completionProvider: completion_vals,
    //  signatureHelpProvider: sig_helpers,
    definitionProvider: false, // disabled until we can resolve the external module location
    typeDefinitionProvider: false,
    referencesProvider: false,
    documentSymbolProvider: false,
    codeActionProvider: false,
    codeLensProvider: codeVals,
    documentHighlightProvider: false,
    documentRangeFormattingProvider: false,
    renameProvider: false,
  };

  let response: capabilities_response = {
    jsonrpc,
    id,
    result: {
      capabilities: capabilities,
    },
  };

  let res = capabilities_response_to_yojson(response);
  let strJson = Yojson.Safe.to_string(res);
  log(strJson);
  send(output, strJson);
};

let send_lenses = (log, output, id: int, lenses: list(lens_t)) => {
  let convertedLenses =
    List.map(
      (l: lens_t) => {
        let rstart: position = {line: l.line - 1, character: 1};
        let rend: position = {line: l.line - 1, character: 1};
        let range = {
          {start: rstart, range_end: rend};
        };

        let command = {title: l.signature, command: "command string"};
        let lsp_lens: lsp_lens_t = {range, command};
        lsp_lens;
      },
      lenses,
    );

  let response: lens_response = {jsonrpc, id, result: convertedLenses};
  let res = lens_response_to_yojson(response);
  let strJson = Yojson.Safe.pretty_to_string(res);
  send(output, strJson);
};

let send_hover = (log, output, id: int, signature, range: range_t) => {
  let range_ext = convert_range(range);
  let hover_info: hover_result = {
    contents: {
      language: "grain",
      value: signature,
    },
    range: range_ext,
  };
  let response: hover_response = {jsonrpc, id, result: hover_info};
  let res = hover_response_to_yojson(response);
  let strJson = Yojson.Safe.pretty_to_string(res);
  log(strJson);
  send(output, strJson);
};

let send_diagnostics =
    (
      log,
      output,
      uri,
      error: option(Grain_diagnostics.Output.lsp_error),
      warnings: option(list(Grain_diagnostics.Output.lsp_warning)),
    ) => {
  let errorDiags =
    switch (error) {
    | None => []
    | Some(err) =>
      let rstart: position = {line: err.line - 1, character: err.startchar};
      let rend: position = {line: err.endline - 1, character: err.endchar};
      let range = {start: rstart, range_end: rend};

      [{range, severity: 1, message: err.lsp_message}];
    };

  let with_warnings =
    switch (warnings) {
    | None => errorDiags
    | Some(warns) =>
      let warningDiags =
        List.map(
          (w: Grain_diagnostics.Output.lsp_warning) => {
            let rstart: position = {line: w.line - 1, character: w.startchar};
            let rend: position = {line: w.endline - 1, character: w.endchar};
            let range = {start: rstart, range_end: rend};
            {range, severity: 2, message: w.lsp_message};
          },
          warns,
        );
      List.append(errorDiags, warningDiags);
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
  log(jsonMessage);
  send(output, jsonMessage);
};

let clear_diagnostics = (log, output, uri) => {
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

let send_go_to_definition =
    (log, output, id: int, uri: string, range: range_t) => {
  let range_ext = convert_range(range);

  let definition_info: definition_result = {uri, range: range_ext};
  let response: definition_response = {jsonrpc, id, result: definition_info};
  let res = definition_response_to_yojson(response);
  let strJson = Yojson.Safe.pretty_to_string(res);

  send(output, strJson);
};

let send_completion =
    (log, output, id: int, completions: list(completion_item)) => {
  // let items = List.map(item => completion_item_to_yojson(item), completions);

  // let sigInfo =
  //   `Assoc([("isIncomplete", `Bool(false)), ("items", `List(items))]);

  // let res =
  //   `Assoc([
  //     ("jsonrpc", `String("2.0")),
  //     ("id", `Int(id)),
  //     ("result", sigInfo),
  //   ]);

  let completion_info: completion_result = {
    isIncomplete: false,
    items: completions,
  };
  let response: completion_response = {jsonrpc, id, result: completion_info};

  let res = completion_response_to_yojson(response);
  let strJson = Yojson.Safe.pretty_to_string(res);

  send(output, strJson);
};
