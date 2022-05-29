open Grain_typed;
open Grain_formatting;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentFormattingParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type formatting_options = {
    [@key "tabSize"]
    tab_size: int,
    [@key "insertSpaces"]
    insert_spaces: bool,
    [@key "trimTrailingWhitespace"] [@default None]
    trim_trailing_whitespace: option(bool),
    [@key "insertFinalNewLine"] [@default None]
    insert_final_new_line: option(bool),
    [@key "trimFinalNewlines"] [@default None]
    trim_final_newlines: option(bool),
  };

  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    options: formatting_options,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textEdit
module ResponseResult = {
  [@deriving yojson]
  type text_edit = {
    range: Protocol.range,
    newText: string,
  };

  [@deriving yojson]
  type t = option(list(text_edit));
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(documents, params.text_document.uri)) {
  | None =>
    Protocol.error(
      ~id,
      {
        code: InvalidParams,
        message:
          "The document is not available on the server: "
          ++ Protocol.uri_to_filename(params.text_document.uri),
      },
    )
  | Some(compiled_code) =>
    switch (Format.parse_source(compiled_code)) {
    | Ok((parsed_program, lines, eol)) =>
      // I'm pretty sure this code path can raise errors. We should change these to Results
      try({
        let formatted_code =
          Format.format_ast(~original_source=lines, ~eol, parsed_program);

        let range: Protocol.range = {
          range_start: {
            line: 0,
            character: 0,
          },
          range_end:
            // Use Int32.max_int to ensure we fit the entire number in JSON
            {
              line: Int32.to_int(Int32.max_int),
              character: Int32.to_int(Int32.max_int),
            },
        };

        let res: ResponseResult.t = Some([{range, newText: formatted_code}]);
        Protocol.response(~id, ResponseResult.to_yojson(res));
      }) {
      | exn =>
        Protocol.error(
          ~id,
          {
            code: RequestFailed,
            message:
              "Failed to format the document: "
              ++ Protocol.uri_to_filename(params.text_document.uri),
          },
        )
      }
    | Error(ParseError(_)) =>
      Protocol.error(
        ~id,
        {
          code: RequestFailed,
          message:
            "Unable to parse the document: "
            ++ Protocol.uri_to_filename(params.text_document.uri),
        },
      )
    | Error(InvalidCompilationState) =>
      Protocol.error(
        ~id,
        {
          code: RequestFailed,
          message:
            "Reached an invalid compilation state when compiling: "
            ++ Protocol.uri_to_filename(params.text_document.uri),
        },
      )
    }
  };
};
