open Grain_typed;
open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_formatting;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentFormattingParams
module RequestParams = {
  // TODO: Implement the rest of the fields
  [@deriving yojson({strict: false})]
  type formatting_options = {
    [@key "tabSize"]
    tab_size: int,
    [@key "insertSpaces"]
    insert_spaces: bool,
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
  type t = list(text_edit);
};

let process =
    (
      ~id: Protocol.message_id,
      ~uri: Protocol.uri,
      ~compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(documents, params.text_document.uri)) {
  | None =>
    Trace.log("The code requested isn't available on the server");
    Protocol.error_response(
      ~id,
      ~code=0,
      "The source code to be formatted isn't available",
    );
  | Some(compiled_code) =>
    switch (Format.parse_source(compiled_code)) {
    | Ok((parsed_program, lines, eol)) =>
      let formatted_code =
        Format.format_ast(~original_source=lines, ~eol, parsed_program);

      let range: Protocol.range = {
        range_start: {
          line: 0,
          character: 0,
        },
        range_end: {
          line: Int.max_int,
          character: Int.max_int,
        },
      };

      Trace.log("ready to return a formatted result");
      let res: ResponseResult.t = [{range, newText: formatted_code}];
      Protocol.response(~id, ResponseResult.to_yojson(res));
    | Error(ParseError(_)) =>
      Trace.log("Unable to parse source code");
      Protocol.error_response(
        ~id,
        ~code=-32700,
        "Unable to format the source code",
      );
    | Error(InvalidCompilationState) =>
      Trace.log("Reached an invalid compilation state");
      Protocol.error_response(
        ~id,
        ~code=-32700,
        "Unable to format the source code",
      );
    }
  };
};
