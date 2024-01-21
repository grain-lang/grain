open Grain;
open Compile;
open Grain_utils;
open Grain_typed;
open Sourcetree;
open Lsp_types;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#publishDiagnosticsParams
module NotificationParams = {
  [@deriving yojson]
  type t = {
    uri: Protocol.uri,
    diagnostics: list(Protocol.diagnostic),
  };
};

type compile_result = {
  program: option(Typedtree.typed_program),
  error: option(Protocol.diagnostic),
  warnings: list(Protocol.diagnostic),
};

let warning_to_diagnostic =
    ((loc: Grain_utils.Warnings.loc, warn: Grain_utils.Warnings.t))
    : Protocol.diagnostic => {
  let (_, line, startchar) =
    Grain_parsing.Location.get_pos_info(loc.loc_start);
  let (_, endline, endchar) =
    Grain_parsing.Location.get_pos_info(loc.loc_end);

  let range: Protocol.range = {
    range_start: {
      line: line - 1,
      character: startchar,
    },
    range_end: {
      line: endline - 1,
      character: endchar,
    },
  };

  {
    range,
    severity: Warning,
    message: Grain_utils.Warnings.message(warn),
    related_information: [],
  };
};

let compile_source = (uri, source) => {
  let filename = Utils.uri_to_filename(uri);

  Trace.log("Compiling " ++ filename);

  switch (
    Compile.compile_string(
      ~is_root_file=true,
      ~hook=stop_after_typed_well_formed,
      ~name=filename,
      source,
    )
  ) {
  | exception exn =>
    switch (Grain_parsing.Location.error_of_exn(exn)) {
    | Some(`Ok(e)) =>
      let (file, line, startchar) =
        Grain_parsing.Location.get_pos_info(e.error_loc.loc_start);
      let (_, endline, endchar) =
        Grain_parsing.Location.get_pos_info(e.error_loc.loc_end);

      let startchar = startchar < 0 ? 0 : startchar;
      let endchar = endchar < 0 ? 0 : endchar;

      let error: Protocol.diagnostic =
        if (filename == file) {
          let source_range: Protocol.range = {
            range_start: {
              line: line - 1,
              character: startchar,
            },
            range_end: {
              line: endline - 1,
              character: endchar,
            },
          };

          {
            range: source_range,
            severity: Error,
            message: e.msg,
            related_information: [],
          };
        } else {
          let source_range: Protocol.range = {
            range_start: {
              line: 0,
              character: 0,
            },
            range_end: {
              line: 0,
              character: 1,
            },
          };

          let file_range: Protocol.range = {
            range_start: {
              line: line - 1,
              character: startchar,
            },
            range_end: {
              line: endline - 1,
              character: endchar,
            },
          };

          {
            range: source_range,
            severity: Error,
            message: "Failed to compile " ++ file,
            related_information: [
              {
                location: {
                  uri: Utils.filename_to_uri(file),
                  range: file_range,
                },
                message: e.msg,
              },
            ],
          };
        };

      {program: None, error: Some(error), warnings: []};
    | _ =>
      let range: Protocol.range = {
        range_start: {
          line: 0,
          character: 0,
        },
        range_end: {
          line: 0,
          character: 1,
        },
      };

      {
        program: None,
        error:
          Some({
            range,
            severity: Error,
            message: "Unable to parse",
            related_information: [],
          }),
        warnings: [],
      };
    }

  | {cstate_desc: TypedWellFormed(typed_program)} =>
    let warnings =
      List.map(warning_to_diagnostic, Grain_utils.Warnings.get_warnings());
    {program: Some(typed_program), error: None, warnings};
  | _ =>
    let range: Protocol.range = {
      range_start: {
        line: 0,
        character: 0,
      },
      range_end: {
        line: 0,
        character: 1,
      },
    };

    {
      program: None,
      error:
        Some({
          range,
          severity: Error,
          message: "Compilation failed with an internal error",
          related_information: [],
        }),
      warnings: [],
    };
  };
};

let send_diagnostics =
    (
      ~uri,
      warnings: list(Protocol.diagnostic),
      error: option(Protocol.diagnostic),
    ) => {
  let diagnostics =
    switch (error) {
    | None => warnings
    | Some(err) => [err, ...warnings]
    };

  Protocol.notification(
    ~method="textDocument/publishDiagnostics",
    NotificationParams.to_yojson({uri, diagnostics}),
  );
};

let clear_diagnostics = (~uri, ()) => {
  Protocol.notification(
    ~method="textDocument/publishDiagnostics",
    NotificationParams.to_yojson({uri, diagnostics: []}),
  );
};

module DidOpen = {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didOpenTextDocumentParams
  module RequestParams = {
    [@deriving yojson({strict: false})]
    type t = {
      [@key "textDocument"]
      text_document: Protocol.text_document_item,
    };
  };

  let process =
      (
        ~uri: Protocol.uri,
        ~compiled_code: Hashtbl.t(Protocol.uri, code),
        ~documents: Hashtbl.t(Protocol.uri, string),
        params: RequestParams.t,
      ) => {
    Hashtbl.replace(documents, uri, params.text_document.text);

    let compilerRes = compile_source(uri, params.text_document.text);
    switch (compilerRes) {
    | {program: Some(typed_program), error: None, warnings} =>
      Hashtbl.replace(
        compiled_code,
        uri,
        {
          program: typed_program,
          sourcetree: Sourcetree.from_program(typed_program),
          dirty: false,
        },
      );
      switch (warnings) {
      | [] => clear_diagnostics(~uri, ())
      | _ => send_diagnostics(~uri, warnings, None)
      };

    | {program, error: Some(err), warnings} =>
      switch (Hashtbl.find_opt(compiled_code, uri)) {
      | Some(code) =>
        Hashtbl.replace(compiled_code, uri, {...code, dirty: true})
      | None => ()
      };
      send_diagnostics(~uri, warnings, Some(err));
    | {program: None, error: None, warnings} => clear_diagnostics(~uri, ())
    };
  };
};

module DidChange = {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didChangeTextDocumentParams
  module RequestParams = {
    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentContentChangeEvent
    [@deriving yojson({strict: false})]
    type text_document_content_change_event = {
      [@default None]
      range: option(Protocol.range),
      text: string,
    };

    [@deriving yojson({strict: false})]
    type t = {
      [@key "textDocument"]
      text_document: Protocol.versioned_text_document_identifier,
      [@key "contentChanges"]
      content_changes: list(text_document_content_change_event),
    };
  };

  let process =
      (
        ~uri: Protocol.uri,
        ~compiled_code: Hashtbl.t(Protocol.uri, code),
        ~documents: Hashtbl.t(Protocol.uri, string),
        params: RequestParams.t,
      ) => {
    // TODO: Handle all `content_changes` items
    let change = List.hd(params.content_changes);
    Hashtbl.replace(documents, uri, change.text);

    let compilerRes = compile_source(uri, change.text);
    switch (compilerRes) {
    | {program: Some(typed_program), error: None, warnings} =>
      Hashtbl.replace(
        compiled_code,
        uri,
        {
          program: typed_program,
          sourcetree: Sourcetree.from_program(typed_program),
          dirty: false,
        },
      );
      switch (warnings) {
      | [] => clear_diagnostics(~uri, ())
      | _ => send_diagnostics(~uri, warnings, None)
      };

    | {program, error: Some(err), warnings} =>
      switch (Hashtbl.find_opt(compiled_code, uri)) {
      | Some(code) =>
        Hashtbl.replace(compiled_code, uri, {...code, dirty: true})
      | None => ()
      };
      send_diagnostics(~uri, warnings, Some(err));
    | {program: None, error: None, warnings} => clear_diagnostics(~uri, ())
    };
  };
};
