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
  error: list(Protocol.diagnostic),
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

let compile = (file, src) => {
  reset_compiler_state();
  Module_resolution.load_dependency_graph_from_string(file, src);
  let to_compile = Module_resolution.get_out_of_date_dependencies();
  List.iter(
    file => {
      ignore(
        compile_file(~outfile=Compile.default_object_filename(file), file),
      )
    },
    to_compile,
  );
  compile_string(~hook=stop_after_typed_well_formed, ~name=file, src);
};

let replace_line_range = (source, range: Protocol.range, replacement) => {
  let lines = String.split_on_char('\n', source);
  let start_line = range.range_start.line;
  let end_line = range.range_end.line;
  if (start_line != end_line) {
    source;
  } else {
    switch (List.nth_opt(lines, start_line)) {
    | None => source
    | Some(line) =>
      let before = String.sub(line, 0, range.range_start.character);
      let after =
        String.sub(
          line,
          range.range_end.character,
          String.length(line) - range.range_end.character,
        );
      let next_line = before ++ replacement ++ after;
      List.mapi((idx, l) => idx == start_line ? next_line : l, lines)
      |> String.concat("\n");
    };
  };
};

let message_contains = (needle, haystack) =>
  try(
    {
      ignore(Str.search_forward(Str.regexp_string(needle), haystack, 0));
      true;
    }
  ) {
  | Not_found => false
  };

let recover_source_from_diagnostics = (source, errors) =>
  List.fold_left(
    (source, diagnostic: Protocol.diagnostic) =>
      if (String.starts_with(~prefix="Unbound value", diagnostic.message)) {
        replace_line_range(source, diagnostic.range, "true");
      } else if (message_contains(
                   "Expected an expression",
                   diagnostic.message,
                 )) {
        replace_line_range(source, diagnostic.range, "void");
      } else {
        source;
      },
    source,
    errors,
  );

let try_compile_recovered = (filename, source, errors) => {
  let recovered = recover_source_from_diagnostics(source, errors);
  if (recovered == source) {
    None;
  } else {
    switch (
      Config.preserve_config(() => {
        Config.print_warnings := false;
        compile(filename, recovered);
      })
    ) {
    | exception _ => None
    | {cstate_desc: TypedWellFormed(typed_program)} => Some(typed_program)
    | _ => None
    };
  };
};

let store_compiled_program =
    (
      ~compiled_code: Hashtbl.t(Protocol.uri, code),
      uri,
      typed_program: Typedtree.typed_program,
      ~dirty,
    ) => {
  Hashtbl.replace(
    compiled_code,
    uri,
    {
      program: typed_program,
      sourcetree: Sourcetree.from_program(typed_program),
      dirty,
    },
  );
};

let compile_source = (uri, source) => {
  let filename = Utils.uri_to_filename(uri);

  Trace.log("Compiling " ++ filename);

  switch (
    Config.preserve_config(() => {
      // Warnings will be reported in diagnostics so no need to print them
      Config.print_warnings := false;
      compile(filename, source);
    })
  ) {
  | exception exn =>
    let file_start_range: Protocol.range = {
      range_start: {
        line: 0,
        character: 0,
      },
      range_end: {
        line: 0,
        character: 1,
      },
    };
    let errors =
      List.map(
        exn => {
          switch (Grain_parsing.Location.error_of_exn(exn)) {
          | Some(`Ok(e)) =>
            let (file, line, startchar) =
              Grain_parsing.Location.get_pos_info(e.error_loc.loc_start);
            let (_, endline, endchar) =
              Grain_parsing.Location.get_pos_info(e.error_loc.loc_end);

            let startchar = startchar < 0 ? 0 : startchar;
            let endchar = endchar < 0 ? 0 : endchar;

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

            let error: Protocol.diagnostic =
              if (filename == file) {
                {
                  range: file_range,
                  severity: Error,
                  message: e.msg,
                  related_information: [],
                };
              } else {
                {
                  range: file_start_range,
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
            error;
          | _ => {
              range: file_start_range,
              severity: Error,
              message: "Unable to parse",
              related_information: [],
            }
          }
        },
        [exn, ...Grain_parsing.Location.reported_exceptions^],
      );
    let program =
      switch (try_compile_recovered(filename, source, errors)) {
      | Some(typed_program) => Some(typed_program)
      | None => None
      };
    {
      program,
      error: errors,
      warnings: [],
    };
  | {cstate_desc: TypedWellFormed(typed_program)} =>
    let warnings =
      List.map(warning_to_diagnostic, Grain_utils.Warnings.get_warnings());
    {
      program: Some(typed_program),
      error: [],
      warnings,
    };
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
      error: [
        {
          range,
          severity: Error,
          message: "Compilation failed with an internal error",
          related_information: [],
        },
      ],
      warnings: [],
    };
  };
};

let send_diagnostics =
    (
      ~uri,
      warnings: list(Protocol.diagnostic),
      errors: list(Protocol.diagnostic),
    ) => {
  let diagnostics = List.append(errors, warnings);

  Protocol.notification(
    ~method="textDocument/publishDiagnostics",
    NotificationParams.to_yojson({
      uri,
      diagnostics,
    }),
  );
};

let clear_diagnostics = (~uri, ()) => {
  Protocol.notification(
    ~method="textDocument/publishDiagnostics",
    NotificationParams.to_yojson({
      uri,
      diagnostics: [],
    }),
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
        ~parse_trees:
           Hashtbl.t(Protocol.uri, (int, Grain_tree_sitter.parse_tree)),
        params: RequestParams.t,
      ) => {
    Hashtbl.replace(documents, uri, params.text_document.text);
    let _ =
      Grain_tree_sitter.cached_tree(
        ~parse_trees,
        uri,
        params.text_document.text,
      );

    let compilerRes = compile_source(uri, params.text_document.text);
    switch (compilerRes) {
    | {program: Some(typed_program), error: [], warnings} =>
      store_compiled_program(
        ~compiled_code,
        uri,
        typed_program,
        ~dirty=false,
      );
      switch (warnings) {
      | [] => clear_diagnostics(~uri, ())
      | _ => send_diagnostics(~uri, warnings, [])
      };

    | {program: Some(typed_program), error: [_, ..._] as errs, warnings} =>
      store_compiled_program(~compiled_code, uri, typed_program, ~dirty=true);
      send_diagnostics(~uri, warnings, errs);
    | {program: None, error: [_, ..._] as errs, warnings} =>
      switch (Hashtbl.find_opt(compiled_code, uri)) {
      | Some(code) =>
        Hashtbl.replace(
          compiled_code,
          uri,
          {
            ...code,
            dirty: true,
          },
        )
      | None => ()
      };
      send_diagnostics(~uri, warnings, errs);
    | {program: None, error: [], warnings} => clear_diagnostics(~uri, ())
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
        ~parse_trees:
           Hashtbl.t(Protocol.uri, (int, Grain_tree_sitter.parse_tree)),
        params: RequestParams.t,
      ) => {
    // TODO: Handle all `content_changes` items
    let change = List.hd(params.content_changes);
    Hashtbl.replace(documents, uri, change.text);
    let _ = Grain_tree_sitter.cached_tree(~parse_trees, uri, change.text);

    let compilerRes = compile_source(uri, change.text);
    switch (compilerRes) {
    | {program: Some(typed_program), error: [], warnings} =>
      store_compiled_program(
        ~compiled_code,
        uri,
        typed_program,
        ~dirty=false,
      );
      switch (warnings) {
      | [] => clear_diagnostics(~uri, ())
      | _ => send_diagnostics(~uri, warnings, [])
      };

    | {program: Some(typed_program), error: [_, ..._] as errs, warnings} =>
      store_compiled_program(~compiled_code, uri, typed_program, ~dirty=true);
      send_diagnostics(~uri, warnings, errs);
    | {program: None, error: [_, ..._] as errs, warnings} =>
      switch (Hashtbl.find_opt(compiled_code, uri)) {
      | Some(code) =>
        Hashtbl.replace(
          compiled_code,
          uri,
          {
            ...code,
            dirty: true,
          },
        )
      | None => ()
      };
      send_diagnostics(~uri, warnings, errs);
    | {program: None, error: [], warnings} => clear_diagnostics(~uri, ())
    };
  };
};

module DidClose = {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didCloseTextDocumentParams
  module RequestParams = {
    [@deriving yojson({strict: false})]
    type t = {
      [@key "textDocument"]
      text_document: Protocol.text_document_identifier,
    };
  };

  let process =
      (
        ~uri: Protocol.uri,
        ~compiled_code: Hashtbl.t(Protocol.uri, code),
        ~documents: Hashtbl.t(Protocol.uri, string),
        _params: RequestParams.t,
      ) => {
    Hashtbl.remove(documents, uri);
    Hashtbl.remove(compiled_code, uri);
    clear_diagnostics(~uri, ());
  };
};
