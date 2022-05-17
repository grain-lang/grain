open Grain;
open Compile;
open Grain_utils;
open Grain_typed;

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
type diagnostic_t = {
  range: Rpc.range,
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
  jsonrpc: Rpc.version,
  method: string,
  params: document_diagnostics,
};

let exn_to_lsp_error = (exn: exn): option(lsp_error) => {
  let error = Grain_parsing.Location.error_of_exn(exn);

  switch (error) {
  | Some(err) =>
    switch (err) {
    | `Ok(e) =>
      let (file, line, startchar) =
        Grain_parsing.Location.get_pos_info(e.loc.loc_start);
      let (_, endline, endchar) =
        Grain_parsing.Location.get_pos_info(e.loc.loc_end);
      let error: lsp_error = {
        file,
        line,
        startchar,
        endline,
        endchar,
        lsp_message: e.msg,
      };
      Some(error);
    | _ => None
    }
  | _ => None
  };
};

let warning_to_lsp_warning =
    ((loc: Grain_utils.Warnings.loc, warn: Grain_utils.Warnings.t)) => {
  let (file, line, startchar) =
    Grain_parsing.Location.get_pos_info(loc.loc_start);
  let (_, endline, endchar) =
    Grain_parsing.Location.get_pos_info(loc.loc_end);
  let warning: lsp_warning = {
    file,
    line,
    startchar,
    endline,
    endchar,
    number: Grain_utils.Warnings.number(warn),
    lsp_message: Grain_utils.Warnings.message(warn),
  };
  warning;
};

let compile_source = (uri, source) => {
  let filename = Utils.convert_uri_to_filename(uri);

  Logfile.log("Compiling" ++ filename);

  switch (
    Compile.compile_string(
      ~hook=stop_after_typed_well_formed,
      ~name=filename,
      source,
    )
  ) {
  | exception exn =>
    switch (exn_to_lsp_error(exn)) {
    | None =>
      let lsp_err: lsp_error = {
        file: uri,
        line: 0,
        startchar: 0,
        endline: 0,
        endchar: 0,
        lsp_message: "Unable to parse",
      };
      (None, Some(lsp_err), []);
    | Some(err) => (None, Some(err), [])
    }

  | {cstate_desc: TypedWellFormed(typed_program)} =>
    let warnings =
      List.map(warning_to_lsp_warning, Grain_utils.Warnings.get_warnings());
    (Some(typed_program), None, warnings);
  | _ =>
    let lsp_error: lsp_error = {
      file: uri,
      line: 0,
      startchar: 0,
      endline: 0,
      endchar: 0,
      lsp_message: "Compilation failed with an internal error",
    };
    (None, Some(lsp_error), []);
  };
};

let get_text_document_from_params = json => {
  let params = Yojson.Safe.Util.member("params", json);
  let textDocument = Yojson.Safe.Util.member("textDocument", params);
  let uri =
    Yojson.Safe.Util.member("uri", textDocument)
    |> Yojson.Safe.Util.to_string_option;

  switch (uri) {
  | None => None
  | Some(u) =>
    let text =
      Yojson.Safe.Util.member("text", textDocument)
      |> Yojson.Safe.Util.to_string_option;

    switch (text) {
    | Some(t) => Some((u, t))
    | _ =>
      let changes = Yojson.Safe.Util.member("contentChanges", params);

      switch (changes) {
      | `Null => None
      | _ =>
        let text =
          Yojson.Safe.Util.member("text", Yojson.Safe.Util.index(0, changes))
          |> Yojson.Safe.Util.to_string_option;

        switch (text) {
        | Some(t) => Some((u, t))
        | _ => None
        };
      };
    };
  };
};

let send_diagnostics =
    (~uri, warnings: list(lsp_warning), error: option(lsp_error)) => {
  let error_diags =
    switch (error) {
    | None => []
    | Some(err) when err.line < 0 || err.startchar < 0 =>
      // dummy location so set to zero
      let range: Rpc.range = {
        start: {
          line: 0,
          character: 0,
        },
        range_end: {
          line: 0,
          character: 0,
        },
      };

      [{range, severity: 1, message: err.lsp_message}];
    | Some(err) =>
      let range: Rpc.range = {
        start: {
          line: err.line - 1,
          character: err.startchar,
        },
        range_end: {
          line: err.endline - 1,
          character: err.endchar,
        },
      };

      [{range, severity: 1, message: err.lsp_message}];
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
              let range: Rpc.range = {
                start: {
                  line: 0,
                  character: 0,
                },
                range_end: {
                  line: 0,
                  character: 0,
                },
              };
              {range, severity: 2, message: w.lsp_message};
            } else {
              let range: Rpc.range = {
                start: {
                  line: w.line - 1,
                  character: w.startchar,
                },
                range_end: {
                  line: w.endline - 1,
                  character: w.endchar,
                },
              };
              {range, severity: 2, message: w.lsp_message};
            },
          warnings,
        );
      List.append(error_diags, warnings_diags);
    };

  let message: diagnostics_message = {
    jsonrpc: Rpc.version,
    method: "textDocument/publishDiagnostics",
    params: {
      uri,
      diagnostics: with_warnings,
    },
  };

  let jsonMessage =
    Yojson.Safe.to_string(diagnostics_message_to_yojson(message));

  Rpc.send(stdout, jsonMessage);
};

let clear_diagnostics = (~uri, ()) => {
  let message: diagnostics_message = {
    jsonrpc: Rpc.version,
    method: "textDocument/publishDiagnostics",
    params: {
      uri,
      diagnostics: [],
    },
  };

  let jsonMessage =
    Yojson.Safe.to_string(diagnostics_message_to_yojson(message));

  Rpc.send(stdout, jsonMessage);
};

let process =
    (
      ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
      ~documents,
      request,
    ) => {
  switch (get_text_document_from_params(request)) {
  | Some((uri, text)) =>
    Hashtbl.replace(documents, uri, text);

    let compilerRes = compile_source(uri, text);
    switch (compilerRes) {
    | (Some(typed_program), None, warnings) =>
      Hashtbl.replace(compiled_code, uri, typed_program);
      Hashtbl.replace(cached_code, uri, typed_program);
      switch (warnings) {
      | [] => clear_diagnostics(~uri, ())
      | _ => send_diagnostics(~uri, warnings, None)
      };

    | (_, Some(err), warnings) =>
      if (!Hashtbl.mem(compiled_code, uri)) {
        Hashtbl.remove(compiled_code, uri);
      };
      send_diagnostics(~uri, warnings, Some(err));
    | (None, None, _) => clear_diagnostics(~uri, ())
    };
  | _ => ()
  };
};
