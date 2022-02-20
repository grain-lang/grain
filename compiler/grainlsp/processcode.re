open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;

let compile_source = (log, uri, source) => {
  log("Compiling the source for " ++ uri);

  let filename = Filename.basename(uri);

  switch (
    Compile.compile_string(
      ~hook=stop_after_typed_well_formed,
      ~name=filename,
      source,
    )
  ) {
  | exception exn =>
    let error = Grain_diagnostics.Output.exn_to_lsp_error(exn);
    let errors =
      switch (error) {
      | None =>
        let lsp_err: Grain_diagnostics.Output.lsp_error = {
          file: uri,
          line: 0,
          startchar: 0,
          endline: 0,
          endchar: 0,
          lsp_message: "Unable to parse",
        };
        (None, Some(lsp_err), None);
      | Some(err) => (None, Some(err), None)
      };
    errors;

  | {cstate_desc: TypedWellFormed(typed_program)} =>
    let warnings: list(Grain_diagnostics.Output.lsp_warning) =
      Grain_diagnostics.Output.convert_warnings(
        Grain_utils.Warnings.get_warnings(),
        uri,
      );
    (Some(typed_program), None, Some(warnings));
  | _ =>
    let lsp_error: Grain_diagnostics.Output.lsp_error = {
      file: uri,
      line: 0,
      startchar: 0,
      endline: 0,
      endchar: 0,
      lsp_message: "Compilation failed with an internal error",
    };
    (None, Some(lsp_error), None);
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

let textDocument_didOpenOrChange =
    (~log, ~documents, ~compiled_code, ~cached_code, request) => {
  switch (get_text_document_from_params(request)) {
  | Some((uri, text)) =>
    if (!Hashtbl.mem(documents, uri)) {
      Hashtbl.add(documents, uri, text);
    } else {
      Hashtbl.replace(documents, uri, text);
    };
    let compilerRes = compile_source(log, uri, text);
    switch (compilerRes) {
    | (Some(typed_program), None, warnings) =>
      if (!Hashtbl.mem(compiled_code, uri)) {
        Hashtbl.add(compiled_code, uri, typed_program);
      } else {
        Hashtbl.replace(compiled_code, uri, typed_program);
      };
      if (!Hashtbl.mem(cached_code, uri)) {
        Hashtbl.add(cached_code, uri, typed_program);
      } else {
        Hashtbl.replace(cached_code, uri, typed_program);
      };
      switch (warnings) {
      | None => Rpc.clear_diagnostics(~log, ~output=stdout, uri)
      | Some(wrns) =>
        Rpc.send_diagnostics(~log, ~output=stdout, ~uri, ~warnings, None)
      };

    | (_, Some(err), _) =>
      if (!Hashtbl.mem(compiled_code, uri)) {
        Hashtbl.remove(compiled_code, uri);
      };
      log("Failed to compile");
      Rpc.send_diagnostics(
        ~log,
        ~output=stdout,
        ~uri,
        ~warnings=None,
        Some(err),
      );
    | (None, None, _) => Rpc.clear_diagnostics(~log, ~output=stdout, uri)
    };
  | _ => ()
  };
};
