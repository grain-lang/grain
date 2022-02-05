open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;

let compile_source = (log, uri, source) => {
  log("Compiling the source for " ++ uri);

  let filename = Filename.basename(uri);

  Grain_utils.Config.stdlib_dir := Some("/Users/marcus/Projects/grain/stdlib");
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
    log("Some other state from compilation");
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

let getTextDocumentFromParams = json => {
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

let textDocument_didOpenOrChange = (log, json, documents, compiledCode) => {
  switch (getTextDocumentFromParams(json)) {
  | Some((uri, text)) =>
    if (!Hashtbl.mem(documents, uri)) {
      Hashtbl.add(documents, uri, text);
    } else {
      Hashtbl.replace(documents, uri, text);
    };
    let compilerRes = compile_source(log, uri, text);
    switch (compilerRes) {
    | (Some(typed_program), None, warnings) =>
      if (!Hashtbl.mem(compiledCode, uri)) {
        Hashtbl.add(compiledCode, uri, typed_program);
      } else {
        Hashtbl.replace(compiledCode, uri, typed_program);
      };
      switch (warnings) {
      | None => Rpc.clear_diagnostics(log, stdout, uri)
      | Some(wrns) => Rpc.send_diagnostics(log, stdout, uri, None, warnings)
      };

    | (_, Some(err), _) =>
      Hashtbl.remove(compiledCode, uri);
      log("failed to compile");
      Rpc.send_diagnostics(log, stdout, uri, Some(err), None);
    | (None, None, _) => Rpc.clear_diagnostics(log, stdout, uri)
    };
  | _ => ()
  };
};
