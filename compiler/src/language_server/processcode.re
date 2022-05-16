open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;

let compile_source = (uri, source) => {
  let filename = Utils.convert_uri_to_filename(uri);

  Log.log("Compiling" ++ filename);

  switch (
    Compile.compile_string(
      ~hook=stop_after_typed_well_formed,
      ~name=filename,
      source,
    )
  ) {
  | exception exn =>
    switch (Utils.exn_to_lsp_error(exn)) {
    | None =>
      let lsp_err: Rpc.lsp_error = {
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
    let warnings: list(Rpc.lsp_warning) =
      Utils.convert_warnings(Grain_utils.Warnings.get_warnings(), uri);
    (Some(typed_program), None, warnings);
  | _ =>
    let lsp_error: Rpc.lsp_error = {
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

let textDocument_didOpenOrChange =
    (~documents, ~compiled_code, ~cached_code, request) => {
  switch (get_text_document_from_params(request)) {
  | Some((uri, text)) =>
    Hashtbl.replace(documents, uri, text);

    let compilerRes = compile_source(uri, text);
    switch (compilerRes) {
    | (Some(typed_program), None, warnings) =>
      Hashtbl.replace(compiled_code, uri, typed_program);
      Hashtbl.replace(cached_code, uri, typed_program);
      switch (warnings) {
      | [] => Rpc.clear_diagnostics(~output=stdout, uri)
      | _ => Rpc.send_diagnostics(~output=stdout, ~uri, warnings, None)
      };

    | (_, Some(err), warnings) =>
      if (!Hashtbl.mem(compiled_code, uri)) {
        Hashtbl.remove(compiled_code, uri);
      };
      Rpc.send_diagnostics(~output=stdout, ~uri, warnings, Some(err));
    | (None, None, _) => Rpc.clear_diagnostics(~output=stdout, uri)
    };
  | _ => ()
  };
};
