open Grain_typed;
open Grain_diagnostics;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLensParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLens
module ResponseResult = {
  [@deriving yojson]
  type lense = {
    range: Protocol.range,
    command: Protocol.command,
  };

  [@deriving yojson]
  type t = list(lense);
};

let get_signature_from_statement = (stmt: Typedtree.toplevel_stmt) =>
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) => None
  | TTopForeign(export_flag, value_description) => None
  | TTopData([]) => None
  | TTopData(data_declarations) =>
    let decls =
      List.map(
        (decl: Typedtree.data_declaration) =>
          Printtyp.string_of_type_declaration(
            ~ident=decl.data_id,
            decl.data_type,
          ),
        data_declarations,
      );

    Some(String.concat(", ", decls));
  | TTopLet(export_flag, rec_flag, mut_flag, []) => None
  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
    let bindings =
      List.map(
        (vbs: Typedtree.value_binding) =>
          Printtyp.string_of_type_scheme(vbs.vb_expr.exp_type),
        value_bindings,
      );

    Some(String.concat(", ", bindings));
  | TTopExpr(expression) =>
    Some(Printtyp.string_of_type_scheme(expression.exp_type))
  | TTopException(export_flag, type_exception) => None
  | TTopExport(export_declarations) => None
  };

let get_lenses = (typed_program: Typedtree.typed_program) => {
  List.filter_map(
    (stmt: Typedtree.toplevel_stmt) => {
      let (_, startline, startchar, _) =
        Locations.get_raw_pos_info(stmt.ttop_loc.loc_start);

      let signature = get_signature_from_statement(stmt);
      switch (signature) {
      | None => None
      | Some(sigval) =>
        let lense: ResponseResult.lense = {
          range: {
            range_start: {
              line: startline - 1,
              character: 1,
            },
            range_end: {
              line: startline - 1,
              character: 1,
            },
          },
          command: {
            title: sigval,
            command: "",
          },
        };
        Some(lense);
      };
    },
    typed_program.statements,
  );
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => Protocol.response(~id, ResponseResult.to_yojson([]))
  | Some(compiled_code) =>
    let lenses = get_lenses(compiled_code);
    Protocol.response(~id, ResponseResult.to_yojson(lenses));
  };
};
