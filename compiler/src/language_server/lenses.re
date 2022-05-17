open Grain_typed;
open Grain_diagnostics;

[@deriving yojson]
type command = {
  title: string,
  // Open question: Why was this "command string" in every lense?
  // command: string,
};

[@deriving yojson]
type lense = {
  range: Rpc.range,
  command,
};

[@deriving yojson]
type lense_response = {
  jsonrpc: Rpc.version,
  id: Rpc.msg_id,
  result: list(lense),
};

let send_lenses = (~id: Rpc.msg_id, lenses: list(lense)) => {
  let response: lense_response = {jsonrpc: Rpc.version, id, result: lenses};
  let res = lense_response_to_yojson(response);
  let str_json = Yojson.Safe.to_string(res);
  Rpc.send(stdout, str_json);
};

let get_signature_from_statement =
    (stmt: Grain_typed__Typedtree.toplevel_stmt) =>
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) => None
  | TTopForeign(export_flag, value_description) => None
  | TTopData(data_declarations) when data_declarations == [] => None
  | TTopData(data_declarations) =>
    let decls =
      List.fold_left(
        (acc, decl: Typedtree.data_declaration) =>
          (acc == "" ? "" : acc ++ ", ")
          ++ (
            switch (decl.data_kind) {
            | TDataVariant(_) => "enum " ++ decl.data_name.txt
            | TDataRecord(_) => "record " ++ decl.data_name.txt
            | TDataAbstract =>
              switch (decl.data_manifest) {
              | None => decl.data_name.txt
              | Some(t) => Printtyp.string_of_type_scheme(t.ctyp_type)
              }
            }
          ),
        "",
        data_declarations,
      );

    Some(decls);

  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings)
      when value_bindings == [] =>
    None
  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
    let vbses =
      String.concat(
        ", ",
        List.map(
          (vbs: Typedtree.value_binding) =>
            Printtyp.string_of_type_scheme(vbs.vb_expr.exp_type),
          value_bindings,
        ),
      );
    Some(vbses);

  | TTopExpr(expression) =>
    Some(Printtyp.string_of_type_scheme(expression.exp_type))
  | TTopException(export_flag, type_exception) => None
  | TTopExport(export_declarations) => None
  };

let get_lenses = (typed_program: Typedtree.typed_program) => {
  List.filter_map(
    (stmt: Grain_typed__Typedtree.toplevel_stmt) => {
      let (file, startline, startchar, sbol) =
        Locations.get_raw_pos_info(stmt.ttop_loc.loc_start);

      let signature = get_signature_from_statement(stmt);
      switch (signature) {
      | None => None
      | Some(sigval) =>
        let lense: lense = {
          range: {
            start: {
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
      ~id: Rpc.msg_id,
      ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
      request: Yojson.Safe.t,
    ) => {
  let params = Yojson.Safe.Util.member("params", request);
  let text_document = Yojson.Safe.Util.member("textDocument", params);
  let uri =
    Yojson.Safe.Util.member("uri", text_document)
    |> Yojson.Safe.Util.to_string_option;

  switch (uri) {
  | None => send_lenses(~id, [])
  | Some(u) =>
    switch (Hashtbl.find_opt(compiled_code, u)) {
    | None => send_lenses(~id, [])
    | Some(compiled_code) =>
      let lenses = get_lenses(compiled_code);
      send_lenses(~id, lenses);
    }
  };
};
