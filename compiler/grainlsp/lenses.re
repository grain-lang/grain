open Grain_typed;

let get_signature_from_statement =
    (stmt: Grain_typed__Typedtree.toplevel_stmt) =>
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) => "import declaration"
  | TTopForeign(export_flag, value_description) => "foreign"
  | TTopData(data_declarations) => "data declaration"
  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
    if (List.length(value_bindings) > 0) {
      let vbs: Typedtree.value_binding = List.hd(value_bindings);
      Utils.lens_sig(vbs.vb_expr.exp_type);
    } else {
      "";
    }

  | TTopExpr(expression) =>
    let expr_type = expression.exp_type;
    Utils.lens_sig(expr_type);
  | TTopException(export_flag, type_exception) => "exception"
  | TTopExport(export_declarations) => "export"
  };
let get_lenses = (typed_program: Typedtree.typed_program) => {
  List.fold_left(
    (acc, stmt: Grain_typed__Typedtree.toplevel_stmt) => {
      let (file, startline, startchar, sbol) =
        Utils.get_raw_pos_info(stmt.ttop_loc.loc_start);

      let signature = get_signature_from_statement(stmt);
      let lens: Rpc.lens_t = {line: startline, signature};

      [lens, ...acc];
    },
    [],
    typed_program.statements,
  );
};

let process_get_lenses = (log, id, json, compiled_code) => {
  log("process_get_lenses requested");
  let params = Yojson.Safe.Util.member("params", json);
  let text_document = Yojson.Safe.Util.member("textDocument", params);

  let uri =
    Yojson.Safe.Util.member("uri", text_document)
    |> Yojson.Safe.Util.to_string_option;

  switch (uri) {
  | None => Rpc.send_lenses(log, stdout, id, [])
  | Some(u) =>
    if (Hashtbl.mem(compiled_code, u)) {
      let compiled_code = Hashtbl.find(compiled_code, u);
      let lenses = get_lenses(compiled_code);
      Rpc.send_lenses(log, stdout, id, lenses);
    } else {
      Rpc.send_lenses(log, stdout, id, []);
    }
  };
};
