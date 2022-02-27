open Grain_typed;
open Grain_diagnostics;

let get_signature_from_statement =
    (stmt: Grain_typed__Typedtree.toplevel_stmt) =>
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) => None
  | TTopForeign(export_flag, value_description) => None
  | TTopData(data_declarations) =>
    switch (data_declarations) {
    | [] => None
    | _ =>
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
                | Some(t) => Utils.lens_sig(~env=stmt.ttop_env, t.ctyp_type)
                }
              }
            ),
          "",
          data_declarations,
        );

      Some(decls);
    }
  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
    switch (value_bindings) {
    | [] => None
    | _ =>
      let vbses =
        List.fold_left(
          (acc, vbs: Typedtree.value_binding) =>
            (acc == "" ? "" : acc ++ ", ")
            ++ Utils.simple_sig(vbs.vb_expr.exp_type),
          "",
          value_bindings,
        );
      Some(vbses);
    }
  | TTopExpr(expression) => Some(Utils.simple_sig(expression.exp_type))
  | TTopException(export_flag, type_exception) => None
  | TTopExport(export_declarations) => None
  };

let get_lenses = (typed_program: Typedtree.typed_program) => {
  List.fold_left(
    (acc, stmt: Grain_typed__Typedtree.toplevel_stmt) => {
      let (file, startline, startchar, sbol) =
        Locations.get_raw_pos_info(stmt.ttop_loc.loc_start);

      let signature = get_signature_from_statement(stmt);
      switch (signature) {
      | None => acc
      | Some(sigval) =>
        let lens: Rpc.lens_t = {line: startline, signature: sigval};
        [lens, ...acc];
      };
    },
    [],
    typed_program.statements,
  );
};

let process_get_lenses = (~id, ~compiled_code, request) => {
  let params = Yojson.Safe.Util.member("params", request);
  let text_document = Yojson.Safe.Util.member("textDocument", params);
  let uri =
    Yojson.Safe.Util.member("uri", text_document)
    |> Yojson.Safe.Util.to_string_option;

  switch (uri) {
  | None => Rpc.send_lenses(~output=stdout, ~id, [])
  | Some(u) =>
    if (Hashtbl.mem(compiled_code, u)) {
      let compiled_code = Hashtbl.find(compiled_code, u);
      let lenses = get_lenses(compiled_code);
      Rpc.send_lenses(~output=stdout, ~id, lenses);
    } else {
      Rpc.send_lenses(~output=stdout, ~id, []);
    }
  };
};
