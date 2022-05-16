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
  jsonrpc: string,
  id: Rpc.msg_id,
  result: list(lense),
};

let string_of_type_expr = out_type => {
  let printer = (ppf, out_type) => {
    Oprint.out_type^(ppf, out_type);
  };
  let to_string = out_type => {
    Format.asprintf("%a", printer, out_type);
  };
  Option.map(to_string, out_type);
};

let simple_sig = (t: Types.type_expr) => {
  let tree = Printtyp.tree_of_typexp(true, t);
  switch (string_of_type_expr(Some(tree))) {
  | None => ""
  | Some(tstr) => tstr
  };
};

let map_concat = (~sep, ~print_fn, vals) => {
  String.concat(sep, List.map(v => print_fn(v), vals));
};

let rec lens_sig = (~depth=0, ~env, t: Types.type_expr) => {
  switch (t.desc) {
  | TTyLink(te) => lens_sig(~depth=depth + 1, ~env, te)
  | TTySubst(t) => lens_sig(~depth=depth + 1, ~env, t)
  | TTyConstr(path, types, r) =>
    let decl = Env.find_type(path, env);
    let tk = decl.type_kind;
    switch (tk) {
    | TDataRecord(fields) =>
      // the special case I want to handle, which is to print the full record
      let labelText =
        map_concat(
          ~sep=",\n",
          ~print_fn=
            (field: Types.record_field) => {
              let typeInf = lens_sig(~env, field.rf_type);
              let rf_name = field.rf_name;
              "  " ++ rf_name.name ++ ": " ++ typeInf;
            },
          fields,
        );

      simple_sig(t) ++ " {\n" ++ labelText ++ "\n}";

    | _ => simple_sig(t)
    };

  | _ => simple_sig(t)
  };
};

let send_lenses = (~id: Rpc.msg_id, lenses: list(lense)) => {
  let response: lense_response = {jsonrpc: Rpc.jsonrpc, id, result: lenses};
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
              | Some(t) => lens_sig(~env=stmt.ttop_env, t.ctyp_type)
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
          (vbs: Typedtree.value_binding) => simple_sig(vbs.vb_expr.exp_type),
          value_bindings,
        ),
      );
    Some(vbses);

  | TTopExpr(expression) => Some(simple_sig(expression.exp_type))
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
