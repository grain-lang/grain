open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;

type node_location =
  | LocationSignature(string, Grain_utils.Warnings.loc)
  | LocationError;

[@deriving yojson]
type markup_content = {
  kind: string,
  value: string,
};

[@deriving yojson]
type hover_result = {
  contents: markup_content,
  range: Rpc.range,
};

[@deriving yojson]
type hover_response = {
  jsonrpc: string,
  id: Rpc.msg_id,
  result: hover_result,
};

let loc_to_range = (pos: Location.t): Rpc.range => {
  let (_, startline, startchar, _) =
    Locations.get_raw_pos_info(pos.loc_start);
  let (_, endline, endchar) =
    Grain_parsing.Location.get_pos_info(pos.loc_end);

  {
    start: {
      line: startline - 1,
      character: startchar,
    },
    range_end: {
      line: endline - 1,
      character: endchar,
    },
  };
};

let send_hover = (~id: Rpc.msg_id, ~range: Rpc.range, signature) => {
  let hover_info: hover_result = {
    contents: {
      kind: "markdown",
      value: signature,
    },
    range,
  };
  let response: hover_response = {
    jsonrpc: Rpc.jsonrpc,
    id,
    result: hover_info,
  };
  let res = hover_response_to_yojson(response);
  let str_json = Yojson.Safe.to_string(res);
  Rpc.send(stdout, str_json);
};

let rec expression_lens =
        (~line, ~char, ~compiled_code, e: Typedtree.expression) => {
  let desc = e.exp_desc;
  let txt =
    switch (desc) {
    | TExpRecordGet(expr, loc, field) =>
      if (Utils.is_point_inside_location(~line, ~char, expr.exp_loc)) {
        Utils.lens_sig(~env=e.exp_env, expr.exp_type);
      } else {
        Utils.lens_sig(~env=e.exp_env, e.exp_type);
      }

    | TExpPrim1(_, exp) => Utils.lens_sig(~env=exp.exp_env, exp.exp_type)
    | TExpPrim2(_, exp, exp2) =>
      switch (
        Utils.find_location_in_expressions(
          ~line,
          ~char,
          ~default=NotInRange,
          [exp, exp2],
        )
      ) {
      | Expression(matched) =>
        Utils.lens_sig(~env=e.exp_env, matched.exp_type)
      | _ => ""
      }
    | TExpPrimN(_, expressions) =>
      switch (
        Utils.find_location_in_expressions(
          ~line,
          ~char,
          ~default=NotInRange,
          expressions,
        )
      ) {
      | Expression(matched) =>
        Utils.lens_sig(~env=e.exp_env, matched.exp_type)
      | _ => ""
      }
    | TExpIdent(path, loc, vd) =>
      let parts =
        switch (path) {
        | PIdent(ident) => ("", ident.name)
        | PExternal(mod_path, name, _) => (
            switch (mod_path) {
            | PIdent(ident) => ident.name
            | PExternal(mod_path, name, _) => ""
            },
            name,
          )
        };

      let (modname, _after) = parts;
      // work out if the cursor is in the module name or after it
      if (modname == "" || modname == "Pervasives") {
        Utils.lens_sig(e.exp_type, ~env=e.exp_env);
      } else {
        let lstart = loc.loc.loc_start;
        let mod_start = lstart.pos_cnum - lstart.pos_bol;
        let mod_end = mod_start + String.length(modname);

        if (char < mod_end) {
          let vals =
            switch (path) {
            | PIdent(ident) => []
            | PExternal(mod_path, name, _) =>
              Completion.get_module_exports(mod_path, compiled_code)
            };
          let printed_vals =
            List.fold_left(
              (acc, v: Completion.completion_item) =>
                acc ++ "  let " ++ v.detail ++ "\n",
              "",
              vals,
            );
          printed_vals;
        } else {
          Utils.lens_sig(e.exp_type, ~env=e.exp_env);
        };
      };

    | _ => Utils.lens_sig(~env=e.exp_env, e.exp_type)
    };

  Grain_utils.Markdown.code_block(txt);
};

let get_from_statement =
    (
      ~uri,
      ~line,
      ~char,
      ~documents,
      ~compiled_code: Typedtree.typed_program,
      stmt: Typedtree.toplevel_stmt,
    )
    : node_location => {
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) =>
    LocationSignature(
      Printtyp.string_of_path(import_declaration.timp_path),
      stmt.ttop_loc,
    )
  | TTopForeign(export_flag, value_description) =>
    let tvd_desc = value_description.tvd_desc;
    let type_sig = Utils.lens_sig(~env=stmt.ttop_env, tvd_desc.ctyp_type);
    LocationSignature(type_sig, stmt.ttop_loc);
  | TTopData(data_declarations) when data_declarations == [] => LocationError
  | TTopData(data_declarations) =>
    let matches =
      List.filter(
        (dd: Typedtree.data_declaration) =>
          Utils.is_point_inside_location(~line, ~char, dd.data_loc),
        data_declarations,
      );
    switch (matches) {
    | [] => LocationError
    | [{data_name, data_manifest, data_loc, data_kind: TDataAbstract}, ..._] =>
      switch (data_manifest) {
      | None => LocationSignature(data_name.txt, data_loc)
      | Some(t) =>
        LocationSignature(
          Utils.lens_sig(~env=stmt.ttop_env, t.ctyp_type),
          data_loc,
        )
      }

    | [
        {
          data_name,
          data_manifest,
          data_loc,
          data_kind: TDataVariant(constrs),
        },
        ..._,
      ] =>
      let matches =
        List.filter(
          (cd: Typedtree.constructor_declaration) =>
            Utils.is_point_inside_location(~line, ~char, cd.cd_loc),
          constrs,
        );

      switch (matches) {
      | [decl] =>
        LocationSignature(
          Grain_utils.Markdown.code_block(decl.cd_name.txt),
          decl.cd_loc,
        )
      | _ =>
        LocationSignature(
          Grain_utils.Markdown.code_block("enum " ++ data_name.txt),
          data_loc,
        )
      };

    | [
        {
          data_name,
          data_manifest,
          data_loc,
          data_params,
          data_kind: TDataRecord(_),
        },
        ..._,
      ] =>
      switch (data_params) {
      | [] =>
        LocationSignature(
          Grain_utils.Markdown.code_block(data_name.txt),
          data_loc,
        )
      | _ =>
        let matches =
          List.filter(
            (dp: Typedtree.core_type) =>
              Utils.is_point_inside_location(~line, ~char, dp.ctyp_loc),
            data_params,
          );
        switch (matches) {
        | [decl] =>
          LocationSignature(
            Grain_utils.Markdown.code_block(
              Utils.lens_sig(decl.ctyp_type, ~env=compiled_code.env),
            ),
            decl.ctyp_loc,
          )
        | _ =>
          LocationSignature(
            Grain_utils.Markdown.code_block(data_name.txt),
            data_loc,
          )
        };
      }
    };

  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings)
      when value_bindings == [] =>
    LocationError

  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
    let matches =
      List.map(
        (vb: Typedtree.value_binding) =>
          Utils.get_node_from_expression(~line, ~char, vb.vb_expr),
        value_bindings,
      );
    let filtered =
      List.filter(
        m =>
          switch ((m: Utils.node_t)) {
          | Error(_) => false
          | NotInRange => false
          | _ => true
          },
        matches,
      );
    switch (filtered) {
    | [] =>
      let vb = List.hd(value_bindings);
      let expr = vb.vb_expr;
      let pat = vb.vb_pat;
      LocationSignature(
        expression_lens(~line, ~char, ~compiled_code, expr),
        pat.pat_loc,
      );

    | [node] =>
      switch (node) {
      | Error(err) => LocationError
      | NotInRange => LocationError
      | Expression(e) =>
        LocationSignature(
          expression_lens(~line, ~char, ~compiled_code, e),
          if (e.exp_loc == Grain_parsing.Location.dummy_loc) {
            stmt.ttop_loc;
          } else {
            e.exp_loc;
          },
        )
      | Pattern(p) =>
        LocationSignature(
          Grain_utils.Markdown.code_block(
            Utils.lens_sig(p.pat_type, ~env=compiled_code.env),
          ),
          if (p.pat_loc == Grain_parsing.Location.dummy_loc) {
            stmt.ttop_loc;
          } else {
            p.pat_loc;
          },
        )
      }
    | _ => LocationError
    };

  | TTopExpr(expression) =>
    // Because of the List syntax sugar, we don't get a location
    // But as a cover all, if we have a dummy location here,
    // use the enclosing expression location

    let (loc, node) =
      if (expression.exp_loc == Grain_parsing.Location.dummy_loc) {
        let nd: Utils.node_t = Expression(expression);
        (stmt.ttop_loc, nd);
      } else {
        (
          expression.exp_loc,
          Utils.get_node_from_expression(~line, ~char, expression),
        );
      };

    switch (node) {
    | Error(err) => LocationError
    | NotInRange =>
      LocationSignature(
        Grain_utils.Markdown.code_block(
          Utils.lens_sig(expression.exp_type, ~env=compiled_code.env),
        ),
        loc,
      )
    | Expression(e) =>
      LocationSignature(
        expression_lens(~line, ~char, ~compiled_code, e),
        if (e.exp_loc == Grain_parsing.Location.dummy_loc) {
          loc;
        } else {
          e.exp_loc;
        },
      )

    | Pattern(p) =>
      LocationSignature(
        Grain_utils.Markdown.code_block(
          Utils.lens_sig(p.pat_type, ~env=compiled_code.env),
        ),
        p.pat_loc,
      )
    };

  | TTopException(export_flag, type_exception) => LocationError
  | TTopExport(export_declarations) => LocationError
  };
};

let process =
    (
      ~id: Rpc.msg_id,
      ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
      ~documents: Hashtbl.t(string, string),
      request: Yojson.Safe.t,
    ) => {
  switch (Utils.get_text_document_uri_and_position(request)) {
  | Some(location) =>
    let ln = location.line + 1;
    switch (Hashtbl.find_opt(compiled_code, location.uri)) {
    | None => ()
    | Some(compiled_code) =>
      let node =
        Utils.find_best_match(~line=ln, ~char=location.char, compiled_code);
      switch (node) {
      | Some(stmt) =>
        switch (
          get_from_statement(
            ~uri=location.uri,
            ~line=ln,
            ~char=location.char,
            ~documents,
            ~compiled_code,
            stmt,
          )
        ) {
        | LocationError =>
          send_hover(~id, ~range=loc_to_range(stmt.ttop_loc), "")
        | LocationSignature(signature, loc) =>
          send_hover(~id, ~range=loc_to_range(loc), signature)
        }

      | None => ()
      };
    };

  | _ => ()
  };
};
