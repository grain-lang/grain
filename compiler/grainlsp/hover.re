open Grain_typed;

let get_hover_from_statement =
    (
      log,
      stmt: Typedtree.toplevel_stmt,
      uri,
      line,
      char,
      documents,
      compiled_code: Typedtree.typed_program,
    ) => {
  log("get_hover_from_statement");
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) => ("import declaration", None)
  | TTopForeign(export_flag, value_description) => ("foreign", None)
  | TTopData(data_declarations) =>
    switch (data_declarations) {
    | [] => ("", None)
    | _ =>
      let matches =
        List.filter(
          (dd: Typedtree.data_declaration) =>
            Utils.is_point_inside_location(log, dd.data_loc, line, char),
          data_declarations,
        );
      switch (matches) {
      | [] => ("Internal error parsing", Some(stmt.ttop_loc))
      | [node] =>
        let name = node.data_name;
        switch (node.data_kind) {
        | TDataAbstract => ("Abstract declaration", Some(node.data_loc))
        | TDataVariant(constrs) =>
          let matches =
            List.filter(
              (cd: Typedtree.constructor_declaration) =>
                if (Utils.is_point_inside_location(log, cd.cd_loc, line, char)) {
                  true;
                } else {
                  false;
                },
              constrs,
            );

          switch (matches) {
          | [decl] => (decl.cd_name.txt, Some(decl.cd_loc))
          | _ => (
              Utils.mark_down_grain("enum " ++ name.txt),
              Some(node.data_loc),
            )
          };

        | TDataRecord(_) =>
          switch (node.data_params) {
          | [] => (
              Utils.mark_down_grain(node.data_name.txt),
              Some(node.data_loc),
            )
          | _ =>
            let matches =
              List.filter(
                (dp: Typedtree.core_type) =>
                  if (Utils.is_point_inside_location(
                        log,
                        dp.ctyp_loc,
                        line,
                        char,
                      )) {
                    true;
                  } else {
                    false;
                  },
                node.data_params,
              );
            switch (matches) {
            | [decl] => (
                Utils.lens_sig(decl.ctyp_type, ~env=compiled_code.env),
                Some(decl.ctyp_loc),
              )
            | _ => (
                Utils.mark_down_grain(node.data_name.txt),
                Some(node.data_loc),
              )
            };
          }
        };
      | _ => ("Internal error: Too many matches", None)
      };
    }

  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
    switch (value_bindings) {
    | [] => ("", None)
    | _ =>
      let matches =
        List.map(
          (vb: Typedtree.value_binding) =>
            Utils.get_node_from_expression(log, vb.vb_expr, line, char),
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
        // return the type for the whole statement
        // (
        // Utils.mark_down_grain(
        //   Utils.lens_sig(expr.exp_type, ~env=compiled_code.env) ,
        // ),

        // Some(vb.vb_loc),
        //);

        Utils.expression_lens(log, line, char, expr, compiled_code);

      | [node] =>
        switch (node) {
        | Error(err) => (err, None)
        | NotInRange => ("Not in range", None)
        | Expression(e) =>
          Utils.expression_lens(log, line, char, e, compiled_code)
        //(Utils.lens_sig(e.exp_type), Some(e.exp_loc))
        | Pattern(p) => (
            Utils.mark_down_grain(
              Utils.lens_sig(p.pat_type, ~env=compiled_code.env),
            ),
            Some(p.pat_loc),
          )
        }
      | _ => ("Internal error", None)
      };
    }

  | TTopExpr(expression) =>
    let node = Utils.get_node_from_expression(log, expression, line, char);
    switch (node) {
    | Error(err) => ("NYI: " ++ err, None)
    | NotInRange => (
        Utils.mark_down_grain(
          Utils.lens_sig(expression.exp_type, ~env=compiled_code.env),
        ),
        Some(expression.exp_loc),
      )
    | Expression(e) =>
      Utils.expression_lens(log, line, char, e, compiled_code)

    //   (Utils.lens_sig(e.exp_type), Some(e.exp_loc))
    | Pattern(p) => (
        Utils.mark_down_grain(
          Utils.lens_sig(p.pat_type, ~env=compiled_code.env),
        ),
        Some(p.pat_loc),
      )
    };

  | TTopException(export_flag, type_exception) => ("exception", None)
  | TTopExport(export_declarations) => ("export", None)
  };
};

let get_hover = (log, id, json, compiled_code, documents) => {
  switch (Utils.getTextDocumenUriAndPosition(json)) {
  | (Some(uri), Some(line), Some(char)) =>
    if (Hashtbl.mem(compiled_code, uri)) {
      let ln = line + 1;

      let compiled_code_opt = Hashtbl.find_opt(compiled_code, uri);
      switch (compiled_code_opt) {
      | None => ()
      | Some(compiled_code) =>
        let node = Utils.findBestMatch(compiled_code, ln, char);
        switch (node) {
        | Some(stmt) =>
          let (signature, loc) =
            get_hover_from_statement(
              log,
              stmt,
              uri,
              ln,
              char,
              documents,
              compiled_code,
            );
          let range =
            switch (loc) {
            | None => Utils.loc_to_range(stmt.ttop_loc)
            | Some(l) => Utils.loc_to_range(l)
            };
          Rpc.send_hover(log, stdout, id, signature, range);
        | None => ()
        };
      };
    } else {
      ();
    }
  | _ => ()
  };
};
