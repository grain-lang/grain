open Grain_typed;

let get_hover_from_statement =
    (
      ~uri,
      ~line,
      ~char,
      ~documents,
      ~compiled_code: Typedtree.typed_program,
      stmt: Typedtree.toplevel_stmt,
    ) => {
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) => (
      Utils.print_path(import_declaration.timp_path),
      Some(stmt.ttop_loc),
    )
  | TTopForeign(export_flag, value_description) =>
    let tvd_desc = value_description.tvd_desc;
    let type_sig = Utils.lens_sig(~env=stmt.ttop_env, tvd_desc.ctyp_type);
    (type_sig, Some(stmt.ttop_loc));
  | TTopData(data_declarations) =>
    switch (data_declarations) {
    | [] => ("empty data", None)
    | _ =>
      let matches =
        List.filter(
          (dd: Typedtree.data_declaration) =>
            Utils.is_point_inside_location(~line, ~char, dd.data_loc),
          data_declarations,
        );
      switch (matches) {
      | [] => ("Internal error parsing", Some(stmt.ttop_loc))
      | [node] =>
        let name = node.data_name;
        switch (node.data_kind) {
        | TDataAbstract =>
          switch (node.data_manifest) {
          | None => (name.txt, Some(node.data_loc))
          | Some(t) => (
              Utils.lens_sig(~env=stmt.ttop_env, t.ctyp_type),
              Some(node.data_loc),
            )
          }
        | TDataVariant(constrs) =>
          let matches =
            List.filter(
              (cd: Typedtree.constructor_declaration) =>
                if (Utils.is_point_inside_location(~line, ~char, cd.cd_loc)) {
                  true;
                } else {
                  false;
                },
              constrs,
            );

          switch (matches) {
          | [decl] => (
              Utils.markdown_grain(decl.cd_name.txt),
              Some(decl.cd_loc),
            )
          | _ => (
              Utils.markdown_grain("enum " ++ name.txt),
              Some(node.data_loc),
            )
          };

        | TDataRecord(_) =>
          switch (node.data_params) {
          | [] => (
              Utils.markdown_grain(node.data_name.txt),
              Some(node.data_loc),
            )
          | _ =>
            let matches =
              List.filter(
                (dp: Typedtree.core_type) =>
                  if (Utils.is_point_inside_location(
                        ~line,
                        ~char,
                        dp.ctyp_loc,
                      )) {
                    true;
                  } else {
                    false;
                  },
                node.data_params,
              );
            switch (matches) {
            | [decl] => (
                Utils.markdown_grain(
                  Utils.lens_sig(decl.ctyp_type, ~env=compiled_code.env),
                ),
                Some(decl.ctyp_loc),
              )
            | _ => (
                Utils.markdown_grain(node.data_name.txt),
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
    | [] => ("Internal error: empty let", None)
    | _ =>
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
        (
          Utils.expression_lens(~line, ~char, ~compiled_code, expr),
          Some(pat.pat_loc),
        );

      | [node] =>
        switch (node) {
        | Error(err) => (err, None)
        | NotInRange => ("Not in range", None)
        | Expression(e) => (
            Utils.expression_lens(~line, ~char, ~compiled_code, e),
            if (e.exp_loc == Grain_parsing.Location.dummy_loc) {
              Some(stmt.ttop_loc);
            } else {
              Some(e.exp_loc);
            },
          )
        | Pattern(p) => (
            Utils.markdown_grain(
              Utils.lens_sig(p.pat_type, ~env=compiled_code.env),
            ),
            if (p.pat_loc == Grain_parsing.Location.dummy_loc) {
              Some(stmt.ttop_loc);
            } else {
              Some(p.pat_loc);
            },
          )
        }
      | _ => ("Internal error", None)
      };
    }

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
    | Error(err) => ("Error: " ++ err, None)
    | NotInRange => (
        Utils.markdown_grain(
          Utils.lens_sig(expression.exp_type, ~env=compiled_code.env),
        ),
        Some(loc),
      )
    | Expression(e) => (
        Utils.expression_lens(~line, ~char, ~compiled_code, e),
        if (e.exp_loc == Grain_parsing.Location.dummy_loc) {
          Some(loc);
        } else {
          Some(e.exp_loc);
        },
      )

    | Pattern(p) => (
        Utils.markdown_grain(
          Utils.lens_sig(p.pat_type, ~env=compiled_code.env),
        ),
        Some(p.pat_loc),
      )
    };

  | TTopException(export_flag, type_exception) =>
    let exception_type = type_exception.ext_type; // Not sure how to get a good value here
    ("exception", None);
  | TTopExport(export_declarations) => ("export", None)
  };
};

let get_hover = (~id, ~compiled_code, ~documents, request) => {
  switch (Utils.get_text_document_uri_and_position(request)) {
  | (Some(uri), Some(line), Some(char)) =>
    if (Hashtbl.mem(compiled_code, uri)) {
      let ln = line + 1;

      let compiled_code_opt = Hashtbl.find_opt(compiled_code, uri);
      switch (compiled_code_opt) {
      | None => ()
      | Some(compiled_code) =>
        let node = Utils.find_best_match(~line=ln, ~char, compiled_code);
        switch (node) {
        | Some(stmt) =>
          let (signature, loc) =
            get_hover_from_statement(
              ~uri,
              ~line=ln,
              ~char,
              ~documents,
              ~compiled_code,
              stmt,
            );
          let range =
            switch (loc) {
            | None => Utils.loc_to_range(stmt.ttop_loc)
            | Some(l) => Utils.loc_to_range(l)
            };
          Rpc.send_hover(~output=stdout, ~id, ~range, signature);
        | None => ()
        };
      };
    } else {
      ();
    }
  | _ => ()
  };
};
