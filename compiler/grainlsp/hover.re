open Grain_typed;

let get_hover_from_statement =
    (
      log,
      stmt: Typedtree.toplevel_stmt,
      uri,
      line,
      char,
      documents,
      compiled_code,
    ) =>
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
          | _ => ("enum " ++ name.txt, Some(node.data_loc))
          };

        | TDataRecord(_) =>
          switch (node.data_params) {
          | [] => ("record " ++ node.data_name.txt, Some(node.data_loc))
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
                Utils.lens_sig(decl.ctyp_type),
                Some(decl.ctyp_loc),
              )
            | _ => ("record " ++ node.data_name.txt, Some(node.data_loc))
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
        (Utils.lens_sig(expr.exp_type), Some(vb.vb_loc));

      | [node] =>
        switch (node) {
        | Error(err) => (err, None)
        | NotInRange => ("Not in range", None)
        | Expression(e) => (Utils.lens_sig(e.exp_type), Some(e.exp_loc))
        | Pattern(p) => (Utils.lens_sig(p.pat_type), Some(p.pat_loc))
        }
      | _ => ("Internal error", None)
      };
    }

  | TTopExpr(expression) =>
    let node = Utils.get_node_from_expression(log, expression, line, char);
    switch (node) {
    | Error(err) => ("NYI: " ++ err, None)
    | NotInRange => (
        Utils.lens_sig(expression.exp_type),
        Some(expression.exp_loc),
      )
    | Expression(e) =>
      let desc = e.exp_desc;
      let txt =
        switch (desc) {
        | TExpIdent(path, loc, vd) =>
          log("hover line " ++ string_of_int(line));
          log("hover char " ++ string_of_int(char));

          // let original =
          //   Utils.get_original_text(log, documents, uri, line - 1, char);

          // switch (original) {
          // | Nothing => log("No original code")
          // | Lident(text) => log("hovering over " ++ text)
          // };

          // "TExpIdent";

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

          let (modname, func) = parts;

          // work out if the cursor is in the module name or the func name

          if (modname == "") {
            log("no mod name");
            func;
          } else {
            let lstart = loc.loc.loc_start;

            let mod_start = lstart.pos_cnum - lstart.pos_bol;

            log("mod_start " ++ string_of_int(mod_start));

            let mod_end = mod_start + String.length(modname);

            log("mod_end " ++ string_of_int(mod_end));

            log("char is " ++ string_of_int(char));

            if (char < mod_end) {
              let vals =
                switch (path) {
                | PIdent(ident) => []
                | PExternal(mod_path, name, _) =>
                  Completion.get_module_exports(log, mod_path, compiled_code)
                };

              let printed_vals =
                List.fold_left(
                  (acc, v: Rpc.completion_item) =>
                    acc ++ "let " ++ v.detail ++ "  \n",
                  "",
                  vals,
                );
              "### "
              ++ modname
              ++ "\n"
              ++ "```grain\n"
              ++ "{\n"
              ++ printed_vals
              ++ "}"
              ++ "```";
            } else {
              Utils.lens_sig(e.exp_type);
            };
          };

        | TExpApp(_) => "TExpApp"
        | TExpConstruct(_) => "TExpConstruct"
        | _ =>
          log("some other val");
          Utils.lens_sig(e.exp_type);
        };

      // let extras = e.exp_extra;

      // log("expression info:");
      // let _ =
      //   List.map(
      //     (e: (Grain_typed__Typedtree.exp_extra, Grain_utils__Warnings.loc)) => {
      //       let (x: Typedtree.exp_extra, loc) = e;
      //       switch (x) {
      //       | TExpConstraint(core_t) =>
      //         log("constraint: " ++ Utils.lens_sig(core_t.ctyp_type))
      //       };
      //     },
      //     extras,
      //   );

      (txt, Some(e.exp_loc));

    //   (Utils.lens_sig(e.exp_type), Some(e.exp_loc))
    | Pattern(p) => (Utils.lens_sig(p.pat_type), Some(p.pat_loc))
    };

  | TTopException(export_flag, type_exception) => ("exception", None)
  | TTopExport(export_declarations) => ("export", None)
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
