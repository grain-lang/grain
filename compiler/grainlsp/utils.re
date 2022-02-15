open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;

let mark_down_grain = code => "```grain\n" ++ code ++ "\n```";

type node_t =
  | Expression(Typedtree.expression)
  | Pattern(Typedtree.pattern)
  | NotInRange
  | Error(string);

type source_text =
  | Nothing
  | Lident(string);

let rec print_path = (ident: Path.t) => {
  switch (ident) {
  | PIdent(name) => name.name
  | PExternal(externalIdent, second, _) =>
    print_path(externalIdent) ++ "." ++ second
  };
};

let loc_to_range = (pos: Location.t): Rpc.range_t => {
  let (_, startline, startchar, _) =
    Locations.get_raw_pos_info(pos.loc_start);
  let (_, endline, endchar) =
    Grain_parsing.Location.get_pos_info(pos.loc_end);

  let range: Rpc.range_t = {
    start_line: startline,
    start_char: startchar,
    end_line: endline,
    end_char: endchar,
  };
  range;
};

let is_point_inside_stmt = (loc1: Grain_parsing.Location.t, line: int) => {
  let (_, raw1l, raw1c, _) = Locations.get_raw_pos_info(loc1.loc_start);
  let (_, raw1le, raw1ce, _) = Locations.get_raw_pos_info(loc1.loc_end);
  if (line == raw1l || line == raw1le) {
    true;
  } else if (line > raw1l && line < raw1le) {
    true;
  } else {
    false;
  };
};
let is_point_inside_location =
    (log, loc1: Grain_parsing.Location.t, line: int, char: int) => {
  let (_, raw1l, raw1c, _) = Locations.get_raw_pos_info(loc1.loc_start);
  let (_, raw1le, raw1ce, _) = Locations.get_raw_pos_info(loc1.loc_end);

  let res =
    if (line == raw1l) {
      if (char >= raw1c) {
        if (line == raw1le) {
          if (char <= raw1ce) {
            true;
          } else {
            false;
          };
        } else {
          true;
        };
      } else {
        false;
      };
    } else if (line == raw1le) {
      if (char <= raw1ce) {
        true;
      } else {
        false;
      };
    } else if (line > raw1l && line < raw1le) {
      true;
    } else {
      false;
    };

  res;
};

let findBestMatch = (typed_program: Typedtree.typed_program, line, char) => {
  // see if it falls within a top level statement (which it must!)

  let rec loop = (statements: list(Grain_typed.Typedtree.toplevel_stmt)) =>
    switch (statements) {
    | [] => None
    | [stmt, ...tail] =>
      let loc = stmt.ttop_loc;
      if (is_point_inside_stmt(loc, line)) {
        Some(stmt);
      } else {
        loop(List.tl(statements));
      };
    };
  loop(typed_program.statements);
};

let getTextDocumenUriAndPosition = json => {
  let params = Yojson.Safe.Util.member("params", json);
  let textDocument = Yojson.Safe.Util.member("textDocument", params);
  let position = Yojson.Safe.Util.member("position", params);

  let uri =
    Yojson.Safe.Util.member("uri", textDocument)
    |> Yojson.Safe.Util.to_string_option;

  let line =
    Yojson.Safe.Util.member("line", position)
    |> Yojson.Safe.Util.to_int_option;

  let char =
    Yojson.Safe.Util.member("character", position)
    |> Yojson.Safe.Util.to_int_option;

  (uri, line, char);
};

let rec lens_sig = (~depth=0, ~env, t: Types.type_expr) => {
  switch (t.desc) {
  | TTyRecord(fields) =>
    // let record_name = lens_sig(e.exp_type, ~env=e.exp_env);
    let labelText =
      List.fold_left(
        (acc, (name: string, t: Types.type_expr)) => {
          let existing =
            if (acc == "") {
              acc;
            } else {
              acc ++ ",\n";
            };

          existing ++ "  " ++ name ++ ": " ++ lens_sig(~env, t);
        },
        "",
        fields,
      );

    "record " ++ "record_name" ++ " {\n" ++ labelText ++ "\n}";
  | TTyTuple(args) =>
    switch (args) {
    | [] => ""
    | _ =>
      "("
      ++ List.fold_left(
           (acc, arg) => {
             let existing =
               if (acc == "") {
                 acc;
               } else {
                 acc ++ ", ";
               };
             existing ++ lens_sig(~env, arg);
           },
           "",
           args,
         )
      ++ ")"
    }
  | TTyVar(v) =>
    switch (v) {
    | None => ""
    | Some(txt) => txt
    }
  | TTyUniVar(_) => "TTyUniVar"
  | TTyPoly(_) => "TTyPoly"
  | TTyLink(te) => lens_sig(~depth=depth + 1, ~env, te)

  | TTySubst(_) => "link is a subst"
  | TTyConstr(path, types, r) =>
    let decl = Env.find_type(path, env);

    let tk = decl.type_kind;

    let type_text =
      switch (types) {
      | [] => ""
      | _ =>
        "<"
        ++ List.fold_left(
             (acc, tt: Types.type_expr) => {
               let existing =
                 if (acc == "") {
                   acc;
                 } else {
                   acc ++ ",";
                 };
               let typeInf = lens_sig(~env, tt);

               existing ++ typeInf;
             },
             "",
             types,
           )
        ++ ">"
      };

    switch (tk) {
    | TDataRecord(fields) =>
      let labelText =
        List.fold_left(
          (acc, field: Types.record_field) => {
            let existing =
              if (acc == "") {
                acc;
              } else {
                acc ++ ",\n";
              };
            let typeInf = lens_sig(~env, field.rf_type);
            let rf_name = field.rf_name;

            existing ++ "  " ++ rf_name.name ++ ": " ++ typeInf;
          },
          "",
          fields,
        );

      "record "
      ++ print_path(path)
      ++ type_text
      ++ " {\n"
      ++ labelText
      ++ "\n}";
    | TDataVariant(decls) =>
      let declText =
        List.fold_left(
          (acc, decl: Types.constructor_declaration) => {
            let existing =
              if (acc == "") {
                acc;
              } else {
                acc ++ ",\n";
              };
            let decl_name = decl.cd_id.name;

            let args = decl.cd_args;

            let decl_contructions =
              switch (args) {
              | TConstrTuple(types) =>
                switch (types) {
                | [] => ""
                | _ =>
                  " ("
                  ++ List.fold_left(
                       (acc, t: Types.type_expr) => {
                         let existing =
                           if (acc == "") {
                             acc;
                           } else {
                             acc ++ ",\n";
                           };

                         existing ++ "cons type";
                       },
                       "",
                       types,
                     )
                  ++ ")"
                }
              | TConstrSingleton => ""
              };

            existing ++ "  " ++ decl_name ++ decl_contructions;
          },
          "",
          decls,
        );

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

      let (modname, after) = parts;

      if (modname == "Pervasives") {
        after ++ type_text;
      } else {
        "enum "
        ++ print_path(path)
        ++ type_text
        ++ " {\n"
        ++ declText
        ++ "\n}"
        ++ type_text;
      };

    | TDataAbstract => print_path(path) ++ type_text
    | TDataOpen => print_path(path) ++ type_text
    };

  | TTyArrow(args, rettype, _) =>
    let arg_text =
      List.fold_left(
        (acc, arg) => {
          let existing =
            if (acc == "") {
              acc;
            } else {
              acc ++ ", ";
            };
          existing ++ lens_sig(~env, arg);
        },
        "",
        args,
      );
    "(" ++ arg_text ++ ") -> " ++ lens_sig(~env, rettype);
  };
};

let rec get_node_from_pattern = (log, pattern: Typedtree.pattern, line, char) => {
  switch (pattern.pat_desc) {
  | TPatTuple(args) =>
    let pats =
      List.filter(
        (p: Typedtree.pattern) =>
          is_point_inside_location(log, p.pat_loc, line, char),
        args,
      );
    switch (pats) {
    | [p] => Pattern(p)
    | _ => Error("")
    };

  | TPatConstant(c) => Pattern(pattern)
  | TPatVar(v, _) => Pattern(pattern)
  | _ => Error("Pattern")
  };
};

let rec get_node_from_expression =
        (log, expr: Typedtree.expression, line, char) => {
  let node =
    switch (expr.exp_desc) {
    | TExpLet(rec_flag, mut_flag, vbs) =>
      switch (vbs) {
      | [] => Error("")
      | _ =>
        let matches =
          List.map(
            (vb: Typedtree.value_binding) =>
              get_node_from_expression(log, vb.vb_expr, line, char),
            vbs,
          );
        let filtered =
          List.filter(
            m =>
              switch (m) {
              | Error(_) => false
              | _ => true
              },
            matches,
          );
        switch (filtered) {
        | [] =>
          // return the type for the whole statement
          let vb = List.hd(vbs);
          let expr = vb.vb_expr;
          Expression(expr);
        | [hd] => hd
        | _ => Error("Too many let matches")
        };
      }
    | TExpBlock(expressions) =>
      switch (expressions) {
      | [] => Error("")
      | _ =>
        let exps =
          List.filter(
            (e: Typedtree.expression) =>
              is_point_inside_location(log, e.exp_loc, line, char),
            expressions,
          );
        switch (exps) {
        | [] => NotInRange
        | [expr] => get_node_from_expression(log, expr, line, char)
        | _ => Error("Multiple blocks match")
        };
      }

    | TExpApp(func, expressions) =>
      if (is_point_inside_location(log, func.exp_loc, line, char)) {
        log("is inside function location");
        Expression(func);
      } else {
        log("looking in locations");
        switch (expressions) {
        | [] => Error("")
        | _ =>
          let exps =
            List.filter(
              (e: Typedtree.expression) =>
                is_point_inside_location(log, e.exp_loc, line, char),
              expressions,
            );

          switch (exps) {
          | [] => Error("No function application matches")
          | [expr] => Expression(expr)
          | _ => Error("Multiple function applications match")
          };
        };
      }
    | TExpLambda([{mb_pat: pattern, mb_body: body}], _) =>
      let node = get_node_from_pattern(log, pattern, line, char);
      switch (node) {
      | Error(_) => get_node_from_expression(log, body, line, char)
      | _ => node
      };
    | TExpIf(cond, trueexp, falseexp) =>
      let condNode = get_node_from_expression(log, cond, line, char);
      let trueMatch =
        switch (condNode) {
        | NotInRange
        | Error(_) => get_node_from_expression(log, trueexp, line, char)
        | _ => condNode
        };

      switch (trueMatch) {
      | NotInRange
      | Error(_) => get_node_from_expression(log, falseexp, line, char)
      | _ => trueMatch
      };

    | TExpArray(expressions)
    | TExpTuple(expressions) =>
      let exps =
        List.filter(
          (e: Typedtree.expression) =>
            is_point_inside_location(log, e.exp_loc, line, char),
          expressions,
        );

      switch (exps) {
      | [] => Error("No match")
      | [expr] => Expression(expr)
      | _ => Error("Multiple matches")
      };

    | TExpLambda([], _) => failwith("Impossible: transl_imm: Empty lambda")
    | TExpLambda(_, _) => failwith("NYI: transl_imm: Multi-branch lambda")
    | TExpContinue => Error("TExpContinue")
    | TExpBreak => Error("TExpBreak")
    | TExpNull => Error("TExpNull")
    | TExpIdent(_) => Error("TExpIdent")
    | TExpConstant(const) =>
      if (is_point_inside_location(log, expr.exp_loc, line, char)) {
        Expression(expr);
      } else {
        NotInRange;
      }
    | TExpArrayGet(_) => Error("TExpArrayGet")
    | TExpArraySet(_) => Error("TExpArraySet")
    | TExpRecord(_) => Error("TExpRecord")
    | TExpRecordGet(_) => Error("TExpRecordGet")
    | TExpRecordSet(_) => Error("TExpRecordSet")
    | TExpMatch(_) => Error("TExpMatch")
    | TExpPrim1(_) => Error("TExpPrim1")
    | TExpPrim2(_) => Error("TExpPrim2")
    | TExpPrimN(_) => Error("TExpPrimN")
    | TExpBoxAssign(_) => Error("TExpBoxAssign")
    | TExpAssign(_) => Error("TExpAssign")
    | TExpWhile(_) => Error("TExpWhile")
    | TExpFor(_) => Error("TExpFor")
    | TExpConstruct(_) => Error("TExpConstruct")
    };

  switch (node) {
  | NotInRange
  | Error(_) =>
    if (is_point_inside_location(log, expr.exp_loc, line, char)) {
      Expression(expr);
    } else {
      NotInRange;
    }
  | _ => node
  };
};

let getNodeFromStmt =
    (log, stmt: Grain_typed__Typedtree.toplevel_stmt, line, char) =>
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) => Error("import declaration")
  | TTopForeign(export_flag, value_description) => Error("foreign")
  | TTopData(data_declarations) => Error("data declaration")
  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
    switch (value_bindings) {
    | [] => Error("")
    | _ =>
      let matches =
        List.map(
          (vb: Typedtree.value_binding) =>
            get_node_from_expression(log, vb.vb_expr, line, char),
          value_bindings,
        );
      let filtered =
        List.filter(
          m =>
            switch (m) {
            | Error(_) => false
            | _ => true
            },
          matches,
        );
      switch (filtered) {
      | [] =>
        // return the type for the whole statement
        let vb = List.hd(value_bindings);
        let expr = vb.vb_expr;
        Expression(expr);
      | [node] => node
      | _ => Error("Too many ttoplet matches")
      };
    }

  | TTopExpr(expression) =>
    get_node_from_expression(log, expression, line, char)
  | TTopException(export_flag, type_exception) => Error("exception")
  | TTopExport(export_declarations) => Error("export")
  };

let find_completable = (text, offset) => {
  let rec loop = i => {
    i < 0
      ? Lident(String.sub(text, i + 1, offset - (i + 1)))
      : (
        switch (text.[i]) {
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '.'
        | '_' => loop(i - 1)
        | _ =>
          i == offset - 1
            ? Nothing : Lident(String.sub(text, i + 1, offset - (i + 1)))
        }
      );
  };
  loop(offset - 1);
};

let get_original_text = (log, documents, uri, line, char) =>
  if (!Hashtbl.mem(documents, uri)) {
    log("Can't find source code for " ++ uri);
    Nothing;
  } else {
    let sourceCode = Hashtbl.find(documents, uri);
    // try and find the code we are completing in the original source

    let lines = String.split_on_char('\n', sourceCode);
    let line = List.nth(lines, line);
    let completable = find_completable(line, char);

    let _ =
      switch (completable) {
      | Nothing => log("nothing completable found")
      | Lident(ident) => log("Let's complete on " ++ ident)
      };
    completable;
  };

let get_module_exports =
    (log, mod_ident, compiled_code: Typedtree.typed_program) => {
  switch (Env.find_module(mod_ident, None, compiled_code.env)) {
  | lookup =>
    switch (lookup.md_filepath) {
    | None =>
      log("no module path found");
      [];
    | Some(p) =>
      let mtype: Grain_typed.Types.module_type = lookup.md_type;
      switch (mtype) {
      | TModSignature(sigs) =>
        let fnsigs =
          List.filter_map(
            (s: Types.signature_item) => {
              switch (s) {
              | TSigValue(ident, vd) =>
                let string_of_value_description = (~ident: Ident.t, vd) => {
                  Format.asprintf(
                    "%a",
                    Printtyp.value_description(ident),
                    vd,
                  );
                };
                let item: Rpc.completion_item = {
                  label: ident.name,
                  kind: 3,
                  detail: string_of_value_description(~ident, vd),
                  documentation: "This is some documentation",
                };
                Some(item);

              | TSigType(ident, td, recstatus) =>
                //log("Completing A TSigType");
                // let string_of_type_declaration =
                //     (~ident: Ident.t, td) => {
                //   (
                //     ident.name,
                //     Format.asprintf(
                //       "%a",
                //       Printtyp.type_declaration(ident),
                //       td,
                //     ),
                //   );
                // };
                None
              | _ => None
              }
            },
            sigs,
          );
        fnsigs;
      | _ => []
      };
    }
  | exception _ =>
    log("Module not found");
    [];
  };
};

let rec expression_lens =
        (log, line, char, e: Typedtree.expression, compiled_code) => {
  log("expression_lens");
  let desc = e.exp_desc;
  let txt =
    switch (desc) {
    | TExpRecordGet(expr, loc, field) =>
      log("TExpRecordGet");

      if (is_point_inside_location(log, expr.exp_loc, line, char)) {
        lens_sig(~env=e.exp_env, expr.exp_type);
      } else {
        lens_sig(~env=e.exp_env, e.exp_type);
      };

    | TExpPrim1(_) => "TExpPrim1"
    | TExpPrim2(_) => "TExpPrim2"
    | TExpPrimN(_) => "TExpPrimN"
    | TExpIdent(path, loc, vd) =>
      log("TExpIdent");
      log(print_path(path));
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
      log("modname " ++ modname);

      // // work out if the cursor is in the module name or after it
      if (modname == "" || modname == "Pervasives") {
        lens_sig(e.exp_type, ~env=e.exp_env);
      } else {
        let lstart = loc.loc.loc_start;
        let mod_start = lstart.pos_cnum - lstart.pos_bol;
        let mod_end = mod_start + String.length(modname);

        if (char < mod_end) {
          let vals =
            switch (path) {
            | PIdent(ident) => []
            | PExternal(mod_path, name, _) =>
              get_module_exports(log, mod_path, compiled_code)
            };
          let printed_vals =
            List.fold_left(
              (acc, v: Rpc.completion_item) =>
                acc ++ "  let " ++ v.detail ++ "\n",
              "",
              vals,
            );
          printed_vals;
        } else {
          lens_sig(e.exp_type, ~env=e.exp_env);
        };
      };

    | _ => lens_sig(~env=e.exp_env, e.exp_type)
    };

  (mark_down_grain(txt), Some(e.exp_loc));
};
