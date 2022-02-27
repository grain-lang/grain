open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;

//  CompletionItemKind
let item_kind_completion_text = 1;
let item_kind_completion_function = 3;
let item_kind_completion_constructor = 4;
let item_kind_completion_variable = 6;
let item_kind_completion_struct = 22;

let markdown_grain = code => "```grain\n" ++ code ++ "\n```";

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

let comma_join = (~sep, ~print_fn, vals) => {
  List.fold_left(
    (acc, arg) => {
      let existing =
        if (acc == "") {
          acc;
        } else {
          acc ++ sep;
        };
      existing ++ print_fn(arg);
    },
    "",
    vals,
  );
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

let is_point_inside_stmt = (~line: int, loc: Grain_parsing.Location.t) => {
  let (_, raw1l, raw1c, _) = Locations.get_raw_pos_info(loc.loc_start);
  let (_, raw1le, raw1ce, _) = Locations.get_raw_pos_info(loc.loc_end);
  if (line == raw1l || line == raw1le) {
    true;
  } else if (line > raw1l && line < raw1le) {
    true;
  } else {
    false;
  };
};
let is_point_inside_location =
    (~line: int, ~char: int, loc1: Grain_parsing.Location.t) => {
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

let find_best_match = (~line, ~char, typed_program: Typedtree.typed_program) => {
  // see if it falls within a top level statement (which it must!)
  let rec loop = (statements: list(Grain_typed.Typedtree.toplevel_stmt)) =>
    switch (statements) {
    | [] => None
    | [stmt, ...tail] =>
      let loc = stmt.ttop_loc;
      if (is_point_inside_stmt(~line, loc)) {
        Some(stmt);
      } else {
        loop(List.tl(statements));
      };
    };
  loop(typed_program.statements);
};

let get_text_document_uri_and_position = json => {
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

let print_sig = (t: Types.type_expr) => {
  let buf = Buffer.create(64);
  let ppf = Format.formatter_of_buffer(buf);
  Printtyp.type_expr(ppf, t);
  Format.pp_print_flush(ppf, ());
  let sigStr = Buffer.contents(buf);
  sigStr;
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

let rec lens_sig = (~depth=0, ~env, t: Types.type_expr) => {
  switch (t.desc) {
  | TTyLink(te) => lens_sig(~depth=depth + 1, ~env, te)
  | TTySubst(t) => lens_sig(~depth=depth + 1, ~env, t)
  | TTyConstr(path, types, r) =>
    let decl = Env.find_type(path, env);
    let type_text =
      switch (types) {
      | [] => ""
      | _ => "<" ++ comma_join(~sep=", ", ~print_fn=print_sig, types) ++ ">"
      };

    let tk = decl.type_kind;
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
                  ++ comma_join(~sep=",\n", ~print_fn=print_sig, types)
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

      let (modname, typ) = parts;

      if (modname == "Pervasives") {
        typ ++ type_text;
      } else if (typ == "Void") {
        "Void";
      } else {
        "enum "
        ++ print_path(path)
        ++ type_text
        ++ " {\n"
        ++ declText
        ++ "\n}"
        ++ type_text;
      };
    | _ =>
      let tree = Printtyp.tree_of_typexp(true, t);
      switch (string_of_type_expr(Some(tree))) {
      | None => ""
      | Some(tstr) => tstr
      };
    };

  | _ =>
    let tree = Printtyp.tree_of_typexp(true, t);
    switch (string_of_type_expr(Some(tree))) {
    | None => ""
    | Some(tstr) => tstr
    };
  };
};
// let rec lens_sig2 = (~depth=0, ~env, t: Types.type_expr) => {
//   switch (t.desc) {
//   | TTyRecord(fields) => print_sig(t)

//   | TTyTuple(args) =>
//     switch (args) {
//     | [] => ""
//     | _ => "(" ++ comma_join(~sep=", ", ~print_fn=print_sig, args) ++ ")"
//     }
//   | TTyVar(v) =>
//     switch (v) {
//     | None => ""
//     | Some(txt) => txt
//     }
//   | TTyUniVar(v) =>
//     switch (v) {
//     | None => ""
//     | Some(var) => var
//     }
//   | TTyPoly(te, _) => lens_sig(~depth=depth + 1, ~env, te)
//   | TTyLink(te) => lens_sig(~depth=depth + 1, ~env, te)

//   | TTySubst(t) => lens_sig(~depth=depth + 1, ~env, t)
//   | TTyConstr(path, types, r) =>
//     let decl = Env.find_type(path, env);

//     let tk = decl.type_kind;

//     let type_text =
//       switch (types) {
//       | [] => ""
//       | _ => "<" ++ comma_join(~sep=", ", ~print_fn=print_sig, types) ++ ">"
//       };

//     switch (tk) {
//     | TDataRecord(fields) =>
//       let labelText =
//         List.fold_left(
//           (acc, field: Types.record_field) => {
//             let existing =
//               if (acc == "") {
//                 acc;
//               } else {
//                 acc ++ ",\n";
//               };
//             let typeInf = lens_sig(~env, field.rf_type);
//             let rf_name = field.rf_name;

//             existing ++ "  " ++ rf_name.name ++ ": " ++ typeInf;
//           },
//           "",
//           fields,
//         );

//       "record "
//       ++ print_path(path)
//       ++ type_text
//       ++ " {\n"
//       ++ labelText
//       ++ "\n}";
//     | TDataVariant(decls) =>
//       let declText =
//         List.fold_left(
//           (acc, decl: Types.constructor_declaration) => {
//             let existing =
//               if (acc == "") {
//                 acc;
//               } else {
//                 acc ++ ",\n";
//               };
//             let decl_name = decl.cd_id.name;

//             let args = decl.cd_args;

//             let decl_contructions =
//               switch (args) {
//               | TConstrTuple(types) =>
//                 switch (types) {
//                 | [] => ""
//                 | _ =>
//                   " ("
//                   ++ comma_join(~sep=",\n", ~print_fn=print_sig, types)
//                   ++ ")"
//                 }
//               | TConstrSingleton => ""
//               };

//             existing ++ "  " ++ decl_name ++ decl_contructions;
//           },
//           "",
//           decls,
//         );

//       let parts =
//         switch (path) {
//         | PIdent(ident) => ("", ident.name)
//         | PExternal(mod_path, name, _) => (
//             switch (mod_path) {
//             | PIdent(ident) => ident.name
//             | PExternal(mod_path, name, _) => ""
//             },
//             name,
//           )
//         };

//       let (modname, typ) = parts;

//       if (modname == "Pervasives") {
//         typ ++ type_text;
//       } else if (typ == "Void") {
//         "Void";
//       } else {
//         "enum "
//         ++ print_path(path)
//         ++ type_text
//         ++ " {\n"
//         ++ declText
//         ++ "\n}"
//         ++ type_text;
//       };

//     | TDataAbstract => print_path(path) ++ type_text
//     | TDataOpen => print_path(path) ++ type_text
//     };

//   | TTyArrow(args, rettype, _) =>
//     let arg_text = comma_join(~sep=", ", ~print_fn=print_sig, args);

//     "(" ++ arg_text ++ ") -> " ++ print_sig(rettype);
//   };
// };

let rec get_node_from_pattern = (~line, ~char, pattern: Typedtree.pattern) => {
  switch (pattern.pat_desc) {
  | TPatTuple(args) =>
    let pats =
      List.filter(
        (p: Typedtree.pattern) =>
          is_point_inside_location(~line, ~char, p.pat_loc),
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

let rec find_location_in_expressions = (~line, ~char, ~default, expressions) => {
  let exps =
    List.filter(
      (e: Typedtree.expression) =>
        is_point_inside_location(~line, ~char, e.exp_loc),
      expressions,
    );

  switch (exps) {
  | [] => default
  | [expr] => get_node_from_expression(~line, ~char, expr)
  | _ => Error("Multiple matches, should not happen")
  };
}

and get_node_from_expression = (~line, ~char, expr: Typedtree.expression) => {
  let node =
    switch (expr.exp_desc) {
    | TExpLet(rec_flag, mut_flag, vbs) =>
      switch (vbs) {
      | [] => Error("")
      | _ =>
        let matches =
          List.map(
            (vb: Typedtree.value_binding) =>
              get_node_from_expression(~line, ~char, vb.vb_expr),
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
      | [] => Expression(expr)
      | _ =>
        find_location_in_expressions(
          ~line,
          ~char,
          ~default=Expression(expr),
          expressions,
        )
      }
    | TExpApp(func, expressions) =>
      if (is_point_inside_location(~line, ~char, func.exp_loc)) {
        Expression(func);
      } else {
        find_location_in_expressions(
          ~line,
          ~char,
          ~default=Expression(expr),
          expressions,
        );
      }
    | TExpLambda([{mb_pat: pattern, mb_body: body}], _) =>
      let node = get_node_from_pattern(~line, ~char, pattern);
      switch (node) {
      | Error(_) => get_node_from_expression(~line, ~char, body)
      | _ => node
      };
    | TExpIf(cond, trueexp, falseexp) =>
      let condNode = get_node_from_expression(~line, ~char, cond);
      let trueMatch =
        switch (condNode) {
        | NotInRange
        | Error(_) => get_node_from_expression(~line, ~char, trueexp)
        | _ => condNode
        };

      switch (trueMatch) {
      | NotInRange
      | Error(_) => get_node_from_expression(~line, ~char, falseexp)
      | _ => trueMatch
      };

    | TExpArray(expressions)
    | TExpTuple(expressions) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        expressions,
      )

    | TExpLambda([], _) => failwith("Impossible: transl_imm: Empty lambda")
    | TExpLambda(_, _) =>
      failwith("Impossible: transl_imm: Multi-branch lambda")
    | TExpContinue => Expression(expr)
    | TExpBreak => Expression(expr)
    | TExpNull => Expression(expr)
    | TExpIdent(_) => Expression(expr)
    | TExpConstant(const) =>
      if (is_point_inside_location(~line, ~char, expr.exp_loc)) {
        Expression(expr);
      } else {
        NotInRange;
      }
    | TExpArrayGet(e1, e2) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [e1, e2],
      )
    | TExpArraySet(e1, e2, e3) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [e1, e2, e3],
      )
    | TExpRecord(_) => Expression(expr)
    | TExpRecordGet(e, _, _) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [e],
      )
    | TExpRecordSet(e1, _, _, e2) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [e1, e2],
      )
    | TExpMatch(cond, branches, _) =>
      if (is_point_inside_location(~line, ~char, cond.exp_loc)) {
        Expression(cond);
      } else {
        List.fold_left(
          (acc, mb: Typedtree.match_branch) =>
            if (is_point_inside_location(~line, ~char, mb.mb_pat.pat_loc)) {
              get_node_from_pattern(~line, ~char, mb.mb_pat);
            } else if (is_point_inside_location(
                         ~line,
                         ~char,
                         mb.mb_body.exp_loc,
                       )) {
              get_node_from_expression(~line, ~char, mb.mb_body);
            } else {
              switch (mb.mb_guard) {
              | None => acc
              | Some(e) =>
                if (is_point_inside_location(~line, ~char, e.exp_loc)) {
                  get_node_from_expression(~line, ~char, e);
                } else {
                  acc;
                }
              };
            },
          Expression(expr),
          branches,
        );
      }
    | TExpPrim1(_, e) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [e],
      )
    | TExpPrim2(_, e1, e2) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [e1, e2],
      )
    | TExpPrimN(_, expressions) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        expressions,
      )
    | TExpBoxAssign(e1, e2) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [e1, e2],
      )
    | TExpAssign(e1, e2) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [e1, e2],
      )
    | TExpWhile(cond, block) =>
      if (is_point_inside_location(~line, ~char, cond.exp_loc)) {
        get_node_from_expression(~line, ~char, cond);
      } else if (is_point_inside_location(~line, ~char, block.exp_loc)) {
        get_node_from_expression(~line, ~char, block);
      } else {
        Expression(expr);
      }

    | TExpFor(e1, e2, e3, block) =>
      if (is_point_inside_location(~line, ~char, block.exp_loc)) {
        get_node_from_expression(~line, ~char, block);
      } else {
        List.fold_left(
          (acc, exp) =>
            switch (exp) {
            | None => acc
            | Some(e: Typedtree.expression) =>
              if (is_point_inside_location(~line, ~char, e.exp_loc)) {
                get_node_from_expression(~line, ~char, e);
              } else {
                acc;
              }
            },
          Expression(expr),
          [e1, e2, e3],
        );
      }

    | TExpConstruct(_, _, expressions) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        expressions,
      )
    };

  switch (node) {
  | NotInRange
  | Error(_) =>
    if (is_point_inside_location(~line, ~char, expr.exp_loc)) {
      Expression(expr);
    } else {
      NotInRange;
    }
  | _ => node
  };
};

let get_node_from_statement =
    (~line, ~char, stmt: Grain_typed__Typedtree.toplevel_stmt) => {
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
            get_node_from_expression(~line, ~char, vb.vb_expr),
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
    get_node_from_expression(~line, ~char, expression)
  | TTopException(export_flag, type_exception) => Error("exception")
  | TTopExport(export_declarations) => Error("export")
  };
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

let get_original_text = (documents, uri, line, char) =>
  if (!Hashtbl.mem(documents, uri)) {
    Nothing;
  } else {
    let sourceCode = Hashtbl.find(documents, uri);
    // try and find the code we are completing in the original source

    let lines = String.split_on_char('\n', sourceCode);
    let line = List.nth(lines, line);
    find_completable(line, char);
  };

let get_module_exports = (mod_ident, compiled_code: Typedtree.typed_program) => {
  switch (Env.find_module(mod_ident, None, compiled_code.env)) {
  | lookup =>
    switch (lookup.md_filepath) {
    | None => []
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
                  documentation: "",
                };
                Some(item);

              | TSigType(ident, td, recstatus) =>
                let string_of_type_declaration = (~ident: Ident.t, td) => {
                  Format.asprintf(
                    "%a",
                    Printtyp.type_declaration(ident),
                    td,
                  );
                };
                let item: Rpc.completion_item = {
                  label: ident.name,
                  kind: item_kind_completion_struct,
                  detail: string_of_type_declaration(~ident, td),
                  documentation: "",
                };
                Some(item);
              | _ => None
              }
            },
            sigs,
          );
        fnsigs;
      | _ => []
      };
    }
  | exception _ => []
  };
};

let rec expression_lens =
        (~line, ~char, ~compiled_code, e: Typedtree.expression) => {
  let desc = e.exp_desc;
  let txt =
    switch (desc) {
    | TExpRecordGet(expr, loc, field) =>
      if (is_point_inside_location(~line, ~char, expr.exp_loc)) {
        lens_sig(~env=e.exp_env, expr.exp_type);
      } else {
        lens_sig(~env=e.exp_env, e.exp_type);
      }

    | TExpPrim1(_, exp) => lens_sig(~env=exp.exp_env, exp.exp_type)
    | TExpPrim2(_, exp, exp2) =>
      switch (
        find_location_in_expressions(
          ~line,
          ~char,
          ~default=NotInRange,
          [exp, exp2],
        )
      ) {
      | Expression(matched) => lens_sig(~env=e.exp_env, matched.exp_type)
      | _ => ""
      }
    | TExpPrimN(_, expressions) =>
      switch (
        find_location_in_expressions(
          ~line,
          ~char,
          ~default=NotInRange,
          expressions,
        )
      ) {
      | Expression(matched) => lens_sig(~env=e.exp_env, matched.exp_type)
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
              get_module_exports(mod_path, compiled_code)
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

  markdown_grain(txt);
};
