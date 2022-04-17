open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;

type source_location = {
  uri: string,
  line: int,
  char: int,
};

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

let map_concat = (~sep, ~print_fn, vals) => {
  String.concat(",", List.map(v => print_fn(v), vals));
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
  } else {
    line > raw1l && line < raw1le;
  };
};
let is_point_inside_location =
    (~line: int, ~char: int, loc1: Grain_parsing.Location.t) => {
  let (_, raw1l, raw1c, _) = Locations.get_raw_pos_info(loc1.loc_start);
  let (_, raw1le, raw1ce, _) = Locations.get_raw_pos_info(loc1.loc_end);

  if (line == raw1l) {
    if (char >= raw1c) {
      if (line == raw1le) {
        char <= raw1ce;
      } else {
        true;
      };
    } else {
      false;
    };
  } else if (line == raw1le) {
    char <= raw1ce;
  } else {
    line > raw1l && line < raw1le;
  };
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

  switch (uri, line, char) {
  | (Some(uri), Some(line), Some(char)) => Some({uri, line, char})
  | _ => None
  };
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
      | _ => "<" ++ map_concat(~sep=", ", ~print_fn=print_sig, types) ++ ">"
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
                  ++ map_concat(~sep=",\n", ~print_fn=print_sig, types)
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

      // I'm not using oprint here as it doesn't display the field as I want, just the name
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

let rec get_node_from_pattern = (~line, ~char, pattern: Typedtree.pattern) => {
  switch (pattern.pat_desc) {
  | TPatTuple(args)
  | TPatArray(args) =>
    // these contain patterns we want to search into to find a more accurate match
    let pats =
      List.filter(
        (p: Typedtree.pattern) =>
          is_point_inside_location(~line, ~char, p.pat_loc),
        args,
      );
    switch (pats) {
    | [] => Pattern(pattern) // We should always find a more accurate sub pattern, but if not we will use the parent
    | [p, ..._] => Pattern(p)
    };

  | _ =>
    // we don't go deeper into records as we only display the record type for any part of the pattern
    // All the other patterns are the top level so we just use their location
    Pattern(pattern)
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
  | _ => Error("Ambiguous locations found")
  };
}

and get_node_from_expression = (~line, ~char, expr: Typedtree.expression) => {
  let node =
    switch (expr.exp_desc) {
    | TExpLet(rec_flag, mut_flag, vbs) =>
      switch (vbs) {
      | [] =>
        Error(
          "Invalid code, can't have a let without at least one value binding",
        )
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
        | _ => Error("Ambiguous locations found")
        };
      }
    | TExpBlock(expressions) when expressions == [] => Expression(expr)
    | TExpBlock(expressions) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        expressions,
      )

    | TExpApp(func, expressions)
        when is_point_inside_location(~line, ~char, func.exp_loc) =>
      Expression(func)
    | TExpApp(func, expressions) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        expressions,
      )

    | TExpLambda([{mb_pat: pattern, mb_body: body}], _) =>
      let node = get_node_from_pattern(~line, ~char, pattern);
      switch (node) {
      | Error(_) => get_node_from_expression(~line, ~char, body)
      | _ => node
      };
    | TExpIf(cond, trueexp, falseexp) =>
      let cond_node = get_node_from_expression(~line, ~char, cond);
      let true_match =
        switch (cond_node) {
        | NotInRange
        | Error(_) => get_node_from_expression(~line, ~char, trueexp)
        | _ => cond_node
        };

      switch (true_match) {
      | NotInRange
      | Error(_) => get_node_from_expression(~line, ~char, falseexp)
      | _ => true_match
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
    | TExpMatch(cond, branches, _)
        when is_point_inside_location(~line, ~char, cond.exp_loc) =>
      Expression(cond)
    | TExpMatch(cond, branches, _) =>
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
            | Some(e) when is_point_inside_location(~line, ~char, e.exp_loc) =>
              get_node_from_expression(~line, ~char, e)
            | Some(e) => acc
            };
          },
        Expression(expr),
        branches,
      )

    | TExpPrim0(_) =>
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=Expression(expr),
        [],
      )
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

    | TExpFor(e1, e2, e3, block)
        when is_point_inside_location(~line, ~char, block.exp_loc) =>
      get_node_from_expression(~line, ~char, block)
    | TExpFor(e1, e2, e3, block) =>
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
      )

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
  | Error(_) when is_point_inside_location(~line, ~char, expr.exp_loc) =>
    Expression(expr)
  | Error(_) => NotInRange

  | _ => node
  };
};

// function below taken from
// https://github.com/jaredly/reason-language-server/blob/ce1b3f8ddb554b6498c2a83ea9c53a6bdf0b6081/src/analyze/PartialParser.re

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
  // try and find the code we are completing in the original source
  switch (Hashtbl.find_opt(documents, uri)) {
  | None => Nothing
  | Some(source_code) =>
    let lines = String.split_on_char('\n', source_code);
    let line = List.nth(lines, line);
    find_completable(line, char);
  };

let exn_to_lsp_error = (exn: exn): option(Rpc.lsp_error) => {
  let error = Grain_parsing.Location.error_of_exn(exn);

  switch (error) {
  | Some(err) =>
    switch (err) {
    | `Ok(e) =>
      let (file, line, startchar) =
        Grain_parsing.Location.get_pos_info(e.loc.loc_start);
      let (_, endline, endchar) =
        Grain_parsing.Location.get_pos_info(e.loc.loc_end);
      let error_json: Rpc.lsp_error = {
        file,
        line,
        startchar,
        endline,
        endchar,
        lsp_message: e.msg,
      };
      Some(error_json);
    | _ => None
    }
  | _ => None
  };
};

let convert_warnings = (warnings_this_run, topLevelFileName) => {
  List.map(
    w => {
      let (loc: Grain_utils.Warnings.loc, warn: Grain_utils.Warnings.t) = w;
      let (file, line, startchar) =
        Grain_parsing.Location.get_pos_info(loc.loc_start);
      let (_, endline, endchar) =
        Grain_parsing.Location.get_pos_info(loc.loc_end);
      let warning: Rpc.lsp_warning = {
        file,
        line,
        startchar,
        endline,
        endchar,
        number: Grain_utils.Warnings.number(warn),
        lsp_message: Grain_utils.Warnings.message(warn),
      };
      warning;
    },
    warnings_this_run,
  );
};

let convert_uri_to_filename = (uri: string) => {
  let filetype = "file://";
  let typelen = String.length(filetype);
  switch (String.sub(uri, 0, typelen)) {
  | exception exn => uri
  | start =>
    String.lowercase_ascii(start) == filetype
      ? String.sub(uri, typelen, String.length(uri) - typelen) : uri
  };
};
