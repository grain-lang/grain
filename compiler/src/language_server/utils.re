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
  String.concat(sep, List.map(v => print_fn(v), vals));
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

let print_loc_string = (msg: string, loc: Grain_parsing.Location.t) => {
  let (file, line, startchar, _) = Locations.get_raw_pos_info(loc.loc_start);
  let (_, endline, endchar, _) = Locations.get_raw_pos_info(loc.loc_end);

  if (startchar >= 0) {
    if (line == endline) {
      Printf.sprintf("%s %d:%d,%d\n", msg, line, startchar, endchar);
    } else {
      Printf.sprintf(
        "%s %d:%d - %d:%d\n",
        msg,
        line,
        startchar,
        endline,
        endchar,
      );
    };
  } else {
    Printf.sprintf(
      "%s %d:%d - %d:%d\n",
      msg,
      line,
      startchar,
      endline,
      endchar,
    );
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

let rec get_node_from_pattern = (~line, ~char, pattern: Typedtree.pattern) => {
  switch (pattern.pat_desc) {
  | TPatTuple(args)
  | TPatArray(args) =>
    // these contain patterns we want to search into to find a more accurate match
    let pats =
      List.filter(
        (p: Typedtree.pattern) => {
          let _ = print_loc_string("match pattern", p.pat_loc);
          is_point_inside_location(~line, ~char, p.pat_loc);
        },
        args,
      );
    switch (pats) {
    | [] => NotInRange
    | [p, ..._] => Pattern(p)
    };
  | TPatConstruct(_, _, args) =>
    // these contain patterns we want to search into to find a more accurate match
    let pats =
      List.filter(
        (p: Typedtree.pattern) => {
          let _ = print_loc_string("match pattern", p.pat_loc);
          is_point_inside_location(~line, ~char, p.pat_loc);
        },
        args,
      );
    switch (pats) {
    | [] => Pattern(pattern)
    | [p, ..._] => Pattern(p)
    };
  | TPatConstant(_)
  | TPatVar(_, _) =>
    if (is_point_inside_location(~line, ~char, pattern.pat_loc)) {
      Pattern(pattern);
    } else {
      NotInRange;
    }

  | _ =>
    // we don't go deeper into records as we only display the record type for any part of the pattern
    // All the other patterns are the top level so we just use their location
    NotInRange
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
  | [] =>
    switch (default) {
    | Expression(def) =>
      if (is_point_inside_location(~line, ~char, def.exp_loc)) {
        default;
      } else {
        NotInRange;
      }
    | _ => NotInRange
    }

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
      | Error(_)
      | NotInRange => get_node_from_expression(~line, ~char, body)
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
      let find_match =
        List.fold_left(
          (acc, mb: Typedtree.match_branch) => {
            let found_in_branch =
              if (is_point_inside_location(~line, ~char, mb.mb_pat.pat_loc)) {
                if (acc == NotInRange) {
                  get_node_from_pattern(~line, ~char, mb.mb_pat);
                } else {
                  acc;
                };
              } else if (is_point_inside_location(
                           ~line,
                           ~char,
                           mb.mb_body.exp_loc,
                         )) {
                if (acc == NotInRange) {
                  get_node_from_expression(~line, ~char, mb.mb_body);
                } else {
                  acc;
                };
              } else {
                switch (mb.mb_guard) {
                | None => acc
                | Some(e)
                    when is_point_inside_location(~line, ~char, e.exp_loc) =>
                  if (acc == NotInRange) {
                    get_node_from_expression(~line, ~char, e);
                  } else {
                    acc;
                  }
                | Some(e) => acc
                };
              };

            switch (found_in_branch) {
            | Pattern(_)
            | Expression(_) => found_in_branch
            | _ =>
              // TODO #1221 - default to the branch body when lists mean we don't get locations
              if (is_point_inside_location(~line, ~char, mb.mb_loc)) {
                Expression(mb.mb_body);
              } else {
                found_in_branch;
              }
            };
          },
          NotInRange,
          branches,
        );

      switch (find_match) {
      | NotInRange => Expression(expr)
      | _ => find_match
      };

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
