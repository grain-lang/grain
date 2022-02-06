open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;

type node_t =
  | Expression(Typedtree.expression)
  | Pattern(Typedtree.pattern)
  | NotInRange
  | Error(string);

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

let loc_to_range = (pos: Location.t): Rpc.range_t => {
  let (_, startline, startchar, _) = get_raw_pos_info(pos.loc_start);
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
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);
  let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc1.loc_end);

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
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);
  let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc1.loc_end);

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

let lens_sig = (t: Types.type_expr) => {
  let buf = Buffer.create(64);
  let ppf = Format.formatter_of_buffer(buf);
  Printtyp.type_expr(ppf, t);
  Format.pp_print_flush(ppf, ());
  let sigStr = Buffer.contents(buf);
  sigStr;
};

let print_expr = (~t: Types.type_expr) => {
  let buf = Buffer.create(64);
  let ppf = Format.formatter_of_buffer(buf);
  Printtyp.type_expr(ppf, t);
  Format.pp_print_flush(ppf, ());
  let sigStr = Buffer.contents(buf);
  sigStr;
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
        Expression(func);
      } else {
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
          | _ => Error("Multiple functionn applications match")
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

let rec print_path = (ident: Path.t) => {
  switch (ident) {
  | PIdent(name) => name.name
  | PExternal(externalIdent, second, _) =>
    print_path(externalIdent) ++ "." ++ second
  };
};
