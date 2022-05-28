open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hoverParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    position: Protocol.position,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hover
module ResponseResult = {
  [@deriving yojson]
  type markup_content = {
    kind: string,
    value: string,
  };

  [@deriving yojson]
  type t = {
    contents: markup_content,
    range: Protocol.range,
  };
};

type node_location =
  | LocationSignature(string, Warnings.loc)
  | LocationError;

type node_t =
  | Expression(Typedtree.expression)
  | Pattern(Typedtree.pattern)
  | LabelField((Location.t, Typedtree.expression))
  | NotInRange
  | Error(string);

let loc_to_range = (pos: Location.t): Protocol.range => {
  let (_, startline, startchar, _) =
    Locations.get_raw_pos_info(pos.loc_start);
  let (_, endline, endchar) =
    Grain_parsing.Location.get_pos_info(pos.loc_end);

  {
    range_start: {
      line: startline - 1,
      character: startchar,
    },
    range_end: {
      line: endline - 1,
      character: endchar,
    },
  };
};

// We need to use the "grain-type" markdown syntax to have correct coloring on hover items
let grain_type_code_block = Markdown.code_block(~syntax="grain-type");
// Used for module hovers
let grain_code_block = Markdown.code_block(~syntax="grain");

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

let rec get_node_from_pattern = (~line, ~char, pattern: Typedtree.pattern) => {
  switch (pattern.pat_desc) {
  | TPatTuple(args)
  | TPatArray(args) =>
    // these contain patterns we want to search into to find a more accurate match
    let pats =
      List.filter(
        (p: Typedtree.pattern) => {
          is_point_inside_location(~line, ~char, p.pat_loc)
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
          is_point_inside_location(~line, ~char, p.pat_loc)
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
    | TExpLet(rec_flag, mut_flag, []) =>
      Error(
        "Invalid code, can't have a let without at least one value binding",
      )
    | TExpLet(rec_flag, mut_flag, vbs) =>
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
    | TExpBlock([]) => Expression(expr)
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
    | TExpConstant(const)
        when is_point_inside_location(~line, ~char, expr.exp_loc) =>
      Expression(expr)
    | TExpConstant(const) => NotInRange
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
    | TExpRecord(fields) =>
      switch (
        Array.fold_left(
          (
            acc,
            field: (
              Types.label_description,
              Typedtree.record_label_definition,
            ),
          ) => {
            let (ld, rld) = field;
            switch (rld) {
            | Kept(te) => acc // TODO: not sure what to do here
            | Overridden(loc, expr) =>
              if (is_point_inside_location(~line, ~char, expr.exp_loc)) {
                [Expression(expr)] @ acc;
              } else if (is_point_inside_location(~line, ~char, loc.loc)) {
                [LabelField((loc.loc, expr))] @ acc;
              } else {
                acc;
              }
            };
          },
          [],
          fields,
        )
      ) {
      | [node, ..._] => node
      | _ => Expression(expr)
      }

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
    | TExpWhile(cond, block)
        when is_point_inside_location(~line, ~char, cond.exp_loc) =>
      get_node_from_expression(~line, ~char, cond)
    | TExpWhile(cond, block)
        when is_point_inside_location(~line, ~char, block.exp_loc) =>
      get_node_from_expression(~line, ~char, block)
    | TExpWhile(cond, block) => Expression(expr)
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

let send_hover = (~id: Protocol.message_id, ~range: Protocol.range, signature) => {
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({
      contents: {
        kind: "markdown",
        value: signature,
      },
      range,
    }),
  );
};

// TODO Make hovers for modules work
let rec expression_lens =
        (
          ~line,
          ~char,
          ~compiled_code: Typedtree.typed_program,
          e: Typedtree.expression,
        ) => {
  switch (e) {
  | {exp_desc: TExpRecordGet(expr, loc, field)}
      when is_point_inside_location(~line, ~char, expr.exp_loc) =>
    grain_type_code_block(Printtyp.string_of_type_scheme(expr.exp_type))
  | {exp_desc: TExpRecordGet(expr, loc, field)} =>
    grain_type_code_block(Printtyp.string_of_type_scheme(e.exp_type))
  | {exp_desc: TExpPrim1(_, exp)} =>
    grain_type_code_block(Printtyp.string_of_type_scheme(exp.exp_type))
  | {exp_desc: TExpPrim2(_, exp, exp2)} =>
    switch (
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=NotInRange,
        [exp, exp2],
      )
    ) {
    | Expression(matched) =>
      grain_type_code_block(Printtyp.string_of_type_scheme(matched.exp_type))
    | _ => ""
    }
  | {exp_desc: TExpPrimN(_, expressions)} =>
    switch (
      find_location_in_expressions(
        ~line,
        ~char,
        ~default=NotInRange,
        expressions,
      )
    ) {
    | Expression(matched) =>
      grain_type_code_block(Printtyp.string_of_type_scheme(matched.exp_type))
    | _ => ""
    }
  | {exp_desc: TExpIdent(_), exp_type: {desc: TTyConstr(path, _, _)}}
  | {
      exp_desc: TExpIdent(_),
      exp_type: {desc: TTyLink({desc: TTyConstr(path, _, _)})},
    }
  | {
      exp_desc: TExpIdent(_),
      exp_type: {desc: TTySubst({desc: TTyConstr(path, _, _)})},
    } =>
    let td = Env.find_type(path, e.exp_env);
    grain_type_code_block(
      Printtyp.string_of_type_declaration(~ident=Path.head(path), td),
    );
  | {exp_desc: TExpIdent(PExternal(mod_path, _, _), loc, vd)}
      when Path.name(mod_path) != "Pervasives" =>
    let lstart = loc.loc.loc_start;
    let mod_start = lstart.pos_cnum - lstart.pos_bol;
    let mod_end = mod_start + String.length(Path.name(mod_path));

    if (char < mod_end) {
      let vals = Modules.get_exports(mod_path, compiled_code);
      let signatures =
        List.map(
          (v: Modules.export) =>
            switch (v.kind) {
            | Function
            | Value => Format.sprintf("let %s", v.signature)
            | Record
            | Enum
            | Abstract
            | Exception => v.signature
            },
          vals,
        );
      grain_code_block(String.concat("\n", signatures));
    } else {
      grain_type_code_block(Printtyp.string_of_type_scheme(e.exp_type));
    };

  | _ => grain_type_code_block(Printtyp.string_of_type_scheme(e.exp_type))
  };
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
  | TTopForeign(value_description) =>
    let tvd_desc = value_description.tvd_desc;
    let type_sig = Printtyp.string_of_type_scheme(tvd_desc.ctyp_type);
    LocationSignature(type_sig, stmt.ttop_loc);
  | TTopData([]) => LocationError
  | TTopData(data_declarations) =>
    let matches =
      List.filter(
        (dd: Typedtree.data_declaration) =>
          is_point_inside_location(~line, ~char, dd.data_loc),
        data_declarations,
      );
    switch (matches) {
    | [] => LocationError
    | [{data_name, data_manifest, data_loc, data_kind: TDataAbstract}, ..._] =>
      switch (data_manifest) {
      | None => LocationSignature(data_name.txt, data_loc)
      | Some(t) =>
        LocationSignature(
          grain_type_code_block(Printtyp.string_of_type_scheme(t.ctyp_type)),
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
            is_point_inside_location(~line, ~char, cd.cd_loc),
          constrs,
        );

      switch (matches) {
      | [decl] =>
        LocationSignature(
          grain_type_code_block(decl.cd_name.txt),
          decl.cd_loc,
        )
      | _ =>
        LocationSignature(
          grain_type_code_block("enum " ++ data_name.txt),
          data_loc,
        )
      };

    | [
        {
          data_name,
          data_manifest,
          data_loc,
          data_params,
          data_kind: TDataRecord(record_fields),
        },
        ..._,
      ] =>
      switch (record_fields) {
      | [] =>
        LocationSignature(grain_type_code_block(data_name.txt), data_loc)
      | _ =>
        let matches =
          List.filter(
            (rf: Typedtree.record_field) =>
              is_point_inside_location(~line, ~char, rf.rf_loc),
            record_fields,
          );
        switch (matches) {
        | [field] =>
          LocationSignature(
            grain_type_code_block(
              Printtyp.string_of_type_scheme(field.rf_type.ctyp_type),
            ),
            field.rf_loc,
          )
        | _ =>
          LocationSignature(grain_type_code_block(data_name.txt), data_loc)
        };
      }
    };

  | TTopLet(rec_flag, mut_flag, []) => LocationError
  | TTopLet(rec_flag, mut_flag, value_bindings) =>
    let matches =
      List.map(
        (vb: Typedtree.value_binding) =>
          get_node_from_expression(~line, ~char, vb.vb_expr),
        value_bindings,
      );
    let filtered =
      List.filter(
        m =>
          switch ((m: node_t)) {
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
          grain_type_code_block(Printtyp.string_of_type_scheme(p.pat_type)),
          if (p.pat_loc == Grain_parsing.Location.dummy_loc) {
            stmt.ttop_loc;
          } else {
            p.pat_loc;
          },
        )
      | LabelField((loc, e)) =>
        LocationSignature(
          expression_lens(~line, ~char, ~compiled_code, e),
          if (loc == Grain_parsing.Location.dummy_loc) {
            stmt.ttop_loc;
          } else {
            loc;
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
        let nd: node_t = Expression(expression);
        (stmt.ttop_loc, nd);
      } else {
        (
          expression.exp_loc,
          get_node_from_expression(~line, ~char, expression),
        );
      };

    switch (node) {
    | Error(err) => LocationError
    | NotInRange =>
      LocationSignature(
        grain_type_code_block(
          Printtyp.string_of_type_scheme(expression.exp_type),
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
        grain_type_code_block(Printtyp.string_of_type_scheme(p.pat_type)),
        p.pat_loc,
      )
    | LabelField((field_loc, e)) =>
      LocationSignature(
        expression_lens(~line, ~char, ~compiled_code, e),
        if (field_loc == Grain_parsing.Location.dummy_loc) {
          loc;
        } else {
          field_loc;
        },
      )
    };

  | TTopException(type_exception) => LocationError
  | TTopExport(export_declarations) => LocationError
  };
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(Protocol.uri, Typedtree.typed_program),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  let ln = params.position.line + 1;
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => ()
  | Some(compiled_code) =>
    let node =
      find_best_match(
        ~line=ln,
        ~char=params.position.character,
        compiled_code,
      );
    switch (node) {
    | Some(stmt) =>
      switch (
        get_from_statement(
          ~uri=params.text_document.uri,
          ~line=ln,
          ~char=params.position.character,
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
};
