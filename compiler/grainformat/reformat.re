open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;

module Doc = Res_doc;

type stmtList =
  | BlankLine
  | BlockComment(string)
  | Statement(Parsetree.toplevel_stmt);

type sugaredListItem =
  | Regular(Grain_parsing.Parsetree.expression)
  | Spread(Grain_parsing.Parsetree.expression);

type sugaredPatternItem =
  | RegularPattern(Grain_parsing.Parsetree.pattern)
  | SpreadPattern(Grain_parsing.Parsetree.pattern);

let addParens = doc =>
  Doc.group(
    Doc.concat([
      Doc.lparen,
      Doc.indent(Doc.concat([Doc.softLine, doc])),
      Doc.softLine,
      Doc.rparen,
    ]),
  );

let addBraces = doc =>
  Doc.group(
    Doc.concat([
      Doc.lbrace,
      Doc.indent(Doc.concat([Doc.softLine, doc])),
      Doc.softLine,
      Doc.rbrace,
    ]),
  );

let infixop = (op: string) => {
  switch (op) {
  | "+"
  | "-"
  | "*"
  | "/"
  | "%"
  | "is"
  | "isnt"
  | "=="
  | "++"
  | "!="
  | "^"
  | "<"
  | "<<"
  | ">"
  | ">>"
  | ">>>"
  | "<="
  | ">="
  | "&"
  | "&&"
  | "|"
  | "||" => true
  | _ => false
  };
};
let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

let find_comments_in_range =
    (
      range_start: int,
      range_end: int,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  List.find_all(
    (c: Grain_parsing.Parsetree.comment) => {
      switch (c) {
      | Line(cmt) =>
        let (_file, startline, _startchar, _sbol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        startline >= range_start && startline <= range_end;

      | Shebang(cmt) =>
        let (_file, startline, _startchar, _sbol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        startline >= range_start && startline <= range_end;

      | Block(cmt) =>
        let (_file, startline, _startchar, _sbol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        let (_efile, endline, _endchar, _ebol) =
          get_raw_pos_info(cmt.cmt_loc.loc_end);

        startline >= range_start && endline <= range_end;

      | Doc(cmt) =>
        let (_file, startline, _startchar, _sbol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        let (_efile, endline, _endchar, _ebol) =
          get_raw_pos_info(cmt.cmt_loc.loc_end);

        startline >= range_start && endline <= range_end;
      }
    },
    comments,
  );
};

let rec resugar_list_patterns = (patterns: list(Parsetree.pattern)) => {
  let processed_list = resugar_pattern_list_inner(patterns);
  let items =
    List.map(
      i =>
        switch (i) {
        | RegularPattern(e) => print_pattern(e)
        | SpreadPattern(e) =>
          Doc.concat([Doc.text("..."), print_pattern(e)])
        },
      processed_list,
    );
  Doc.group(
    Doc.concat([
      Doc.lbracket,
      Doc.join(Doc.concat([Doc.comma, Doc.line]), items),
      Doc.rbracket,
    ]),
  );
}

and resugar_pattern_list_inner = (patterns: list(Parsetree.pattern)) => {
  let arg1 = List.nth(patterns, 0);
  let arg2 = List.nth(patterns, 1);

  // print_endline("resugar_pattern_list_inner");
  // print_endline(Doc.toString(~width=80, print_pattern(arg1)));
  // print_endline(Doc.toString(~width=80, print_pattern(arg2)));

  switch (arg2.ppat_desc) {
  | PPatConstruct(innerfunc, innerpatterns) =>
    let func =
      switch (innerfunc.txt) {
      | IdentName(name) => name
      | _ => ""
      };

    if (func == "[]") {
      [RegularPattern(arg1)];
    } else if (is_empty_pattern_array(arg2)) {
      [RegularPattern(arg1)];
    } else {
      [RegularPattern(arg1), SpreadPattern(arg2)];
    };
  | _ =>
    if (is_empty_pattern_array(arg2)) {
      [RegularPattern(arg1)];
    } else {
      [RegularPattern(arg1), SpreadPattern(arg2)];
    }
  };
}

and is_empty_array = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpId(loc) =>
    let loc_txt =
      switch (loc.txt) {
      | IdentName(name) => name
      | _ => ""
      };

    if (loc_txt == "[]") {
      true;
    } else {
      false;
    };
  | _ => false
  };
}

and is_empty_pattern_array = (pat: Parsetree.pattern) => {
  let _ =
    switch (pat.ppat_desc) {
    | PPatAny => false
    | PPatConstant(c) => false
    | PPatVar({txt, _}) =>
      if (txt == "[]") {
        true;
      } else {
        false;
      }
    | PPatTuple(patterns) => false
    | PPatArray(patterns) => false
    | PPatConstraint(pattern, parsed_type) => false
    | PPatConstruct(location, patterns) => false
    | _ => false
    };

  false;
}

and resugar_list = (expressions: list(Parsetree.expression)) => {
  let processed_list = resugar_list_inner(expressions);

  let items =
    List.map(
      i =>
        switch (i) {
        | Regular(e) => print_expression(~expr=e, ~parentIsArrow=false)
        | Spread(e) =>
          Doc.concat([
            Doc.text("..."),
            print_expression(~expr=e, ~parentIsArrow=false),
          ])
        },
      processed_list,
    );

  Doc.group(
    Doc.concat([
      Doc.lbracket,
      Doc.indent(
        Doc.concat([
          Doc.softLine,
          Doc.join(Doc.concat([Doc.comma, Doc.line]), items),
        ]),
      ),
      Doc.softLine,
      Doc.rbracket,
    ]),
  );
}

and resugar_list_inner = (expressions: list(Parsetree.expression)) => {
  let arg1 = List.nth(expressions, 0);
  let arg2 = List.nth(expressions, 1);

  switch (arg2.pexp_desc) {
  | PExpApp(innerfunc, innerexpressions) =>
    let funcName = print_expression(~expr=innerfunc, ~parentIsArrow=false);
    let funcNameAsString = Doc.toString(~width=20, funcName);

    if (funcNameAsString == "[...]") {
      let inner = resugar_list_inner(innerexpressions);
      List.append([Regular(arg1)], inner);
    } else {
      [Regular(arg1), Spread(arg2)];
    };
  | _ =>
    // print_endline(Doc.toString(~width=80, print_expression(arg1)));

    // print_endline(Doc.toString(~width=80, print_expression(arg2)));

    if (is_empty_array(arg2)) {
      [Regular(arg1)];
    } else {
      [Regular(arg1), Spread(arg2)];
    }
  };
}

and print_record_pattern =
    (
      patternlocs:
        list(
          (
            Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
            Grain_parsing__Parsetree.pattern,
          ),
        ),
      closedflag: Grain_parsing__Asttypes.closed_flag,
    ) => {
  let close =
    switch (closedflag) {
    | Open => Doc.concat([Doc.text(","), Doc.space, Doc.text("_")])
    | Closed => Doc.nil
    };
  Doc.concat([
    Doc.lbrace,
    Doc.space,
    Doc.join(
      Doc.concat([Doc.comma, Doc.space]),
      List.map(
        (
          patternloc: (
            Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
            Grain_parsing__Parsetree.pattern,
          ),
        ) => {
          let (loc, pat) = patternloc;

          let printed_ident: Doc.t = print_ident(loc.txt);
          let printed_pat = print_pattern(pat);

          let pun =
            switch (printed_ident) {
            | Text(i) =>
              switch ((printed_pat: Doc.t)) {
              | Text(e) => i == e
              | _ => false
              }
            | _ => false
            };
          if (pun) {
            printed_ident;
          } else {
            Doc.concat([printed_ident, Doc.text(": "), printed_pat]);
          };
        },
        patternlocs,
      ),
    ),
    close,
    Doc.space,
    Doc.rbrace,
  ]);
}
and print_pattern = (pat: Parsetree.pattern) => {
  switch (pat.ppat_desc) {
  | PPatAny => Doc.text("_")
  | PPatConstant(c) => print_constant(c)
  | PPatVar({txt, _}) =>
    if (infixop(txt)) {
      Doc.concat([Doc.lparen, Doc.text(txt), Doc.rparen]);
    } else {
      Doc.text(txt);
    }

  | PPatTuple(patterns) =>
    addParens(
      Doc.join(Doc.comma, List.map(p => print_pattern(p), patterns)),
    )

  | PPatArray(patterns) =>
    Doc.group(
      Doc.concat([
        Doc.lbracket,
        Doc.join(Doc.comma, List.map(p => print_pattern(p), patterns)),
        Doc.rbracket,
      ]),
    )
  | PPatRecord(patternlocs, closedflag) =>
    //print_endline("PPatRecord");
    print_record_pattern(patternlocs, closedflag)
  | PPatConstraint(pattern, parsed_type) =>
    Doc.concat([
      print_pattern(pattern),
      Doc.concat([Doc.text(":"), Doc.space]),
      print_type(parsed_type),
    ])
  | PPatConstruct(location, patterns) =>
    let func =
      switch (location.txt) {
      | IdentName(name) => name
      | _ => ""
      };
    if (func == "[...]") {
      resugar_list_patterns(patterns);
    } else {
      Doc.concat([
        print_ident(location.txt),
        if (List.length(patterns) > 0) {
          addParens(
            Doc.join(
              Doc.comma,
              List.map(pat => print_pattern(pat), patterns),
            ),
          );
        } else {
          Doc.nil;
        },
      ]);
    };

  | PPatOr(pattern1, pattern2) => Doc.text("PPatOr")
  | PPatAlias(pattern, loc) => Doc.text("PPatAlias")
  };
}
and print_constant = (c: Parsetree.constant) => {
  let print_c =
    switch (c) {
    | PConstNumber(PConstNumberInt(i)) => Printf.sprintf("%s", i)
    | PConstNumber(PConstNumberFloat(f)) => Printf.sprintf("%s", f)
    | PConstNumber(PConstNumberRational(n, d)) =>
      Printf.sprintf("%s/%s", n, d)
    | PConstBytes(b) => Printf.sprintf("%s", b)
    | PConstChar(c) => Printf.sprintf("'%s'", c)
    | PConstFloat64(f) => Printf.sprintf("%sd", f)
    | PConstFloat32(f) => Printf.sprintf("%sf", f)
    | PConstInt32(i) => Printf.sprintf("%sl", i)
    | PConstInt64(i) => Printf.sprintf("%sL", i)
    | PConstWasmI32(i) => Printf.sprintf("%sn", i)
    | PConstWasmI64(i) => Printf.sprintf("%sN", i)
    | PConstWasmF32(f) => Printf.sprintf("%sw", f)
    | PConstWasmF64(f) => Printf.sprintf("%sW", f)
    | PConstBool(true) => "true"
    | PConstBool(false) => "false"
    | PConstVoid => "void"
    | PConstString(s) => Printf.sprintf("\"%s\"", s)
    };
  // print_endline(print_c);
  Doc.text(print_c);
}
and print_ident = (ident: Identifier.t) => {
  switch (ident) {
  | IdentName(name) => Doc.text(name)
  | IdentExternal(externalIdent, second) =>
    Doc.concat([
      print_ident(externalIdent),
      Doc.text("."),
      Doc.text(second),
    ])
  };
}

and print_imported_ident = (ident: Identifier.t) => {
  switch (ident) {
  | IdentName(name) =>
    if (infixop(name)) {
      Doc.concat([Doc.lparen, Doc.text(name), Doc.rparen]);
    } else {
      Doc.text(name);
    }

  | IdentExternal(externalIdent, second) =>
    Doc.concat([
      print_ident(externalIdent),
      Doc.text("."),
      Doc.text(second),
    ])
  };
}
and print_record =
    (
      fields:
        list(
          (
            Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
            Grain_parsing__Parsetree.expression,
          ),
        ),
    ) =>
  Doc.concat([
    Doc.lbrace,
    Doc.indent(
      Doc.concat([
        Doc.line,
        Doc.join(
          Doc.concat([Doc.comma, Doc.line]),
          List.map(
            (
              field: (
                Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
                Grain_parsing__Parsetree.expression,
              ),
            ) => {
              let (locidentifier, expr) = field;
              let ident = locidentifier.txt;

              let printed_ident = print_ident(ident);
              let printed_expr =
                print_expression(~expr, ~parentIsArrow=false);

              // print_endline("ident:");
              // Doc.debug(printed_ident);
              // print_endline("expr:");
              // Doc.debug(printed_expr);

              let pun =
                switch (printed_ident) {
                | Text(i) =>
                  switch ((printed_expr: Doc.t)) {
                  | Text(e) => i == e
                  | _ => false
                  }
                | _ => false
                };

              if (!pun) {
                Doc.group(
                  Doc.concat([printed_ident, Doc.text(": "), printed_expr]),
                );
              } else {
                Doc.group(printed_ident);
              };
            },
            fields,
          ),
        ),
        Doc.ifBreaks(Doc.text(","), Doc.nil),
      ]),
    ),
    Doc.line,
    Doc.rbrace,
  ])

and print_type = (p: Grain_parsing__Parsetree.parsed_type) => {
  switch (p.ptyp_desc) {
  | PTyAny => Doc.text("AnyType")
  | PTyVar(name) => Doc.text(name)
  | PTyArrow(types, parsed_type) =>
    Doc.concat([
      Doc.group(
        if (List.length(types) == 1) {
          print_type(List.hd(types));
        } else {
          Doc.concat([
            Doc.lparen,
            Doc.join(
              Doc.concat([Doc.comma, Doc.space]),
              List.map(t => print_type(t), types),
            ),
            Doc.rparen,
          ]);
        },
      ),
      Doc.text(" -> "),
      print_type(parsed_type),
    ])

  | PTyTuple(parsed_types) =>
    addParens(
      Doc.join(Doc.comma, List.map(t => print_type(t), parsed_types)),
    )

  | PTyConstr(locidentifier, parsedtypes) =>
    let ident = locidentifier.txt;
    Doc.concat([
      print_ident(ident),
      Doc.concat(
        List.map(
          typ => {
            Doc.concat([Doc.text("<"), print_type(typ), Doc.text(">")])
          },
          parsedtypes,
        ),
      ),
    ]);
  | PTyPoly(locationstrings, parsed_type) => Doc.text("PTyPoly")
  };
}
and print_application =
    (func: Parsetree.expression, expressions: list(Parsetree.expression)) => {
  let functionNameDoc = print_expression(~expr=func, ~parentIsArrow=false);
  let functionName = Doc.toString(~width=100, functionNameDoc);
  // print_endline("functioname is " ++ functionName);

  if (infixop(functionName)) {
    let first = List.hd(expressions);
    let second = List.hd(List.tl(expressions)); // assumes an infix only has two expressions

    let firstBrackets =
      switch (first.pexp_desc) {
      | PExpApp(_) =>
        Doc.concat([
          Doc.lparen,
          print_expression(~expr=first, ~parentIsArrow=false),
          Doc.rparen,
        ])
      | _ => print_expression(~expr=first, ~parentIsArrow=false)
      };

    let secondBrackets =
      switch (second.pexp_desc) {
      | PExpApp(_) =>
        Doc.concat([
          Doc.lparen,
          print_expression(~expr=second, ~parentIsArrow=false),
          Doc.rparen,
        ])
      | _ => print_expression(~expr=second, ~parentIsArrow=false)
      };
    Doc.group(
      Doc.concat([
        firstBrackets,
        Doc.space,
        print_expression(~expr=func, ~parentIsArrow=false),
        Doc.line,
        secondBrackets,
      ]),
    );
  } else {
    let funcName = print_expression(~expr=func, ~parentIsArrow=false);

    let funcNameAsString = Doc.toString(~width=20, funcName);
    // print_endline("Function application is " ++ funcNameAsString);
    if (funcNameAsString == "[...]") {
      resugar_list(expressions);
    } else if (Doc.toString(~width=20, funcName) == "throw") {
      Doc.concat([
        funcName,
        Doc.space,
        print_expression(~expr=List.hd(expressions), ~parentIsArrow=false),
      ]);
    } else if (funcNameAsString == "!") {
      Doc.concat([
        funcName,
        print_expression(~expr=List.hd(expressions), ~parentIsArrow=false),
      ]);
    } else {
      Doc.concat([
        funcName,
        Doc.lparen,
        Doc.join(
          Doc.concat([Doc.text(","), Doc.line]),
          List.map(
            e => print_expression(~expr=e, ~parentIsArrow=false),
            expressions,
          ),
        ),
        Doc.rparen,
      ]);
    };
  };
}

and print_expression = (~expr: Parsetree.expression, ~parentIsArrow: bool) => {
  switch (expr.pexp_desc) {
  | PExpConstant(x) =>
    // print_endline("PExpConstant");
    print_constant(x)
  | PExpId({txt: id}) =>
    // print_endline("PExpId");
    print_ident(id)
  | PExpLet(rec_flag, mut_flag, vbs) =>
    // print_endline("PExpLet");
    value_bind_print(Asttypes.Nonexported, rec_flag, mut_flag, vbs)
  | PExpTuple(expressions) =>
    // print_endline("PExpTuple");
    addParens(
      Doc.join(
        Doc.comma,
        List.map(
          e => print_expression(~expr=e, ~parentIsArrow=false),
          expressions,
        ),
      ),
    )

  | PExpArray(expressions) =>
    //print_endline("PExpArray");
    Doc.group(
      Doc.concat([
        Doc.lbracket,
        Doc.join(
          Doc.comma,
          List.map(
            e => print_expression(~expr=e, ~parentIsArrow=false),
            expressions,
          ),
        ),
        Doc.rbracket,
      ]),
    )
  | PExpArrayGet(expression1, expression2) =>
    // print_endline("PExpArrayGet");
    Doc.concat([
      print_expression(~expr=expression1, ~parentIsArrow=false),
      Doc.lbracket,
      print_expression(~expr=expression2, ~parentIsArrow=false),
      Doc.rbracket,
    ])
  | PExpArraySet(expression1, expression2, expression3) =>
    //print_endline("PExpArraySet");
    Doc.concat([
      print_expression(~expr=expression1, ~parentIsArrow=false),
      Doc.lbracket,
      print_expression(~expr=expression2, ~parentIsArrow=false),
      Doc.rbracket,
      Doc.text(" = "),
      print_expression(~expr=expression3, ~parentIsArrow=false),
    ])

  | PExpRecord(record) =>
    // print_endline("PExpRecord");
    print_record(record)
  | PExpRecordGet(expression, {txt, _}) =>
    // print_endline("PExpRecordGet");
    Doc.concat([
      print_expression(~expr=expression, ~parentIsArrow=false),
      Doc.dot,
      print_ident(txt),
    ])
  | PExpRecordSet(expression, {txt, _}, expression2) =>
    //  print_endline("PExpRecordSet");
    Doc.concat([
      print_expression(~expr=expression, ~parentIsArrow=false),
      Doc.dot,
      print_ident(txt),
      Doc.text(" = "),
      print_expression(~expr=expression2, ~parentIsArrow=false),
    ])
  | PExpMatch(expression, match_branches) =>
    // print_endline("PExpMatch");
    Doc.concat([
      Doc.group(
        Doc.concat([
          Doc.text("match "),
          Doc.lparen,
          Doc.indent(
            Doc.concat([
              Doc.softLine,
              print_expression(~expr=expression, ~parentIsArrow=false),
            ]),
          ),
          Doc.softLine,
          Doc.rparen,
        ]),
      ),
      Doc.space,
      Doc.lbrace,
      Doc.indent(
        Doc.concat([
          Doc.hardLine,
          Doc.join(
            Doc.concat([Doc.comma, Doc.hardLine]),
            List.map(
              (branch: Parsetree.match_branch) =>
                Doc.concat([
                  print_pattern(branch.pmb_pat),
                  Doc.text(" =>"),
                  Doc.ifBreaks(Doc.space, Doc.nil),
                  print_expression(
                    ~expr=branch.pmb_body,
                    ~parentIsArrow=true,
                  ),
                ]),
              match_branches,
            ),
          ),
          Doc.ifBreaks(Doc.text(","), Doc.nil),
        ]),
      ),
      Doc.hardLine,
      Doc.rbrace,
    ])

  | PExpPrim1(prim1, expression) =>
    print_endline("PExpPrim1");
    Doc.text("PExpPrim1");
  | PExpPrim2(prim2, expression, expression1) =>
    print_endline("PExpPrim2");
    Doc.text("PExpPrim2");
  | PExpPrimN(primn, expressions) =>
    print_endline("PExpPrimN");
    Doc.text("PExpPrimN");
  | PExpIf(condition, trueExpr, falseExpr) =>
    // print_endline("PExpIf");
    let trueClause =
      Doc.group(print_expression(~expr=trueExpr, ~parentIsArrow=false));

    let falseClause =
      switch (falseExpr.pexp_desc) {
      | PExpBlock(expressions) =>
        if (List.length(expressions) > 0) {
          Doc.concat([
            Doc.line,
            Doc.text("else "),
            Doc.group(
              print_expression(~expr=falseExpr, ~parentIsArrow=false),
            ),
          ]);
        } else {
          Doc.nil;
        }
      | _ =>
        Doc.concat([
          Doc.line,
          Doc.text("else "),
          Doc.group(print_expression(~expr=falseExpr, ~parentIsArrow=false)),
        ])
      };

    if (parentIsArrow) {
      Doc.group(
        Doc.indent(
          Doc.concat([
            Doc.line,
            Doc.text("if "),
            Doc.group(
              Doc.concat([
                Doc.lparen,
                print_expression(~expr=condition, ~parentIsArrow=false),
                Doc.rparen,
                Doc.space,
              ]),
            ),
            trueClause,
            falseClause,
          ]),
        ),
      );
    } else {
      Doc.group(
        Doc.concat([
          Doc.line,
          Doc.text("if "),
          Doc.group(
            Doc.concat([
              Doc.lparen,
              print_expression(~expr=condition, ~parentIsArrow=false),
              Doc.rparen,
              Doc.space,
            ]),
          ),
          Doc.space,
          trueClause,
          falseClause,
        ]),
      );
    };
  | PExpWhile(expression, expression1) =>
    Doc.concat([
      Doc.text("while "),
      addParens(print_expression(~expr=expression, ~parentIsArrow=false)),
      Doc.space,
      Doc.group(print_expression(~expr=expression1, ~parentIsArrow=false)),
    ])

  | PExpFor(optexpression1, optexpression2, optexpression3, expression4) =>
    //  print_endline("PExpFor");
    Doc.concat([
      Doc.text("for "),
      addParens(
        Doc.concat([
          switch (optexpression1) {
          | Some(expr) => print_expression(~expr, ~parentIsArrow=false)
          | None => Doc.nil
          },
          Doc.text(";"),
          switch (optexpression2) {
          | Some(expr) =>
            Doc.concat([
              Doc.space,
              print_expression(~expr, ~parentIsArrow=false),
            ])
          | None => Doc.nil
          },
          Doc.text(";"),
          switch (optexpression3) {
          | Some(expr) =>
            Doc.concat([
              Doc.space,
              print_expression(~expr, ~parentIsArrow=false),
            ])
          | None => Doc.nil
          },
        ]),
      ),
      Doc.space,
      print_expression(~expr=expression4, ~parentIsArrow=false),
    ])
  | PExpContinue =>
    //print_endline("PExpContinue");
    Doc.group(Doc.concat([Doc.text("continue"), Doc.hardLine]))
  | PExpBreak =>
    //print_endline("PExpBreak");
    Doc.group(Doc.concat([Doc.text("break"), Doc.hardLine]))
  | PExpConstraint(expression, parsed_type) =>
    //print_endline("PExpConstraint");
    Doc.group(
      Doc.concat([
        print_expression(~expr=expression, ~parentIsArrow=false),
        Doc.text(": "),
        print_type(parsed_type),
      ]),
    )
  | PExpLambda(patterns, expression) =>
    // print_endline("PExpLambda");
    Doc.group(
      Doc.concat([
        if (List.length(patterns) == 0) {
          Doc.text("()");
        } else if (List.length(patterns) > 1) {
          addParens(
            Doc.concat([
              Doc.join(
                Doc.concat([Doc.text(","), Doc.line]),
                List.map(p => print_pattern(p), patterns),
              ),
            ]),
          );
        } else {
          let pat = List.hd(patterns);

          switch (pat.ppat_desc) {
          | PPatConstraint(_) =>
            Doc.concat([Doc.lparen, print_pattern(pat), Doc.rparen])
          | _ => print_pattern(pat)
          };
        },
        Doc.space,
        Doc.text("=>"),
        Doc.space,
        print_expression(~expr=expression, ~parentIsArrow=false),
        //Doc.hardLine,
      ]),
    )

  | PExpApp(func, expressions) =>
    // print_endline("PExpApp");
    print_application(func, expressions)
  | PExpBlock(expressions) =>
    // print_endline("PExpBlock");

    let block =
      Doc.join(
        Doc.hardLine,
        List.map(
          e => Doc.group(print_expression(~expr=e, ~parentIsArrow=false)),
          expressions,
        ),
      );

    Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat([
        Doc.lbrace,
        Doc.indent(Doc.concat([Doc.line, block])),
        Doc.line,
        Doc.rbrace,
      ]),
    );

  | PExpBoxAssign(expression, expression1) =>
    //  print_endline("PExpBoxAssign");
    Doc.concat([
      print_expression(~expr=expression, ~parentIsArrow=false),
      Doc.text(" := "),
      print_expression(~expr=expression1, ~parentIsArrow=false),
    ])
  | PExpAssign(expression, expression1) =>
    switch (expression1.pexp_desc) {
    | PExpApp(func, expressions) =>
      let functionNameDoc =
        print_expression(~expr=func, ~parentIsArrow=false);
      let op = Doc.toString(~width=100, functionNameDoc);
      let trimmedOperator = String.trim(op);
      //let desugared = print_expression(expression1);

      let left = print_expression(~expr=expression, ~parentIsArrow=false);

      let first =
        print_expression(~expr=List.hd(expressions), ~parentIsArrow=false);

      if (left == first) {
        //print_endline("Possible sugar as name is the same");
        // +=, -=, *=, /=, and %=
        switch (trimmedOperator) {
        | "+"
        | "-"
        | "*"
        | "/"
        | "%" =>
          let sugaredOp = Doc.text(" " ++ trimmedOperator ++ "= ");

          Doc.concat([
            print_expression(~expr=expression, ~parentIsArrow=false),
            sugaredOp,
            print_expression(
              ~expr=List.hd(List.tl(expressions)),
              ~parentIsArrow=false,
            ),
          ]);
        | _ =>
          Doc.concat([
            print_expression(~expr=expression, ~parentIsArrow=false),
            Doc.text(" = "),
            print_expression(~expr=expression1, ~parentIsArrow=false),
          ])
        };
      } else {
        Doc.concat([
          print_expression(~expr=expression, ~parentIsArrow=false),
          Doc.text(" = "),
          print_expression(~expr=expression1, ~parentIsArrow=false),
        ]);
      };

    | _ =>
      Doc.concat([
        print_expression(~expr=expression, ~parentIsArrow=false),
        Doc.text(" = "),
        print_expression(~expr=expression1, ~parentIsArrow=false),
      ])
    }

  | /** Used for modules without body expressions */ PExpNull =>
    //   print_endline("PExpNull");
    Doc.text("PExpNull")
  };
}
and value_bind_print =
    (
      export_flag: Asttypes.export_flag,
      rec_flag,
      mut_flag,
      vbs: list(Parsetree.value_binding),
    ) => {
  let exported =
    switch (export_flag) {
    | Nonexported => Doc.nil
    | Exported => Doc.text("export ")
    };
  let recursive =
    switch (rec_flag) {
    | Nonrecursive => Doc.nil
    | Recursive => Doc.text("rec ")
    };
  let mutble =
    switch (mut_flag) {
    | Immutable => Doc.nil
    | Mutable => Doc.text("mut ")
    };
  let vb = List.hd(vbs);
  Doc.group(
    Doc.concat([
      exported,
      Doc.text("let "),
      recursive,
      mutble,
      print_pattern(vb.pvb_pat),
      Doc.text(" = "),
      print_expression(~expr=vb.pvb_expr, ~parentIsArrow=false),
    ]),
  );
};
let rec print_data = (d: Grain_parsing__Parsetree.data_declaration) => {
  let nameloc = d.pdata_name;
  switch (d.pdata_kind) {
  | PDataVariant(constr_declarations) =>
    Doc.group(
      Doc.concat([
        Doc.text("enum"),
        Doc.space,
        Doc.text(nameloc.txt),
        if (List.length(d.pdata_params) > 0) {
          Doc.concat([
            Doc.text("<"),
            Doc.join(
              Doc.text(", "),
              List.map(t => print_type(t), d.pdata_params),
            ),
            Doc.text(">"),
            Doc.space,
          ]);
        } else {
          Doc.space;
        },
        Doc.lbrace,
        Doc.indent(
          Doc.concat([
            Doc.line,
            Doc.join(
              Doc.concat([Doc.comma, Doc.line]),
              List.map(
                (d: Grain_parsing__Parsetree.constructor_declaration) =>
                  Doc.group(
                    Doc.concat([
                      Doc.text(d.pcd_name.txt),
                      switch (d.pcd_args) {
                      | PConstrTuple(parsed_types) =>
                        if (List.length(parsed_types) > 0) {
                          Doc.concat([
                            Doc.text("("),
                            Doc.join(
                              Doc.concat([Doc.comma, Doc.line]),
                              List.map(t => print_type(t), parsed_types),
                            ),
                            Doc.text(")"),
                          ]);
                        } else {
                          Doc.nil;
                        }
                      | PConstrSingleton => Doc.nil
                      },
                    ]),
                  ),
                constr_declarations,
              ),
            ),
          ]),
        ),
        Doc.ifBreaks(Doc.text(","), Doc.nil),
        Doc.line,
        Doc.rbrace,
      ]),
    )

  | PDataRecord(label_declarations) =>
    Doc.group(
      Doc.concat([
        Doc.text("record"),
        Doc.space,
        Doc.text(nameloc.txt),
        Doc.space,
        if (List.length(d.pdata_params) > 0) {
          Doc.concat([
            Doc.text("<"),
            Doc.join(
              Doc.text(","),
              List.map(t => print_type(t), d.pdata_params),
            ),
            Doc.text(">"),
            Doc.space,
          ]);
        } else {
          Doc.nil;
        },
        Doc.concat([
          Doc.lbrace,
          Doc.indent(
            Doc.concat([
              Doc.line,
              Doc.join(
                Doc.concat([Doc.comma, Doc.line]),
                List.map(
                  (decl: Grain_parsing__Parsetree.label_declaration) => {
                    let isMutable =
                      switch (decl.pld_mutable) {
                      | Mutable => Doc.text("mut ")
                      | Immutable => Doc.nil
                      };
                    Doc.group(
                      Doc.concat([
                        isMutable,
                        print_ident(decl.pld_name.txt),
                        Doc.text(":"),
                        Doc.space,
                        print_type(decl.pld_type),
                      ]),
                    );
                  },
                  label_declarations,
                ),
              ),
            ]),
          ),
          Doc.line,
          Doc.rbrace,
        ]),
      ]),
    )
  };
};
let data_print =
    (
      datas:
        list(
          (
            Grain_parsing__Parsetree.export_flag,
            Grain_parsing__Parsetree.data_declaration,
          ),
        ),
    ) => {
  Doc.join(
    Doc.text(","),
    List.map(
      data => {
        let (expt, decl) = data;
        Doc.concat([
          switch ((expt: Asttypes.export_flag)) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          },
          print_data(decl),
        ]);
      },
      datas,
    ),
  );
};
let import_print = (imp: Parsetree.import_declaration) => {
  let vals =
    List.map(
      (v: Parsetree.import_value) => {
        switch (v) {
        | PImportModule(identloc) => print_ident(identloc.txt)
        | PImportAllExcept(identlocs) =>
          Doc.concat([
            Doc.text("*"),
            if (List.length(identlocs) > 0) {
              Doc.concat([
                Doc.text(" except "),
                addBraces(
                  Doc.join(
                    Doc.comma,
                    List.map(
                      (identloc: Location.loc(Grain_parsing__Identifier.t)) =>
                        print_ident(identloc.txt),
                      identlocs,
                    ),
                  ),
                ),
              ]);
            } else {
              Doc.nil;
            },
          ])
        | PImportValues(identlocsopts) =>
          Doc.concat([
            Doc.lbrace,
            Doc.indent(
              Doc.concat([
                Doc.line,
                if (List.length(identlocsopts) > 0) {
                  Doc.join(
                    Doc.concat([Doc.comma, Doc.line]),
                    List.map(
                      (
                        identlocopt: (
                          Grain_parsing.Parsetree.loc(
                            Grain_parsing.Identifier.t,
                          ),
                          option(
                            Grain_parsing.Parsetree.loc(
                              Grain_parsing.Identifier.t,
                            ),
                          ),
                        ),
                      ) => {
                        let (loc, optloc) = identlocopt;
                        switch (optloc) {
                        | None => print_imported_ident(loc.txt)
                        | Some(alias) =>
                          Doc.concat([
                            print_imported_ident(loc.txt),
                            Doc.text(" as "),
                            print_imported_ident(alias.txt),
                          ])
                        };
                      },
                      identlocsopts,
                    ),
                  );
                } else {
                  Doc.nil;
                },
              ]),
            ),
            Doc.line,
            Doc.rbrace,
          ])
        }
      },
      imp.pimp_val,
    );

  let path = imp.pimp_path.txt;

  Doc.group(
    Doc.concat([
      Doc.text("import "),
      //   Doc.group(Doc.concat(vals)),
      Doc.join(Doc.concat([Doc.comma, Doc.space]), vals),
      Doc.text(" from "),
      Doc.doubleQuote,
      Doc.text(path),
      Doc.doubleQuote,
    ]),
  );
};

let print_export_desc = (desc: Parsetree.export_declaration_desc) => {
  let ident = desc.pex_name.txt;

  let fixedIdent =
    if (infixop(ident)) {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };
  Doc.concat([
    fixedIdent,
    switch (desc.pex_alias) {
    | Some(alias) => Doc.concat([Doc.text(" as "), Doc.text(alias.txt)])
    | None => Doc.nil
    },
  ]);
};

let print_export_declaration = (decl: Parsetree.export_declaration) => {
  switch (decl) {
  | ExportData(data) => print_export_desc(data)
  | ExportValue(value) => print_export_desc(value)
  };
};

//export foreign wasm debug: a -> Void from "console"
let print_foreign_value_description = (vd: Parsetree.value_description) => {
  let ident = vd.pval_name.txt;

  let fixedIdent =
    if (infixop(ident) || ident == "!") {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };

  Doc.concat([
    fixedIdent,
    Doc.text(" : "),
    print_type(vd.pval_type),
    Doc.text(" from "),
    Doc.text("\""),
    Doc.text(vd.pval_mod.txt),
    Doc.text("\""),
  ]);
};

let print_primitive_value_description = (vd: Parsetree.value_description) => {
  let ident = vd.pval_name.txt;

  let fixedIdent =
    if (infixop(ident) || ident == "!") {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };

  Doc.concat([
    fixedIdent,
    Doc.text(" : "),
    print_type(vd.pval_type),
    Doc.text(" = "),
    Doc.text("\""),
    //   Doc.text(vd.pval_mod.txt),
    Doc.join(Doc.text(","), List.map(p => Doc.text(p), vd.pval_prim)),
    Doc.text("\""),
  ]);
};

let reformat_ast = (parsed_program: Parsetree.parsed_program) => {
  let toplevel_print = (data: Parsetree.toplevel_stmt) => {
    // let (file, startline, startchar, sbol) =
    //   get_raw_pos_info(data.ptop_loc.loc_start);

    // let comments = []; //find_comment(startline, parsed_program.comments);

    let attributes = data.ptop_attributes;

    let withoutComments =
      switch (data.ptop_desc) {
      | PTopImport(import_declaration) =>
        //   print_endline("PTopImport");
        import_print(import_declaration)
      | PTopForeign(export_flag, value_description) =>
        // print_endline("PTopForeign");

        let export =
          switch (export_flag) {
          | Nonexported => Doc.text("import ")
          | Exported => Doc.text("export ")
          };
        Doc.concat([
          export,
          Doc.text("foreign "),
          print_foreign_value_description(value_description),
        ]);
      | PTopPrimitive(export_flag, value_description) =>
        //  print_endline("PTopPrimitive");
        let export =
          switch (export_flag) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          };
        Doc.concat([
          export,
          Doc.text("primitive "),
          print_primitive_value_description(value_description),
        ]);
      | PTopData(data_declarations) =>
        //print_endline("PTopData");
        data_print(data_declarations)
      | PTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
        // print_endline("PTopLet");
        value_bind_print(export_flag, rec_flag, mut_flag, value_bindings)
      | PTopExpr(expression) =>
        // print_endline("PTopExpr");
        print_expression(~expr=expression, ~parentIsArrow=false)
      | PTopException(export_flag, type_exception) =>
        // print_endline("PTopException");
        let export =
          switch (export_flag) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          };
        let cstr = type_exception.ptyexn_constructor;

        let kind =
          switch (cstr.pext_kind) {
          | PExtDecl(sargs) =>
            switch (sargs) {
            | PConstrSingleton => Doc.nil
            | PConstrTuple(parsed_types) =>
              if (List.length(parsed_types) > 0) {
                Doc.concat([
                  Doc.text("("),
                  Doc.join(
                    Doc.comma,
                    List.map(t => print_type(t), parsed_types),
                  ),
                  Doc.text(")"),
                ]);
              } else {
                Doc.nil;
              }
            }

          | PExtRebind(lid) => print_ident(lid.txt)
          };

        Doc.concat([
          export,
          Doc.text("exception "),
          Doc.text(cstr.pext_name.txt),
          kind,
        ]);
      | PTopExport(export_declarations) =>
        //  print_endline("PTopExport");
        Doc.group(
          Doc.concat([
            Doc.text("export "),
            Doc.join(
              Doc.concat([Doc.comma, Doc.line]),
              List.map(
                e => print_export_declaration(e),
                export_declarations,
              ),
            ),
          ]),
        )
      | PTopExportAll(export_excepts) =>
        //  print_endline("PTopExportAll");
        Doc.concat([
          Doc.text("export * "),
          if (List.length(export_excepts) > 0) {
            Doc.concat([
              Doc.text("except "),
              Doc.join(
                Doc.comma,
                List.map(
                  (excpt: Parsetree.export_except) =>
                    switch (excpt) {
                    | ExportExceptData(data) => Doc.text(data.txt)
                    | ExportExceptValue(value) => Doc.text(value.txt)
                    },
                  export_excepts,
                ),
              ),
            ]);
          } else {
            Doc.nil;
          },
        ])
      };

    let attributeText =
      if (List.length(attributes) > 0) {
        Doc.concat([
          Doc.join(
            Doc.space,
            List.map(
              (a: Location.loc(string)) =>
                Doc.concat([Doc.text("@"), Doc.text(a.txt)]),
              attributes,
            ),
          ),
          Doc.hardLine,
        ]);
      } else {
        Doc.nil;
      };

    let commentText = Doc.nil;
    // if (List.length(comments) > 0) {
    //   let comment = List.hd(comments);

    //   switch (comment) {
    //   | Line(cmt) => Doc.concat([Doc.space, Doc.text(cmt.cmt_source)])
    //   | _ => Doc.nil
    //   };
    // } else {
    //   Doc.nil;
    // };

    Doc.concat([attributeText, withoutComments, commentText]);
  };

  let commentToDoc = (comment: Parsetree.comment) =>
    switch (comment) {
    | Line(cmt) => Doc.text(cmt.cmt_source)
    | Block(cmt) => Doc.concat([Doc.text(cmt.cmt_source), Doc.hardLine])
    | Doc(cmt) => Doc.concat([Doc.text(cmt.cmt_source), Doc.hardLine])
    | Shebang(cmt) => Doc.text(cmt.cmt_source)
    };

  let getCommentEndLine = (comment: Parsetree.comment) => {
    let endloc =
      switch (comment) {
      | Line(cmt) => cmt.cmt_loc.loc_end
      | Block(cmt) => cmt.cmt_loc.loc_end
      | Doc(cmt) => cmt.cmt_loc.loc_end
      | Shebang(cmt) => cmt.cmt_loc.loc_end
      };

    let (_, endline, _, _) = get_raw_pos_info(endloc);
    endline;
  };

  let previousStatement: ref(option(Grain_parsing.Parsetree.toplevel_stmt)) =
    ref(None);

  let printedDoc =
    Doc.join(
      Doc.hardLine,
      List.map(
        (stmt: Grain_parsing.Parsetree.toplevel_stmt) => {
          // let's see if we had any comments above us

          let commentsAbove =
            switch (previousStatement^) {
            | None =>
              let (_file, stmtline, _startchar, _sbol) =
                get_raw_pos_info(stmt.ptop_loc.loc_start);

              find_comments_in_range(
                1,
                stmtline - 1,
                parsed_program.comments,
              );

            | Some(prevStmt) =>
              let (_file, stmtline, _startchar, _sbol) =
                get_raw_pos_info(stmt.ptop_loc.loc_start);

              let (_pfile, prevline, _pstartchar, _psbol) =
                get_raw_pos_info(prevStmt.ptop_loc.loc_end);

              find_comments_in_range(
                prevline,
                stmtline - 1,
                parsed_program.comments,
              );
            };

          let commentDocs =
            Doc.concat(
              List.map(
                (c: Parsetree.comment) => commentToDoc(c),
                commentsAbove,
              ),
            );

          // we may also have blank lines, we want to reduce them to one

          let (_, stmtstart, _, _) =
            get_raw_pos_info(stmt.ptop_loc.loc_start);

          //  print_endline("Statement line is " ++ string_of_int(stmtstart));

          let lastCommentLine =
            if (List.length(commentsAbove) > 0) {
              Some(List.hd(List.rev(commentsAbove)));
            } else {
              None;
            };

          let lastLine =
            switch (previousStatement^) {
            | None =>
              switch (lastCommentLine) {
              | None => 1
              | Some(cmt) => getCommentEndLine(cmt)
              }
            | Some(s) =>
              switch (lastCommentLine) {
              | None =>
                let (_, prevline, _, _) =
                  get_raw_pos_info(s.ptop_loc.loc_end);

                prevline;

              | Some(cmt) => getCommentEndLine(cmt)
              }
            };

          //  print_endline("stmtstart " ++ string_of_int(stmtstart));
          //  print_endline("lastLine " ++ string_of_int(lastLine));

          let blankLineAbove =
            if (stmtstart - lastLine > 1) {
              // print_endline("Add a blank line");
              Doc.hardLine;
            } else {
              Doc.nil;
            };

          previousStatement := Some(stmt);
          Doc.concat([
            commentDocs,
            blankLineAbove,
            Doc.group(toplevel_print(stmt)),
          ]);
        },
        parsed_program.statements,
      ),
    );

  //Doc.debug(printedDoc);
  //
  Doc.toString(~width=80, printedDoc) |> print_endline;
  // print_endline(
  //   Yojson.Basic.pretty_to_string(
  //     Yojson.Safe.to_basic(
  //       Grain_parsing__Parsetree.parsed_program_to_yojson(parsed_program),
  //     ),
  //   ),
  // );
  // let a = (1 * 2 + 3) * 4;
  // let b = 1 * 2 + 3 * 4;
  // ();
};

// reallylongdfuncrionNamereallylongdfuncrionName(e => {
//   Grain_parsing__Parsetree.parsed_program_to_yojson(parsed_program)
// });

// let rec insert = (value, index, list) => {
//   if (index < 0) {
//    true
//   } else {
//     switch(list) {
//      | [] => if (index == 0) true else false ,
//      |[first, ...rest] => if (index == 0) [value, ...list] else [first, ...insert(value, index - 1, rest)]
//     }
//   }
// }

// type c =
//   | SomethingLong
//   | SomethiingLnger;

// let d = SomethiingLnger;

// switch (d) {
// | SomethiingLnger =>
//   if (c == SomethingLong) {
//     let a = 1 + 3 + 4;
//     ();
//   } else {
//     let c = 5 + 6 + 7 + 7 + 9;
//     ();
//   }
// | _ => ()
// };

let num = 1 + 2 + (3 + 4 + (4 + 5));
