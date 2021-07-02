open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;

module Doc = Res_doc;

type stmtList =
  | BlankLine
  | BlockComment(string)
  | Statement(Parsetree.toplevel_stmt);

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

let rec remove_cons = (expression: Parsetree.expression) => {
  print_expression(expression);
}

and build_sugar_docs = (first: Doc.t, second: Doc.t) => {
  // print_endline("first:" ++ Doc.toString(~width=100, first));
  // print_endline("second:" ++ Doc.toString(~width=100, second));
  switch (second) {
  | Text(txt) =>
    if (txt == "[]") {
      Doc.concat([Doc.lbracket, Doc.group(first), Doc.rbracket]);
    } else {
      Doc.concat([
        Doc.lbracket,
        Doc.group(first),
        Doc.comma,
        Doc.space,
        Doc.group(Doc.concat([Doc.text("..."), second])),
        Doc.rbracket,
      ]);
    }
  | Concat(docs) =>
    //let noBrack = Doc.concat(List.tl(docs));
    let noBrack = Doc.concat([Doc.text("..."), Doc.concat(docs)]);
    Doc.concat([
      Doc.lbracket,
      Doc.group(first),
      Doc.comma,
      Doc.space,
      noBrack,
      Doc.rbracket,
    ]);
  | _ =>
    // Should never end up here
    Doc.concat([
      Doc.lbracket,
      Doc.group(first),
      Doc.comma,
      Doc.space,
      Doc.group(second),
      Doc.rbracket,
    ])
  };
}

and resugar_list_patterns = (patterns: list(Parsetree.pattern)) => {
  let second: Doc.t = print_pattern(List.hd(List.tl(patterns)));
  let first = print_pattern(List.hd(patterns));
  build_sugar_docs(first, second);
}
and resugar_list = (expressions: list(Parsetree.expression)) => {
  let second: Doc.t = print_expression(List.hd(List.tl(expressions)));
  let first = print_expression(List.hd(expressions));

  build_sugar_docs(first, second);
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
  addBraces(
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
          Doc.concat([
            print_ident(loc.txt),
            Doc.text(": "),
            print_pattern(pat),
          ]);
        },
        patternlocs,
      ),
    ),
  );
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
      Doc.concat([Doc.text(" :"), Doc.space]),
      print_type(parsed_type),
    ])
  | PPatConstruct(location, patterns) =>
    //print_endline("PPatConstruct");
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
  addBraces(
    Doc.join(
      Doc.concat([Doc.comma, Doc.softLine]),
      List.map(
        (
          field: (
            Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
            Grain_parsing__Parsetree.expression,
          ),
        ) => {
          let (locidentifier, expr) = field;
          let ident = locidentifier.txt;
          Doc.group(
            Doc.concat([
              Doc.line,
              print_ident(ident),
              Doc.text(":"),
              print_expression(expr),
            ]),
          );
        },
        fields,
      ),
    ),
  )

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
          addParens(
            Doc.join(
              Doc.concat([Doc.comma, Doc.line]),
              List.map(t => print_type(t), types),
            ),
          );
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
  let functionNameDoc = print_expression(func);
  let functionName = Doc.toString(~width=100, functionNameDoc);
  // print_endline("functioname is " ++ functionName);

  if (infixop(functionName)) {
    let first = List.hd(expressions);
    let second = List.hd(List.tl(expressions)); // assumes an infix only has two expressions

    let firstBrackets =
      switch (first.pexp_desc) {
      | PExpApp(_) =>
        Doc.concat([Doc.lparen, print_expression(first), Doc.rparen])
      | _ => print_expression(first)
      };

    let secondBrackets =
      switch (second.pexp_desc) {
      | PExpApp(_) =>
        Doc.concat([Doc.lparen, print_expression(second), Doc.rparen])
      | _ => print_expression(second)
      };
    Doc.group(
      Doc.concat([
        firstBrackets,
        Doc.line,
        Doc.group(print_expression(func)),
        Doc.space,
        secondBrackets,
      ]),
    );
  } else {
    let funcName = print_expression(func);
    if (Doc.toString(~width=20, funcName) == "[...]") {
      resugar_list(expressions);
    } else if (Doc.toString(~width=20, funcName) == "throw") {
      Doc.concat([
        funcName,
        Doc.space,
        print_expression(List.hd(expressions)),
      ]);
    } else {
      Doc.concat([
        funcName,
        Doc.lparen,
        Doc.join(
          Doc.concat([Doc.text(", ")]),
          List.map(e => Doc.group(print_expression(e)), expressions),
        ),
        Doc.rparen,
      ]);
    };
  };
}
and print_expression = (expr: Parsetree.expression) => {
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
      Doc.join(Doc.comma, List.map(e => print_expression(e), expressions)),
    )

  | PExpArray(expressions) =>
    //print_endline("PExpArray");
    Doc.group(
      Doc.concat([
        Doc.lbracket,
        Doc.join(
          Doc.comma,
          List.map(e => print_expression(e), expressions),
        ),
        Doc.rbracket,
      ]),
    )
  | PExpArrayGet(expression1, expression2) =>
    // print_endline("PExpArrayGet");
    Doc.concat([
      print_expression(expression1),
      Doc.lbracket,
      print_expression(expression2),
      Doc.rbracket,
    ])
  | PExpArraySet(expression1, expression2, expression3) =>
    //print_endline("PExpArraySet");
    Doc.concat([
      print_expression(expression1),
      Doc.lbracket,
      print_expression(expression2),
      Doc.rbracket,
      Doc.text(" = "),
      print_expression(expression3),
    ])

  | PExpRecord(record) =>
    // print_endline("PExpRecord");
    print_record(record)
  | PExpRecordGet(expression, {txt, _}) =>
    // print_endline("PExpRecordGet");
    Doc.concat([print_expression(expression), Doc.dot, print_ident(txt)])
  | PExpRecordSet(expression, {txt, _}, expression2) =>
    //  print_endline("PExpRecordSet");
    Doc.concat([
      print_expression(expression),
      Doc.dot,
      print_ident(txt),
      Doc.text(" = "),
      print_expression(expression2),
    ])
  | PExpMatch(expression, match_branches) =>
    // print_endline("PExpMatch");
    Doc.group(
      Doc.concat([
        Doc.text("match"),
        Doc.space,
        addParens(print_expression(expression)),
        Doc.space,
        addBraces(
          Doc.join(
            Doc.concat([Doc.comma, Doc.hardLine]),
            List.map(
              (branch: Parsetree.match_branch) => {
                Doc.group(
                  Doc.concat([
                    print_pattern(branch.pmb_pat),
                    Doc.text(" => "),
                    Doc.indent(
                      Doc.concat([
                        Doc.softLine,
                        print_expression(branch.pmb_body),
                      ]),
                    ),
                  ]),
                )
              },
              match_branches,
            ),
          ),
        ),
      ]),
    )

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
    let falseClause =
      switch (falseExpr.pexp_desc) {
      | PExpBlock(expressions) =>
        if (List.length(expressions) > 0) {
          Doc.concat([Doc.text(" else "), print_expression(falseExpr)]);
        } else {
          Doc.nil;
        }
      | _ => Doc.concat([Doc.text(" else "), print_expression(falseExpr)])
      };
    Doc.group(
      Doc.concat([
        Doc.text("if "),
        Doc.text("("),
        print_expression(condition),
        Doc.text(")"),
        Doc.space,
        print_expression(trueExpr),
        falseClause,
      ]),
    );
  | PExpWhile(expression, expression1) =>
    Doc.concat([
      Doc.text("while "),
      addParens(print_expression(expression)),
      Doc.line,
      print_expression(expression1),
    ])

  | PExpFor(optexpression1, optexpression2, optexpression3, expression4) =>
    //  print_endline("PExpFor");
    Doc.concat([
      Doc.text("for "),
      addParens(
        Doc.concat([
          switch (optexpression1) {
          | Some(expr) => print_expression(expr)
          | None => Doc.nil
          },
          Doc.text(";"),
          switch (optexpression2) {
          | Some(expr) => Doc.concat([Doc.space, print_expression(expr)])
          | None => Doc.nil
          },
          Doc.text(";"),
          switch (optexpression3) {
          | Some(expr) => Doc.concat([Doc.space, print_expression(expr)])
          | None => Doc.nil
          },
        ]),
      ),
      Doc.line,
      print_expression(expression4),
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
        print_expression(expression),
        Doc.text(" : "),
        print_type(parsed_type),
      ]),
    )
  | PExpLambda(patterns, expression) =>
    // print_endline("PExpLambda");
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
        print_pattern(List.hd(patterns));
      },
      Doc.space,
      Doc.text("=>"),
      Doc.space,
      print_expression(expression),
      //Doc.hardLine,
    ])

  | PExpApp(func, expressions) =>
    // print_endline("PExpApp");
    print_application(func, expressions)
  | PExpBlock(expressions) =>
    // print_endline("PExpBlock");
    Doc.group(
      Doc.concat([
        Doc.text("{"),
        Doc.indent(
          Doc.group(
            Doc.concat([
              Doc.hardLine,
              Doc.group(
                Doc.join(
                  Doc.hardLine,
                  List.map(e => print_expression(e), expressions),
                ),
              ),
            ]),
          ),
        ),
        Doc.hardLine,
        Doc.text("}"),
      ]),
    )
  | PExpBoxAssign(expression, expression1) =>
    //  print_endline("PExpBoxAssign");
    Doc.concat([
      print_expression(expression),
      Doc.text(" := "),
      print_expression(expression1),
    ])
  | PExpAssign(expression, expression1) =>
    switch (expression1.pexp_desc) {
    | PExpApp(func, expressions) =>
      let functionNameDoc = print_expression(func);
      let op = Doc.toString(~width=100, functionNameDoc);
      let trimmedOperator = String.trim(op);
      //let desugared = print_expression(expression1);

      let left = print_expression(expression);
      let first = print_expression(List.hd(expressions));

      if (left == first) {
        print_endline("Possible sugar as name is the same");

        // +=, -=, *=, /=, and %=
        switch (trimmedOperator) {
        | "+"
        | "-"
        | "*"
        | "/"
        | "%" =>
          let sugaredOp = Doc.text(" " ++ trimmedOperator ++ "= ");

          Doc.concat([
            print_expression(expression),
            sugaredOp,
            print_expression(List.hd(List.tl(expressions))),
          ]);
        | _ =>
          Doc.concat([
            print_expression(expression),
            Doc.text(" = "),
            print_expression(expression1),
          ])
        };
      } else {
        Doc.concat([
          print_expression(expression),
          Doc.text(" = "),
          print_expression(expression1),
        ]);
      };

    | _ =>
      Doc.concat([
        print_expression(expression),
        Doc.text(" = "),
        print_expression(expression1),
      ])
    }

  | /** Used for modules without body expressions */ PExpNull =>
    print_endline("PExpNull");
    Doc.text("PExpNull");
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
      print_expression(vb.pvb_expr),
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
          Doc.nil;
        },
        addBraces(
          Doc.join(
            Doc.concat([Doc.comma, Doc.softLine]),
            List.map(
              (d: Grain_parsing__Parsetree.constructor_declaration) =>
                Doc.group(
                  Doc.concat([
                    Doc.line,
                    Doc.text(d.pcd_name.txt),
                    switch (d.pcd_args) {
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
                    | PConstrSingleton => Doc.nil
                    },
                  ]),
                ),
              constr_declarations,
            ),
          ),
        ),
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
        addBraces(
          Doc.join(
            Doc.concat([Doc.line, Doc.comma]),
            List.map(
              (decl: Grain_parsing__Parsetree.label_declaration) => {
                let isMutable =
                  switch (decl.pld_mutable) {
                  | Mutable => Doc.text("mut ")
                  | Immutable => Doc.nil
                  };
                Doc.group(
                  Doc.concat([
                    Doc.line,
                    isMutable,
                    print_ident(decl.pld_name.txt),
                    Doc.text(":"),
                    print_type(decl.pld_type),
                  ]),
                );
              },
              label_declarations,
            ),
          ),
        ),
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
            Doc.text("{ "),
            if (List.length(identlocsopts) > 0) {
              Doc.join(
                Doc.concat([Doc.comma, Doc.space]),
                List.map(
                  (
                    identlocopt: (
                      Grain_parsing.Parsetree.loc(Grain_parsing.Identifier.t),
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
            Doc.text(" }"),
          ])
        }
      },
      imp.pimp_val,
    );

  let path = imp.pimp_path.txt;

  Doc.group(
    Doc.concat([
      Doc.text("import "),
      Doc.group(Doc.concat(vals)),
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
        print_expression(expression)
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

          // print_endline("stmtstart " ++ string_of_int(stmtstart));
          // print_endline("lastLine " ++ string_of_int(lastLine));

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
