open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;

module Doc = Res_doc;

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
let rec remove_cons = (expression: Parsetree.expression) => {
  print_expression(expression);
}
and resugar_list = (expressions: list(Parsetree.expression)) => {
  print_endline("resugar_list");
  let second: Doc.t = print_expression(List.hd(List.tl(expressions)));
  let first = print_expression(List.hd(expressions));

  print_endline("first:" ++ Doc.toString(~width=100, first));
  print_endline("second:" ++ Doc.toString(~width=100, second));

  switch (second) {
  | Text(txt) =>
    if (txt == "[]") {
      print_endline("start with empty");
      Doc.concat([Doc.lbracket, Doc.group(first), Doc.rbracket]);
    } else {
      Doc.concat([
        Doc.lbracket,
        Doc.group(first),
        Doc.comma,
        Doc.space,
        Doc.group(second),
        Doc.rbracket,
      ]);
    }
  | Concat(docs) =>
    let noBrack = Doc.concat(List.tl(docs));
    Doc.concat([
      Doc.lbracket,
      Doc.group(first),
      Doc.comma,
      Doc.space,
      noBrack,
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
  Doc.group(
    Doc.concat([
      Doc.indent(
        Doc.group(
          Doc.concat([
            Doc.lbrace,
            Doc.space,
            Doc.indent(
              Doc.join(
                Doc.concat([Doc.comma, Doc.space]),
                List.map(
                  (
                    patternloc: (
                      Grain_parsing__Location.loc(
                        Grain_parsing__Identifier.t,
                      ),
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
            ),
            Doc.space,
            Doc.rbrace,
          ]),
        ),
      ),
    ]),
  );
}
and print_pattern = (pat: Parsetree.pattern) => {
  switch (pat.ppat_desc) {
  | PPatAny => Doc.text("_")
  | PPatConstant(c) => print_constant(c)
  | PPatVar({txt, _}) => Doc.text(txt)
  | PPatTuple(patterns) =>
    Doc.group(
      Doc.concat([
        Doc.lparen,
        Doc.join(Doc.comma, List.map(p => print_pattern(p), patterns)),
        Doc.rparen,
      ]),
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
    print_endline("PPatRecord");
    print_record_pattern(patternlocs, closedflag);
  | PPatConstraint(pattern, parsed_type) =>
    Doc.concat([
      print_pattern(pattern),
      Doc.concat([Doc.text(":"), Doc.space]),
      print_type(parsed_type),
    ])
  | PPatConstruct(location, patterns) =>
    print_endline("PPatConstruct");
    Doc.concat([
      print_ident(location.txt),
      if (List.length(patterns) > 0) {
        Doc.group(
          Doc.concat([
            Doc.lparen,
            Doc.join(
              Doc.comma,
              List.map(pat => print_pattern(pat), patterns),
            ),
            Doc.rparen,
          ]),
        );
      } else {
        Doc.nil;
      },
    ]);
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
    | PConstFloat64(f)
    | PConstFloat32(f) => Printf.sprintf("%s", f)
    | PConstInt32(i) => Printf.sprintf("%s", i)
    | PConstInt64(i) => Printf.sprintf("%s", i)
    | PConstWasmI32(i) => Printf.sprintf("%s", i)
    | PConstWasmI64(i) => Printf.sprintf("%s", i)
    | PConstWasmF32(f) => Printf.sprintf("%s", f)
    | PConstWasmF64(f) => Printf.sprintf("%s", f)
    | PConstBool(true) => "true"
    | PConstBool(false) => "false"
    | PConstVoid => "void"
    | PConstString(s) => Printf.sprintf("\"%s\"", s)
    };
  print_endline(print_c);
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
and print_record =
    (
      fields:
        list(
          (
            Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
            Grain_parsing__Parsetree.expression,
          ),
        ),
    ) => {
  Doc.group(
    Doc.concat([
      Doc.group(Doc.indent(Doc.line)),
      Doc.indent(
        Doc.group(
          Doc.concat([
            Doc.lbrace,
            Doc.indent(
              Doc.join(
                Doc.concat([Doc.comma, Doc.softLine]),
                List.map(
                  (
                    field: (
                      Grain_parsing__Location.loc(
                        Grain_parsing__Identifier.t,
                      ),
                      Grain_parsing__Parsetree.expression,
                    ),
                  ) => {
                    let (locidentifier, expr) = field;
                    let ident = locidentifier.txt;
                    Doc.group(
                      Doc.concat([
                        Doc.line,
                        print_ident(ident),
                        Doc.text(": "),
                        print_expression(expr),
                      ]),
                    );
                  },
                  fields,
                ),
              ),
            ),
            //ifBreak(","),
            Doc.rbrace,
          ]),
        ),
      ),
    ]),
  );
}
and print_type = (p: Grain_parsing__Parsetree.parsed_type) => {
  switch (p.ptyp_desc) {
  | PTyAny => Doc.text("AnyType")
  | PTyVar(name) => Doc.text(name)
  | PTyArrow(types, parsed_type) => Doc.text("PTyArrow")
  | PTyTuple(parsed_types) => Doc.text("PTyTuple")
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
  print_endline("functioname is " ++ functionName);
  if (infixop(functionName)) {
    Doc.group(
      Doc.concat([
        print_expression(List.hd(expressions)),
        Doc.space,
        print_expression(func),
        Doc.space,
        print_expression(List.hd(List.tl(expressions))),
      ]),
    );
  } else {
    let funcName = print_expression(func);
    if (Doc.toString(~width=20, funcName) == "[...]") {
      resugar_list(expressions);
    } else {
      Doc.group(
        Doc.concat([
          funcName,
          Doc.lparen,
          Doc.join(
            Doc.concat([Doc.softLine, Doc.text(",")]),
            List.map(e => print_expression(e), expressions),
          ),
          Doc.rparen,
        ]),
      );
    };
  };
}
and print_expression = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpConstant(x) =>
    print_endline("PExpConstant");
    print_constant(x);
  | PExpId({txt: id}) =>
    print_endline("PExpId");
    print_ident(id);
  | PExpLet(rec_flag, mut_flag, vbs) =>
    print_endline("PExpLet");
    value_bind_print(Asttypes.Nonexported, rec_flag, mut_flag, vbs);
  | PExpTuple(expressions) =>
    print_endline("PExpTuple");
    Doc.group(
      Doc.concat([
        Doc.lparen,
        Doc.join(
          Doc.comma,
          List.map(e => print_expression(e), expressions),
        ),
        Doc.rparen,
      ]),
    );
  | PExpArray(expressions) =>
    print_endline("PExpArray");
    Doc.group(
      Doc.concat([
        Doc.lbracket,
        Doc.join(
          Doc.comma,
          List.map(e => print_expression(e), expressions),
        ),
        Doc.rbracket,
      ]),
    );
  | PExpArrayGet(expression1, expression2) =>
    print_endline("PExpArrayGet");
    Doc.text("PExpArrayGet");
  | PExpArraySet(expression1, expression2, expression3) =>
    print_endline("PExpArraySet");
    Doc.text("PExpArraySet");
  | PExpRecord(record) =>
    print_endline("PExpRecord");
    print_record(record);
  | PExpRecordGet(expression, identloc) =>
    print_endline("PExpRecordGet");
    Doc.text("PExpRecordGet");
  | PExpRecordSet(expression, identloc, expression2) =>
    print_endline("PExpRecordSet");
    Doc.text("PExpRecordSet");
  | PExpMatch(expression, match_branches) =>
    print_endline("PExpMatch");
    Doc.group(
      Doc.concat([
        Doc.text("match"),
        Doc.space,
        Doc.lparen,
        Doc.indent(Doc.concat([print_expression(expression)])),
        Doc.rparen,
        Doc.group(Doc.indent(Doc.line)),
        Doc.indent(
          Doc.group(
            Doc.concat([
              Doc.lbrace,
              Doc.indent(
                Doc.join(
                  Doc.comma,
                  List.map(
                    (branch: Parsetree.match_branch) => {
                      Doc.concat([
                        Doc.hardLine,
                        print_pattern(branch.pmb_pat),
                        Doc.text(" => "),
                        print_expression(branch.pmb_body),
                      ])
                    },
                    match_branches,
                  ),
                ),
              ),
              Doc.line,
              Doc.rbrace,
            ]),
          ),
        ),
      ]),
    );
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
    print_endline("PExpIf");
    Doc.group(
      Doc.concat([
        Doc.text("if "),
        Doc.text("("),
        print_expression(condition),
        Doc.text(")"),
        print_expression(trueExpr),
        Doc.text(" else "),
        print_expression(falseExpr),
      ]),
    );
  | PExpWhile(expression, expression1) =>
    print_endline("PExpWhile");
    Doc.text("PExpWhile");
  | PExpFor(optexpression1, optexpression2, optexpression3, expression4) =>
    print_endline("PExpFor");
    Doc.text("PExpFor");
  | PExpContinue =>
    print_endline("PExpContinue");
    Doc.group(Doc.concat([Doc.text("continue"), Doc.hardLine]));
  | PExpBreak =>
    print_endline("PExpBreak");
    Doc.group(Doc.concat([Doc.text("break"), Doc.hardLine]));
  | PExpConstraint(expression, parsed_type) =>
    print_endline("PExpConstraint");
    Doc.group(
      Doc.concat([
        print_expression(expression),
        Doc.text(": "),
        print_type(parsed_type),
      ]),
    );
  | PExpLambda(patterns, expression) =>
    print_endline("PExpLambda");
    Doc.group(
      Doc.concat([
        Doc.lparen,
        Doc.indent(
          Doc.group(
            Doc.concat([
              Doc.softLine,
              Doc.join(
                Doc.concat([Doc.softLine, Doc.text(",")]),
                List.map(p => print_pattern(p), patterns),
              ),
            ]),
          ),
        ),
        Doc.rparen,
        Doc.space,
        Doc.text("=>"),
        Doc.space,
        print_expression(expression),
        Doc.hardLine,
      ]),
    );
  | PExpApp(func, expressions) =>
    print_endline("PExpApp");
    print_application(func, expressions);
  | PExpBlock(expressions) =>
    print_endline("PExpBlock");
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
    );
  | PExpBoxAssign(expression, expression1) =>
    print_endline("PExpBoxAssign");
    Doc.text("PExpBoxAssign");
  | PExpAssign(expression, expression1) =>
    print_endline("PExpAssign");

    let desugared = print_expression(expression1);

    let sugarMutableNumbers = (docs: list(Res_doc.t)) =>
      // +=, -=, *=, /=, and %=
      // operator should be second
      if (List.length(docs) > 2) {
        let op = List.nth(docs, 1);

        let sugaredOp =
          switch (op) {
          | Text(operator) =>
            let trimmedOperator = String.trim(operator);
            switch (trimmedOperator) {
            | "+"
            | "-"
            | "*"
            | "/"
            | "%" => Doc.text(trimmedOperator ++ "= ")
            | _ => op
            };

          | _ => op
          };

        let start = Doc.concat([List.nth(docs, 0), sugaredOp]);
        let tail = Doc.concat(List.map(d => d, List.tl(List.tl(docs))));
        Doc.concat([start, tail]);
      } else {
        Doc.concat(docs);
      };

    let sugared =
      switch (desugared) {
      | Group(grp) =>
        switch (grp.doc) {
        | Concat(docs) => sugarMutableNumbers(docs)
        | _ => desugared
        }
      | Concat(docs) => sugarMutableNumbers(docs)
      | _ => desugared
      };

    sugared;

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
        Doc.space,
        Doc.group(
          Doc.indent(
            Doc.group(
              Doc.concat([
                Doc.lbrace,
                Doc.indent(
                  Doc.join(
                    Doc.concat([Doc.line, Doc.comma]),
                    List.map(
                      (d: Grain_parsing__Parsetree.constructor_declaration) =>
                        Doc.text(d.pcd_name.txt),
                      constr_declarations,
                    ),
                  ),
                ),
                Doc.rbrace,
              ]),
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
        Doc.indent(
          Doc.group(
            Doc.concat([
              Doc.lbrace,
              Doc.indent(
                Doc.join(
                  Doc.concat([Doc.line, Doc.comma]),
                  List.map(
                    (decl: Grain_parsing__Parsetree.label_declaration) => {
                      Doc.group(
                        Doc.concat([
                          Doc.line,
                          print_ident(decl.pld_name.txt),
                          Doc.text(":"),
                          print_type(decl.pld_type),
                        ]),
                      )
                    },
                    label_declarations,
                  ),
                ),
              ),
              Doc.rbrace,
            ]),
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
  Doc.concat(
    List.map(
      data => {
        let (expt, decl) = data;
        print_data(decl);
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
                Doc.text(" except "), /// need to find the syntax for this
                Doc.join(
                  Doc.comma,
                  List.map(
                    (identloc: Location.loc(Grain_parsing__Identifier.t)) =>
                      print_ident(identloc.txt),
                    identlocs,
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
                    print_ident(loc.txt);
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
let reformat_ast = (parsed_program: Parsetree.parsed_program) => {
  let toplevel_print = (data: Grain_parsing__Parsetree.toplevel_stmt) => {
    switch (data.ptop_desc) {
    | PTopImport(import_declaration) =>
      print_endline("PTopImport");
      import_print(import_declaration);
    | PTopForeign(export_flag, value_description) =>
      print_endline("PTopForeign");
      Doc.nil;
    | PTopPrimitive(export_flag, value_description) =>
      print_endline("PTopPrimitive");
      Doc.nil;
    | PTopData(data_declarations) =>
      print_endline("PTopData");
      data_print(data_declarations);
    | PTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
      print_endline("PTopLet");
      value_bind_print(export_flag, rec_flag, mut_flag, value_bindings);
    | PTopExpr(expression) =>
      print_endline("PTopExpr");
      print_expression(expression);
    | PTopException(export_flag, type_exception) =>
      print_endline("PTopException");
      Doc.nil;
    | PTopExport(export_declarations) =>
      print_endline("PTopExport");
      Doc.nil;
    | PTopExportAll(export_excepts) =>
      print_endline("PTopExportAll");
      Doc.nil;
    };
  };
  let printedDoc =
    Doc.join(
      Doc.hardLine,
      List.map(stmt => {toplevel_print(stmt)}, parsed_program.statements),
    );
  Doc.toString(~width=80, printedDoc) |> print_endline;
  // print_endline(
  //   Yojson.Basic.pretty_to_string(
  //     Yojson.Safe.to_basic(
  //       Grain_parsing__Parsetree.parsed_program_to_yojson(parsed_program),
  //     ),
  //   ),
  // );
};
