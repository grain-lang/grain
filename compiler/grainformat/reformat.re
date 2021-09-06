open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_diagnostics;

module Doc = Res_doc;

let exception_primitives = [|"throw", "fail", "assert"|];
let list_cons = "[...]";

type error =
  | Illegal_parse(string)
  | Unsupported_syntax(string)
  | FormatterError(string);

exception Error(error);

type sugared_list_item =
  | Regular(Grain_parsing.Parsetree.expression)
  | Spread(Grain_parsing.Parsetree.expression);

type sugared_pattern_item =
  | RegularPattern(Grain_parsing.Parsetree.pattern)
  | SpreadPattern(Grain_parsing.Parsetree.pattern);

let get_original_code =
    (location: Grain_parsing.Location.t, source: array(string)) => {
  let (_, startline, startc, _) =
    Locations.get_raw_pos_info(location.loc_start);
  let (_, endline, endc, _) = Locations.get_raw_pos_info(location.loc_end);

  let text = ref("");
  if (Array.length(source) > endline - 1) {
    if (startline == endline) {
      let full_line = source[startline - 1];

      let without_trailing = Str.string_before(full_line, endc);

      text := text^ ++ without_trailing;
    } else {
      for (line in startline - 1 to endline - 1) {
        if (line + 1 == startline) {
          text := text^ ++ Str.string_after(source[line], startc) ++ "\n";
        } else if (line + 1 == endline) {
          text := text^ ++ source[line];
        } else {
          text := text^ ++ source[line] ++ "\n";
        };
      };
    };
    text^;
  } else {
    raise(Error(FormatterError("Requested beyond end of original source")));
  };
};

let last_file_loc = (): Grain_parsing.Location.t => {
  let endpos: Stdlib__lexing.position = {
    pos_fname: "",
    pos_lnum: max_int,
    pos_cnum: max_int,
    pos_bol: 0,
  };

  let end_loc: Grain_parsing.Location.t = {
    loc_start: endpos,
    loc_end: endpos,
    loc_ghost: true,
  };
  end_loc;
};

let make_line_start = (loc: Grain_parsing.Location.t) => {
  let startpos: Stdlib__lexing.position = {
    pos_fname: "",
    pos_lnum: loc.loc_start.pos_lnum,
    pos_cnum: 0,
    pos_bol: 0,
  };

  let merged_loc: Grain_parsing.Location.t = {
    loc_start: startpos,
    loc_end: loc.loc_start,
    loc_ghost: loc.loc_ghost,
  };
  merged_loc;
};

let make_start_loc = (loc: Grain_parsing.Location.t) => {
  let merged_loc: Grain_parsing.Location.t = {
    loc_start: loc.loc_start,
    loc_end: loc.loc_start,
    loc_ghost: loc.loc_ghost,
  };
  merged_loc;
};

let make_end_loc = (loc: Grain_parsing.Location.t) => {
  let merged_loc: Grain_parsing.Location.t = {
    loc_start: loc.loc_end,
    loc_end: loc.loc_end,
    loc_ghost: loc.loc_ghost,
  };
  merged_loc;
};

let get_loc_line = (loc: Grain_parsing.Location.t) => {
  let (_, line, _, _) = Locations.get_raw_pos_info(loc.loc_start);
  line;
};

let get_end_loc_line = (loc: Grain_parsing.Location.t) => {
  let (_, line, _, _) = Locations.get_raw_pos_info(loc.loc_end);
  line;
};

let comment_to_doc = (comment: Parsetree.comment) => {
  Doc.text(String.trim(Comments.get_comment_source(comment)));
};

let comment_hardline = (comment: Parsetree.comment) => {
  switch (comment) {
  | Line(cmt)
  | Shebang(cmt) => Doc.hardLine
  | Block(cmt)
  | Doc(cmt) => Doc.line
  };
};

let is_disable_formatting_comment = (comment: Parsetree.comment) => {
  switch (comment) {
  | Line(cmt) =>
    if (String.trim(Comments.get_comment_source(comment))
        == "// formatter-ignore") {
      true;
    } else {
      false;
    }
  | _ => false
  };
};

// split the comments into the ones one this line and the ones that come after
let split_comments =
    (comments: list(Grain_parsing__Parsetree.comment), line: int)
    : (
        list(Grain_parsing__Parsetree.comment),
        list(Grain_parsing__Parsetree.comment),
      ) => {
  let (thisline, below) =
    List.fold_left(
      (acc, c) =>
        if (get_loc_line(Locations.get_comment_loc(c)) == line) {
          let (thisline, below) = acc;
          ([c, ...thisline], below);
        } else {
          let (thisline, below) = acc;
          (thisline, [c, ...below]);
        },
      ([], []),
      comments,
    );
  (List.rev(thisline), List.rev(below));
};

let print_leading_comments = (comments: list(Parsetree.comment), line: int) => {
  let prev_line = ref(line);
  let prev_comment: ref(option(Parsetree.comment)) = ref(None);

  let lines_of_comments =
    Doc.join(
      Doc.hardLine,
      List.map(
        c => {
          let next_line = get_loc_line(Locations.get_comment_loc(c));
          let ret =
            if (next_line > prev_line^ + 1) {
              Doc.concat([Doc.hardLine, comment_to_doc(c)]);
            } else {
              comment_to_doc(c);
            };

          prev_line := get_end_loc_line(Locations.get_comment_loc(c));
          prev_comment := Some(c);
          ret;
        },
        comments,
      ),
    );

  let last_comment_line =
    switch (prev_comment^) {
    | None => line
    | Some(c) => get_end_loc_line(Locations.get_comment_loc(c))
    };
  (Doc.concat([lines_of_comments, Doc.hardLine]), last_comment_line);
};

let print_multi_comments_raw =
    (comments: list(Parsetree.comment), line: int, leadSpace) => {
  let prev_line = ref(line);
  let prev_comment: ref(option(Parsetree.comment)) = ref(None);

  let lines_of_comment =
    Doc.join(
      Doc.space,
      List.map(
        c => {
          let nextLine = get_loc_line(Locations.get_comment_loc(c));
          let ret =
            if (nextLine > prev_line^) {
              if (nextLine > prev_line^ + 1) {
                Doc.concat([Doc.hardLine, Doc.hardLine, comment_to_doc(c)]);
              } else {
                Doc.concat([Doc.hardLine, comment_to_doc(c)]);
              };
            } else {
              comment_to_doc(c);
            };
          prev_line := get_end_loc_line(Locations.get_comment_loc(c));
          prev_comment := Some(c);
          ret;
        },
        comments,
      ),
    );

  Doc.concat([leadSpace, lines_of_comment]);
};

let print_multi_comments = (comments: list(Parsetree.comment), line: int) =>
  if (List.length(comments) > 0) {
    let first_comment = List.hd(comments);
    let first_comment_line =
      get_loc_line(Locations.get_comment_loc(first_comment));

    let lead_space =
      if (first_comment_line > line) {
        Doc.nil;
      } else {
        Doc.space;
      };
    print_multi_comments_raw(comments, line, lead_space);
  } else {
    Doc.nil;
  };

let print_multi_comments_no_space =
    (comments: list(Parsetree.comment), line: int) =>
  if (List.length(comments) > 0) {
    let lead_space = Doc.nil;
    print_multi_comments_raw(comments, line, lead_space);
  } else {
    Doc.nil;
  };

let special_join =
    (
      decls:
        list(
          (
            Reformat__Res_doc.t,
            list(Grain_parsing__Parsetree.comment),
            list(Grain_parsing__Parsetree.comment),
          ),
        ),
    ) => {
  let commmaTerminated = ref(false);

  let data_comments =
      (comments: list(Parsetree.comment), ~last_line, below_line_comments) => {
    let lineEnd =
      if (List.length(comments) < 1) {
        if (last_line) {
          if (List.length(below_line_comments) < 1) {
            commmaTerminated := false;
            Doc.nil;
          } else {
            commmaTerminated := true;
            Doc.concat([Doc.comma]);
          };
        } else {
          commmaTerminated := true;
          Doc.concat([Doc.comma, Doc.line]);
        };
      } else {
        let comment = List.hd(comments);
        commmaTerminated := true;
        Doc.concat([
          Doc.comma,
          Doc.space,
          comment_to_doc(comment),
          if (last_line) {
            Doc.nil;
          } else {
            comment_hardline(comment);
          },
        ]);
      };

    let belowLine =
      if (List.length(below_line_comments) == 0) {
        Doc.nil;
      } else {
        commmaTerminated := true;

        if (last_line) {
          Doc.concat([
            Doc.hardLine,
            print_multi_comments_no_space(below_line_comments, max_int),
          ]);
        } else {
          Doc.concat([
            print_multi_comments_no_space(below_line_comments, max_int),
            Doc.line,
          ]);
        };
      };

    Doc.concat([lineEnd, belowLine]);
  };

  let rec loop = (acc, docs) =>
    switch (docs) {
    | [] => List.rev(acc)
    | [(docs, comments, belowlinecomments)] =>
      List.rev([
        Doc.concat([
          docs,
          data_comments(comments, ~last_line=true, belowlinecomments),
        ]),
        ...acc,
      ])
    | [(docs, comments, belowlinecomments), ...xs] =>
      loop(
        [
          Doc.concat([
            docs,
            data_comments(comments, ~last_line=false, belowlinecomments),
          ]),
          ...acc,
        ],
        xs,
      )
    };

  // must use an intermediate to make sure commmaTerminated has been set
  let inter = Doc.concat(loop([], decls));

  (inter, commmaTerminated^);
};

let add_parens = doc =>
  Doc.concat([
    Doc.lparen,
    Doc.indent(Doc.concat([Doc.softLine, doc])),
    Doc.softLine,
    Doc.rparen,
  ]);

let add_braces = doc =>
  Doc.concat([
    Doc.lbrace,
    Doc.indent(Doc.concat([Doc.softLine, doc])),
    Doc.softLine,
    Doc.rbrace,
  ]);

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

let prefixop = (op: string) => {
  switch (op) {
  | "!" => true
  | _ => false
  };
};

let rec resugar_list_patterns =
        (
          ~patterns: list(Parsetree.pattern),
          ~parent_loc,
          ~original_source: array(string),
        ) => {
  let processed_list = resugar_pattern_list_inner(patterns, parent_loc);
  let items =
    List.map(
      i =>
        switch (i) {
        | RegularPattern(e) =>
          print_pattern(~pat=e, ~parent_loc, ~original_source)
        | SpreadPattern(e) =>
          Doc.concat([
            Doc.text("..."),
            print_pattern(~pat=e, ~parent_loc, ~original_source),
          ])
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

and resugar_pattern_list_inner =
    (patterns: list(Parsetree.pattern), parent_loc) => {
  let arg1 = List.nth(patterns, 0);
  let arg2 = List.nth(patterns, 1);

  switch (arg2.ppat_desc) {
  | PPatConstruct(innerfunc, innerpatterns) =>
    let func =
      switch (innerfunc.txt) {
      | IdentName(name) => name
      | _ => ""
      };

    if (func == "[]") {
      [RegularPattern(arg1)];
    } else {
      [RegularPattern(arg1), SpreadPattern(arg2)];
    };
  | _ => [RegularPattern(arg1), SpreadPattern(arg2)]
  };
}

and is_empty_list = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpId(loc) =>
    let loc_txt =
      switch (loc.txt) {
      | IdentName(name) => name
      | _ => ""
      };

    loc_txt == "[]";
  | _ => false
  };
}

and resugar_list =
    (
      ~expressions: list(Parsetree.expression),
      ~parent_loc: Grain_parsing.Location.t,
      ~original_source: array(string),
    ) => {
  let processed_list = resugar_list_inner(expressions, parent_loc);

  let items =
    List.map(
      i =>
        switch (i) {
        | Regular(e) =>
          Doc.group(
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~original_source,
              ~parent_loc,
              e,
            ),
          )
        | Spread(e) =>
          Doc.group(
            Doc.concat([
              Doc.text("..."),
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                e,
              ),
            ]),
          )
        },
      processed_list,
    );

  Doc.group(
    Doc.concat([
      Doc.indent(
        Doc.concat([
          Doc.lbracket,
          Doc.concat([
            Doc.softLine,
            Doc.join(Doc.concat([Doc.comma, Doc.line]), items),
          ]),
          Doc.ifBreaks(Doc.comma, Doc.nil),
        ]),
      ),
      Doc.softLine,
      Doc.rbracket,
    ]),
  );
}

and resugar_list_inner =
    (expressions: list(Parsetree.expression), parent_loc) =>
  if (List.length(expressions) < 2) {
    // Grain syntax makes it impossible to construct a list cons without
    // two arguments, but we'll check just to make sure
    raise(
      Error(Illegal_parse("List cons should always have two expressions")),
    );
  } else {
    let arg1 = List.nth(expressions, 0);
    let arg2 = List.nth(expressions, 1);

    switch (arg2.pexp_desc) {
    | PExpApp(innerfunc, innerexpressions) =>
      let func_name = get_function_name(innerfunc);

      if (func_name == list_cons) {
        let inner = resugar_list_inner(innerexpressions, parent_loc);
        List.append([Regular(arg1)], inner);
      } else {
        [Regular(arg1), Spread(arg2)];
      };
    | _ =>
      if (is_empty_list(arg2)) {
        [Regular(arg1)];
      } else {
        [Regular(arg1), Spread(arg2)];
      }
    };
  }

and print_record_pattern =
    (
      ~patternlocs:
         list(
           (
             Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
             Grain_parsing__Parsetree.pattern,
           ),
         ),
      ~closedflag: Grain_parsing__Asttypes.closed_flag,
      ~parent_loc: Grain_parsing__Location.t,
      ~original_source: array(string),
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
          let printed_pat =
            print_pattern(~pat, ~parent_loc, ~original_source);

          let pun =
            switch (printed_ident, printed_pat: Doc.t) {
            | (Text(i), Text(e)) => i == e
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

and print_pattern =
    (
      ~pat: Parsetree.pattern,
      ~parent_loc: Grain_parsing__Location.t,
      ~original_source: array(string),
    ) => {
  let (leading_comments, trailing_comments) =
    Walktree.partition_comments(pat.ppat_loc, Some(parent_loc));
  Walktree.remove_used_comments(leading_comments, trailing_comments);

  let expr_line = get_end_loc_line(pat.ppat_loc);

  let leading_comment_docs =
    print_multi_comments_no_space(leading_comments, expr_line);
  let trailing_comment_docs =
    print_multi_comments(trailing_comments, expr_line);

  let printed_pattern: (Doc.t, bool) =
    switch (pat.ppat_desc) {
    | PPatAny => (Doc.text("_"), false)
    | PPatConstant(c) => (print_constant(c), false)
    | PPatVar({txt, _}) =>
      if (infixop(txt) || prefixop(txt)) {
        (Doc.concat([Doc.lparen, Doc.text(txt), Doc.rparen]), false);
      } else {
        (Doc.text(txt), false);
      }
    | PPatTuple(patterns) => (
        Doc.concat([
          Doc.join(
            Doc.concat([Doc.comma, Doc.line]),
            List.map(
              p =>
                Doc.group(
                  print_pattern(
                    ~pat=p,
                    ~parent_loc=pat.ppat_loc,
                    ~original_source,
                  ),
                ),
              patterns,
            ),
          ),
          if (List.length(patterns) == 1) {
            Doc.comma;
          } else {
            Doc.nil;
          },
        ]),
        true,
      )
    | PPatArray(patterns) => (
        Doc.group(
          Doc.concat([
            Doc.lbracket,
            Doc.text(">"),
            Doc.space,
            Doc.join(
              Doc.concat([Doc.comma, Doc.space]),
              List.map(
                p => print_pattern(~pat=p, ~parent_loc, ~original_source),
                patterns,
              ),
            ),
            Doc.rbracket,
          ]),
        ),
        false,
      )
    | PPatRecord(patternlocs, closedflag) => (
        print_record_pattern(
          ~patternlocs,
          ~closedflag,
          ~parent_loc,
          ~original_source,
        ),
        false,
      )
    | PPatConstraint(pattern, parsed_type) => (
        Doc.concat([
          print_pattern(
            ~pat=pattern,
            ~parent_loc=pat.ppat_loc,
            ~original_source,
          ),
          Doc.concat([Doc.text(":"), Doc.space]),
          print_type(parsed_type, original_source),
        ]),
        false,
      )
    | PPatConstruct(location, patterns) =>
      let func =
        switch (location.txt) {
        | IdentName(name) => name
        | _ => ""
        };
      if (func == list_cons) {
        (
          resugar_list_patterns(~patterns, ~parent_loc, ~original_source),
          false,
        );
      } else {
        (
          Doc.concat([
            print_ident(location.txt),
            if (List.length(patterns) > 0) {
              add_parens(
                Doc.join(
                  Doc.comma,
                  List.map(
                    pat => print_pattern(~pat, ~parent_loc, ~original_source),
                    patterns,
                  ),
                ),
              );
            } else {
              Doc.nil;
            },
          ]),
          false,
        );
      };

    | PPatOr(pattern1, pattern2) =>
      /* currently unsupported so just replace with the original source */
      let originalCode = get_original_code(pat.ppat_loc, original_source);
      Walktree.remove_comments_in_ignore_block(pat.ppat_loc);

      (Doc.text(originalCode), false);
    | PPatAlias(pattern, loc) =>
      /* currently unsupported so just replace with the original source */
      let originalCode = get_original_code(pat.ppat_loc, original_source);
      Walktree.remove_comments_in_ignore_block(pat.ppat_loc);

      (Doc.text(originalCode), false);
    };

  let (pattern, parens) = printed_pattern;

  let with_leading =
    if (leading_comment_docs == Doc.nil) {
      [pattern];
    } else {
      [leading_comment_docs, Doc.space, pattern];
    };
  let with_trailing =
    if (trailing_comment_docs == Doc.nil) {
      with_leading;
    } else {
      List.append(with_leading, [trailing_comment_docs]);
    };

  let clean_pattern =
    if (List.length(with_trailing) == 1) {
      List.hd(with_trailing);
    } else {
      Doc.concat(with_trailing);
    };

  if (parens) {
    Doc.concat([
      Doc.lparen,
      Doc.indent(
        Doc.concat([
          Doc.softLine,
          clean_pattern,
          Doc.ifBreaks(Doc.comma, Doc.nil),
        ]),
      ),
      Doc.softLine,
      Doc.rparen,
    ]);
  } else {
    clean_pattern;
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
    | PConstChar(c) => Printf.sprintf("'%s'", String.escaped(c))
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
    | PConstString(s) => Printf.sprintf("\"%s\"", String.escaped(s))
    };
  Doc.text(print_c);
}
and print_ident = (ident: Identifier.t) => {
  switch (ident) {
  | IdentName(name) =>
    if (infixop(name) || prefixop(name)) {
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
      ~fields:
         list(
           (
             Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
             Grain_parsing__Parsetree.expression,
           ),
         ),
      ~parent_loc: Grain_parsing__Location.t,
      ~original_source: array(string),
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
                print_expression(
                  ~parentIsArrow=false,
                  ~endChar=None,
                  ~original_source,
                  ~parent_loc,
                  expr,
                );
              let punned_expr = check_for_pun(expr);

              let pun =
                switch (printed_ident, punned_expr: Doc.t) {
                | (Text(i), Text(e)) => i == e
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
        if (List.length(fields) == 1) {
          // TODO: not needed once we annotate with ::
          Doc.comma; //  append a comma as single argument record look like block {data:val}
        } else {
          Doc.ifBreaks(Doc.text(","), Doc.nil);
        },
      ]),
    ),
    Doc.line,
    Doc.rbrace,
  ])

and print_type =
    (p: Grain_parsing__Parsetree.parsed_type, original_source: array(string)) => {
  switch (p.ptyp_desc) {
  | PTyAny => Doc.text("_")
  | PTyVar(name) => Doc.text(name)
  | PTyArrow(types, parsed_type) =>
    Doc.concat([
      Doc.group(
        if (List.length(types) == 1) {
          print_type(List.hd(types), original_source);
        } else {
          Doc.concat([
            Doc.lparen,
            Doc.indent(
              Doc.concat([
                Doc.line,
                Doc.join(
                  Doc.concat([Doc.comma, Doc.line]),
                  List.map(t => print_type(t, original_source), types),
                ),
              ]),
            ),
            Doc.line,
            Doc.rparen,
          ]);
        },
      ),
      Doc.space,
      Doc.text("->"),
      Doc.space,
      print_type(parsed_type, original_source),
    ])

  | PTyTuple(parsed_types) =>
    Doc.concat([
      Doc.lparen,
      Doc.join(
        Doc.comma,
        List.map(t => print_type(t, original_source), parsed_types),
      ),
      if (List.length(parsed_types) == 1) {
        // single arg tuple
        Doc.comma;
      } else {
        Doc.nil;
      },
      Doc.rparen,
    ])

  | PTyConstr(locidentifier, parsedtypes) =>
    let ident = locidentifier.txt;
    if (List.length(parsedtypes) == 0) {
      print_ident(ident);
    } else {
      Doc.concat([
        print_ident(ident),
        Doc.text("<"),
        Doc.join(
          Doc.concat([Doc.comma, Doc.space]),
          List.map(typ => {print_type(typ, original_source)}, parsedtypes),
        ),
        Doc.text(">"),
      ]);
    };
  | PTyPoly(locationstrings, parsed_type) =>
    let originalCode = get_original_code(p.ptyp_loc, original_source);
    Walktree.remove_comments_in_ignore_block(p.ptyp_loc);
    Doc.text(originalCode);
  };
}
and print_application =
    (
      ~func: Parsetree.expression,
      ~expressions: list(Parsetree.expression),
      ~parent_loc: Location.t,
      ~original_source: array(string),
    ) => {
  let function_name = get_function_name(func);

  switch (expressions) {
  | [first] when prefixop(function_name) =>
    switch (first.pexp_desc) {
    | PExpApp(fn, _) =>
      Doc.concat([
        Doc.text(function_name),
        Doc.lparen,
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          first,
        ),
        Doc.rparen,
      ])

    | _ =>
      Doc.concat([
        Doc.text(function_name),
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          first,
        ),
      ])
    }

  | [first, second] when infixop(function_name) =>
    let first_brackets =
      switch (first.pexp_desc) {
      | PExpIf(_) =>
        Doc.concat([
          Doc.lparen,
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            first,
          ),
          Doc.rparen,
        ])
      | PExpApp(fn, _) =>
        let leftfn = get_function_name(fn);
        if (infixop(leftfn)) {
          Doc.concat([
            Doc.lparen,
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~original_source,
              ~parent_loc,
              first,
            ),
            Doc.rparen,
          ]);
        } else {
          Doc.concat([
            Doc.lparen,
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~original_source,
              ~parent_loc,
              first,
            ),
            Doc.rparen,
          ]);
        };
      | _ =>
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          first,
        )
      };

    let second_brackets =
      switch (second.pexp_desc) {
      | PExpIf(_)
      | PExpApp(_) =>
        Doc.concat([
          Doc.lparen,
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            second,
          ),
          Doc.rparen,
        ])
      | _ =>
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          second,
        )
      };
    Doc.concat([
      first_brackets,
      Doc.space,
      Doc.text(function_name),
      Doc.space,
      second_brackets,
    ]);

  | _ when prefixop(function_name) || infixop(function_name) =>
    raise(Error(Illegal_parse("Formatter error, wrong number of args ")))
  | _ =>
    if (function_name == list_cons) {
      resugar_list(~expressions, ~parent_loc, ~original_source);
    } else if (Array.exists(fn => function_name == fn, exception_primitives)) {
      Doc.concat([
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          func,
        ),
        Doc.space,
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          List.hd(expressions),
        ),
      ]);
    } else {
      Doc.group(
        Doc.concat([
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc=func.pexp_loc,
            func,
          ),
          Doc.lparen,
          Doc.indent(
            Doc.concat([
              Doc.softLine,
              Doc.join(
                Doc.concat([Doc.text(","), Doc.line]),
                List.map(
                  e =>
                    print_expression(
                      ~parentIsArrow=false,
                      ~endChar=None,
                      ~original_source,
                      ~parent_loc,
                      e,
                    ),
                  expressions,
                ),
              ),
              Doc.ifBreaks(Doc.comma, Doc.nil),
            ]),
          ),
          Doc.softLine,
          Doc.rparen,
        ]),
      );
    }
  };
}

and get_function_name = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpConstant(x) =>
    switch (x) {
    | PConstString(str) => str
    | _ => ""
    }

  | PExpId({txt: id}) =>
    switch (id) {
    | IdentName(name) => name
    | _ => ""
    }
  | _ => ""
  };
}

and check_for_pun = (expr: Parsetree.expression) =>
  switch (expr.pexp_desc) {
  | PExpId({txt: id}) => print_ident(id)
  | _ => Doc.nil
  }

and print_expression =
    (
      ~parentIsArrow: bool,
      ~endChar: option(Doc.t), // not currently used but will be in the next iteration
      ~original_source: array(string),
      ~parent_loc: Grain_parsing__Location.t,
      expr: Parsetree.expression,
    ) => {
  let (leading_comments, trailing_comments) =
    Walktree.partition_comments(expr.pexp_loc, Some(expr.pexp_loc));

  let expr_line = get_end_loc_line(expr.pexp_loc);

  let trailing_comment_docs =
    if (List.length(trailing_comments) > 0) {
      Doc.concat([print_multi_comments(trailing_comments, expr_line)]);
    } else {
      Doc.nil;
    };

  Walktree.remove_used_comments(leading_comments, trailing_comments);

  let leading_comment_docs =
    if (List.length(leading_comments) > 0) {
      Doc.concat([
        print_multi_comments_no_space(leading_comments, expr_line),
        Doc.space,
      ]);
    } else {
      Doc.nil;
    };

  let expression_doc =
    switch (expr.pexp_desc) {
    | PExpConstant(x) => print_constant(x)
    | PExpId({txt: id}) => print_ident(id)
    | PExpLet(rec_flag, mut_flag, vbs) =>
      print_value_bind(
        Asttypes.Nonexported,
        rec_flag,
        mut_flag,
        vbs,
        parent_loc,
        original_source,
      )
    | PExpTuple(expressions) =>
      Doc.concat([
        Doc.lparen,
        Doc.join(
          Doc.concat([Doc.comma, Doc.space]),
          List.map(
            e =>
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                e,
              ),
            expressions,
          ),
        ),
        if (List.length(expressions) == 1) {
          // single arg tuple
          Doc.comma;
        } else {
          Doc.nil;
        },
        Doc.rparen,
      ])

    | PExpArray(expressions) =>
      Doc.group(
        if (List.length(expressions) == 0) {
          Doc.text("[>]");
        } else {
          Doc.concat([
            Doc.lbracket,
            Doc.text("> "),
            Doc.join(
              Doc.concat([Doc.comma, Doc.space]),
              List.map(
                e =>
                  print_expression(
                    ~parentIsArrow=false,
                    ~endChar=None,
                    ~original_source,
                    ~parent_loc,
                    e,
                  ),
                expressions,
              ),
            ),
            Doc.space,
            Doc.rbracket,
          ]);
        },
      )
    | PExpArrayGet(expression1, expression2) =>
      Doc.concat([
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          expression1,
        ),
        Doc.lbracket,
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          expression2,
        ),
        Doc.rbracket,
      ])
    | PExpArraySet(expression1, expression2, expression3) =>
      Doc.group(
        Doc.concat([
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            expression1,
          ),
          Doc.lbracket,
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            expression2,
          ),
          Doc.rbracket,
          Doc.space,
          Doc.text("="),
          Doc.indent(
            Doc.concat([
              Doc.line,
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                expression3,
              ),
            ]),
          ),
        ]),
      )

    | PExpRecord(record) =>
      print_record(~fields=record, ~parent_loc, ~original_source)
    | PExpRecordGet(expression, {txt, _}) =>
      Doc.concat([
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          expression,
        ),
        Doc.dot,
        print_ident(txt),
      ])
    | PExpRecordSet(expression, {txt, _}, expression2) =>
      Doc.concat([
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          expression,
        ),
        Doc.dot,
        print_ident(txt),
        Doc.space,
        Doc.equal,
        Doc.space,
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          expression2,
        ),
      ])
    | PExpMatch(expression, match_branches) =>
      let arg =
        Doc.concat([
          Doc.lparen,
          Doc.group(
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~original_source,
              ~parent_loc,
              expression,
            ),
          ),
          Doc.rparen,
        ]);

      Doc.breakableGroup(
        ~forceBreak=false,
        Doc.concat([
          Doc.concat([Doc.text("match "), arg, Doc.space]),
          Doc.text("{"),
          Doc.indent(
            Doc.concat([
              Doc.hardLine,
              Doc.join(
                Doc.line, // need to inject comma before comments
                List.map(
                  (branch: Parsetree.match_branch) => {
                    let (leading_comments, trailing_comments) =
                      Walktree.partition_comments(
                        branch.pmb_loc,
                        Some(expr.pexp_loc),
                      );

                    let this_line = get_end_loc_line(branch.pmb_loc);

                    let (this_line_comments, below_line_comments) =
                      split_comments(trailing_comments, this_line);

                    Walktree.remove_used_comments(
                      leading_comments,
                      trailing_comments,
                    );

                    let (stmt_leading_comment_docs_1, prevLine) =
                      print_leading_comments(leading_comments, this_line);
                    let stmt_leading_comment_docs =
                      if (List.length(leading_comments) > 0) {
                        stmt_leading_comment_docs_1;
                      } else {
                        Doc.nil;
                      };
                    let trailing_line_comment_docs =
                      print_multi_comments(this_line_comments, this_line);
                    let below_line_comment_docs =
                      print_multi_comments(below_line_comments, this_line);

                    Doc.concat([
                      Doc.group(
                        Doc.concat([
                          stmt_leading_comment_docs,
                          Doc.concat([
                            Doc.group(
                              print_pattern(
                                ~pat=branch.pmb_pat,
                                ~parent_loc,
                                ~original_source,
                              ),
                            ),
                            switch (branch.pmb_guard) {
                            | None => Doc.nil
                            | Some(guard) =>
                              Doc.concat([
                                Doc.space,
                                Doc.text("when"),
                                Doc.space,
                                print_expression(
                                  ~parentIsArrow=false,
                                  ~endChar=None,
                                  ~original_source,
                                  ~parent_loc,
                                  guard,
                                ),
                              ])
                            },
                            Doc.space,
                            Doc.text("=>"),
                          ]),
                          Doc.group(
                            switch (branch.pmb_body.pexp_desc) {
                            | PExpBlock(expressions) =>
                              Doc.concat([
                                Doc.space,
                                print_expression(
                                  ~parentIsArrow=true,
                                  ~endChar=None,
                                  ~original_source,
                                  ~parent_loc,
                                  branch.pmb_body,
                                ),
                              ])
                            | _ =>
                              Doc.indent(
                                Doc.concat([
                                  Doc.line,
                                  print_expression(
                                    ~parentIsArrow=true,
                                    ~endChar=None,
                                    ~original_source,
                                    ~parent_loc,
                                    branch.pmb_body,
                                  ),
                                ]),
                              )
                            },
                          ),
                          Doc.comma,
                          trailing_line_comment_docs,
                        ]),
                      ),
                      if (List.length(below_line_comments) > 0) {
                        Doc.concat([below_line_comment_docs]);
                      } else {
                        Doc.nil;
                      },
                    ]);
                  },
                  match_branches,
                ),
              ),
              // keeping this as I think we can start to not add commas again
              //  Doc.text(","),
              //Doc.ifBreaks(Doc.text(","), Doc.nil),
            ]),
          ),
          Doc.line,
          Doc.text("}"),
        ]),
      );

    | PExpPrim1(prim1, expression) =>
      let originalCode = get_original_code(expr.pexp_loc, original_source);
      Walktree.remove_comments_in_ignore_block(expr.pexp_loc);
      Doc.text(originalCode);
    | PExpPrim2(prim2, expression, expression1) =>
      let originalCode = get_original_code(expr.pexp_loc, original_source);
      Walktree.remove_comments_in_ignore_block(expr.pexp_loc);
      Doc.text(originalCode);
    | PExpPrimN(primn, expressions) =>
      let originalCode = get_original_code(expr.pexp_loc, original_source);
      Walktree.remove_comments_in_ignore_block(expr.pexp_loc);
      Doc.text(originalCode);
    | PExpIf(condition, trueExpr, falseExpr) =>
      let (leading_condition_comments, _trailing_comments) =
        Walktree.partition_comments(condition.pexp_loc, Some(parent_loc));
      Walktree.remove_used_comments(leading_condition_comments, []);

      let expr_line = get_end_loc_line(condition.pexp_loc);

      let leading_condition_comment_docs =
        if (List.length(leading_condition_comments) > 0) {
          Doc.concat([
            print_multi_comments_no_space(
              leading_condition_comments,
              expr_line,
            ),
            Doc.space,
          ]);
        } else {
          Doc.nil;
        };

      let true_false_space = Doc.space;
      // keep this - we need this if we don't force single lines into block expressions
      // switch (trueExpr.pexp_desc) {
      // | PExpBlock(expressions) => Doc.space
      // | _ => Doc.line
      // };

      let true_clause =
        switch (trueExpr.pexp_desc) {
        | PExpBlock(expressions) =>
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            trueExpr,
          )

        | _ =>
          Doc.concat([
            Doc.lbrace,
            Doc.indent(
              Doc.concat([
                Doc.hardLine,
                print_expression(
                  ~parentIsArrow=false,
                  ~endChar=None,
                  ~original_source,
                  ~parent_loc,
                  trueExpr,
                ),
              ]),
            ),
            Doc.hardLine,
            Doc.rbrace,
          ])
        };

      let false_clause =
        switch (falseExpr.pexp_desc) {
        | PExpBlock(expressions) =>
          if (List.length(expressions) > 0) {
            Doc.concat([
              true_false_space,
              Doc.text("else "),
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                falseExpr,
              ),
            ]);
          } else {
            Doc.nil;
          }
        | PExpIf(_condition, _trueExpr, _falseExpr) =>
          Doc.concat([
            true_false_space,
            Doc.text("else "),
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~original_source,
              ~parent_loc,
              falseExpr,
            ),
          ])
        | _ =>
          Doc.concat([
            true_false_space,
            Doc.text("else"),
            Doc.group(
              Doc.concat([
                Doc.space,
                Doc.lbrace,
                Doc.indent(
                  Doc.concat([
                    Doc.hardLine,
                    print_expression(
                      ~parentIsArrow=false,
                      ~endChar=None,
                      ~original_source,
                      ~parent_loc,
                      falseExpr,
                    ),
                  ]),
                ),
                Doc.hardLine,
                Doc.rbrace,
              ]),
            ),
          ])
        };

      if (parentIsArrow) {
        Doc.group(
          Doc.concat([
            Doc.text("if"),
            Doc.space,
            Doc.group(
              Doc.concat([
                Doc.lparen,
                leading_condition_comment_docs,
                print_expression(
                  ~parentIsArrow=false,
                  ~endChar=None,
                  ~original_source,
                  ~parent_loc,
                  condition,
                ),
                Doc.rparen,
                Doc.space,
              ]),
            ),
            true_clause,
            false_clause,
          ]),
        );
      } else {
        Doc.concat([
          Doc.group(
            Doc.concat([
              Doc.text("if"),
              Doc.space,
              Doc.lparen,
              leading_condition_comment_docs,
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                condition,
              ),
              Doc.rparen,
              Doc.space,
            ]),
          ),
          true_clause,
          false_clause,
        ]);
      };
    | PExpWhile(expression, expression1) =>
      Doc.concat([
        Doc.text("while"),
        Doc.space,
        Doc.group(
          Doc.concat([
            Doc.lparen,
            Doc.space,
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~original_source,
              ~parent_loc,
              expression,
            ),
            Doc.space,
            Doc.rparen,
          ]),
        ),
        Doc.space,
        Doc.group(
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            expression1,
          ),
        ),
      ])

    | PExpFor(optexpression1, optexpression2, optexpression3, expression4) =>
      Doc.concat([
        Doc.group(
          Doc.concat([
            Doc.text("for"),
            Doc.space,
            Doc.lparen,
            Doc.indent(
              Doc.concat([
                Doc.line,
                Doc.concat([
                  switch (optexpression1) {
                  | Some(expr) =>
                    print_expression(
                      ~parentIsArrow=false,
                      ~endChar=None,
                      ~original_source,
                      ~parent_loc,
                      expr,
                    )
                  | None => Doc.nil
                  },
                  Doc.text(";"),
                  switch (optexpression2) {
                  | Some(expr) =>
                    Doc.concat([
                      Doc.line,
                      Doc.group(
                        print_expression(
                          ~parentIsArrow=false,
                          ~endChar=None,
                          ~original_source,
                          ~parent_loc,
                          expr,
                        ),
                      ),
                    ])
                  | None => Doc.nil
                  },
                  Doc.text(";"),
                  switch (optexpression3) {
                  | Some(expr) =>
                    Doc.concat([
                      Doc.line,
                      Doc.group(
                        print_expression(
                          ~parentIsArrow=false,
                          ~endChar=None,
                          ~original_source,
                          ~parent_loc,
                          expr,
                        ),
                      ),
                    ])
                  | None => Doc.nil
                  },
                ]),
              ]),
            ),
            Doc.line,
            Doc.rparen,
          ]),
        ),
        Doc.space,
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          expression4,
        ),
      ])
    | PExpContinue => Doc.group(Doc.concat([Doc.text("continue")]))
    | PExpBreak => Doc.group(Doc.concat([Doc.text("break")]))
    | PExpConstraint(expression, parsed_type) =>
      Doc.group(
        Doc.concat([
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            expression,
          ),
          Doc.text(": "),
          print_type(parsed_type, original_source),
        ]),
      )
    | PExpLambda(patterns, expression) =>
      let args =
        if (List.length(patterns) == 0) {
          Doc.concat([Doc.lparen, Doc.rparen]);
        } else if (List.length(patterns) > 1) {
          Doc.group(
            Doc.indent(
              Doc.concat([
                Doc.softLine,
                Doc.lparen,
                Doc.indent(
                  Doc.concat([
                    Doc.softLine,
                    Doc.join(
                      Doc.concat([Doc.text(","), Doc.line]),
                      List.map(
                        p =>
                          print_pattern(
                            ~pat=p,
                            ~parent_loc,
                            ~original_source,
                          ),
                        patterns,
                      ),
                    ),
                    Doc.ifBreaks(Doc.text(","), Doc.nil),
                  ]),
                ),
                Doc.softLine,
                Doc.rparen,
              ]),
            ),
          );
        } else {
          let pat = List.hd(patterns);

          switch (pat.ppat_desc) {
          | PPatConstraint(_) =>
            Doc.concat([
              Doc.lparen,
              print_pattern(~pat, ~parent_loc, ~original_source),
              Doc.rparen,
            ])
          | PPatTuple(_) =>
            Doc.concat([
              Doc.lparen,
              print_pattern(~pat, ~parent_loc, ~original_source),
              Doc.rparen,
            ])
          | _ => print_pattern(~pat, ~parent_loc, ~original_source)
          };
        };

      let followsArrow =
        switch (expression.pexp_desc) {
        | PExpBlock(_) => [
            Doc.group(
              Doc.concat([args, Doc.space, Doc.text("=>"), Doc.space]),
            ),
            Doc.group(
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                expression,
              ),
            ),
          ]
        | _ => [
            Doc.concat([
              args,
              Doc.space,
              Doc.text("=>"),
              Doc.indent(
                Doc.concat([
                  Doc.line,
                  Doc.group(
                    print_expression(
                      ~parentIsArrow=false,
                      ~endChar=None,
                      ~original_source,
                      ~parent_loc,
                      expression,
                    ),
                  ),
                ]),
              ),
            ]),
          ]
        };

      Doc.concat(followsArrow);

    | PExpApp(func, expressions) =>
      print_application(~func, ~expressions, ~parent_loc, ~original_source)
    | PExpBlock(expressions) =>
      if (List.length(expressions) > 0) {
        let previous_line = ref(get_loc_line(expr.pexp_loc));
        let block =
          Doc.join(
            Doc.hardLine,
            List.map(
              (e: Parsetree.expression) => {
                // get the leading comments
                let (leading_comments, _trailing_comments) =
                  Walktree.partition_comments(e.pexp_loc, None);

                let disable_formatting =
                  List.exists(
                    is_disable_formatting_comment,
                    leading_comments,
                  );

                Walktree.remove_used_comments(leading_comments, []);

                let (stmt_leading_comment_docs_1, prevLine) =
                  print_leading_comments(leading_comments, previous_line^);

                let stmt_leading_comment_docs =
                  if (List.length(leading_comments) > 0) {
                    stmt_leading_comment_docs_1;
                  } else {
                    Doc.nil;
                  };

                let blank_line_above =
                  if (get_loc_line(e.pexp_loc) > prevLine + 1) {
                    Doc.hardLine;
                  } else {
                    Doc.nil;
                  };

                if (disable_formatting) {
                  let originalCode =
                    get_original_code(e.pexp_loc, original_source);
                  Walktree.remove_comments_in_ignore_block(e.pexp_loc);

                  Doc.concat([
                    stmt_leading_comment_docs,
                    blank_line_above,
                    Doc.group(Doc.text(originalCode)),
                  ]);
                } else {
                  let printed_expression =
                    print_expression(
                      ~parentIsArrow=false,
                      ~endChar=None,
                      ~original_source,
                      ~parent_loc,
                      e,
                    );

                  previous_line := get_end_loc_line(e.pexp_loc);

                  Doc.group(
                    Doc.concat([
                      stmt_leading_comment_docs,
                      blank_line_above,
                      Doc.group(printed_expression),
                    ]),
                  );
                };
              },
              expressions,
            ),
          );

        // we handle the  comments after the last expression in a block
        // specially here

        let remaining_comments_in_block =
          Walktree.get_comments_inside_location(expr.pexp_loc);
        Walktree.remove_used_comments([], remaining_comments_in_block);
        let blockEndCommentDocs =
          if (List.length(remaining_comments_in_block) > 0) {
            print_multi_comments(remaining_comments_in_block, previous_line^);
          } else {
            Doc.nil;
          };

        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat([
            Doc.lbrace,
            Doc.indent(Doc.concat([Doc.line, block, blockEndCommentDocs])),
            Doc.line,
            Doc.rbrace,
          ]),
        );
      } else {
        let (_leading_comments, trailing_comments) =
          Walktree.partition_comments(expr.pexp_loc, Some(parent_loc));

        Walktree.remove_used_comments([], trailing_comments);

        let expr_line = get_end_loc_line(expr.pexp_loc) + 1;

        let blockCommentDocs =
          if (List.length(trailing_comments) > 0) {
            Doc.concat([
              print_multi_comments_no_space(trailing_comments, expr_line),
              Doc.hardLine,
            ]);
          } else {
            Doc.nil;
          };
        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat([
            Doc.lbrace,
            Doc.indent(Doc.concat([Doc.line, blockCommentDocs])),
            Doc.line,
            Doc.rbrace,
          ]),
        );
      }

    | PExpBoxAssign(expression, expression1) =>
      Doc.concat([
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          expression,
        ),
        Doc.space,
        Doc.text(":= "),
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc,
          expression1,
        ),
      ])
    | PExpAssign(expression, expression1) =>
      switch (expression1.pexp_desc) {
      | PExpApp(func, expressions) =>
        let functionName = get_function_name(func);

        let trimmed_operator = String.trim(functionName);

        let left =
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~parent_loc,
            ~original_source,
            expression,
          );

        let leftMatchesFirst =
          switch (expressions) {
          | [expr, ...remainder] =>
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~parent_loc,
              ~original_source,
              expr,
            )
            == left
          | _ => false
          };

        if (leftMatchesFirst) {
          // +=, -=, *=, /=, and %=
          switch (trimmed_operator) {
          | "+"
          | "-"
          | "*"
          | "/"
          | "%" =>
            let sugaredOp = Doc.text(" " ++ trimmed_operator ++ "= ");
            Doc.concat([
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                expression,
              ),
              sugaredOp,
              switch (expressions) {
              | [] =>
                raise(
                  Error(
                    Illegal_parse("Sugared op needs at least one expression"),
                  ),
                )
              | [expression] =>
                print_expression(
                  ~parentIsArrow=false,
                  ~endChar=None,
                  ~original_source,
                  ~parent_loc,
                  expression,
                )
              | [expression1, expression2, ...rest] =>
                print_expression(
                  ~parentIsArrow=false,
                  ~endChar=None,
                  ~original_source,
                  ~parent_loc,
                  expression2,
                )
              },
            ]);
          | _ =>
            Doc.concat([
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                expression,
              ),
              Doc.space,
              Doc.equal,
              Doc.space,
              print_expression(
                ~parentIsArrow=false,
                ~endChar=None,
                ~original_source,
                ~parent_loc,
                expression1,
              ),
            ])
          };
        } else {
          Doc.concat([
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~original_source,
              ~parent_loc,
              expression,
            ),
            Doc.space,
            Doc.equal,
            Doc.space,
            print_expression(
              ~parentIsArrow=false,
              ~endChar=None,
              ~original_source,
              ~parent_loc,
              expression1,
            ),
          ]);
        };

      | _ =>
        Doc.concat([
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            expression,
          ),
          Doc.space,
          Doc.equal,
          Doc.space,
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            expression1,
          ),
        ])
      }

    | /** Used for modules without body expressions */ PExpNull => Doc.nil
    };

  let endingDoc =
    switch (endChar) {
    | None => Doc.nil
    | Some(d) => Doc.concat([d])
    };

  if (trailing_comment_docs == Doc.nil) {
    Doc.concat([leading_comment_docs, expression_doc, endingDoc]);
  } else {
    Doc.concat([
      leading_comment_docs,
      expression_doc,
      endingDoc,
      trailing_comment_docs,
    ]);
  };
}
and print_value_bind =
    (
      export_flag: Asttypes.export_flag,
      rec_flag,
      mut_flag,
      vbs: list(Parsetree.value_binding),
      parent_loc: Grain_parsing__Location.t,
      original_source: array(string),
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

  let value_bindings =
    List.map(
      (vb: Parsetree.value_binding) => {
        let expression =
          print_expression(
            ~parentIsArrow=false,
            ~endChar=None,
            ~original_source,
            ~parent_loc,
            vb.pvb_expr,
          );

        let expressionGrp =
          switch (vb.pvb_expr.pexp_desc) {
          | PExpBlock(_) => Doc.concat([Doc.space, expression])
          | _ => Doc.concat([Doc.space, expression])
          };

        Doc.concat([
          Doc.group(
            print_pattern(
              ~pat=vb.pvb_pat,
              ~parent_loc=vb.pvb_loc,
              ~original_source,
            ),
          ),
          Doc.space,
          Doc.equal,
          Doc.group(expressionGrp),
        ]);
      },
      vbs,
    );

  Doc.concat([
    Doc.group(
      Doc.concat([
        exported,
        Doc.text("let"),
        Doc.space,
        recursive,
        mutble,
        Doc.join(
          Doc.concat([Doc.comma, Doc.hardLine]),
          List.map(vb => vb, value_bindings),
        ),
      ]),
    ),
  ]);
};

let rec print_data =
        (
          data: Grain_parsing__Parsetree.data_declaration,
          original_source: array(string),
        ) => {
  let nameloc = data.pdata_name;
  switch (data.pdata_kind) {
  | PDataVariant(constr_declarations) =>
    let decls =
      List.map(
        (d: Grain_parsing__Parsetree.constructor_declaration) => {
          let (leading_comments, trailing_comments) =
            Walktree.partition_comments(d.pcd_loc, Some(data.pdata_loc));

          let this_line = get_end_loc_line(d.pcd_loc);

          let (this_line_comments, below_line_comments) =
            split_comments(trailing_comments, this_line);

          Walktree.remove_used_comments(leading_comments, trailing_comments);

          let (stmt_leading_comment_docs_1, prevLine) =
            print_leading_comments(leading_comments, this_line);
          let stmt_leading_comment_docs =
            if (List.length(leading_comments) > 0) {
              stmt_leading_comment_docs_1;
            } else {
              Doc.nil;
            };
          (
            Doc.concat([
              Doc.group(
                Doc.concat([
                  stmt_leading_comment_docs,
                  Doc.text(d.pcd_name.txt),
                  switch (d.pcd_args) {
                  | PConstrTuple(parsed_types) =>
                    if (List.length(parsed_types) > 0) {
                      Doc.group(
                        Doc.concat([
                          Doc.lparen,
                          Doc.join(
                            Doc.concat([Doc.comma, Doc.line]),
                            List.map(
                              t => print_type(t, original_source),
                              parsed_types,
                            ),
                          ),
                          Doc.rparen,
                        ]),
                      );
                    } else {
                      Doc.nil;
                    }
                  | PConstrSingleton => Doc.nil
                  },
                ]),
              ),
            ]),
            this_line_comments,
            below_line_comments,
          );
        },
        constr_declarations,
      );

    let (joinedDecls, commaTerminated) = special_join(decls);

    Doc.group(
      Doc.concat([
        Doc.text("enum"),
        Doc.space,
        Doc.text(nameloc.txt),
        if (List.length(data.pdata_params) > 0) {
          Doc.concat([
            Doc.text("<"),
            Doc.join(
              Doc.text(", "),
              List.map(
                t => print_type(t, original_source),
                data.pdata_params,
              ),
            ),
            Doc.text(">"),
            Doc.space,
          ]);
        } else {
          Doc.space;
        },
        Doc.lbrace,
        Doc.indent(Doc.concat([Doc.line, joinedDecls])),
        if (!commaTerminated) {
          Doc.ifBreaks(Doc.comma, Doc.nil);
        } else {
          Doc.nil;
        },
        Doc.line,
        Doc.rbrace,
      ]),
    );

  | PDataRecord(label_declarations) =>
    let decls =
      List.map(
        (decl: Grain_parsing__Parsetree.label_declaration) => {
          let isMutable =
            switch (decl.pld_mutable) {
            | Mutable => Doc.text("mut ")
            | Immutable => Doc.nil
            };

          let (leading_comments, trailing_comments) =
            Walktree.partition_comments(decl.pld_loc, Some(data.pdata_loc));

          let this_line = get_end_loc_line(decl.pld_loc);

          let (this_line_comments, below_line_comments) =
            split_comments(trailing_comments, this_line);

          Walktree.remove_used_comments(leading_comments, trailing_comments);

          let (stmt_leading_comment_docs_1, prevLine) =
            print_leading_comments(leading_comments, this_line);
          let stmt_leading_comment_docs =
            if (List.length(leading_comments) > 0) {
              stmt_leading_comment_docs_1;
            } else {
              Doc.nil;
            };

          (
            Doc.concat([
              Doc.group(
                Doc.concat([
                  stmt_leading_comment_docs,
                  isMutable,
                  print_ident(decl.pld_name.txt),
                  Doc.text(":"),
                  Doc.space,
                  print_type(decl.pld_type, original_source),
                ]),
              ),
            ]),
            this_line_comments,
            below_line_comments,
          );
        },
        label_declarations,
      );

    let (joinedDecls, commaTerminated) = special_join(decls);

    Doc.group(
      Doc.concat([
        Doc.text("record"),
        Doc.space,
        Doc.text(nameloc.txt),
        if (List.length(data.pdata_params) > 0) {
          Doc.concat([
            Doc.text("<"),
            Doc.join(
              Doc.concat([Doc.text(","), Doc.space]),
              List.map(
                t => print_type(t, original_source),
                data.pdata_params,
              ),
            ),
            Doc.text(">"),
            Doc.space,
          ]);
        } else {
          Doc.space;
        },
        Doc.concat([
          Doc.lbrace,
          Doc.indent(Doc.concat([Doc.line, joinedDecls])),
          if (!commaTerminated) {
            Doc.ifBreaks(Doc.comma, Doc.nil);
          } else {
            Doc.nil;
          },
          Doc.line,
          Doc.rbrace,
        ]),
      ]),
    );
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
      original_source: array(string),
    ) => {
  Doc.join(
    Doc.comma,
    List.map(
      data => {
        let (expt, decl) = data;
        Doc.concat([
          switch ((expt: Asttypes.export_flag)) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          },
          print_data(decl, original_source),
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
                Doc.space,
                Doc.text("except"),
                Doc.space,
                add_braces(
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
                        | None => print_ident(loc.txt)
                        | Some(alias) =>
                          Doc.concat([
                            print_ident(loc.txt),
                            Doc.space,
                            Doc.text("as"),
                            Doc.space,
                            print_ident(alias.txt),
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
      Doc.join(Doc.concat([Doc.comma, Doc.space]), vals),
      Doc.space,
      Doc.text("from"),
      Doc.space,
      Doc.doubleQuote,
      Doc.text(path),
      Doc.doubleQuote,
    ]),
  );
};

let print_export_desc = (desc: Parsetree.export_declaration_desc) => {
  let ident = desc.pex_name.txt;

  let fixedIdent =
    if (infixop(ident) || prefixop(ident)) {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };
  Doc.concat([
    fixedIdent,
    switch (desc.pex_alias) {
    | Some(alias) =>
      Doc.concat([
        Doc.space,
        Doc.text("as"),
        Doc.space,
        Doc.text(alias.txt),
      ])
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

let print_foreign_value_description =
    (vd: Parsetree.value_description, original_source: array(string)) => {
  let ident = vd.pval_name.txt;

  let fixedIdent =
    if (infixop(ident) || prefixop(ident)) {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };

  Doc.concat([
    fixedIdent,
    Doc.text(":"),
    Doc.space,
    print_type(vd.pval_type, original_source),
    switch (vd.pval_name_alias) {
    | None => Doc.space
    | Some(alias) =>
      Doc.concat([
        Doc.space,
        Doc.text("as"),
        Doc.space,
        Doc.text(alias.txt),
        Doc.space,
      ])
    },
    Doc.text("from"),
    Doc.space,
    Doc.text("\""),
    Doc.text(vd.pval_mod.txt),
    Doc.text("\""),
  ]);
};

let print_primitive_value_description =
    (vd: Parsetree.value_description, original_source: array(string)) => {
  let ident = vd.pval_name.txt;

  let fixedIdent =
    if (infixop(ident) || prefixop(ident)) {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };

  Doc.concat([
    fixedIdent,
    Doc.text(" : "),
    print_type(vd.pval_type, original_source),
    Doc.space,
    Doc.equal,
    Doc.space,
    Doc.text("\""),
    Doc.join(Doc.text(","), List.map(p => Doc.text(p), vd.pval_prim)),
    Doc.text("\""),
  ]);
};

let toplevel_print =
    (
      ~data: Parsetree.toplevel_stmt,
      ~original_source: array(string),
      ~previous_line: int,
    ) => {
  let attributes = data.ptop_attributes;

  let (leading_comments, _trailing_comments) =
    Walktree.partition_comments(data.ptop_loc, None);

  // check to see if we have a comment to disable formatting

  let disable_formatting =
    List.exists(is_disable_formatting_comment, leading_comments);

  Walktree.remove_used_comments(leading_comments, []);

  // remove all nodes before this top level statement

  Walktree.remove_nodes_before(data.ptop_loc);

  let (stmt_leading_comment_docs, prevLine) =
    print_leading_comments(leading_comments, previous_line);

  let stmt_leading_comment_docs =
    if (List.length(leading_comments) > 0) {
      stmt_leading_comment_docs;
    } else {
      Doc.nil;
    };

  let blank_line_above =
    if (get_loc_line(data.ptop_loc) > prevLine + 1) {
      Doc.hardLine;
    } else {
      Doc.nil;
    };
  let attribute_text =
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

  if (disable_formatting) {
    let originalCode = get_original_code(data.ptop_loc, original_source);
    // need to remove any comments that were inside the disabled block

    Walktree.remove_comments_in_ignore_block(data.ptop_loc);

    Doc.concat([
      stmt_leading_comment_docs,
      blank_line_above,
      attribute_text,
      Doc.group(Doc.text(originalCode)),
    ]);
  } else {
    let without_comments =
      switch (data.ptop_desc) {
      | PTopImport(import_declaration) => import_print(import_declaration)
      | PTopForeign(export_flag, value_description) =>
        let export =
          switch (export_flag) {
          | Nonexported => Doc.text("import ")
          | Exported => Doc.text("export ")
          };
        Doc.concat([
          export,
          Doc.text("foreign wasm "),
          print_foreign_value_description(value_description, original_source),
        ]);
      | PTopPrimitive(export_flag, value_description) =>
        let export =
          switch (export_flag) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          };
        Doc.concat([
          export,
          Doc.text("primitive "),
          print_primitive_value_description(
            value_description,
            original_source,
          ),
        ]);
      | PTopData(data_declarations) =>
        data_print(data_declarations, original_source)
      | PTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
        print_value_bind(
          export_flag,
          rec_flag,
          mut_flag,
          value_bindings,
          data.ptop_loc,
          original_source,
        )
      | PTopExpr(expression) =>
        print_expression(
          ~parentIsArrow=false,
          ~endChar=None,
          ~original_source,
          ~parent_loc=data.ptop_loc,
          expression,
        )
      | PTopException(export_flag, type_exception) =>
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
                  Doc.lparen,
                  Doc.join(
                    Doc.comma,
                    List.map(
                      t => print_type(t, original_source),
                      parsed_types,
                    ),
                  ),
                  Doc.rparen,
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
        Doc.concat([
          Doc.text("export "),
          Doc.join(
            Doc.concat([Doc.comma, Doc.line]),
            List.map(e => print_export_declaration(e), export_declarations),
          ),
        ])

      | PTopExportAll(export_excepts) =>
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

    Doc.concat([
      stmt_leading_comment_docs,
      blank_line_above,
      attribute_text,
      Doc.group(without_comments),
    ]);
  };
};

let reformat_ast =
    (
      parsed_program: Parsetree.parsed_program,
      original_source: array(string),
    ) => {
  let _ =
    Walktree.walktree(parsed_program.statements, parsed_program.comments);

  let last_stmt = ref(None);
  let previous_line = ref(0);
  let printed_doc =
    Doc.join(
      Doc.hardLine,
      List.map(
        (stmt: Grain_parsing.Parsetree.toplevel_stmt) => {
          let line =
            toplevel_print(
              ~data=stmt,
              ~original_source,
              ~previous_line=previous_line^,
            );

          previous_line := get_end_loc_line(stmt.ptop_loc);
          last_stmt := Some(stmt);
          line;
        },
        parsed_program.statements,
      ),
    );

  let (_leading_comments, trailing_comments) =
    switch (last_stmt^) {
    | None => ([], [])
    | Some(stmt) => Walktree.partition_comments(stmt.ptop_loc, None)
    };

  let trailing_comment_docs =
    if (List.length(trailing_comments) > 0) {
      Doc.concat([print_multi_comments(trailing_comments, previous_line^)]);
    } else {
      Doc.nil;
    };

  let final_doc = Doc.concat([printed_doc, trailing_comment_docs]);

  // Use this to check the generated output
  //Doc.debug(final_doc);
  //

  Doc.toString(~width=80, final_doc) |> print_endline;
  //use this to see the AST in JSON
  // print_endline(
  //   Yojson.Basic.pretty_to_string(
  //     Yojson.Safe.to_basic(
  //       Grain_parsing__Parsetree.parsed_program_to_yojson(parsed_program),
  //     ),
  //   ),
  // );
};
