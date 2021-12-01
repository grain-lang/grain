open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_diagnostics;

module Doc = Res_doc;

let exception_primitives = [|"throw", "fail", "assert"|];

let op_precedence = fn =>
  switch (fn) {
  | "*"
  | "/"
  | "%" => 120
  | "+"
  | "-"
  | "++" => 110
  | "<<"
  | ">>"
  | ">>>" => 100
  | "<"
  | "<="
  | ">"
  | ">=" => 90
  | "=="
  | "!="
  | "is"
  | "isnt" => 80
  | "&" => 70
  | "^" => 60
  | "|" => 50
  | "&&" => 40
  | "||" => 30
  | "_" => 10
  | _ => 9999
  };
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

let get_original_code_snippet =
    (location: Grain_parsing.Location.t, source: array(string)) => {
  let (_, startline, startc, _) =
    Locations.get_raw_pos_info(location.loc_start);
  let (_, endline, endc, _) = Locations.get_raw_pos_info(location.loc_end);

  if (Array.length(source) > endline - 1) {
    if (startline == endline) {
      String.sub(source[startline - 1], startc, endc - startc);
    } else {
      let text = ref("");
      for (line in startline - 1 to endline - 1) {
        if (line + 1 == startline) {
          text := text^ ++ Str.string_after(source[line], startc) ++ "\n";
        } else if (line + 1 == endline) {
          text := text^ ++ String.sub(source[line], 0, endc);
        } else {
          text := text^ ++ source[line] ++ "\n";
        };
      };
      text^;
    };
  } else {
    raise(Error(FormatterError("Requested beyond end of original source")));
  };
};

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

let line_separator =
    (
      this_line: int,
      line_above: int,
      comments: list(Grain_parsing.Parsetree.comment),
    ) =>
  if (this_line - line_above > 1) {
    if (List.length(comments) > 0) {
      let listLen = List.length(comments);
      let lastComment = List.nth(comments, listLen - 1);
      let (_, line, _, _) =
        Locations.get_raw_pos_info(
          Locations.get_comment_loc(lastComment).loc_end,
        );

      if (this_line - line > 1) {
        Doc.hardLine;
      } else {
        Doc.nil;
      };
    } else {
      Doc.hardLine;
    };
  } else {
    Doc.nil;
  };

let comments_line_separator =
    (line_above: int, comments: list(Grain_parsing.Parsetree.comment)) =>
  switch (comments) {
  | [] => Doc.nil
  | [firstComment, ...tl] =>
    let (_, line, _, _) =
      Locations.get_raw_pos_info(
        Locations.get_comment_loc(firstComment).loc_start,
      );
    if (line - line_above > 1) {
      Doc.hardLine;
    } else {
      Doc.nil;
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

let add_parens = (doc: Doc.t) =>
  Doc.concat([
    Doc.lparen,
    Doc.indent(Doc.concat([Doc.softLine, doc])),
    Doc.softLine,
    Doc.rparen,
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

let no_attribute = _ => Doc.nil;

let remove_used_comments =
    (
      comments: list(Grain_parsing__Parsetree.comment),
      all_comments: list(Grain_parsing__Parsetree.comment),
    ) => {
  List.filter(c => !List.mem(c, all_comments), comments);
};

let rec block_item_iterator =
        (
          bracket_line: int,
          items: list('a),
          previous: option('a),
          comments: list(Grain_parsing__Parsetree.comment),
          original_source,
          ~get_loc: 'a => Grain_parsing.Location.t,
          ~get_attribute_text: 'a => Doc.t,
          ~print_item: 'a => Doc.t,
          ~separator: Doc.t,
          ~trailing_separator: bool,
          ~break_separator: Doc.t,
          ~isBlock: bool,
          ~all_comments: list(Grain_parsing__Parsetree.comment),
        ) =>
  switch (items) {
  | [] => Doc.nil
  | [item, ...rem] =>
    let attribute_text = get_attribute_text(item);

    let line_above =
      switch (previous) {
      | None => bracket_line
      | Some(e) =>
        let (_, cmtsline, _, _) =
          Locations.get_raw_pos_info(get_loc(e).loc_end);
        cmtsline;
      };

    let this_item_loc = get_loc(item);

    let (_, this_line, this_char, _) =
      Locations.get_raw_pos_info(this_item_loc.loc_start);

    let line_leading_comments =
      Comment_utils.get_comments_on_line_start(
        this_line,
        this_char,
        comments,
      );

    let leading_comments =
      Comment_utils.get_comments_between_lines(
        line_above,
        this_line,
        comments,
      );

    let leading_comments_with_breaking_block =
      switch (previous) {
      | None =>
        Comment_utils.get_comments_between_lines(
          line_above,
          this_line,
          comments,
        )
      | Some(l) =>
        let lineLoc = get_loc(l);
        let (_, last_src_line, last_char, _) =
          Locations.get_raw_pos_info(lineLoc.loc_end);
        let cmts =
          Comment_utils.get_comments_between_locations(
            ~loc1=get_loc(l),
            ~loc2=get_loc(item),
            all_comments,
          );
        List.filter(
          c => {
            let (_, cmt_line, _, _) =
              Locations.get_raw_pos_info(
                Locations.get_comment_loc(c).loc_end,
              );
            cmt_line > last_src_line;
          },
          cmts,
        );
      };

    let line_sep =
      line_separator(
        this_line,
        line_above,
        leading_comments_with_breaking_block,
      );

    let checked_line_above =
      switch (previous) {
      | None => line_above
      | Some(l) =>
        let lineLoc = get_loc(l);
        let (_, last_src_line, last_char, _) =
          Locations.get_raw_pos_info(lineLoc.loc_end);

        let overhangingComments =
          Comment_utils.get_comments_on_line_end(
            last_src_line,
            last_char,
            all_comments,
          );

        if (List.length(overhangingComments) > 0) {
          let last_comment =
            List.nth(
              overhangingComments,
              List.length(overhangingComments) - 1,
            );
          let (_, last_cmt_line, _, _) =
            Locations.get_raw_pos_info(
              Locations.get_comment_loc(last_comment).loc_end,
            );

          last_cmt_line;
        } else {
          last_src_line;
        };
      };

    let comment_line_sep =
      comments_line_separator(
        checked_line_above,
        leading_comments_with_breaking_block,
      );

    let leading_comment_docs_i =
      Comment_utils.line_of_comments_to_doc_no_break(
        ~offset=false,
        leading_comments,
      );
    let leading_comment_docs =
      if (leading_comment_docs_i == Doc.nil) {
        Doc.nil;
      } else {
        Doc.concat([leading_comment_docs_i, Doc.hardLine]);
      };

    // look for disable

    let leading_comment_count = List.length(leading_comments);

    let disable_formatting =
      if (leading_comment_count > 0) {
        let lastComment =
          List.nth(leading_comments, leading_comment_count - 1);

        is_disable_formatting_comment(lastComment);
      } else {
        false;
      };

    if (disable_formatting) {
      let original_code = get_original_code(get_loc(item), original_source);
      // need to remove any comments that were inside the disabled block
      let origDoc =
        Doc.concat([
          comment_line_sep,
          leading_comment_docs,
          line_sep,
          Doc.group(Doc.text(original_code)),
        ]);
      if (List.length(items) == 1) {
        let (_, line_end, _, _) =
          Locations.get_raw_pos_info(get_loc(item).loc_end);

        let block_trailing_comments =
          Comment_utils.get_comments_after_line(line_end, comments);

        if (List.length(block_trailing_comments) > 0) {
          let block_trailing_comment_docs =
            Comment_utils.line_of_comments_to_doc_no_break(
              ~offset=false,
              block_trailing_comments,
            );

          Doc.concat([origDoc, break_separator, block_trailing_comment_docs]);
        } else {
          origDoc;
        };
      } else {
        Doc.concat([
          origDoc,
          break_separator,
          block_item_iterator(
            bracket_line,
            List.tl(items),
            Some(item),
            comments,
            original_source,
            ~get_loc,
            ~print_item,
            ~separator,
            ~trailing_separator,
            ~break_separator,
            ~get_attribute_text,
            ~isBlock,
            ~all_comments,
          ),
        ]);
      };
    } else {
      // Normal formatting

      let line_trailing_comments =
        Comment_utils.get_comments_to_end_of_line(
          ~location=get_loc(item),
          comments,
        );

      let line_end =
        Comment_utils.line_of_comments_to_doc_no_break(
          ~offset=true,
          line_trailing_comments,
        );

      let leading_comment_space =
        if (List.length(line_leading_comments) > 0) {
          Doc.space;
        } else {
          Doc.nil;
        };

      let item_doc =
        Doc.concat([
          comment_line_sep,
          leading_comment_docs,
          line_sep,
          attribute_text,
          Comment_utils.line_of_comments_to_doc_no_break(
            ~offset=false,
            line_leading_comments,
          ),
          leading_comment_space,
          print_item(item),
        ]);

      if (List.length(items) == 1) {
        let (_, line, _, _) =
          Locations.get_raw_pos_info(get_loc(item).loc_end);

        let block_trailing_comments =
          if (isBlock) {
            Comment_utils.get_comments_after_line(line, comments);
          } else {
            [];
          };

        let trail_sep =
          if (trailing_separator) {
            Doc.ifBreaks(separator, Doc.nil);
          } else {
            Doc.nil;
          };

        if (List.length(block_trailing_comments) > 0) {
          let block_trailing_comment_docs =
            Comment_utils.line_of_comments_to_doc_no_break(
              ~offset=false,
              block_trailing_comments,
            );
          let comment_line_sep =
            comments_line_separator(line, block_trailing_comments);

          Doc.concat([
            item_doc,
            trail_sep,
            line_end,
            Doc.hardLine,
            comment_line_sep,
            block_trailing_comment_docs,
          ]);
        } else {
          Doc.concat([item_doc, trail_sep, line_end]);
        };
      } else {
        let new_used_comments =
          line_leading_comments @ leading_comments @ line_trailing_comments;
        Doc.concat([
          item_doc,
          separator,
          line_end,
          Comment_utils.hard_line_needed(
            ~separator=break_separator,
            line_trailing_comments,
          ),
          block_item_iterator(
            bracket_line,
            List.tl(items),
            Some(item),
            remove_used_comments(comments, new_used_comments),
            original_source,
            ~get_loc,
            ~print_item,
            ~separator,
            ~trailing_separator,
            ~break_separator,
            ~get_attribute_text,
            ~isBlock,
            ~all_comments,
          ),
        ]);
      };
    };
  };

let rec resugar_list_patterns =
        (
          ~bracket_line,
          ~patterns: list(Parsetree.pattern),
          ~original_source: array(string),
          ~comments: list(Grain_parsing.Parsetree.comment),
          ~next_loc: Grain_parsing.Location.t,
        ) => {
  let processed_list = resugar_pattern_list_inner(patterns);

  let get_loc = (pattern: sugared_pattern_item) => {
    switch (pattern) {
    | RegularPattern(p)
    | SpreadPattern(p) => p.ppat_loc
    };
  };

  let print_item = (pattern: sugared_pattern_item) => {
    let localComments =
      Comment_utils.get_comments_inside_location(
        ~location=get_loc(pattern),
        comments,
      );

    switch (pattern) {
    | RegularPattern(e) =>
      Doc.group(
        print_pattern(
          e,
          ~original_source,
          ~comments=localComments,
          ~next_loc,
        ),
      )
    | SpreadPattern(e) =>
      Doc.group(
        Doc.concat([
          Doc.text("..."),
          print_pattern(
            e,
            ~original_source,
            ~comments=localComments,
            ~next_loc,
          ),
        ]),
      )
    };
  };

  let printed_patterns =
    block_item_iterator(
      bracket_line,
      processed_list,
      None,
      comments,
      original_source,
      ~get_loc,
      ~print_item,
      ~separator=Doc.comma,
      ~trailing_separator=false,
      ~break_separator=Doc.line,
      ~get_attribute_text=no_attribute,
      ~isBlock=true,
      ~all_comments=comments,
    );

  Doc.group(
    Doc.concat([
      Doc.indent(
        Doc.concat([
          Doc.lbracket,
          Doc.concat([Doc.softLine, printed_patterns]),
        ]),
      ),
      Doc.softLine,
      Doc.rbracket,
    ]),
  );
}

and resugar_pattern_list_inner = (patterns: list(Parsetree.pattern)) => {
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
    } else if (func == list_cons) {
      let inner = resugar_pattern_list_inner(innerpatterns);
      List.append([RegularPattern(arg1)], inner);
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
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
    ) => {
  let processed_list = resugar_list_inner(expressions);

  let last_item_was_spread = ref(false);

  let items =
    List.map(
      i =>
        switch (i) {
        | Regular(e) =>
          last_item_was_spread := false;

          Doc.group(
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments,
              e,
            ),
          );
        | Spread(e) =>
          last_item_was_spread := true;
          Doc.group(
            Doc.concat([
              Doc.text("..."),
              print_expression(
                ~parent_is_arrow=false,
                ~original_source,
                ~comments,
                e,
              ),
            ]),
          );
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
          if (last_item_was_spread^) {
            Doc.nil;
          } else {
            Doc.ifBreaks(Doc.comma, Doc.nil);
          },
        ]),
      ),
      Doc.softLine,
      Doc.rbracket,
    ]),
  );
}

and resugar_list_inner = (expressions: list(Parsetree.expression)) =>
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
        let inner = resugar_list_inner(innerexpressions);
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
      ~original_source: array(string),
      ~comments: list(Grain_parsing.Parsetree.comment),
      ~next_loc: Grain_parsing__Location.t,
      patloc: Grain_parsing__Location.t,
    ) => {
  let close =
    switch (closedflag) {
    | Open => Doc.concat([Doc.text(","), Doc.space, Doc.text("_")])
    | Closed => Doc.nil
    };

  let after_brace_comments =
    Comment_utils.get_after_brace_comments(patloc, comments);
  let remaining_comments =
    List.filter(c => !List.mem(c, after_brace_comments), comments);

  let get_loc =
      (
        patternloc: (
          Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
          Grain_parsing__Parsetree.pattern,
        ),
      ) => {
    let (_, pat) = patternloc;
    pat.ppat_loc;
  };

  let print_item =
      (
        patternloc: (
          Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
          Grain_parsing__Parsetree.pattern,
        ),
      ) => {
    let (loc, pat) = patternloc;
    let printed_ident: Doc.t = print_ident(loc.txt);

    let localComments =
      Comment_utils.get_comments_inside_location(
        ~location=get_loc(patternloc),
        comments,
      );

    let printed_pat =
      print_pattern(
        pat,
        ~original_source,
        ~comments=localComments,
        ~next_loc,
      );
    let pun =
      switch (printed_ident, printed_pat: Doc.t) {
      | (Text(i), Text(e)) => i == e
      | _ => false
      };

    if (pun) {
      printed_ident;
    } else {
      Doc.concat([printed_ident, Doc.text(":"), Doc.space, printed_pat]);
    };
  };

  let (_, bracket_line, _, _) = Locations.get_raw_pos_info(patloc.loc_end);
  let printed_fields =
    block_item_iterator(
      bracket_line,
      patternlocs,
      None,
      remaining_comments,
      original_source,
      ~get_loc,
      ~print_item,
      ~separator=Doc.comma,
      ~trailing_separator=true,
      ~break_separator=Doc.line,
      ~get_attribute_text=no_attribute,
      ~isBlock=true,
      ~all_comments=comments,
    );

  Doc.concat([
    Doc.lbrace,
    Comment_utils.line_of_comments_to_doc_no_break(
      ~offset=true,
      after_brace_comments,
    ),
    Doc.indent(
      Doc.concat([
        Comment_utils.hard_line_needed(
          ~separator=Doc.line,
          after_brace_comments,
        ),
        printed_fields,
        close,
      ]),
    ),
    Doc.line,
    Doc.rbrace,
  ]);
}

and print_pattern =
    (
      pat: Parsetree.pattern,
      ~original_source: array(string),
      ~comments: list(Grain_parsing.Parsetree.comment),
      ~next_loc: Grain_parsing.Location.t,
    ) => {
  let printed_pattern: (Doc.t, bool) =
    switch (pat.ppat_desc) {
    | PPatAny => (Doc.text("_"), false)
    | PPatConstant(c) => (
        print_constant(~original_source, ~loc=pat.ppat_loc, c),
        false,
      )
    | PPatVar({txt, _}) =>
      if (infixop(txt) || prefixop(txt)) {
        (Doc.concat([Doc.lparen, Doc.text(txt), Doc.rparen]), false);
      } else {
        (Doc.text(txt), false);
      }
    | PPatTuple(patterns) => (
        Doc.group(
          print_patterns(
            pat.ppat_loc,
            patterns,
            None,
            next_loc,
            comments,
            original_source,
          ),
        ),
        true,
      )
    | PPatArray(patterns) => (
        Doc.group(
          Doc.concat([
            Doc.lbracket,
            Doc.text(">"),
            Doc.space,
            print_patterns(
              pat.ppat_loc,
              patterns,
              None,
              next_loc,
              comments,
              original_source,
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
          ~original_source,
          ~comments,
          pat.ppat_loc,
          ~next_loc,
        ),
        false,
      )
    | PPatConstraint(pattern, parsed_type) => (
        Doc.concat([
          print_patterns(
            pat.ppat_loc,
            [pattern],
            None,
            next_loc,
            comments,
            original_source,
          ),
          Doc.concat([Doc.text(":"), Doc.space]),
          print_type(
            parsed_type,
            original_source,
            comments,
            ~trailing_separator=false,
          ),
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
        let (_, bracket_line, _, _) =
          Locations.get_raw_pos_info(pat.ppat_loc.loc_start);

        (
          resugar_list_patterns(
            ~bracket_line,
            ~patterns,
            ~original_source,
            ~comments,
            ~next_loc,
          ),
          false,
        );
      } else {
        (
          Doc.concat([
            print_ident(location.txt),
            switch (patterns) {
            | [] => Doc.nil
            | _patterns =>
              add_parens(
                print_patterns(
                  pat.ppat_loc,
                  patterns,
                  None,
                  next_loc,
                  comments,
                  original_source,
                ),
              )
            },
          ]),
          false,
        );
      };

    | PPatOr(pattern1, pattern2) =>
      /* currently unsupported so just replace with the original source */
      let original_code = get_original_code(pat.ppat_loc, original_source);

      (Doc.text(original_code), false);
    | PPatAlias(pattern, loc) =>
      /* currently unsupported so just replace with the original source */
      let original_code = get_original_code(pat.ppat_loc, original_source);

      (Doc.text(original_code), false);
    };

  let (pattern, parens) = printed_pattern;

  let with_leading = [pattern];

  let after_parens_comments =
    Comment_utils.get_comments_to_end_of_line(pat.ppat_loc, comments);
  let after_parens_comments_docs =
    Comment_utils.line_of_comments_to_doc_no_break(
      ~offset=true,
      after_parens_comments,
    );
  let with_trailing =
    if (after_parens_comments_docs == Doc.nil) {
      with_leading;
    } else {
      List.append(
        with_leading,
        [
          after_parens_comments_docs,
          Comment_utils.hard_line_needed(
            ~separator=Doc.nil,
            after_parens_comments,
          ),
        ],
      );
    };

  let clean_pattern =
    switch (with_trailing) {
    | [fst] => fst
    | _ => Doc.concat(with_trailing)
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

and print_constant =
    (
      ~original_source: array(string),
      ~loc: Grain_parsing__Location.t,
      c: Parsetree.constant,
    ) => {
  // we get the original code here to ensure it's well formatted and retains the
  // approach of the original code, e.g. char format, number format
  Doc.text(
    get_original_code_snippet(loc, original_source),
  );
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
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      recloc: Grain_parsing__Location.t,
    ) => {
  let after_brace_comments =
    Comment_utils.get_after_brace_comments(recloc, comments);
  let remaining_comments =
    List.filter(c => !List.mem(c, after_brace_comments), comments);

  let (_, bracket_line, _, _) = Locations.get_raw_pos_info(recloc.loc_start);

  let get_loc =
      (
        field: (
          Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
          Grain_parsing__Parsetree.expression,
        ),
      ) => {
    let (_, expr) = field;
    expr.pexp_loc;
  };

  let print_item =
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
        ~parent_is_arrow=false,
        ~original_source,
        ~comments=remaining_comments,
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
        Doc.concat([printed_ident, Doc.text(":"), Doc.space, printed_expr]),
      );
    } else {
      Doc.group(printed_ident);
    };
  };

  let printed_fields =
    block_item_iterator(
      bracket_line,
      fields,
      None,
      comments,
      original_source,
      ~get_loc,
      ~print_item,
      ~separator=Doc.comma,
      ~trailing_separator=List.length(fields) != 1,
      ~break_separator=Doc.line,
      ~get_attribute_text=no_attribute,
      ~isBlock=true,
      ~all_comments=comments,
    );

  Doc.concat([
    Doc.lbrace,
    Comment_utils.line_of_comments_to_doc_no_break(
      ~offset=true,
      after_brace_comments,
    ),
    Doc.indent(
      Doc.concat([
        Comment_utils.hard_line_needed(
          ~separator=Doc.line,
          after_brace_comments,
        ),
        printed_fields,
        if (List.length(fields) == 1) {
          // TODO: not needed once we annotate with ::
          Doc.comma; //  append a comma as single argument record look like block {data:val}
        } else {
          Doc.nil;
        },
      ]),
    ),
    Doc.line,
    Doc.rbrace,
  ]);
}

and print_type =
    (
      p: Grain_parsing__Parsetree.parsed_type,
      original_source: array(string),
      comments: list(Grain_parsing__Parsetree.comment),
      ~trailing_separator: bool,
    ) => {
  switch (p.ptyp_desc) {
  | PTyAny => Doc.text("_")
  | PTyVar(name) => Doc.text(name)
  | PTyArrow(types, parsed_type) =>
    Doc.concat([
      Doc.group(
        switch (types) {
        | [] => Doc.concat([Doc.lparen, Doc.rparen])
        | [t] =>
          print_type(t, original_source, comments, ~trailing_separator)
        | _types =>
          Doc.concat([
            Doc.lparen,
            Doc.indent(
              Doc.concat([
                Doc.softLine,
                Doc.join(
                  Doc.concat([Doc.comma, Doc.line]),
                  List.map(
                    t =>
                      print_type(
                        t,
                        original_source,
                        comments,
                        ~trailing_separator,
                      ),
                    types,
                  ),
                ),
              ]),
            ),
            Doc.softLine,
            Doc.rparen,
          ])
        },
      ),
      Doc.space,
      Doc.text("->"),
      Doc.space,
      print_type(parsed_type, original_source, comments, ~trailing_separator),
    ])

  | PTyTuple(parsed_types) =>
    Doc.concat([
      Doc.lparen,
      Doc.indent(
        Doc.concat([
          Doc.softLine,
          Doc.join(
            Doc.concat([Doc.comma, Doc.line]),
            List.map(
              t =>
                print_type(t, original_source, comments, ~trailing_separator),
              parsed_types,
            ),
          ),
          if (List.length(parsed_types) == 1) {
            // single arg tuple
            Doc.comma;
          } else {
            Doc.nil;
          },
        ]),
      ),
      Doc.softLine,
      Doc.rparen,
    ])

  | PTyConstr(locidentifier, parsedtypes) =>
    let ident = locidentifier.txt;
    switch (parsedtypes) {
    | [] => print_ident(ident)
    | [first, ...rem] =>
      let get_loc = (t: Grain_parsing__Parsetree.parsed_type) => {
        t.ptyp_loc;
      };
      let print_item = (t: Grain_parsing__Parsetree.parsed_type) => {
        let localComments =
          Comment_utils.get_comments_inside_location(
            ~location=get_loc(t),
            comments,
          );
        print_type(t, original_source, localComments, ~trailing_separator);
      };

      let (_, open_line, _, _) =
        Locations.get_raw_pos_info(get_loc(first).loc_end);
      let types =
        block_item_iterator(
          open_line,
          parsedtypes,
          None,
          comments,
          original_source,
          ~get_loc,
          ~print_item,
          ~separator=Doc.comma,
          ~trailing_separator=false,
          ~break_separator=Doc.line,
          ~get_attribute_text=no_attribute,
          ~isBlock=true,
          ~all_comments=comments,
        );

      Doc.group(
        Doc.concat([
          print_ident(ident),
          Doc.text("<"),
          Doc.indent(Doc.group(Doc.concat([Doc.softLine, types]))),
          Doc.softLine,
          Doc.text(">"),
        ]),
      );
    };

  | PTyPoly(locationstrings, parsed_type) =>
    let original_code = get_original_code(p.ptyp_loc, original_source);
    Doc.text(original_code);
  };
}
and print_application =
    (
      ~expressions: list(Parsetree.expression),
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      func: Parsetree.expression,
    ) => {
  let function_name = get_function_name(func);

  switch (expressions) {
  | [first] when prefixop(function_name) =>
    switch (first.pexp_desc) {
    | PExpApp(fn, _) =>
      let inner_fn = get_function_name(fn);
      if (infixop(inner_fn)) {
        Doc.concat([
          Doc.text(function_name),
          Doc.lparen,
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments,
            first,
          ),
          Doc.rparen,
        ]);
      } else {
        Doc.concat([
          Doc.text(function_name),
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments,
            first,
          ),
        ]);
      };

    | _ =>
      Doc.concat([
        Doc.text(function_name),
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          first,
        ),
      ])
    }

  | [first, second] when infixop(function_name) =>
    let left_expr =
      print_expression(
        ~parent_is_arrow=false,
        ~original_source,
        ~comments,
        first,
      );

    let right_expr =
      print_expression(
        ~parent_is_arrow=false,
        ~original_source,
        ~comments,
        second,
      );

    // wrap if in parens

    let left_is_if =
      switch (first.pexp_desc) {
      | PExpIf(_) => true
      | _ => false
      };

    let right_is_if =
      switch (second.pexp_desc) {
      | PExpIf(_) => true
      | _ => false
      };

    let (left_grouping_required, right_grouping_required) =
      switch (first.pexp_desc, second.pexp_desc) {
      | (PExpApp(fn1, _), PExpApp(fn2, _)) =>
        let left_prec = op_precedence(get_function_name(fn1));
        let right_prec = op_precedence(get_function_name(fn2));
        let parent_prec = op_precedence(function_name);

        // the equality check is needed for the function on the right
        // as we process from the left by default when the same prededence

        let neededLeft = left_prec < parent_prec;
        let neededRight = right_prec <= parent_prec;

        (neededLeft, neededRight);

      | (PExpApp(fn1, _), _) =>
        let left_prec = op_precedence(get_function_name(fn1));
        let parent_prec = op_precedence(function_name);
        if (left_prec < parent_prec) {
          (true, false);
        } else {
          (false, false);
        };
      | (_, PExpApp(fn2, _)) =>
        let parent_prec = op_precedence(function_name);
        let right_prec = op_precedence(get_function_name(fn2));
        if (right_prec <= parent_prec) {
          (false, true);
        } else {
          (false, false);
        };

      | _ => (false, false)
      };

    let left_needs_parens = false || left_is_if || left_grouping_required;
    let right_needs_parens = false || right_is_if || right_grouping_required;

    let wrapped_left =
      if (left_needs_parens) {
        Doc.concat([Doc.lparen, left_expr, Doc.rparen]);
      } else {
        left_expr;
      };

    let wrapped_right =
      if (right_needs_parens) {
        Doc.concat([Doc.lparen, right_expr, Doc.rparen]);
      } else {
        right_expr;
      };
    Doc.group(
      Doc.concat([
        Doc.group(wrapped_left),
        Doc.space,
        Doc.text(function_name),
        Doc.line,
        Doc.group(wrapped_right),
      ]),
    );

  | _ when prefixop(function_name) || infixop(function_name) =>
    raise(Error(Illegal_parse("Formatter error, wrong number of args ")))
  | _ =>
    if (function_name == list_cons) {
      resugar_list(~expressions, ~original_source, ~comments);
    } else if (Array.exists(fn => function_name == fn, exception_primitives)) {
      let first_expr = List.hd(expressions);
      Doc.concat([
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          func,
        ),
        Doc.space,
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          first_expr,
        ),
      ]);
    } else {
      Doc.group(
        switch (expressions) {
        | [] =>
          Doc.concat([
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments,
              func,
            ),
            Doc.lparen,
            Doc.rparen,
          ])
        | _ =>
          Doc.concat([
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments,
              func,
            ),
            Doc.lparen,
            Doc.indent(
              Doc.concat([
                Doc.softLine,
                Doc.join(
                  Doc.concat([Doc.comma, Doc.line]),
                  List.map(
                    e =>
                      print_expression(
                        ~parent_is_arrow=false,
                        ~original_source,
                        ~comments,
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
          ])
        },
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

and print_attributes =
    (attributes: list(Grain_parsing__Location.loc(string))) =>
  switch (attributes) {
  | [] => Doc.nil
  | _ =>
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
    ])
  }

and print_patterns =
    (
      wrapper: Grain_parsing.Location.t,
      patterns: list(Grain_parsing__Parsetree.pattern),
      previous: option(Grain_parsing__Parsetree.pattern),
      next_loc: Grain_parsing.Location.t,
      comments: list(Grain_parsing__Parsetree.comment),
      original_source: array(string),
    ) => {
  let get_loc = (p: Grain_parsing__Parsetree.pattern) => p.ppat_loc;
  let print_item = (p: Grain_parsing__Parsetree.pattern) => {
    let localComments =
      Comment_utils.get_comments_inside_location(
        ~location=get_loc(p),
        comments,
      );
    print_pattern(p, ~original_source, ~comments=localComments, ~next_loc);
  };

  switch (patterns) {
  | [] => Doc.nil
  | _ =>
    let (_, line, _, _) = Locations.get_raw_pos_info(wrapper.loc_end);

    block_item_iterator(
      line,
      patterns,
      None,
      comments,
      original_source,
      ~get_loc,
      ~print_item,
      ~separator=Doc.comma,
      ~trailing_separator=false,
      ~break_separator=Doc.line,
      ~get_attribute_text=no_attribute,
      ~isBlock=true,
      ~all_comments=comments,
    );
  };
}

and paren_wrap_patterns =
    (
      wrapper: Grain_parsing.Location.t,
      patterns: list(Grain_parsing__Parsetree.pattern),
      next_loc: Grain_parsing.Location.t,
      comments: list(Grain_parsing__Parsetree.comment),
      original_source: array(string),
    ) => {
  let args =
    print_patterns(
      wrapper,
      patterns,
      None,
      next_loc,
      comments,
      original_source,
    );

  switch (patterns) {
  | [] => Doc.concat([Doc.lparen, args, Doc.rparen])
  | [pat] =>
    switch (pat.ppat_desc) {
    | PPatVar(_) => args
    | _ => Doc.concat([Doc.lparen, args, Doc.rparen])
    }
  | _patterns =>
    let trail_sep = Doc.ifBreaks(Doc.comma, Doc.nil);

    Doc.group(
      Doc.indent(
        Doc.concat([
          Doc.softLine,
          Doc.lparen,
          Doc.indent(Doc.concat([Doc.softLine, args, trail_sep])),
          Doc.softLine,
          Doc.rparen,
        ]),
      ),
    );
  };
}
and print_expression =
    (
      ~parent_is_arrow: bool,
      ~original_source: array(string),
      ~comments: list(Grain_parsing__Parsetree.comment),
      expr: Parsetree.expression,
    ) => {
  let expression_doc =
    switch (expr.pexp_desc) {
    | PExpConstant(x) =>
      print_constant(~original_source, ~loc=expr.pexp_loc, x)
    | PExpId({txt: id}) => print_ident(id)
    | PExpLet(rec_flag, mut_flag, vbs) =>
      print_value_bind(
        Asttypes.Nonexported,
        rec_flag,
        mut_flag,
        vbs,
        original_source,
        comments,
      )
    | PExpTuple(expressions) =>
      let get_loc = (e: Grain_parsing__Parsetree.expression) => {
        e.pexp_loc;
      };
      let print_item = (e: Grain_parsing__Parsetree.expression) => {
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          e,
        );
      };
      let (_, bracket_line, _, _) =
        Locations.get_raw_pos_info(expr.pexp_loc.loc_end);
      Doc.group(
        Doc.concat([
          Doc.lparen,
          Doc.indent(
            Doc.concat([
              Doc.softLine,
              block_item_iterator(
                bracket_line,
                expressions,
                None,
                comments,
                original_source,
                ~get_loc,
                ~print_item,
                ~separator=Doc.comma,
                ~trailing_separator=true,
                ~break_separator=Doc.line,
                ~get_attribute_text=no_attribute,
                ~isBlock=true,
                ~all_comments=comments,
              ),
            ]),
          ),
          if (List.length(expressions) == 1) {
            // single arg tuple
            Doc.ifBreaks(
              Doc.nil,
              Doc.comma // looks backwards but we already added one if the line breaks
            );
          } else {
            Doc.nil;
          },
          Doc.softLine,
          Doc.rparen,
        ]),
      );

    | PExpArray(expressions) =>
      let get_loc = (e: Grain_parsing__Parsetree.expression) => {
        e.pexp_loc;
      };
      let print_item = (e: Grain_parsing__Parsetree.expression) => {
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          e,
        );
      };
      let (_, bracket_line, _, _) =
        Locations.get_raw_pos_info(expr.pexp_loc.loc_end);
      Doc.group(
        switch (expressions) {
        | [] => Doc.text("[>]")
        | _ =>
          Doc.concat([
            Doc.lbracket,
            Doc.text("> "),
            Doc.indent(
              Doc.concat([
                Doc.softLine,
                block_item_iterator(
                  bracket_line,
                  expressions,
                  None,
                  comments,
                  original_source,
                  ~get_loc,
                  ~print_item,
                  ~separator=Doc.comma,
                  ~trailing_separator=true,
                  ~break_separator=Doc.line,
                  ~get_attribute_text=no_attribute,
                  ~isBlock=true,
                  ~all_comments=comments,
                ),
              ]),
            ),
            Doc.softLine,
            Doc.rbracket,
          ])
        },
      );
    | PExpArrayGet(expression1, expression2) =>
      Doc.concat([
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          expression1,
        ),
        Doc.lbracket,
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          expression2,
        ),
        Doc.rbracket,
      ])
    | PExpArraySet(expression1, expression2, expression3) =>
      Doc.group(
        Doc.concat([
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments,
            expression1,
          ),
          Doc.lbracket,
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments,
            expression2,
          ),
          Doc.rbracket,
          Doc.space,
          Doc.text("="),
          Doc.indent(
            Doc.concat([
              Doc.space,
              print_expression(
                ~parent_is_arrow=false,
                ~original_source,
                ~comments,
                expression3,
              ),
            ]),
          ),
        ]),
      )

    | PExpRecord(record) =>
      print_record(~fields=record, ~original_source, ~comments, expr.pexp_loc)
    | PExpRecordGet(expression, {txt, _}) =>
      Doc.concat([
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          expression,
        ),
        Doc.dot,
        print_ident(txt),
      ])
    | PExpRecordSet(expression, {txt, _}, expression2) =>
      Doc.concat([
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          expression,
        ),
        Doc.dot,
        print_ident(txt),
        Doc.space,
        Doc.equal,
        Doc.space,
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          expression2,
        ),
      ])
    | PExpMatch(expression, match_branches) =>
      let after_brace_comments =
        Comment_utils.get_after_brace_comments(expression.pexp_loc, comments);

      let arg =
        Doc.concat([
          Doc.lparen,
          Doc.group(
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments,
              expression,
            ),
          ),
          Doc.rparen,
        ]);

      let (_, bracket_line, _, _) =
        Locations.get_raw_pos_info(expression.pexp_loc.loc_end);

      let get_loc = (branch: Grain_parsing__Parsetree.match_branch) => {
        branch.pmb_loc;
      };

      let print_item = (branch: Grain_parsing__Parsetree.match_branch) => {
        let branch_comments =
          Comment_utils.get_comments_inside_location(
            ~location=branch.pmb_loc,
            comments,
          );

        Doc.group(
          Doc.concat([
            Doc.concat([
              Doc.group(
                print_pattern(
                  branch.pmb_pat,
                  ~original_source,
                  ~comments=branch_comments,
                  ~next_loc=
                    switch (branch.pmb_guard) {
                    | None => branch.pmb_body.pexp_loc
                    | Some(b) => b.pexp_loc
                    },
                ),
              ),
              switch (branch.pmb_guard) {
              | None => Doc.nil
              | Some(guard) =>
                Doc.concat([
                  Doc.space,
                  Doc.text("when"),
                  Doc.space,
                  Doc.group(
                    print_expression(
                      ~parent_is_arrow=false,
                      ~original_source,
                      ~comments=branch_comments,
                      guard,
                    ),
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
                    ~parent_is_arrow=true,
                    ~original_source,
                    ~comments=branch_comments,
                    branch.pmb_body,
                  ),
                ])
              | _ =>
                Doc.indent(
                  Doc.concat([
                    Doc.line,
                    print_expression(
                      ~parent_is_arrow=true,
                      ~original_source,
                      ~comments=branch_comments,
                      branch.pmb_body,
                    ),
                  ]),
                )
              },
            ),
          ]),
        );
      };

      let printed_branches =
        block_item_iterator(
          bracket_line,
          match_branches,
          None,
          comments,
          original_source,
          ~get_loc,
          ~print_item,
          ~separator=Doc.comma,
          ~trailing_separator=true,
          ~break_separator=Doc.hardLine,
          ~get_attribute_text=no_attribute,
          ~isBlock=true,
          ~all_comments=comments,
        );

      Doc.breakableGroup(
        ~forceBreak=false,
        Doc.concat([
          Doc.concat([Doc.text("match "), arg, Doc.space]),
          Doc.lbrace,
          Comment_utils.line_of_comments_to_doc_no_break(
            ~offset=true,
            after_brace_comments,
          ),
          Doc.indent(
            Doc.concat([
              Comment_utils.hard_line_needed(
                ~separator=Doc.line,
                after_brace_comments,
              ),
              printed_branches,
            ]),
          ),
          Doc.line,
          Doc.rbrace,
        ]),
      );

    | PExpPrim1(prim1, expression) =>
      let original_code = get_original_code(expr.pexp_loc, original_source);
      Doc.text(original_code);
    | PExpPrim2(prim2, expression, expression1) =>
      let original_code = get_original_code(expr.pexp_loc, original_source);
      Doc.text(original_code);
    | PExpPrimN(primn, expressions) =>
      let original_code = get_original_code(expr.pexp_loc, original_source);
      Doc.text(original_code);
    | PExpIf(condition, trueExpr, falseExpr) =>
      let condLeadingCmt =
        Comment_utils.get_comments_from_start_of_enclosing_location(
          ~wrapper=expr.pexp_loc,
          ~location=condition.pexp_loc,
          comments,
        );

      let condTrailingCmt =
        Comment_utils.get_comments_between_locs(
          ~loc1=condition.pexp_loc,
          ~loc2=trueExpr.pexp_loc,
          comments,
        );

      let trueTrailingCmt =
        Comment_utils.get_comments_between_locs(
          ~loc1=trueExpr.pexp_loc,
          ~loc2=falseExpr.pexp_loc,
          comments,
        );

      let true_is_block =
        switch (trueExpr.pexp_desc) {
        | PExpBlock(_) => true
        | _ => false
        };

      let false_is_block =
        switch (falseExpr.pexp_desc) {
        | PExpBlock(expressions) => List.length(expressions) > 0
        | _ => false
        };

      //  let true_false_space = Doc.space;
      //  keep this - we need this if we force single lines into block expressions
      let true_false_space =
        switch (trueExpr.pexp_desc) {
        | PExpBlock(expressions) => Doc.space
        | _ => if (false_is_block) {Doc.space} else {Doc.line}
        };

      let commentsInCondition =
        Comment_utils.get_comments_inside_location(
          ~location=condition.pexp_loc,
          comments,
        );

      let comments_in_true_statement =
        Comment_utils.get_comments_inside_location(
          ~location=trueExpr.pexp_loc,
          comments,
        );

      let true_clause =
        switch (trueExpr.pexp_desc) {
        | PExpBlock(expressions) =>
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments=comments_in_true_statement,
            trueExpr,
          )

        | _ =>
          if (false_is_block) {
            Doc.concat([
              Doc.lbrace,
              // no comment to add here as this was a single line expression
              Doc.indent(
                Doc.concat([
                  Doc.hardLine,
                  print_expression(
                    ~parent_is_arrow=false,
                    ~original_source,
                    ~comments=comments_in_true_statement,
                    trueExpr,
                  ),
                ]),
              ),
              Doc.hardLine,
              Doc.rbrace,
            ]);
          } else {
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments=comments_in_true_statement,
              trueExpr,
            );
          }
        };

      let comments_in_false_statement =
        Comment_utils.get_comments_inside_location(
          ~location=falseExpr.pexp_loc,
          comments,
        );

      let false_clause =
        switch (falseExpr.pexp_desc) {
        | PExpBlock(expressions) =>
          switch (expressions) {
          | [] => Doc.nil
          | _ =>
            Doc.concat([
              true_false_space,
              Doc.text("else "),
              print_expression(
                ~parent_is_arrow=false,
                ~original_source,
                ~comments=comments_in_false_statement,
                falseExpr,
              ),
            ])
          }
        | PExpIf(_condition, _trueExpr, _falseExpr) =>
          Doc.concat([
            true_false_space,
            Doc.text("else "),
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments=comments_in_false_statement,
              falseExpr,
            ),
          ])
        | _ =>
          Doc.concat([
            true_false_space,
            Doc.text("else"),
            Doc.space,
            if (true_is_block) {
              Doc.group(
                Doc.concat([
                  Doc.space,
                  Doc.lbrace,
                  // no comments to add here as original was single line
                  Doc.indent(
                    Doc.concat([
                      Doc.hardLine,
                      print_expression(
                        ~parent_is_arrow=false,
                        ~original_source,
                        ~comments=comments_in_false_statement,
                        falseExpr,
                      ),
                    ]),
                  ),
                  Doc.hardLine,
                  Doc.rbrace,
                ]),
              );
            } else {
              print_expression(
                ~parent_is_arrow=false,
                ~original_source,
                ~comments=comments_in_false_statement,
                falseExpr,
              );
            },
          ])
        };

      if (parent_is_arrow) {
        Doc.group(
          Doc.concat([
            Doc.text("if"),
            Doc.space,
            Doc.group(
              Doc.concat([
                Doc.lparen,
                Comment_utils.comments_to_docs(~offset=false, condLeadingCmt),
                switch (condLeadingCmt) {
                | [] => Doc.nil
                | _ => Doc.space
                },
                print_expression(
                  ~parent_is_arrow=false,
                  ~original_source,
                  ~comments=commentsInCondition,
                  condition,
                ),
                Comment_utils.comments_to_docs(~offset=true, condTrailingCmt),
                Doc.rparen,
                Doc.space,
              ]),
            ),
            true_clause,
            Comment_utils.comments_to_docs(~offset=true, trueTrailingCmt),
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
              Comment_utils.comments_to_docs(~offset=false, condLeadingCmt),
              switch (condLeadingCmt) {
              | [] => Doc.nil
              | _ => Doc.space
              },
              print_expression(
                ~parent_is_arrow=false,
                ~original_source,
                ~comments=commentsInCondition,
                condition,
              ),
              Comment_utils.comments_to_docs(~offset=true, condTrailingCmt),
              Doc.rparen,
              Doc.space,
            ]),
          ),
          true_clause,
          Comment_utils.comments_to_docs(~offset=true, trueTrailingCmt),
          false_clause,
        ]);
      };
    | PExpWhile(expression, expression1) =>
      let comments_in_expression =
        Comment_utils.get_comments_inside_location(
          ~location=expression.pexp_loc,
          comments,
        );
      let comments_in_expression_1 =
        Comment_utils.get_comments_inside_location(
          ~location=expression1.pexp_loc,
          comments,
        );
      Doc.concat([
        Doc.text("while"),
        Doc.space,
        Doc.group(
          Doc.concat([
            Doc.lparen,
            Doc.indent(
              Doc.concat([
                Doc.softLine,
                print_expression(
                  ~parent_is_arrow=false,
                  ~original_source,
                  ~comments=comments_in_expression,
                  expression,
                ),
              ]),
            ),
            Doc.softLine,
            Doc.rparen,
          ]),
        ),
        Doc.space,
        Doc.group(
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments=comments_in_expression_1,
            expression1,
          ),
        ),
      ]);

    | PExpFor(optexpression1, optexpression2, optexpression3, expression4) =>
      let comments_in_expression4 =
        Comment_utils.get_comments_inside_location(
          ~location=expression4.pexp_loc,
          comments,
        );
      Doc.concat([
        Doc.group(
          Doc.concat([
            Doc.text("for"),
            Doc.space,
            Doc.lparen,
            Doc.indent(
              Doc.concat([
                Doc.softLine,
                Doc.concat([
                  switch (optexpression1) {
                  | Some(expr) =>
                    print_expression(
                      ~parent_is_arrow=false,
                      ~original_source,
                      ~comments,
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
                          ~parent_is_arrow=false,
                          ~original_source,
                          ~comments,
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
                          ~parent_is_arrow=false,
                          ~original_source,
                          ~comments,
                          expr,
                        ),
                      ),
                    ])
                  | None => Doc.nil
                  },
                ]),
              ]),
            ),
            Doc.softLine,
            Doc.rparen,
          ]),
        ),
        Doc.space,
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments=comments_in_expression4,
          expression4,
        ),
      ]);
    | PExpContinue => Doc.group(Doc.concat([Doc.text("continue")]))
    | PExpBreak => Doc.group(Doc.concat([Doc.text("break")]))
    | PExpConstraint(expression, parsed_type) =>
      let comments_in_expression =
        Comment_utils.get_comments_inside_location(
          ~location=expression.pexp_loc,
          comments,
        );

      Doc.group(
        Doc.concat([
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments=comments_in_expression,
            expression,
          ),
          Doc.text(":"),
          Doc.space,
          Doc.indent(
            Doc.concat([
              Doc.softLine,
              Doc.lparen, // TODO needed to fix compiler bug (trailing type annotation needs paren, #866)
              print_type(
                parsed_type,
                original_source,
                comments,
                ~trailing_separator=false,
              ),
              Doc.rparen,
            ]),
          ),
        ]),
      );
    | PExpLambda(patterns, expression) =>
      let comments_in_expression =
        Comment_utils.get_comments_inside_location(
          ~location=expression.pexp_loc,
          comments,
        );

      let patterns_comments =
        Comment_utils.get_comments_enclosed_and_before_location(
          ~loc1=expr.pexp_loc,
          ~loc2=expression.pexp_loc,
          comments,
        );

      let args =
        paren_wrap_patterns(
          expr.pexp_loc,
          patterns,
          expression.pexp_loc, // next loc
          patterns_comments,
          original_source,
        );

      let followsArrow =
        switch (expression.pexp_desc) {
        | PExpBlock(_)
        | PExpLambda(_) => [
            Doc.group(
              Doc.concat([args, Doc.space, Doc.text("=>"), Doc.space]),
            ),
            Doc.group(
              print_expression(
                ~parent_is_arrow=false,
                ~original_source,
                ~comments=comments_in_expression,
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
                      ~parent_is_arrow=false,
                      ~original_source,
                      ~comments=comments_in_expression,
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
      print_application(~expressions, ~original_source, ~comments, func)
    | PExpBlock(expressions) =>
      let after_brace_comments =
        Comment_utils.get_after_brace_comments(expr.pexp_loc, comments);
      let remaining_comments =
        List.filter(c => !List.mem(c, after_brace_comments), comments);
      switch (expressions) {
      | [] =>
        // I think not legal syntac
        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat([
            Doc.lbrace,
            Doc.indent(Doc.line),
            Doc.line,
            Doc.rbrace,
          ]),
        )
      | _ =>
        let get_loc = (expr: Grain_parsing__Parsetree.expression) => {
          expr.pexp_loc;
        };

        let print_item = (expr: Grain_parsing__Parsetree.expression) => {
          let commentsInExpr =
            Comment_utils.get_comments_inside_location(
              ~location=expr.pexp_loc,
              remaining_comments,
            );

          Doc.concat([
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments=commentsInExpr,
              expr,
            ),
          ]);
        };

        let (_, bracket_line, _, _) =
          Locations.get_raw_pos_info(expr.pexp_loc.loc_start);

        let printed_expressions =
          block_item_iterator(
            bracket_line,
            expressions,
            None,
            comments,
            original_source,
            ~get_loc,
            ~print_item,
            ~separator=Doc.nil,
            ~trailing_separator=true,
            ~break_separator=Doc.hardLine,
            ~get_attribute_text=
              expr => print_attributes(expr.pexp_attributes),
            ~isBlock=true,
            ~all_comments=comments,
          );

        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat([
            Doc.lbrace,
            Comment_utils.line_of_comments_to_doc_no_break(
              ~offset=true,
              after_brace_comments,
            ),
            Doc.indent(
              Doc.concat([
                Comment_utils.hard_line_needed(
                  ~separator=Doc.line,
                  after_brace_comments,
                ),
                printed_expressions,
              ]),
            ),
            Doc.line,
            Doc.rbrace,
          ]),
        );
      };

    | PExpBoxAssign(expression, expression1) =>
      Doc.concat([
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
          expression,
        ),
        Doc.space,
        Doc.text(":="),
        Doc.space,
        print_expression(
          ~parent_is_arrow=false,
          ~original_source,
          ~comments,
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
            ~parent_is_arrow=false,
            ~original_source,
            expression,
            ~comments,
          );

        let leftMatchesFirst =
          switch (expressions) {
          | [expr, ...remainder] =>
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments,
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
                ~parent_is_arrow=false,
                ~original_source,
                ~comments,
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
                  ~parent_is_arrow=false,
                  ~original_source,
                  ~comments,
                  expression,
                )
              | [expression1, expression2, ...rest] =>
                print_expression(
                  ~parent_is_arrow=false,
                  ~original_source,
                  ~comments,
                  expression2,
                )
              },
            ]);
          | _ =>
            Doc.concat([
              print_expression(
                ~parent_is_arrow=false,
                ~original_source,
                ~comments,
                expression,
              ),
              Doc.space,
              Doc.equal,
              Doc.space,
              print_expression(
                ~parent_is_arrow=false,
                ~original_source,
                ~comments,
                expression1,
              ),
            ])
          };
        } else {
          Doc.concat([
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments,
              expression,
            ),
            Doc.space,
            Doc.equal,
            Doc.space,
            print_expression(
              ~parent_is_arrow=false,
              ~original_source,
              ~comments,
              expression1,
            ),
          ]);
        };

      | _ =>
        Doc.concat([
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments,
            expression,
          ),
          Doc.space,
          Doc.equal,
          Doc.space,
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments,
            expression1,
          ),
        ])
      }

    | /** Used for modules without body expressions */ PExpNull => Doc.nil
    };

  expression_doc;
}
and print_value_bind =
    (
      export_flag: Asttypes.export_flag,
      rec_flag: Grain_parsing.Asttypes.rec_flag,
      mut_flag: Grain_parsing.Asttypes.mut_flag,
      vbs: list(Parsetree.value_binding),
      original_source: array(string),
      comments: list(Parsetree.comment),
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
    switch (vbs) {
    | [] => Doc.nil
    | [first, ...rem] =>
      let get_loc = (vb: Parsetree.value_binding) => vb.pvb_loc;
      let print_item = (vb: Parsetree.value_binding) => {
        let after_let_comments =
          Comment_utils.get_comments_enclosed_and_before_location(
            ~loc1=get_loc(vb),
            ~loc2=vb.pvb_pat.ppat_loc,
            comments,
          );

        let after_let_comments_docs =
          switch (after_let_comments) {
          | [] => Doc.nil
          | _ =>
            Comment_utils.line_of_comments_to_doc_no_break(
              ~offset=false,
              after_let_comments,
            )
          };

        let exprComments =
          Comment_utils.get_comments_inside_location(
            ~location=vb.pvb_expr.pexp_loc,
            comments,
          );
        let expression =
          print_expression(
            ~parent_is_arrow=false,
            ~original_source,
            ~comments=exprComments,
            vb.pvb_expr,
          );

        let expressionGrp =
          switch (vb.pvb_expr.pexp_desc) {
          | PExpBlock(_) => Doc.concat([Doc.space, expression])
          | _ => Doc.concat([Doc.space, expression])
          };

        let patternComments =
          Comment_utils.get_comments_enclosed_and_before_location(
            ~loc1=vb.pvb_loc,
            ~loc2=vb.pvb_expr.pexp_loc,
            comments,
          );

        Doc.concat([
          Doc.group(
            print_pattern(
              vb.pvb_pat,
              ~original_source,
              ~comments=patternComments,
              ~next_loc=vb.pvb_loc,
            ),
          ),
          after_let_comments_docs,
          Comment_utils.hard_line_needed(
            ~separator=Doc.nil,
            after_let_comments,
          ),
          switch (after_let_comments) {
          | [] => Doc.space
          | _ => Doc.nil
          },
          Doc.equal,
          Doc.group(expressionGrp),
        ]);
      };

      let (_, open_line, _, _) =
        Locations.get_raw_pos_info(get_loc(first).loc_end);

      block_item_iterator(
        open_line,
        vbs,
        None,
        comments,
        original_source,
        ~get_loc,
        ~print_item,
        ~separator=Doc.comma,
        ~trailing_separator=false,
        ~break_separator=Doc.line,
        ~get_attribute_text=no_attribute,
        ~isBlock=true,
        ~all_comments=comments,
      );
    };

  Doc.concat([
    Doc.group(
      Doc.concat([
        exported,
        Doc.text("let"),
        Doc.space,
        recursive,
        mutble,
        value_bindings,
      ]),
    ),
  ]);
};

let rec print_data =
        (
          data: Grain_parsing__Parsetree.data_declaration,
          original_source: array(string),
          comments: list(Grain_parsing.Parsetree.comment),
        ) => {
  let nameloc = data.pdata_name;
  switch (data.pdata_kind) {
  | PDataAbstract =>
    let get_loc = (t: Grain_parsing__Parsetree.parsed_type) => t.ptyp_loc;
    let print_item = (t: Grain_parsing__Parsetree.parsed_type) => {
      let localComments =
        Comment_utils.get_comments_inside_location(
          ~location=get_loc(t),
          comments,
        );
      print_type(
        t,
        original_source,
        localComments,
        ~trailing_separator=false,
      );
    };

    let (_, open_line, _, _) =
      Locations.get_raw_pos_info(data.pdata_name.loc.loc_end);

    // weird comment position after the equals
    let after_brace_comments =
      switch (data.pdata_params) {
      | [] => Comment_utils.get_after_brace_comments(data.pdata_loc, comments)
      | [first, ...rem] =>
        Comment_utils.get_comments_enclosed_and_before_location(
          ~loc1=data.pdata_name.loc,
          ~loc2=first.ptyp_loc,
          comments,
        )
      };

    let remaining_comments =
      List.filter(c => !List.mem(c, after_brace_comments), comments);

    let types =
      block_item_iterator(
        open_line,
        data.pdata_params,
        None,
        remaining_comments,
        original_source,
        ~get_loc,
        ~print_item,
        ~separator=Doc.comma,
        ~trailing_separator=false,
        ~break_separator=Doc.line,
        ~get_attribute_text=no_attribute,
        ~isBlock=true,
        ~all_comments=comments,
      );

    if (List.length(data.pdata_params) == 0) {
      Doc.concat([
        Doc.text("type"),
        Doc.space,
        Doc.text(data.pdata_name.txt),
        Doc.group(
          Doc.concat([
            switch (data.pdata_manifest) {
            | Some(manifest) =>
              Doc.concat([
                Doc.space,
                Doc.equal,
                Doc.concat([
                  Comment_utils.line_of_comments_to_doc_no_break(
                    ~offset=true,
                    after_brace_comments,
                  ),
                ]),
                Doc.indent(
                  Doc.concat([
                    if (List.length(after_brace_comments) > 0) {
                      Comment_utils.hard_line_needed(
                        ~separator=Doc.line,
                        after_brace_comments,
                      );
                    } else {
                      Doc.space;
                    },
                    print_type(
                      manifest,
                      original_source,
                      remaining_comments,
                      ~trailing_separator=false,
                    ),
                  ]),
                ),
              ])
            | None => Doc.nil
            },
          ]),
        ),
      ]);
    } else {
      let params = [
        Doc.text("<"),
        if (List.length(after_brace_comments) > 0) {
          Comment_utils.line_of_comments_to_doc_no_break(
            ~offset=true,
            after_brace_comments,
          );
        } else {
          Doc.nil;
        },
        Doc.indent(
          Doc.concat([
            Comment_utils.hard_line_needed(
              ~separator=Doc.softLine,
              after_brace_comments,
            ),
            types,
          ]),
        ),
        Doc.softLine,
        Doc.text(">"),
      ];
      Doc.concat([
        Doc.text("type"),
        Doc.space,
        Doc.group(Doc.concat([Doc.text(data.pdata_name.txt), ...params])),
        Doc.group(
          Doc.concat([
            switch (data.pdata_manifest) {
            | Some(manifest) =>
              Doc.concat([
                Doc.space,
                Doc.equal,
                Doc.space,
                print_type(
                  manifest,
                  original_source,
                  remaining_comments,
                  ~trailing_separator=false,
                ),
              ])
            | None => Doc.nil
            },
          ]),
        ),
      ]);
    };

  | PDataVariant(constr_declarations) =>
    let get_loc = (lbl: Grain_parsing.Parsetree.constructor_declaration) => {
      lbl.pcd_loc;
    };

    let print_item = (d: Grain_parsing.Parsetree.constructor_declaration) => {
      let localComments =
        Comment_utils.get_comments_inside_location(
          ~location=get_loc(d),
          comments,
        );

      Doc.concat([
        Doc.group(
          Doc.concat([
            Doc.text(d.pcd_name.txt),
            switch (d.pcd_args) {
            | PConstrTuple(parsed_types) =>
              switch (parsed_types) {
              | [] => Doc.nil
              | [first, ...rem] =>
                let get_loc = (t: Grain_parsing.Parsetree.parsed_type) =>
                  t.ptyp_loc;
                let print_item = (t: Grain_parsing.Parsetree.parsed_type) => {
                  let localComments =
                    Comment_utils.get_comments_inside_location(
                      ~location=get_loc(t),
                      comments,
                    );
                  print_type(
                    t,
                    original_source,
                    localComments,
                    ~trailing_separator=true,
                  );
                };

                let (_, open_line, _, _) =
                  Locations.get_raw_pos_info(get_loc(first).loc_end);

                Doc.group(
                  Doc.concat([
                    Doc.lparen,
                    Doc.indent(
                      Doc.concat([
                        Doc.softLine,
                        block_item_iterator(
                          open_line,
                          parsed_types,
                          None,
                          localComments,
                          original_source,
                          ~get_loc,
                          ~print_item,
                          ~separator=Doc.comma,
                          ~trailing_separator=false,
                          ~break_separator=Doc.line,
                          ~get_attribute_text=no_attribute,
                          ~isBlock=true,
                          ~all_comments=comments,
                        ),
                      ]),
                    ),
                    Doc.softLine,
                    Doc.rparen,
                  ]),
                );
              }
            | PConstrSingleton => Doc.nil
            },
          ]),
        ),
      ]);
    };

    let (_, bracket_line, _, _) =
      Locations.get_raw_pos_info(data.pdata_loc.loc_start);

    let printed_decls =
      block_item_iterator(
        bracket_line,
        constr_declarations,
        None,
        comments,
        original_source,
        ~get_loc,
        ~print_item,
        ~separator=Doc.comma,
        ~trailing_separator=true,
        ~break_separator=Doc.line,
        ~get_attribute_text=no_attribute,
        ~isBlock=true,
        ~all_comments=comments,
      );

    let after_brace_comments =
      Comment_utils.get_after_brace_comments(data.pdata_loc, comments);

    Doc.group(
      Doc.concat([
        Doc.text("enum"),
        Doc.space,
        Doc.text(nameloc.txt),
        switch (data.pdata_params) {
        | [] => Doc.space
        | [first, ...rem] =>
          let get_loc = (t: Grain_parsing.Parsetree.parsed_type) =>
            t.ptyp_loc;
          let print_item = (t: Grain_parsing.Parsetree.parsed_type) => {
            let localComments =
              Comment_utils.get_comments_inside_location(
                ~location=get_loc(t),
                comments,
              );
            print_type(
              t,
              original_source,
              localComments,
              ~trailing_separator=false,
            );
          };

          let (_, open_line, _, _) =
            Locations.get_raw_pos_info(get_loc(first).loc_end);
          Doc.group(
            Doc.concat([
              Doc.text("<"),
              Doc.indent(
                Doc.concat([
                  Doc.softLine,
                  block_item_iterator(
                    open_line,
                    data.pdata_params,
                    None,
                    comments,
                    original_source,
                    ~get_loc,
                    ~print_item,
                    ~separator=Doc.comma,
                    ~trailing_separator=false,
                    ~break_separator=Doc.line,
                    ~get_attribute_text=no_attribute,
                    ~isBlock=true,
                    ~all_comments=comments,
                  ),
                ]),
              ),
              Doc.softLine,
              Doc.text(">"),
              Doc.space,
            ]),
          );
        },
        Doc.lbrace,
        Comment_utils.line_of_comments_to_doc_no_break(
          ~offset=true,
          after_brace_comments,
        ),
        Doc.indent(
          Doc.concat([
            Comment_utils.hard_line_needed(
              ~separator=Doc.line,
              after_brace_comments,
            ),
            printed_decls,
          ]),
        ),
        Doc.line,
        Doc.rbrace,
      ]),
    );

  | PDataRecord(label_declarations) =>
    let get_loc = (lbl: Grain_parsing.Parsetree.label_declaration) => {
      lbl.pld_loc;
    };

    let print_item = (lbl: Grain_parsing.Parsetree.label_declaration) => {
      let localComments =
        Comment_utils.get_comments_inside_location(
          ~location=get_loc(lbl),
          comments,
        );
      let isMutable =
        switch (lbl.pld_mutable) {
        | Mutable => Doc.text("mut ")
        | Immutable => Doc.nil
        };
      Doc.concat([
        isMutable,
        print_ident(lbl.pld_name.txt),
        Doc.text(":"),
        Doc.space,
        print_type(
          lbl.pld_type,
          original_source,
          localComments,
          ~trailing_separator=true,
        ),
      ]);
    };

    let (_, bracket_line, _, _) =
      Locations.get_raw_pos_info(data.pdata_loc.loc_start);
    let printed_decls =
      block_item_iterator(
        bracket_line,
        label_declarations,
        None,
        comments,
        original_source,
        ~get_loc,
        ~print_item,
        ~separator=Doc.comma,
        ~trailing_separator=true,
        ~break_separator=Doc.line,
        ~get_attribute_text=no_attribute,
        ~isBlock=true,
        ~all_comments=comments,
      );

    let after_brace_comments =
      Comment_utils.get_after_brace_comments(data.pdata_loc, comments);

    Doc.group(
      Doc.concat([
        Doc.text("record"),
        Doc.space,
        Doc.text(nameloc.txt),
        switch (data.pdata_params) {
        | [] => Doc.space
        | [first, ...rem] =>
          let get_loc = (t: Grain_parsing.Parsetree.parsed_type) =>
            t.ptyp_loc;
          let print_item = (t: Grain_parsing.Parsetree.parsed_type) => {
            let localComments =
              Comment_utils.get_comments_inside_location(
                ~location=get_loc(t),
                comments,
              );
            print_type(
              t,
              original_source,
              localComments,
              ~trailing_separator=false,
            );
          };

          let (_, open_line, _, _) =
            Locations.get_raw_pos_info(get_loc(first).loc_end);
          Doc.group(
            Doc.concat([
              Doc.text("<"),
              Doc.indent(
                Doc.concat([
                  Doc.softLine,
                  block_item_iterator(
                    open_line,
                    data.pdata_params,
                    None,
                    comments,
                    original_source,
                    ~get_loc,
                    ~print_item,
                    ~separator=Doc.comma,
                    ~trailing_separator=false,
                    ~break_separator=Doc.line,
                    ~get_attribute_text=no_attribute,
                    ~isBlock=true,
                    ~all_comments=comments,
                  ),
                ]),
              ),
              Doc.softLine,
              Doc.text(">"),
              Doc.space,
            ]),
          );
        },
        Doc.concat([
          Doc.lbrace,
          Comment_utils.line_of_comments_to_doc_no_break(
            ~offset=true,
            after_brace_comments,
          ),
          Doc.indent(
            Doc.concat([
              Comment_utils.hard_line_needed(
                ~separator=Doc.line,
                after_brace_comments,
              ),
              printed_decls,
            ]),
          ),
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
      comments: list(Grain_parsing__Parsetree.comment),
    ) => {
  Doc.join(
    Doc.concat([Doc.comma, Doc.hardLine]),
    List.map(
      data => {
        let (expt, decl) = data;
        Doc.concat([
          switch ((expt: Asttypes.export_flag)) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          },
          print_data(decl, original_source, comments),
        ]);
      },
      datas,
    ),
  );
};
let import_print =
    (
      imp: Parsetree.import_declaration,
      comments: list(Grain_parsing__Parsetree.comment),
      original_source: array(string),
    ) => {
  let vals =
    List.map(
      (v: Parsetree.import_value) => {
        switch (v) {
        | PImportModule(identloc) => print_ident(identloc.txt)
        | PImportAllExcept(identlocs) =>
          Doc.concat([
            Doc.text("*"),
            switch (identlocs) {
            | [] => Doc.nil
            | [first, ...rem] =>
              let get_loc =
                  (identloc: Location.loc(Grain_parsing__Identifier.t)) => {
                identloc.loc;
              };

              let print_item =
                  (identloc: Location.loc(Grain_parsing__Identifier.t)) => {
                print_ident(identloc.txt);
              };

              let (_, bracket_line, _, _) =
                Locations.get_raw_pos_info(get_loc(first).loc_end);

              let exceptions =
                block_item_iterator(
                  bracket_line,
                  identlocs,
                  None,
                  comments,
                  original_source,
                  ~get_loc,
                  ~print_item,
                  ~separator=Doc.comma,
                  ~trailing_separator=true,
                  ~break_separator=Doc.line,
                  ~get_attribute_text=no_attribute,
                  ~isBlock=true,
                  ~all_comments=comments,
                );
              Doc.concat([
                Doc.space,
                Doc.text("except"),
                Doc.space,
                Doc.lbrace,
                Doc.indent(Doc.concat([Doc.line, exceptions])),
                Doc.line,
                Doc.rbrace,
              ]);
            },
          ])
        | PImportValues(identlocsopts) =>
          Doc.concat([
            Doc.lbrace,
            Doc.indent(
              Doc.concat([
                Doc.line,
                switch (identlocsopts) {
                | [] => Doc.nil
                | [first, ...rem] =>
                  let get_loc =
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
                    loc.loc;
                  };

                  let print_item =
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
                    | None => Doc.concat([print_ident(loc.txt)])
                    | Some(alias) =>
                      Doc.concat([
                        print_ident(loc.txt),
                        Doc.space,
                        Doc.text("as"),
                        Doc.space,
                        print_ident(alias.txt),
                      ])
                    };
                  };

                  let (_, bracket_line, _, _) =
                    Locations.get_raw_pos_info(get_loc(first).loc_end);

                  block_item_iterator(
                    bracket_line,
                    identlocsopts,
                    None,
                    comments,
                    original_source,
                    ~get_loc,
                    ~print_item,
                    ~separator=Doc.comma,
                    ~trailing_separator=true,
                    ~break_separator=Doc.line,
                    ~get_attribute_text=no_attribute,
                    ~isBlock=true,
                    ~all_comments=comments,
                  );
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
    (
      vd: Parsetree.value_description,
      original_source: array(string),
      comments: list(Grain_parsing__Parsetree.comment),
    ) => {
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
    print_type(
      vd.pval_type,
      original_source,
      comments,
      ~trailing_separator=false,
    ),
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
    (
      vd: Parsetree.value_description,
      original_source: array(string),
      comments: list(Grain_parsing__Parsetree.comment),
    ) => {
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
    print_type(
      vd.pval_type,
      original_source,
      comments,
      ~trailing_separator=false,
    ),
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
      data: Parsetree.toplevel_stmt,
      ~original_source: array(string),
      ~all_comments: list(Grain_parsing__Parsetree.comment),
    ) => {
  let comments =
    Comment_utils.get_comments_inside_location(
      ~location=data.ptop_loc,
      all_comments,
    );

  let without_comments =
    switch (data.ptop_desc) {
    | PTopImport(import_declaration) =>
      import_print(import_declaration, comments, original_source)
    | PTopForeign(export_flag, value_description) =>
      let export =
        switch (export_flag) {
        | Nonexported => Doc.text("import ")
        | Exported => Doc.text("export ")
        };
      Doc.concat([
        export,
        Doc.text("foreign wasm "),
        print_foreign_value_description(
          value_description,
          original_source,
          comments,
        ),
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
          comments,
        ),
      ]);
    | PTopData(data_declarations) =>
      data_print(data_declarations, original_source, comments)
    | PTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
      print_value_bind(
        export_flag,
        rec_flag,
        mut_flag,
        value_bindings,
        original_source,
        comments,
      )
    | PTopExpr(expression) =>
      print_expression(
        ~parent_is_arrow=false,
        ~original_source,
        ~comments,
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
                    t =>
                      print_type(
                        t,
                        original_source,
                        comments,
                        ~trailing_separator=false,
                      ),
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

  Doc.group(without_comments);
};

let reformat_ast =
    (
      parsed_program: Parsetree.parsed_program,
      original_source: array(string),
    ) => {
  let get_loc = (stmt: Grain_parsing.Parsetree.toplevel_stmt) => {
    stmt.ptop_loc;
  };

  let print_item = (stmt: Grain_parsing.Parsetree.toplevel_stmt) => {
    toplevel_print(
      stmt,
      ~original_source,
      ~all_comments=parsed_program.comments,
    );
  };

  let top_level_stmts =
    block_item_iterator(
      0,
      parsed_program.statements,
      None,
      parsed_program.comments,
      original_source,
      ~get_loc,
      ~print_item,
      ~separator=Doc.nil,
      ~trailing_separator=true,
      ~break_separator=Doc.hardLine,
      ~get_attribute_text=
        stmt => {
          let attributes = stmt.ptop_attributes;
          print_attributes(attributes);
        },
      ~isBlock=true,
      ~all_comments=parsed_program.comments,
    );

  let final_doc = Doc.concat([top_level_stmts, Doc.hardLine]);

  Doc.toString(~width=80, final_doc);
};
