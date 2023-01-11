open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_diagnostics;

module Doc = Res_doc;

type iterator_item_type =
  | IteratedListPattern
  | IteratedRecordPattern
  | IteratedRecord
  | IteratedTypeConstructor
  | IteratedPatterns
  | IteratedArgs
  | IteratedMatchItem
  | IteratedDataDeclarations
  | IteratedRecordLabels
  | IteratedTupleExpression
  | IteratedArrayExpression
  | IteratedTypeItems
  | IteratedTupleConstructor
  | IteratedEnum
  | IteratedRecordData
  | IteratedValueBindings;
type expression_parent_type =
  | InfixExpression
  | GenericExpression
  | AccessExpression;

let exception_primitives = [|"throw", "fail", "assert"|];

let is_shift_or_concat_op = fn =>
  if (String.length(fn) > 1) {
    switch (String.sub(fn, 0, 2)) {
    | "<<"
    | ">>"
    | "++"
    | "||" => true
    | _ => false
    };
  } else {
    false;
  };

let is_logic_op = fn =>
  if (String.length(fn) > 1) {
    switch (String.sub(fn, 0, 2)) {
    | "<="
    | ">="
    | "=="
    | "!="
    | "is"
    | "isnt"
    | "&&"
    | "||" => true
    | _ => false
    };
  } else {
    false;
  };
let is_math_op = fn =>
  if (is_logic_op(fn) || is_shift_or_concat_op(fn)) {
    false;
  } else if (String.length(fn) > 0) {
    switch (fn.[0]) {
    | '*'
    | '/'
    | '%'
    | '+'
    | '-'
    | '<'
    | '>'
    | '&'
    | '^'
    | '|' => true
    | _ => false
    };
  } else {
    false;
  };

let op_precedence = fn => {
  let op_precedence = fn =>
    switch (fn) {
    | '*'
    | '/'
    | '%' => 120
    | '+'
    | '-' => 110
    | '<'
    | '>' => 90
    | '&' => 70
    | '^' => 60
    | '|' => 50
    | '_' => 10
    | _ => 9999
    };
  if (String.length(fn) > 1) {
    switch (String.sub(fn, 0, 2)) {
    | "++" => 110
    | "<<"
    | ">>" => 100
    | "=="
    | "!="
    | "is" => 80
    | "&&" => 40
    | "||"
    | "??" => 30
    | _ => op_precedence(fn.[0])
    };
  } else if (String.length(fn) > 0) {
    op_precedence(fn.[0]);
  } else {
    9999;
  };
};
let list_cons = "[...]";

exception IllegalParse(string);
exception FormatterError(string);

type compilation_error =
  | ParseError(exn)
  | InvalidCompilationState;

type sugared_list_item =
  | Regular(Parsetree.expression)
  | Spread(Parsetree.expression);

type record_item =
  | Field((Location.loc(Identifier.t), Parsetree.expression))
  | RecordSpread(Parsetree.expression);

type sugared_pattern_item =
  | RegularPattern(Parsetree.pattern)
  | SpreadPattern(Parsetree.pattern);

let get_original_code = (location: Location.t, source: array(string)) => {
  let (_, start_line, startc, _) =
    Locations.get_raw_pos_info(location.loc_start);
  let (_, end_line, endc, _) = Locations.get_raw_pos_info(location.loc_end);

  if (Array.length(source) > end_line - 1) {
    if (start_line == end_line) {
      String_utils.Utf8.sub(source[start_line - 1], startc, endc - startc);
    } else {
      let text = ref("");
      for (line in start_line - 1 to end_line - 1) {
        if (line + 1 == start_line) {
          text :=
            text^
            ++ String_utils.Utf8.string_after(source[line], startc)
            ++ "\n";
        } else if (line + 1 == end_line) {
          text := text^ ++ String_utils.Utf8.sub(source[line], 0, endc);
        } else {
          text := text^ ++ source[line] ++ "\n";
        };
      };
      text^;
    };
  } else {
    raise(FormatterError("Requested beyond end of original source"));
  };
};

// Be AWARE!  This is only to be called when you know the comments list is not empty.
// Moved here in case we want to change the implementation in future
let get_last_item_in_list = comments =>
  List.nth(comments, List.length(comments) - 1);

let is_disable_formatting_comment = (comment: Parsetree.comment) => {
  switch (comment) {
  | Line(cmt) =>
    if (cmt.cmt_content == "formatter-ignore") {
      true;
    } else {
      false;
    }
  | _ => false
  };
};

let print_attributes = attributes =>
  switch (attributes) {
  | [] => Doc.nil
  | _ =>
    Doc.concat([
      Doc.join(
        ~sep=Doc.space,
        List.map(
          ((a: Location.loc(string), args: list(Location.loc(string)))) => {
            switch (args) {
            | [] => Doc.concat([Doc.text("@"), Doc.text(a.txt)])
            | _ =>
              Doc.concat([
                Doc.text("@"),
                Doc.text(a.txt),
                Doc.text("("),
                Doc.join(
                  ~sep=Doc.concat([Doc.comma, Doc.space]),
                  List.map(
                    (b: Location.loc(string)) =>
                      Doc.concat([
                        Doc.text("\""),
                        Doc.text(b.txt),
                        Doc.text("\""),
                      ]),
                    args,
                  ),
                ),
                Doc.text(")"),
              ])
            }
          },
          attributes,
        ),
      ),
      Doc.hardLine,
    ])
  };
let force_break_if_line_comment =
    (~separator, comments: list(Parsetree.comment)) => {
  switch (comments) {
  | [] => separator
  | _ =>
    let last_comment = get_last_item_in_list(comments);

    switch (last_comment) {
    | Line(_) => Doc.hardLine
    | _ => separator
    };
  };
};

let break_parent_if_line_comment =
    (~separator, comments: list(Parsetree.comment)) => {
  switch (comments) {
  | [] => separator
  | _ =>
    let last_comment = get_last_item_in_list(comments);

    switch (last_comment) {
    | Line(_) => Doc.breakParent
    | _ => separator
    };
  };
};

let item_separator = (~this_line: int, ~line_above: int, break_separator) =>
  if (this_line - line_above > 1) {
    Doc.concat([break_separator, Doc.hardLine]);
  } else {
    break_separator;
  };

let comment_separator =
    (~this_line: int, ~line_above: int, comment: Parsetree.comment) =>
  if (this_line - line_above > 1) {
    switch (comment) {
    | Line(_) => Doc.hardLine
    | Shebang(_) => Doc.hardLine
    | Doc(_) => Doc.hardLine
    | _ => Doc.concat([Doc.hardLine, Doc.hardLine])
    };
  } else {
    switch (comment) {
    | Line(_)
    | Shebang(_) => Doc.nil
    | Doc(_) => Doc.softLine
    | Block(_) => Doc.hardLine
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
  switch (op.[0]) {
  | '+'
  | '-'
  | '*'
  | '/'
  | '%'
  | '='
  | '^'
  | '<'
  | '>'
  | '&'
  | '|'
  | '?' => true
  | _ when op == "is" => true
  | _ when op == "isnt" => true
  | _ when String.starts_with(~prefix="!=", op) => true
  | _ => false
  | exception _ => false
  };
};

let prefixop = (op: string) => {
  switch (op.[0]) {
  | '!' => true
  | _ => false
  | exception _ => false
  };
};

let no_attribute = _ => Doc.nil;

let remove_used_comments =
    (
      ~remove_comments: list(Parsetree.comment),
      all_comments: list(Parsetree.comment),
    ) => {
  List.filter(c => !List.mem(c, remove_comments), all_comments);
};

type prev_item_t =
  | PreviousItem(Location.t)
  | Block(Location.t)
  | TopOfFile;

let before_comments_break_line =
    (~previous: prev_item_t, ~this_line, comments) =>
  switch (previous) {
  | TopOfFile => Doc.nil
  | Block(block) =>
    let (_, bracket_line, _, _) =
      Locations.get_raw_pos_info(block.loc_start);
    switch (comments) {
    | [] => Doc.nil
    | [first_comment, ...rem] =>
      let (_, first_comment_line, _, _) =
        Locations.get_raw_pos_info(
          Locations.get_comment_loc(first_comment).loc_start,
        );
      if (first_comment_line - bracket_line > 1) {
        Doc.line;
      } else {
        Doc.nil;
      };
    };

  | PreviousItem(prev) =>
    let (_, prev_stmt_line, _, _) = Locations.get_raw_pos_info(prev.loc_end);
    switch (comments) {
    | [] => item_separator(~this_line, ~line_above=prev_stmt_line, Doc.line)

    | [first_comment, ...rem] =>
      let (_, first_comment_line, _, _) =
        Locations.get_raw_pos_info(
          Locations.get_comment_loc(first_comment).loc_start,
        );

      if (first_comment_line == prev_stmt_line) {
        Doc.space;
      } else {
        item_separator(
          ~this_line=first_comment_line,
          ~line_above=prev_stmt_line,
          Doc.line,
        );
      };
    };
  };

let handle_after_comments_break = (~this_line, leading_comments) =>
  switch (leading_comments) {
  | [] => Doc.nil
  | in_comments =>
    let last_comment = get_last_item_in_list(in_comments);
    let (_, last_comment_line, _, _) =
      Locations.get_raw_pos_info(
        Locations.get_comment_loc(last_comment).loc_end,
      );

    if (last_comment_line == this_line) {
      Doc.line;
    } else {
      comment_separator(
        ~this_line,
        ~line_above=last_comment_line,
        last_comment,
      );
    };
  };

let before_comments_break = (~previous: prev_item_t, ~this_line, comments) =>
  switch (previous) {
  | TopOfFile => Doc.nil
  | Block(block) =>
    let (_, bracket_line, _, _) =
      Locations.get_raw_pos_info(block.loc_start);
    switch (comments) {
    | [] => Doc.nil
    | [first_comment, ...rem] =>
      let (_, first_comment_line, _, _) =
        Locations.get_raw_pos_info(
          Locations.get_comment_loc(first_comment).loc_start,
        );
      if (first_comment_line - bracket_line > 1) {
        Doc.hardLine;
      } else {
        Doc.nil;
      };
    };

  | PreviousItem(prev) =>
    let (_, prev_stmt_line, _, _) = Locations.get_raw_pos_info(prev.loc_end);
    switch (comments) {
    | [] =>
      item_separator(~this_line, ~line_above=prev_stmt_line, Doc.hardLine)

    | [first_comment, ...rem] =>
      let (_, first_comment_line, _, _) =
        Locations.get_raw_pos_info(
          Locations.get_comment_loc(first_comment).loc_start,
        );

      if (first_comment_line == prev_stmt_line) {
        Doc.space;
      } else {
        item_separator(
          ~this_line=first_comment_line,
          ~line_above=prev_stmt_line,
          Doc.hardLine,
        );
      };
    };
  };

let rec block_item_iterator_line =
        (
          ~previous: prev_item_t,
          ~get_loc: 'a => Location.t,
          ~print_item: (~comments: list(Parsetree.comment), 'a) => Doc.t,
          ~comments: list(Parsetree.comment),
          ~original_source,
          ~separator: option(Doc.t)=?,
          items: list('a),
        ) => {
  switch (items) {
  | [] => Doc.nil
  | [item, ...remainder] =>
    let leading_comments =
      switch (previous) {
      | Block(prev_node) =>
        Comment_utils.get_comments_before_location(
          ~location=get_loc(item),
          comments,
        )
      | PreviousItem(prev_node) =>
        Comment_utils.get_comments_between_locations(
          ~loc1=prev_node,
          ~loc2=get_loc(item),
          comments,
        )
      | TopOfFile =>
        Comment_utils.get_comments_before_location(
          ~location=get_loc(item),
          comments,
        )
      };

    let leading_comment_docs =
      Comment_utils.new_comments_to_docs(leading_comments);

    let this_loc = get_loc(item);
    let (_, this_line, this_char, _) =
      Locations.get_raw_pos_info(this_loc.loc_start);

    let after_comments_break =
      handle_after_comments_break(~this_line, leading_comments);

    let bcb =
      before_comments_break_line(~previous, ~this_line, leading_comments);

    let block_top_spacing =
      switch (previous) {
      | Block(block_loc) =>
        switch (leading_comments) {
        | [] =>
          let (_, block_line, _, _) =
            Locations.get_raw_pos_info(block_loc.loc_start);
          if (this_line - block_line > 1) {
            Doc.hardLine;
          } else {
            Doc.nil;
          };
        | _ => Doc.nil
        }
      | _ => Doc.nil
      };

    let item_comments =
      Comment_utils.get_comments_inside_location(
        ~location=get_loc(item),
        comments,
      );
    let comments_without_leading =
      remove_used_comments(~remove_comments=leading_comments, comments);

    switch (remainder) {
    | [] =>
      let trailing_comments =
        remove_used_comments(
          ~remove_comments=item_comments,
          comments_without_leading,
        );

      let trailing_comment_docs =
        Comment_utils.block_trailing_comments_docs(trailing_comments);

      let (_, last_stmt_line, _, _) =
        Locations.get_raw_pos_info(get_loc(item).loc_end);
      let trailing_comment_separator =
        switch (trailing_comments) {
        | [] => Doc.nil

        | [first_comment, ...rem] =>
          let (_, first_comment_line, _, _) =
            Locations.get_raw_pos_info(
              Locations.get_comment_loc(first_comment).loc_start,
            );

          if (first_comment_line == last_stmt_line) {
            Doc.space;
          } else {
            Doc.line;
          };
        };

      let this_item =
        Doc.concat([
          bcb,
          leading_comment_docs,
          after_comments_break,
          print_item(~comments=item_comments, item),
          Doc.ifBreaks(Option.value(~default=Doc.nil, separator), Doc.nil),
          trailing_comment_separator,
          trailing_comment_docs,
        ]);

      this_item;

    | _more =>
      let this_item =
        Doc.concat([
          bcb,
          block_top_spacing,
          leading_comment_docs,
          after_comments_break,
          print_item(~comments=item_comments, item),
          Option.value(~default=Doc.nil, separator),
        ]);

      let comments_without_item_comments =
        remove_used_comments(
          ~remove_comments=item_comments,
          comments_without_leading,
        );

      Doc.concat([
        this_item,
        block_item_iterator_line(
          ~previous=PreviousItem(get_loc(item)),
          ~get_loc,
          ~print_item,
          ~comments=comments_without_item_comments,
          ~original_source,
          ~separator?,
          remainder,
        ),
      ]);
    };
  };
};

let rec block_item_iterator =
        (
          ~previous: prev_item_t,
          ~get_loc: 'a => Location.t,
          ~print_item: (~comments: list(Parsetree.comment), 'a) => Doc.t,
          ~comments: list(Parsetree.comment),
          ~print_attribute: 'a => Doc.t,
          ~original_source,
          items: list('a),
        ) => {
  switch (items) {
  | [] => Doc.nil
  | [item, ...remainder] =>
    let attribute_text = print_attribute(item);
    let leading_comments =
      switch (previous) {
      | Block(prev_node) =>
        Comment_utils.get_comments_before_location(
          ~location=get_loc(item),
          comments,
        )
      | PreviousItem(prev_node) =>
        Comment_utils.get_comments_between_locations(
          ~loc1=prev_node,
          ~loc2=get_loc(item),
          comments,
        )
      | TopOfFile =>
        Comment_utils.get_comments_before_location(
          ~location=get_loc(item),
          comments,
        )
      };
    let leading_comment_docs =
      Comment_utils.new_comments_to_docs(leading_comments);

    let this_loc = get_loc(item);
    let (_, this_line, this_char, _) =
      Locations.get_raw_pos_info(this_loc.loc_start);

    let after_comments_break =
      handle_after_comments_break(~this_line, leading_comments);

    let bcb = before_comments_break(~previous, ~this_line, leading_comments);

    let block_top_spacing =
      switch (previous) {
      | Block(block_loc) =>
        switch (leading_comments) {
        | [] =>
          let (_, block_line, _, _) =
            Locations.get_raw_pos_info(block_loc.loc_start);
          if (this_line - block_line > 1) {
            Doc.hardLine;
          } else {
            Doc.nil;
          };
        | _ => Doc.nil
        }
      | _ => Doc.nil
      };

    let disable_formatting =
      switch (leading_comments) {
      | [] => false
      | cmts =>
        let last_comment = get_last_item_in_list(cmts);

        is_disable_formatting_comment(last_comment);
      };

    if (disable_formatting) {
      let original_code = get_original_code(get_loc(item), original_source);

      let orig_doc =
        Doc.concat([
          before_comments_break(~previous, ~this_line, leading_comments),
          leading_comment_docs,
          Doc.group(Doc.text(original_code)),
        ]);

      let included_comments =
        Comment_utils.get_comments_inside_location(
          ~location=get_loc(item),
          comments,
        );

      let cleaned_comments =
        remove_used_comments(~remove_comments=included_comments, comments);

      switch (items) {
      | [last_item] =>
        let block_trailing_comments =
          Comment_utils.get_comments_after_location(
            ~location=get_loc(last_item),
            cleaned_comments,
          );

        switch (block_trailing_comments) {
        | [] => orig_doc
        | _ =>
          let block_trailing_comment_docs =
            Comment_utils.block_trailing_comments_docs(
              block_trailing_comments,
            );

          Doc.concat([
            orig_doc,
            before_comments_break(
              ~previous=PreviousItem(get_loc(last_item)),
              ~this_line,
              block_trailing_comments,
            ),
            block_trailing_comment_docs,
          ]);
        };
      | _ =>
        let item_comments =
          Comment_utils.get_comments_inside_location(
            ~location=get_loc(item),
            comments,
          );
        let comments_without_leading =
          remove_used_comments(~remove_comments=leading_comments, comments);
        let comments_without_item_comments =
          remove_used_comments(
            ~remove_comments=item_comments,
            comments_without_leading,
          );
        Doc.concat([
          orig_doc,
          block_item_iterator(
            ~previous=PreviousItem(get_loc(item)),
            ~get_loc,
            ~print_item,
            ~comments=comments_without_item_comments,
            ~print_attribute,
            ~original_source,
            remainder,
          ),
        ]);
      };
    } else {
      // regular formatting

      let item_comments =
        Comment_utils.get_comments_inside_location(
          ~location=get_loc(item),
          comments,
        );

      let comments_without_leading =
        remove_used_comments(~remove_comments=leading_comments, comments);

      switch (remainder) {
      | [] =>
        let trailing_comments =
          remove_used_comments(
            ~remove_comments=item_comments,
            comments_without_leading,
          );

        let trailing_comment_docs =
          Comment_utils.block_trailing_comments_docs(trailing_comments);

        let (_, last_stmt_line, _, _) =
          Locations.get_raw_pos_info(get_loc(item).loc_end);
        let trailing_comment_separator =
          switch (trailing_comments) {
          | [] => Doc.nil

          | [first_comment, ...rem] =>
            let (_, first_comment_line, _, _) =
              Locations.get_raw_pos_info(
                Locations.get_comment_loc(first_comment).loc_start,
              );

            if (first_comment_line == last_stmt_line) {
              Doc.space;
            } else {
              item_separator(
                ~this_line=first_comment_line,
                ~line_above=last_stmt_line,
                Doc.hardLine,
              );
            };
          };

        let this_item =
          Doc.concat([
            bcb,
            block_top_spacing,
            leading_comment_docs,
            after_comments_break,
            attribute_text,
            print_item(~comments=item_comments, item),
            trailing_comment_separator,
            trailing_comment_docs,
          ]);

        this_item;

      | _more =>
        let this_item =
          Doc.concat([
            bcb,
            block_top_spacing,
            leading_comment_docs,
            after_comments_break,
            attribute_text,
            print_item(~comments=item_comments, item),
          ]);

        let comments_without_item_comments =
          remove_used_comments(
            ~remove_comments=item_comments,
            comments_without_leading,
          );

        Doc.concat([
          this_item,
          block_item_iterator(
            ~previous=PreviousItem(get_loc(item)),
            ~get_loc,
            ~print_item,
            ~comments=comments_without_item_comments,
            ~print_attribute,
            ~original_source,
            remainder,
          ),
        ]);
      };
    };
  };
};

let print_trailing_comments = (~separator, ~itemloc: Location.t, comments) => {
  let next_comment = ref(None);

  let items =
    List.fold_right(
      (comment: Parsetree.comment, acc) => {
        let (_, this_comment_line, _, _) =
          Locations.get_raw_pos_info(
            Locations.get_comment_loc(comment).loc_start,
          );

        switch (next_comment^) {
        | None =>
          let (_, code_line, _, _) =
            Locations.get_raw_pos_info(itemloc.loc_end);
          if (this_comment_line > code_line) {
            [
              Doc.hardLine,
              Comment_utils.nobreak_comment_to_doc(comment),
              ...acc,
            ];
          } else {
            [Comment_utils.nobreak_comment_to_doc(comment), ...acc];
          };
        | Some(next) =>
          let (_, next_comment_line, _, _) =
            Locations.get_raw_pos_info(
              Locations.get_comment_loc(next).loc_end,
            );

          next_comment := Some(comment);

          if (this_comment_line <= next_comment_line) {
            [
              Doc.hardLine,
              Comment_utils.nobreak_comment_to_doc(comment),
              ...acc,
            ];
          } else {
            [Comment_utils.nobreak_comment_to_doc(comment), ...acc];
          };
        };
      },
      comments,
      [],
    );

  switch (items) {
  | [] => Doc.nil
  | items => Doc.concat([Doc.space, ...items])
  };
};

let mix_comments_and_separator =
    (~item_location: Location.t, ~separator, comments) => {
  let separated = ref(false);

  let next_comment = ref(None);

  let force_break_for_comment = (comment, acc) =>
    switch ((comment: Parsetree.comment)) {
    | Line(_) =>
      separated := true;
      [
        separator,
        Doc.space,
        Comment_utils.nobreak_comment_to_doc(comment),
        Doc.breakParent, // forces the lines to break, and so make this line comment force a new line
        ...acc,
      ];

    | _ => [Doc.space, Comment_utils.comment_to_doc(comment), ...acc]
    };

  let items =
    List.fold_right(
      (comment: Parsetree.comment, acc) => {
        let (_, this_comment_line, _, _) =
          Locations.get_raw_pos_info(
            Locations.get_comment_loc(comment).loc_start,
          );

        switch (next_comment^) {
        | None =>
          let (_, code_line, _, _) =
            Locations.get_raw_pos_info(item_location.loc_end);

          if (this_comment_line > code_line) {
            [
              Doc.hardLine,
              Comment_utils.nobreak_comment_to_doc(comment),
              ...acc,
            ];
          } else {
            force_break_for_comment(comment, acc);
          };

        | Some(next) =>
          let (_, next_comment_line, _, _) =
            Locations.get_raw_pos_info(
              Locations.get_comment_loc(next).loc_end,
            );

          next_comment := Some(comment);

          if (this_comment_line < next_comment_line) {
            [
              Doc.hardLine,
              Comment_utils.nobreak_comment_to_doc(comment),
              ...acc,
            ];
          } else {
            force_break_for_comment(comment, acc);
          };
        };
      },
      comments,
      [],
    );

  if (separated^) {
    Doc.concat(items);
  } else {
    Doc.ifBreaks(
      Doc.concat([separator, Doc.concat(items)]),
      Doc.concat([Doc.concat(items), separator]),
    );
  };
};

let rec item_iterator =
        (
          ~previous: option(Location.t)=?,
          ~get_loc: 'a => Location.t,
          ~print_item: (~comments: list(Parsetree.comment), 'a) => Doc.t,
          ~comments: list(Parsetree.comment),
          ~followed_by_arrow: option(bool)=?,
          ~iterated_item: iterator_item_type,
          items: list('a),
        ) => {
  let trailing_separator =
    switch (iterated_item) {
    | IteratedListPattern
    | IteratedDataDeclarations
    | IteratedMatchItem
    | IteratedRecordPattern
    | IteratedTupleExpression
    | IteratedArrayExpression
    | IteratedRecord
    | IteratedRecordData
    | IteratedRecordLabels => true
    | IteratedTypeItems
    | IteratedEnum
    | IteratedTypeConstructor
    | IteratedArgs
    | IteratedPatterns // we don't apply separators here as we may also need to apply type annotatins
    | IteratedTupleConstructor
    | IteratedValueBindings => false
    };

  let separator = Doc.comma;

  switch (items) {
  | [] => []
  | [first_item, ...rest] =>
    // special case for the first item, we look for leading comments
    // for all others, we look at the comments that come after them as that has the
    // impact on where to place comments and separators

    let leading_comments =
      Comment_utils.get_comments_before_location(
        ~location=get_loc(first_item),
        comments,
      );
    let leading_comments_docs =
      switch (leading_comments) {
      | [] => Doc.nil
      | _ =>
        Doc.group(
          Doc.concat([
            Comment_utils.new_comments_to_docs(leading_comments),
            Doc.ifBreaks(Doc.nil, Doc.space),
          ]),
        )
      };

    let number_items = List.length(items);

    List.mapi(
      (index, item) => {
        let itemdoc =
          print_item(
            ~comments=
              Comment_utils.get_comments_inside_location(
                ~location=get_loc(item),
                comments,
              ),
            item,
          );

        let trailing_comments =
          if (index == number_items - 1) {
            Comment_utils.get_comments_after_location(
              ~location=get_loc(item),
              comments,
            );
          } else {
            let next_item = List.nth(items, index + 1);
            Comment_utils.get_comments_between_locations(
              ~loc1=get_loc(item),
              ~loc2=get_loc(next_item),
              comments,
            );
          };

        let cmts =
          if (index == number_items - 1) {
            // last item
            Doc.concat([
              Doc.ifBreaks(
                if (trailing_separator) {separator} else {Doc.nil},
                Doc.nil,
              ),
              print_trailing_comments(
                ~separator,
                ~itemloc=get_loc(item),
                trailing_comments,
              ),
            ]);
          } else {
            mix_comments_and_separator(
              ~item_location=get_loc(item),
              ~separator,
              trailing_comments,
            );
          };

        // we only have leading comments for the first item
        if (index == 0) {
          Doc.concat([leading_comments_docs, itemdoc, cmts]);
        } else {
          Doc.concat([itemdoc, cmts]);
        };
      },
      items,
    );
  };
};

let rec resugar_list_patterns =
        (
          ~bracket_line,
          ~original_source: array(string),
          ~comments: list(Parsetree.comment),
          ~next_loc: Location.t,
          patterns: list(Parsetree.pattern),
        ) => {
  let processed_list = resugar_pattern_list_inner(patterns);

  let get_loc = (pattern: sugared_pattern_item) => {
    switch (pattern) {
    | RegularPattern(p)
    | SpreadPattern(p) => p.ppat_loc
    };
  };

  let print_item = (~comments, pattern: sugared_pattern_item) => {
    switch (pattern) {
    | RegularPattern(e) =>
      Doc.group(print_pattern(~original_source, ~comments, ~next_loc, e))
    | SpreadPattern(e) =>
      Doc.group(
        Doc.concat([
          Doc.text("..."),
          print_pattern(~original_source, ~comments, ~next_loc, e),
        ]),
      )
    };
  };

  let items =
    item_iterator(
      ~get_loc,
      ~print_item,
      ~comments,
      ~iterated_item=IteratedListPattern,
      processed_list,
    );
  let printed_patterns = Doc.join(~sep=Doc.line, items);
  let printed_patterns_after_bracket =
    Doc.concat([Doc.softLine, printed_patterns]);

  Doc.group(
    Doc.concat([
      Doc.lbracket,
      Doc.indent(printed_patterns_after_bracket),
      Doc.ifBreaks(Doc.comma, Doc.nil),
      Doc.softLine,
      Doc.rbracket,
    ]),
  );
}

and resugar_pattern_list_inner = (patterns: list(Parsetree.pattern)) => {
  switch (patterns) {
  | [arg1, arg2, ..._] =>
    switch (arg2.ppat_desc) {
    | PPatConstruct(innercstr, innerpatterns) =>
      let cstr =
        switch (innercstr.txt) {
        | IdentName({txt: name}) => name
        | _ => ""
        };

      if (cstr == "[]") {
        [RegularPattern(arg1)];
      } else if (cstr == list_cons) {
        let inner = resugar_pattern_list_inner(innerpatterns);
        [RegularPattern(arg1), ...inner];
      } else {
        [RegularPattern(arg1), SpreadPattern(arg2)];
      };

    | _ => [RegularPattern(arg1), SpreadPattern(arg2)]
    }
  | _ =>
    raise(IllegalParse("List pattern cons should always have two patterns"))
  };
}

and resugar_list =
    (
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      expressions: list(Parsetree.expression),
    ) => {
  let processed_list = resugar_list_inner(expressions);

  let last_item_was_spread = ref(false);

  let list_length = List.length(processed_list);

  let items =
    List.mapi(
      (index, item) =>
        switch (item) {
        | Regular(e) =>
          last_item_was_spread := false;

          // Do we have any comments on this line?
          // If so, we break the whole list

          // we might have a list list [1, 2 // comment
          //                            3]
          // so need to use the comment after the last item
          // [1,
          //  2, //comment
          //  3]

          let end_line_comments =
            if (index < list_length - 2) {
              let next_item = List.nth(processed_list, index + 1);
              Comment_utils.get_comments_between_locations(
                ~loc1=e.pexp_loc,
                ~loc2=
                  switch (next_item) {
                  | Regular(e)
                  | Spread(e) => e.pexp_loc
                  },
                comments,
              );
            } else {
              let (_, item_line, item_char, _) =
                Locations.get_raw_pos_info(e.pexp_loc.loc_end);
              Comment_utils.get_comments_on_line_end(
                ~line=item_line,
                ~char=item_char,
                comments,
              );
            };

          (
            print_expression(
              ~expression_parent=GenericExpression,
              ~original_source,
              ~comments=
                Comment_utils.get_comments_inside_location(
                  ~location=e.pexp_loc,
                  comments,
                ),
              e,
            ),
            end_line_comments,
          );

        | Spread(e) =>
          last_item_was_spread := true;

          (
            Doc.concat([
              Doc.text("..."),
              print_expression(
                ~expression_parent=GenericExpression,
                ~original_source,
                ~comments=
                  Comment_utils.get_comments_inside_location(
                    ~location=e.pexp_loc,
                    comments,
                  ),
                e,
              ),
            ]),
            [],
          );
        },
      processed_list,
    );

  // We have to compose this list by hand because of the complexity of if a list item
  // is followed by a comment, the comma must come before the comment.
  // It also impacts how we force a new line for a line ending comment at the end of a list
  // without introducing an extra blank line when bringing the indentation back in again

  let last_line_breaks_for_comments = ref(false);
  let items_length = List.length(items);
  let list_items =
    List.mapi(
      (i, (item, item_comments)) => {
        let final_item = items_length - 1 == i;

        let comment_doc =
          switch (item_comments) {
          | [] =>
            last_line_breaks_for_comments := false;
            if (final_item) {
              Doc.nil;
            } else {
              Doc.concat([Doc.comma, Doc.line]);
            };
          | _ =>
            let trailing_comments =
              List.map(
                (cmt: Parsetree.comment) =>
                  Doc.concat([
                    Doc.space,
                    Comment_utils.nobreak_comment_to_doc(cmt),
                  ]),
                item_comments,
              );

            last_line_breaks_for_comments := true;
            Doc.concat([
              Doc.comma,
              Doc.concat(trailing_comments),
              if (final_item) {Doc.nil} else {Doc.hardLine},
            ]);
          };

        Doc.concat([Doc.group(item), comment_doc]);
      },
      items,
    );

  Doc.group(
    Doc.concat([
      Doc.lbracket,
      Doc.indent(
        Doc.concat([
          Doc.softLine,
          Doc.concat(list_items),
          if (last_item_was_spread^ || last_line_breaks_for_comments^) {
            Doc.nil;
          } else {
            Doc.ifBreaks(Doc.comma, Doc.nil);
          },
        ]),
      ),
      if (last_line_breaks_for_comments^) {
        Doc.hardLine;
      } else {
        Doc.softLine;
      },
      Doc.rbracket,
    ]),
  );
}

and resugar_list_inner = (expressions: list(Parsetree.expression)) =>
  switch (expressions) {
  | [arg1, arg2] =>
    switch (arg2.pexp_desc) {
    | PExpConstruct({txt: IdentName({txt: "[...]"})}, innerexpressions) =>
      let inner = resugar_list_inner(innerexpressions);
      List.append([Regular(arg1)], inner);
    | PExpConstruct({txt: IdentName({txt: "[]"})}, _) => [Regular(arg1)]
    | _ => [Regular(arg1), Spread(arg2)]
    }
  | _ =>
    // Grain syntax makes it impossible to construct a list cons without
    // two arguments, but we'll check just to make sure
    raise(IllegalParse("List cons should always have two expressions"))
  }

and check_for_pattern_pun = (pat: Parsetree.pattern) =>
  switch (pat.ppat_desc) {
  | PPatVar({txt, _}) => Doc.text(txt)
  | _ => Doc.nil
  }

and print_record_pattern =
    (
      ~patternlocs: list((Location.loc(Identifier.t), Parsetree.pattern)),
      ~closedflag: Asttypes.closed_flag,
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      ~next_loc: Location.t,
      patloc: Location.t,
    ) => {
  let close =
    switch (closedflag) {
    | Open => Doc.concat([Doc.text(","), Doc.space, Doc.text("_")])
    | Closed => Doc.nil
    };

  let get_loc =
      (patternloc: (Location.loc(Identifier.t), Parsetree.pattern)) => {
    let (_, pat) = patternloc;
    pat.ppat_loc;
  };

  let print_item =
      (
        ~comments,
        patternloc: (Location.loc(Identifier.t), Parsetree.pattern),
      ) => {
    let (loc, pat) = patternloc;
    let printed_ident: Doc.t = print_ident(loc.txt);

    let printed_pat =
      print_pattern(~original_source, ~comments, ~next_loc, pat);

    let punned_pat = check_for_pattern_pun(pat);

    let pun =
      switch (printed_ident, punned_pat: Doc.t) {
      | (Text(i), Text(e)) => i == e
      | _ => false
      };

    if (pun) {
      printed_ident;
    } else {
      Doc.concat([printed_ident, Doc.text(":"), Doc.space, printed_pat]);
    };
  };

  let after_brace_comments =
    Comment_utils.get_after_brace_comments(~loc=patloc, comments);
  let cleaned_comments =
    remove_used_comments(~remove_comments=after_brace_comments, comments);

  let items =
    item_iterator(
      ~get_loc,
      ~print_item,
      ~comments=cleaned_comments,
      ~iterated_item=IteratedRecordPattern,
      patternlocs,
    );
  let printed_fields = Doc.join(~sep=Doc.line, items);

  let printed_fields_after_brace =
    Doc.concat([
      force_break_if_line_comment(~separator=Doc.line, after_brace_comments),
      printed_fields,
    ]);

  Doc.concat([
    Doc.lbrace,
    Comment_utils.single_line_of_comments(after_brace_comments),
    Doc.indent(Doc.concat([printed_fields_after_brace, close])),
    Doc.line,
    Doc.rbrace,
  ]);
}

and print_pattern =
    (
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      ~next_loc: Location.t,
      pat: Parsetree.pattern,
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
          print_patterns(~next_loc, ~comments, ~original_source, patterns),
        ),
        true,
      )
    | PPatArray(patterns) => (
        Doc.group(
          Doc.concat([
            Doc.lbracket,
            Doc.text(">"),
            Doc.space,
            print_patterns(~next_loc, ~comments, ~original_source, patterns),
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
          ~next_loc,
          pat.ppat_loc,
        ),
        false,
      )
    | PPatConstraint(pattern, parsed_type) => (
        Doc.concat([
          print_patterns(~next_loc, ~comments, ~original_source, [pattern]),
          Doc.text(":"),
          Doc.space,
          print_type(~original_source, ~comments, parsed_type),
        ]),
        false,
      )
    | PPatConstruct(location, patterns) =>
      let func =
        switch (location.txt) {
        | IdentName({txt: name}) => name
        | _ => ""
        };
      if (func == list_cons) {
        let (_, bracket_line, _, _) =
          Locations.get_raw_pos_info(pat.ppat_loc.loc_start);

        (
          resugar_list_patterns(
            ~bracket_line,
            ~original_source,
            ~comments,
            ~next_loc,
            patterns,
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
                  ~next_loc,
                  ~comments,
                  ~original_source,
                  patterns,
                ),
              )
            },
          ]),
          false,
        );
      };

    | PPatOr(pattern1, pattern2) => (
        Doc.group(
          Doc.concat([
            Doc.group(
              print_pattern(~original_source, ~comments, ~next_loc, pattern1),
            ),
            Doc.space,
            Doc.text("|"),
            Doc.line,
            Doc.group(
              print_pattern(~original_source, ~comments, ~next_loc, pattern2),
            ),
          ]),
        ),
        false,
      )

    | PPatAlias(pattern, loc) => (
        Doc.group(
          Doc.concat([
            print_pattern(~original_source, ~comments, ~next_loc, pattern),
            Doc.space,
            Doc.text("as"),
            Doc.space,
            Doc.text(loc.txt),
          ]),
        ),
        false,
      )
    };

  let (pattern, parens) = printed_pattern;

  let with_leading = [pattern];

  let after_parens_comments =
    Comment_utils.get_comments_to_end_of_line(
      ~location=pat.ppat_loc,
      comments,
    );
  let after_parens_comments_docs =
    Comment_utils.inbetween_comments_to_docs(
      ~offset=true,
      after_parens_comments,
    );

  let with_trailing =
    if (after_parens_comments_docs == Doc.nil) {
      with_leading;
    } else {
      List.append(with_leading, [after_parens_comments_docs]);
    };

  let clean_pattern =
    switch (with_trailing) {
    | [fst] => fst
    | _ => Doc.concat(with_trailing)
    };

  if (parens) {
    Doc.concat([
      Doc.lparen,
      Doc.indent(Doc.concat([Doc.softLine, clean_pattern])),
      Doc.ifBreaks(Doc.comma, Doc.nil),
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
      ~loc: Location.t,
      c: Parsetree.constant,
    ) => {
  // we get the original code here to ensure it's well formatted and retains the
  // approach of the original code, e.g. char format, number format
  Doc.text(
    get_original_code(loc, original_source),
  );
}

and print_ident = (ident: Identifier.t) => {
  switch (ident) {
  | IdentName({txt: name}) =>
    if (infixop(name) || prefixop(name)) {
      Doc.concat([Doc.lparen, Doc.text(name), Doc.rparen]);
    } else {
      Doc.text(name);
    }
  | IdentExternal(externalIdent, {txt: second}) =>
    Doc.concat([
      print_ident(externalIdent),
      Doc.text("."),
      Doc.text(second),
    ])
  };
}

and print_record =
    (
      ~base: option(Parsetree.expression),
      ~fields: list((Location.loc(Identifier.t), Parsetree.expression)),
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      recloc: Location.t,
    ) => {
  let get_loc = item => {
    switch (item) {
    | Field((_, expr)) => expr.pexp_loc
    | RecordSpread(base) => base.pexp_loc
    };
  };

  let print_item = (~comments, item) => {
    switch (item) {
    | Field(field) =>
      let (locidentifier, expr) = field;
      let ident = locidentifier.txt;
      let printed_ident = print_ident(ident);
      let printed_expr =
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
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
          Doc.concat([
            printed_ident,
            Doc.text(":"),
            Doc.space,
            printed_expr,
          ]),
        );
      } else {
        Doc.group(printed_ident);
      };
    | RecordSpread(base) =>
      Doc.concat([
        Doc.text("..."),
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
          base,
        ),
      ])
    };
  };

  let items =
    Option.to_list(Option.map(x => RecordSpread(x), base))
    @ List.map(x => Field(x), fields);

  let after_brace_comments =
    switch (items) {
    | [item, ..._] =>
      let loc =
        switch (item) {
        | Field((ident, _)) => ident.loc
        | RecordSpread(exp) => exp.pexp_loc
        };

      Comment_utils.get_after_brace_comments(
        ~loc=recloc,
        ~first=loc,
        comments,
      );

    | _ => Comment_utils.get_after_brace_comments(~loc=recloc, comments) // let s = {}  is not legal syntax, but we can use all the comments
    };

  let cleaned_comments =
    remove_used_comments(~remove_comments=after_brace_comments, comments);

  let items =
    item_iterator(
      ~get_loc,
      ~print_item,
      ~comments=cleaned_comments,
      ~iterated_item=IteratedRecord,
      items,
    );
  let printed_fields = Doc.join(~sep=Doc.line, items);

  let printed_fields_after_brace =
    Doc.concat([
      force_break_if_line_comment(~separator=Doc.line, after_brace_comments),
      printed_fields,
    ]);

  Doc.concat([
    Doc.lbrace,
    Comment_utils.single_line_of_comments(after_brace_comments),
    Doc.indent(
      Doc.concat([
        printed_fields_after_brace,
        Doc.ifBreaks(
          Doc.nil,
          switch (items) {
          | [_one] =>
            // TODO: not needed once we annotate with ::
            Doc.comma // append a comma as single argument record look like block {data:val}
          | _ => Doc.nil
          },
        ),
      ]),
    ),
    Doc.line,
    Doc.rbrace,
  ]);
}

and print_type =
    (
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      p: Parsetree.parsed_type,
    ) => {
  switch (p.ptyp_desc) {
  | PTyAny => Doc.text("_")
  | PTyVar(name) => Doc.text(name)
  | PTyArrow(types, parsed_type) =>
    Doc.concat([
      Doc.group(
        switch (types) {
        | [] => Doc.concat([Doc.lparen, Doc.rparen])
        | [t] => print_type(~original_source, ~comments, t)
        | _types =>
          Doc.concat([
            Doc.lparen,
            Doc.indent(
              Doc.concat([
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat([Doc.comma, Doc.line]),
                  List.map(
                    t => print_type(~original_source, ~comments, t),
                    types,
                  ),
                ),
              ]),
            ),
            Doc.ifBreaks(Doc.comma, Doc.nil),
            Doc.softLine,
            Doc.rparen,
          ])
        },
      ),
      Doc.space,
      Doc.text("->"),
      Doc.space,
      print_type(~original_source, ~comments, parsed_type),
    ])

  | PTyTuple(parsed_types) =>
    Doc.concat([
      Doc.lparen,
      Doc.indent(
        Doc.concat([
          Doc.softLine,
          Doc.join(
            ~sep=Doc.concat([Doc.comma, Doc.line]),
            List.map(
              t => print_type(~original_source, ~comments, t),
              parsed_types,
            ),
          ),
          switch (parsed_types) {
          | [_one] => Doc.comma
          | _ => Doc.nil
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
      let get_loc = (t: Parsetree.parsed_type) => {
        t.ptyp_loc;
      };
      let print_item = (~comments, t: Parsetree.parsed_type) => {
        print_type(~original_source, ~comments, t);
      };

      let after_angle_comments =
        Comment_utils.get_after_brace_comments(
          ~loc=get_loc(first),
          comments,
        );
      let cleaned_comments =
        remove_used_comments(~remove_comments=after_angle_comments, comments);

      let type_items =
        item_iterator(
          ~get_loc,
          ~print_item,
          ~comments=cleaned_comments,
          ~iterated_item=IteratedTypeConstructor,
          parsedtypes,
        );
      let printed_types = Doc.join(~sep=Doc.line, type_items);
      let printed_types_after_angle =
        Doc.concat([
          force_break_if_line_comment(
            ~separator=Doc.softLine,
            after_angle_comments,
          ),
          printed_types,
        ]);

      Doc.group(
        Doc.concat([
          print_ident(ident),
          Doc.text("<"),
          Doc.indent(Doc.group(printed_types_after_angle)),
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
      ~expression_parent: expression_parent_type,
      ~expressions: list(Parsetree.expression),
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      func: Parsetree.expression,
    ) => {
  let function_name = get_function_name(func);

  switch (expressions) {
  | [first, second] when infixop(function_name) =>
    print_infix_application(
      ~expression_parent,
      ~expressions,
      ~original_source,
      ~comments,
      func,
    )
  | _ =>
    print_other_application(
      ~expression_parent,
      ~expressions,
      ~original_source,
      ~comments,
      func,
    )
  };
}

and print_infix_application =
    (
      ~expression_parent: expression_parent_type,
      ~expressions: list(Parsetree.expression),
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      func: Parsetree.expression,
    ) => {
  let function_name = get_function_name(func);

  switch (expressions) {
  | [first, second] =>
    let next_comments =
      Comment_utils.get_comments_between_locations(
        ~loc1=first.pexp_loc,
        ~loc2=second.pexp_loc,
        comments,
      );

    let (_, line, _, _) = Locations.get_raw_pos_info(first.pexp_loc.loc_end);

    let line_comments =
      Comment_utils.get_comments_on_line(line, next_comments);

    let after_comments =
      remove_used_comments(~remove_comments=line_comments, next_comments);

    let after_comments_docs =
      Comment_utils.block_trailing_comments_docs(after_comments);

    let line_comment_docs =
      Comment_utils.single_line_of_comments(line_comments);

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

    let parent_prec = op_precedence(function_name);
    let left_is_leaf =
      switch (first.pexp_desc) {
      | PExpApp(fn, expr) =>
        let child_name = get_function_name(fn);
        let this_prec = op_precedence(child_name);

        this_prec < parent_prec || child_name != function_name;
      | _ => true
      };

    let right_is_leaf =
      switch (second.pexp_desc) {
      | PExpApp(fn, expr) =>
        let child_name = get_function_name(fn);
        let this_prec = op_precedence(child_name);

        this_prec < parent_prec || child_name != function_name;
      | _ => true
      };

    let left_grouping_required =
      switch (first.pexp_desc) {
      | PExpApp(fn1, _) =>
        op_precedence(get_function_name(fn1)) < parent_prec
      | PExpConstant(PConstNumber(PConstNumberRational(_, _))) =>
        op_precedence("/") < parent_prec
      | _ => false
      };

    let right_grouping_required =
      // the equality check is needed for the value on the right
      // as we process from the left by default when the same prededence
      switch (second.pexp_desc) {
      | PExpApp(fn1, _) =>
        op_precedence(get_function_name(fn1)) <= parent_prec
      | PExpConstant(PConstNumber(PConstNumberRational(_, _))) =>
        op_precedence("/") <= parent_prec
      | _ => false
      };

    // Put parens around different operators for clarity, except
    // math and logic operations where precedence is well-known
    let left_is_different_op =
      switch (first.pexp_desc) {
      | PExpApp(fn1, _) =>
        let fn = get_function_name(fn1);
        if (infixop(fn)) {
          (!is_math_op(function_name) && !is_logic_op(function_name))
          && fn != function_name;
        } else {
          false;
        };
      | _ => false
      };

    let left_needs_parens =
      left_is_if || left_grouping_required || left_is_different_op;
    let right_needs_parens = right_is_if || right_grouping_required;

    let lhs =
      if (left_needs_parens) {
        let l1 =
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments,
            first,
          );
        Doc.concat([
          Doc.lparen,
          if (Doc.willBreak(l1)) {
            Doc.indent(l1);
          } else {
            l1;
          },
          Doc.rparen,
        ]);
      } else {
        print_expression(
          ~expression_parent,
          ~original_source,
          ~comments,
          first,
        );
      };

    let rhs_expr =
      if (right_needs_parens) {
        Doc.concat([
          Doc.lparen,
          Doc.concat([
            print_expression(
              ~expression_parent=GenericExpression,
              ~original_source,
              ~comments,
              second,
            ),
          ]),
          Doc.rparen,
        ]);
      } else {
        Doc.concat([
          print_expression(
            ~expression_parent,
            ~original_source,
            ~comments,
            second,
          ),
        ]);
      };

    Doc.concat([
      if (left_is_leaf) {
        Doc.group(lhs);
      } else {
        lhs;
      },
      Doc.space,
      Doc.text(function_name),
      line_comment_docs,
      if (after_comments_docs == Doc.nil) {
        Doc.nil;
      } else {
        force_break_if_line_comment(~separator=Doc.nil, line_comments);
      },
      after_comments_docs,
      switch (expression_parent) {
      | InfixExpression =>
        Doc.concat([
          Doc.line,
          if (right_is_leaf) {
            Doc.group(rhs_expr);
          } else {
            rhs_expr;
          },
        ])
      | GenericExpression
      | AccessExpression =>
        Doc.indent(
          Doc.concat([
            Doc.line,
            if (right_is_leaf) {
              Doc.group(rhs_expr);
            } else {
              rhs_expr;
            },
          ]),
        )
      },
    ]);

  | _ => raise(IllegalParse("Formatter error, wrong number of args "))
  };
}

and print_arg_lambda =
    (~comments, ~original_source, lambda: Parsetree.expression) => {
  switch (lambda.pexp_desc) {
  | PExpLambda(patterns, expression) =>
    let comments_in_expression =
      Comment_utils.get_comments_inside_location(
        ~location=expression.pexp_loc,
        comments,
      );

    let raw_args =
      print_patterns(
        ~next_loc=expression.pexp_loc,
        ~comments,
        ~original_source,
        ~followed_by_arrow=true,
        patterns,
      );

    let args =
      Doc.group(
        switch (patterns) {
        | [] => Doc.concat([Doc.lparen, raw_args, Doc.rparen])
        | [pat] =>
          switch (pat.ppat_desc) {
          | PPatVar(_) => raw_args
          | _ => Doc.group(Doc.concat([Doc.lparen, raw_args, Doc.rparen]))
          }
        | _patterns =>
          Doc.concat([
            Doc.lparen,
            Doc.indent(Doc.concat([Doc.softLine, raw_args])),
            Doc.softLine,
            Doc.rparen,
          ])
        },
      );

    Doc.group(
      switch (expression.pexp_desc) {
      | PExpBlock(block_expressions) =>
        let body =
          switch (block_expressions) {
          | [] =>
            // Not legal syntax so we shouldn't ever hit it, but we'll handle
            // it just in case.
            Doc.group(Doc.concat([Doc.lbrace, Doc.rbrace]))
          | _ =>
            let get_loc = (expr: Parsetree.expression) => {
              expr.pexp_loc;
            };
            let print_item = (~comments, expr: Parsetree.expression) => {
              Doc.group(
                print_expression(
                  ~expression_parent=GenericExpression,
                  ~original_source,
                  ~comments,
                  expr,
                ),
              );
            };
            let after_brace_comments =
              Comment_utils.get_after_brace_comments(
                ~loc=expression.pexp_loc,
                comments_in_expression,
              );
            let cleaned_comments =
              remove_used_comments(
                ~remove_comments=after_brace_comments,
                comments_in_expression,
              );

            let print_attribute = (expr: Parsetree.expression) =>
              print_attributes(expr.pexp_attributes);
            let printed_expressions =
              block_item_iterator(
                ~previous=Block(expression.pexp_loc),
                ~get_loc,
                ~print_item,
                ~comments=cleaned_comments,
                ~print_attribute,
                ~original_source,
                block_expressions,
              );
            let start_after_brace =
              Doc.concat([Doc.hardLine, printed_expressions]);

            Doc.concat([
              Doc.lbrace,
              Comment_utils.single_line_of_comments(after_brace_comments),
              Doc.indent(start_after_brace),
              Doc.hardLine,
              Doc.rbrace,
            ]);
          };
        Doc.concat([args, Doc.space, Doc.text("=>"), Doc.space, body]);

      | _ =>
        let body =
          Doc.group(
            print_expression(
              ~expression_parent=GenericExpression,
              ~original_source,
              ~comments=comments_in_expression,
              expression,
            ),
          );
        if (Doc.willBreak(body)) {
          Doc.concat([
            args,
            Doc.space,
            Doc.text("=>"),
            Doc.concat([Doc.space, body]),
          ]);
        } else {
          Doc.concat([
            args,
            Doc.space,
            Doc.text("=>"),
            Doc.indent(Doc.concat([Doc.line, body])),
          ]);
        };
      },
    );

  | _ => raise(IllegalParse("Called on a non-lambda"))
  };
}

and print_arg = (~original_source, ~comments, arg: Parsetree.expression) => {
  switch (arg.pexp_desc) {
  | PExpLambda(patterns, expression) =>
    print_arg_lambda(~comments, ~original_source, arg)
  | _ =>
    Doc.group(
      print_expression(
        ~expression_parent=InfixExpression,
        ~original_source,
        ~comments,
        arg,
      ),
    )
  };
}

and print_args_with_comments =
    (
      ~comments: list(Parsetree.comment),
      ~original_source,
      args: list(Parsetree.expression),
    ) => {
  let get_loc = (e: Parsetree.expression) => e.pexp_loc;
  let print_item = (~comments, e: Parsetree.expression) => {
    Doc.group(
      print_expression(
        ~expression_parent=InfixExpression,
        ~original_source,
        ~comments,
        e,
      ),
    );
  };

  let args =
    item_iterator(
      ~get_loc,
      ~print_item,
      ~comments,
      ~followed_by_arrow=false,
      ~iterated_item=IteratedArgs,
      args,
    );

  Doc.join(~sep=Doc.line, args);
}

and print_arguments_with_callback_in_first_position =
    (~original_source, ~comments, args: list(Parsetree.expression)) => {
  switch (args) {
  | [] => Doc.nil
  | [callback] =>
    // we handle the special case of just one callback here as we call this if the first arg is a callback

    print_arg_lambda(~comments, ~original_source, callback)

  | [callback, expr] =>
    let printed_callback =
      print_arg_lambda(~comments, ~original_source, callback);

    let printed_arg = print_arg(~comments, ~original_source, expr);

    Doc.ifBreaks(
      Doc.concat([printed_callback, Doc.comma, Doc.space, printed_arg]),
      Doc.concat([
        Doc.indent(
          Doc.concat([
            Doc.softLine,
            printed_callback,
            Doc.comma,
            Doc.line,
            printed_arg,
          ]),
        ),
        Doc.softLine,
      ]),
    );
  | [callback, ...remainder] =>
    let printed_callback =
      print_arg_lambda(~comments, ~original_source, callback);

    let printed_args =
      print_args_with_comments(~comments, ~original_source, remainder);

    Doc.concat([
      printed_callback,
      Doc.comma,
      Doc.group(
        Doc.concat([
          Doc.line,
          printed_args,
          Doc.ifBreaks(Doc.line, Doc.nil),
        ]),
      ),
    ]);
  };
}

and print_arguments_with_callback_in_last_position =
    (~original_source, ~comments, args: list(Parsetree.expression)) =>
  switch (args) {
  | [] => Doc.nil
  | [expr, callback] =>
    let printed_callback =
      print_arg_lambda(~comments, ~original_source, callback);
    let printed_first_arg = print_arg(~comments, ~original_source, expr);

    Doc.concat([printed_first_arg, Doc.comma, Doc.space, printed_callback]);

  | _ =>
    let last_expression = get_last_item_in_list(args);
    let printed_callback =
      print_arg_lambda(~comments, ~original_source, last_expression);

    let remainderArr =
      Array.sub(Array.of_list(args), 0, List.length(args) - 1);

    let printed_args =
      print_args_with_comments(
        ~comments,
        ~original_source,
        Array.to_list(remainderArr),
      );

    Doc.concat([
      Doc.indent(
        Doc.concat([
          Doc.softLine,
          printed_args,
          Doc.comma,
          Doc.line,
          printed_callback,
        ]),
      ),
      Doc.softLine,
    ]);
  }

and print_other_application =
    (
      ~expression_parent: expression_parent_type,
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
          Doc.group(
            print_expression(
              ~expression_parent,
              ~original_source,
              ~comments,
              first,
            ),
          ),
          Doc.rparen,
        ]);
      } else {
        Doc.concat([
          Doc.text(function_name),
          Doc.group(
            print_expression(
              ~expression_parent,
              ~original_source,
              ~comments,
              first,
            ),
          ),
        ]);
      };

    | _ =>
      Doc.concat([
        Doc.text(function_name),
        Doc.group(
          print_expression(
            ~expression_parent,
            ~original_source,
            ~comments,
            first,
          ),
        ),
      ])
    }

  | [first, second] when infixop(function_name) =>
    print_infix_application(
      ~expression_parent,
      ~expressions,
      ~original_source,
      ~comments,
      func,
    )
  | _ when infixop(function_name) =>
    raise(IllegalParse("Formatter error, wrong number of args "))
  | [first_expr, ..._]
      when Array.exists(fn => function_name == fn, exception_primitives) =>
    Doc.concat([
      print_expression(
        ~expression_parent=GenericExpression,
        ~original_source,
        ~comments,
        func,
      ),
      Doc.space,
      print_expression(
        ~expression_parent=GenericExpression,
        ~original_source,
        ~comments,
        first_expr,
      ),
    ])
  | [first_expr, ..._] =>
    // standard function application
    // look out for special cases of callbacks in first or last position

    let first_arg_is_callback =
      switch (first_expr.pexp_desc) {
      | PExpLambda(_) => true
      | _ => false
      };

    let last_arg_is_callback =
      switch (expressions) {
      | [] => false
      | _ =>
        let last_expression = get_last_item_in_list(expressions);

        switch (last_expression.pexp_desc) {
        | PExpLambda(_) => true
        | _ => false
        };
      };

    if (first_arg_is_callback) {
      let printed_args =
        print_arguments_with_callback_in_first_position(
          ~original_source,
          ~comments,
          expressions,
        );
      Doc.concat([
        print_expression(
          ~expression_parent=AccessExpression,
          ~original_source,
          ~comments,
          func,
        ),
        Doc.lparen,
        printed_args,
        Doc.rparen,
      ]);
    } else if (last_arg_is_callback) {
      let printed_args =
        print_arguments_with_callback_in_last_position(
          ~original_source,
          ~comments,
          expressions,
        );
      Doc.concat([
        print_expression(
          ~expression_parent=AccessExpression,
          ~original_source,
          ~comments,
          func,
        ),
        Doc.lparen,
        printed_args,
        Doc.rparen,
      ]);
    } else {
      let printed_args =
        print_args_with_comments(~comments, ~original_source, expressions);

      Doc.group(
        Doc.concat([
          print_expression(
            ~expression_parent=AccessExpression,
            ~original_source,
            ~comments,
            func,
          ),
          Doc.lparen,
          Doc.indent(Doc.concat([Doc.softLine, printed_args])),
          Doc.softLine,
          Doc.rparen,
        ]),
      );
    };

  | [] =>
    Doc.group(
      Doc.concat([
        print_expression(
          ~expression_parent=AccessExpression,
          ~original_source,
          ~comments,
          func,
        ),
        Doc.lparen,
        Doc.softLine,
        Doc.rparen,
      ]),
    )
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
    | IdentName(name) => name.txt
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

and print_patterns =
    (
      ~next_loc: Location.t,
      ~comments: list(Parsetree.comment),
      ~original_source: array(string),
      ~followed_by_arrow: option(bool)=?,
      patterns: list(Parsetree.pattern),
    ) => {
  let get_loc = (p: Parsetree.pattern) => p.ppat_loc;
  let print_item = (~comments, p: Parsetree.pattern) => {
    print_pattern(~original_source, ~comments, ~next_loc, p);
  };

  let comments_in_scope =
    Comment_utils.get_comments_before_location(~location=next_loc, comments);

  switch (patterns) {
  | [] => Doc.nil
  | _ =>
    let items =
      item_iterator(
        ~get_loc,
        ~print_item,
        ~comments=comments_in_scope,
        ~followed_by_arrow?,
        ~iterated_item=IteratedPatterns,
        patterns,
      );
    Doc.join(~sep=Doc.line, items);
  };
}

and paren_wrap_patterns =
    (
      ~wrapper: Location.t,
      ~next_loc: Location.t,
      ~comments: list(Parsetree.comment),
      ~original_source: array(string),
      ~followed_by_arrow: bool,
      patterns: list(Parsetree.pattern),
    ) => {
  let args =
    print_patterns(
      ~next_loc,
      ~comments,
      ~original_source,
      ~followed_by_arrow,
      patterns,
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
and print_expression_inner =
    (
      ~expression_parent: expression_parent_type,
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      expr: Parsetree.expression,
    ) => {
  let expression_doc =
    switch (expr.pexp_desc) {
    | PExpConstant(x) =>
      print_constant(~original_source, ~loc=expr.pexp_loc, x)
    | PExpId({txt: id}) => print_ident(id)
    | PExpLet(rec_flag, mut_flag, vbs) =>
      print_value_bind(
        ~export_flag=Asttypes.Nonexported,
        ~rec_flag,
        ~mut_flag,
        ~original_source,
        ~comments,
        vbs,
      )
    | PExpTuple(expressions) =>
      let get_loc = (e: Parsetree.expression) => {
        e.pexp_loc;
      };
      let print_item = (~comments, e: Parsetree.expression) => {
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
          e,
        );
      };

      let after_paren_comments =
        Comment_utils.get_after_brace_comments(~loc=expr.pexp_loc, comments);
      let cleaned_comments =
        remove_used_comments(~remove_comments=after_paren_comments, comments);
      let expr_items =
        item_iterator(
          ~get_loc,
          ~print_item,
          ~comments=cleaned_comments,
          ~iterated_item=IteratedTupleExpression,
          expressions,
        );

      let printed_expr_items = Doc.join(~sep=Doc.line, expr_items);
      let printed_expr_items_after_paren =
        Doc.concat([Doc.softLine, printed_expr_items]);
      Doc.group(
        Doc.concat([
          Doc.lparen,
          Comment_utils.single_line_of_comments(after_paren_comments),
          Doc.indent(printed_expr_items_after_paren),
          Doc.ifBreaks(
            Doc.nil,
            switch (expressions) {
            | [_one] => Doc.comma
            | _ => Doc.nil
            },
          ),
          Doc.softLine,
          Doc.rparen,
        ]),
      );

    | PExpArray(expressions) =>
      let get_loc = (e: Parsetree.expression) => {
        e.pexp_loc;
      };
      let print_item = (~comments, e: Parsetree.expression) => {
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
          e,
        );
      };

      let after_bracket_comments =
        Comment_utils.get_after_brace_comments(~loc=expr.pexp_loc, comments);
      let cleaned_comments =
        remove_used_comments(
          ~remove_comments=after_bracket_comments,
          comments,
        );
      let items =
        item_iterator(
          ~get_loc,
          ~print_item,
          ~comments=cleaned_comments,
          ~iterated_item=IteratedArrayExpression,
          expressions,
        );
      Doc.group(
        switch (expressions) {
        | [] => Doc.text("[>]")
        | _ =>
          Doc.concat([
            Doc.lbracket,
            Doc.text("> "),
            Comment_utils.single_line_of_comments(after_bracket_comments),
            Doc.indent(
              Doc.concat([
                force_break_if_line_comment(
                  ~separator=Doc.softLine,
                  after_bracket_comments,
                ),
                Doc.join(~sep=Doc.line, items),
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
          ~expression_parent=AccessExpression,
          ~original_source,
          ~comments,
          expression1,
        ),
        Doc.lbracket,
        Doc.group(
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments,
            expression2,
          ),
        ),
        Doc.rbracket,
      ])
    | PExpArraySet(expression1, expression2, expression3) =>
      Doc.group(
        Doc.concat([
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments,
            expression1,
          ),
          Doc.lbracket,
          Doc.group(
            print_expression(
              ~expression_parent=GenericExpression,
              ~original_source,
              ~comments,
              expression2,
            ),
          ),
          Doc.rbracket,
          Doc.space,
          Doc.text("="),
          Doc.indent(
            Doc.concat([
              Doc.space,
              print_expression(
                ~expression_parent=GenericExpression,
                ~original_source,
                ~comments,
                expression3,
              ),
            ]),
          ),
        ]),
      )

    | PExpRecord(base, record) =>
      print_record(
        ~base,
        ~fields=record,
        ~original_source,
        ~comments,
        expr.pexp_loc,
      )
    | PExpRecordGet(expression, {txt, _}) =>
      Doc.concat([
        print_expression(
          ~expression_parent=AccessExpression,
          ~original_source,
          ~comments,
          expression,
        ),
        Doc.dot,
        print_ident(txt),
      ])
    | PExpRecordSet(expression, {txt, _}, expression2) =>
      let left =
        Doc.concat([
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments,
            expression,
          ),
          Doc.dot,
          print_ident(txt),
        ]);
      print_assignment(~original_source, ~comments, left, expression2);
    | PExpMatch(expression, match_branches) =>
      let arg =
        Doc.concat([
          Doc.lparen,
          Doc.group(
            print_expression(
              ~expression_parent=GenericExpression,
              ~original_source,
              ~comments,
              expression,
            ),
          ),
          Doc.rparen,
        ]);

      let get_loc = (branch: Parsetree.match_branch) => {
        branch.pmb_loc;
      };

      let print_item = (~comments, branch: Parsetree.match_branch) => {
        let branch_comments =
          Comment_utils.get_comments_inside_location(
            ~location=branch.pmb_loc,
            comments,
          );

        let branch_pattern_comments =
          Comment_utils.get_comments_inside_location(
            ~location=branch.pmb_pat.ppat_loc,
            comments,
          );

        Doc.group(
          Doc.concat([
            Doc.group(
              print_pattern(
                ~original_source,
                ~comments=branch_pattern_comments,
                ~next_loc=
                  switch (branch.pmb_guard) {
                  | None => branch.pmb_body.pexp_loc
                  | Some(b) => b.pexp_loc
                  },
                branch.pmb_pat,
              ),
            ),
            switch (branch.pmb_guard) {
            | None =>
              Doc.concat([
                Doc.space,
                Doc.text("=>"),
                switch (branch.pmb_body.pexp_desc) {
                | PExpBlock(expressions) =>
                  Doc.concat([
                    Doc.space,
                    print_expression(
                      ~expression_parent=GenericExpression,
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
                        ~expression_parent=GenericExpression,
                        ~original_source,
                        ~comments=branch_comments,
                        branch.pmb_body,
                      ),
                    ]),
                  )
                },
              ])
            | Some(guard) =>
              let branch_guard_comments =
                Comment_utils.get_comments_inside_location(
                  ~location=guard.pexp_loc,
                  comments,
                );
              let guard_doc =
                Doc.group(
                  print_expression(
                    ~expression_parent=InfixExpression,
                    ~original_source,
                    ~comments=branch_guard_comments,
                    guard,
                  ),
                );
              Doc.concat([
                Doc.space,
                Doc.text("when"),
                Doc.space,
                Doc.group(
                  Doc.concat([
                    Doc.ifBreaks(Doc.lparen, Doc.nil),
                    Doc.indent(Doc.concat([Doc.softLine, guard_doc])),
                    Doc.softLine,
                    Doc.ifBreaks(Doc.rparen, Doc.nil),
                  ]),
                ),
                Doc.space,
                Doc.text("=>"),
                switch (branch.pmb_body.pexp_desc) {
                | PExpBlock(_)
                | PExpIf(_) =>
                  Doc.concat([
                    Doc.space,
                    Doc.group(
                      print_expression(
                        ~expression_parent=GenericExpression,
                        ~original_source,
                        ~comments=branch_comments,
                        branch.pmb_body,
                      ),
                    ),
                  ])

                | _ =>
                  Doc.indent(
                    Doc.concat([
                      Doc.line,
                      Doc.group(
                        print_expression(
                          ~expression_parent=GenericExpression,
                          ~original_source,
                          ~comments=branch_comments,
                          branch.pmb_body,
                        ),
                      ),
                    ]),
                  )
                },
              ]);
            },
          ]),
        );
      };

      let after_brace_comments =
        Comment_utils.get_after_brace_comments(~loc=expr.pexp_loc, comments);
      let cleaned_comments =
        remove_used_comments(~remove_comments=after_brace_comments, comments);

      let items =
        item_iterator(
          ~get_loc,
          ~print_item,
          ~comments=cleaned_comments,
          ~iterated_item=IteratedMatchItem,
          match_branches,
        );
      let printed_branches = Doc.join(~sep=Doc.hardLine, items);

      let printed_branches_after_brace =
        Doc.concat([
          force_break_if_line_comment(
            ~separator=Doc.line,
            after_brace_comments,
          ),
          printed_branches,
        ]);

      Doc.group(
        Doc.concat([
          Doc.text("match"),
          Doc.space,
          arg,
          Doc.space,
          Doc.lbrace,
          Comment_utils.single_line_of_comments(after_brace_comments),
          Doc.indent(printed_branches_after_brace),
          Doc.line,
          Doc.rbrace,
        ]),
      );

    | PExpPrim0(prim0) =>
      let original_code = get_original_code(expr.pexp_loc, original_source);
      Doc.text(original_code);
    | PExpPrim1(prim1, expression) =>
      let original_code = get_original_code(expr.pexp_loc, original_source);
      Doc.text(original_code);
    | PExpPrim2(prim2, expression, expression1) =>
      let original_code = get_original_code(expr.pexp_loc, original_source);
      Doc.text(original_code);
    | PExpPrimN(primn, expressions) =>
      let original_code = get_original_code(expr.pexp_loc, original_source);
      Doc.text(original_code);
    | PExpIf(condition, true_expr, false_expr) =>
      let cond_leading_comment =
        Comment_utils.get_comments_from_start_of_enclosing_location(
          ~enclosing_location=expr.pexp_loc,
          ~location=condition.pexp_loc,
          comments,
        );

      let cond_trailing_comment =
        Comment_utils.get_comments_between_locs(
          ~begin_loc=condition.pexp_loc,
          ~end_loc=true_expr.pexp_loc,
          comments,
        );

      let last_comment_different_line =
        switch (cond_trailing_comment) {
        | [] => false
        | [first, ...rest] =>
          let (_, first_comment_line, _, _) =
            Locations.get_raw_pos_info(
              Locations.get_comment_loc(first).loc_start,
            );

          let (_, condition_line, _, _) =
            Locations.get_raw_pos_info(condition.pexp_loc.loc_end);

          first_comment_line > condition_line;
        };

      let same_line_comments =
        last_comment_different_line ? [] : cond_trailing_comment;
      let later_line_comments =
        last_comment_different_line ? cond_trailing_comment : [];

      let print_later_comments = (~default, later_line_comments) =>
        switch (later_line_comments) {
        | [] => default
        | cmts =>
          Doc.concat([
            Doc.line,
            Comment_utils.new_comments_to_docs(later_line_comments),
          ])
        };

      let true_trailing_comment =
        Comment_utils.get_comments_between_locs(
          ~begin_loc=true_expr.pexp_loc,
          ~end_loc=false_expr.pexp_loc,
          comments,
        );

      let true_is_block =
        switch (true_expr.pexp_desc) {
        | PExpBlock(_) => true
        | _ => false
        };

      let true_is_if =
        switch (true_expr.pexp_desc) {
        | PExpIf(_) => true
        | _ => false
        };

      let false_is_block =
        switch (false_expr.pexp_desc) {
        | PExpBlock(expressions) => List.length(expressions) > 0
        | _ => false
        };

      let false_is_if =
        switch (false_expr.pexp_desc) {
        | PExpBlock(expressions) =>
          switch (expressions) {
          | [] => false
          | [hd, ...tail] =>
            switch (hd.pexp_desc) {
            | PExpIf(_) => true
            | _ => false
            }
          }
        | _ => false
        };

      let commentsInCondition =
        Comment_utils.get_comments_inside_location(
          ~location=condition.pexp_loc,
          comments,
        );

      let comments_in_true_statement =
        Comment_utils.get_comments_inside_location(
          ~location=true_expr.pexp_loc,
          comments,
        );

      let true_made_block = ref(false);
      let false_made_block = ref(false);

      let true_clause =
        switch (true_expr.pexp_desc) {
        | PExpBlock(expressions) =>
          Doc.concat([
            Doc.space,
            print_expression(
              ~expression_parent=GenericExpression,
              ~original_source,
              ~comments=comments_in_true_statement,
              true_expr,
            ),
          ])

        | _ =>
          if (false_is_block) {
            true_made_block := true;
            Doc.concat([
              Doc.space,
              Doc.lbrace,
              // no comment to add here as this was a single line expression
              Doc.indent(
                Doc.concat([
                  Doc.hardLine,
                  print_expression(
                    ~expression_parent=GenericExpression,
                    ~original_source,
                    ~comments=comments_in_true_statement,
                    true_expr,
                  ),
                ]),
              ),
              Doc.hardLine,
              Doc.rbrace,
            ]);
          } else if (true_is_if) {
            Doc.concat([
              print_later_comments(~default=Doc.space, later_line_comments),
              Doc.lparen,
              Doc.indent(
                Doc.concat([
                  Doc.softLine,
                  print_expression(
                    ~expression_parent=GenericExpression,
                    ~original_source,
                    ~comments=comments_in_true_statement,
                    true_expr,
                  ),
                ]),
              ),
              Doc.softLine,
              Doc.rparen,
            ]);
          } else {
            Doc.indent(
              Doc.concat([
                print_later_comments(~default=Doc.line, later_line_comments),
                print_expression(
                  ~expression_parent=GenericExpression,
                  ~original_source,
                  ~comments=comments_in_true_statement,
                  true_expr,
                ),
              ]),
            );
          }
        };

      let comments_in_false_statement =
        Comment_utils.get_comments_inside_location(
          ~location=false_expr.pexp_loc,
          comments,
        );

      let false_clause =
        switch (false_expr.pexp_desc) {
        | PExpBlock(expressions) =>
          switch (expressions) {
          | [] => Doc.nil
          | _ =>
            Doc.concat([
              Doc.space,
              Doc.text("else"),
              Doc.space,
              print_expression(
                ~expression_parent=GenericExpression,
                ~original_source,
                ~comments=comments_in_false_statement,
                false_expr,
              ),
            ])
          }
        | PExpIf(_condition, _true_expr, _false_expr) =>
          Doc.concat([
            Doc.space,
            Doc.text("else"),
            if (false_is_if) {
              Doc.concat([
                Doc.space,
                Doc.lparen,
                Doc.indent(
                  Doc.concat([
                    Doc.softLine,
                    print_expression(
                      ~expression_parent=GenericExpression,
                      ~original_source,
                      ~comments=comments_in_false_statement,
                      false_expr,
                    ),
                  ]),
                ),
                Doc.softLine,
                Doc.rparen,
              ]);
            } else {
              Doc.concat([
                Doc.space,
                print_expression(
                  ~expression_parent=GenericExpression,
                  ~original_source,
                  ~comments=comments_in_false_statement,
                  false_expr,
                ),
              ]);
            },
          ])
        | _ =>
          Doc.concat([
            if (true_is_block) {
              false_made_block := true;
              Doc.concat([
                Doc.space,
                Doc.text("else"),
                Doc.space,
                Doc.lbrace,
                // no comments to add here as original was single line
                Doc.indent(
                  Doc.concat([
                    Doc.hardLine,
                    print_expression(
                      ~expression_parent=GenericExpression,
                      ~original_source,
                      ~comments=comments_in_false_statement,
                      false_expr,
                    ),
                  ]),
                ),
                Doc.hardLine,
                Doc.rbrace,
              ]);
            } else {
              Doc.concat([
                Doc.line,
                Doc.text("else"),
                Doc.space,
                print_expression(
                  ~expression_parent=GenericExpression,
                  ~original_source,
                  ~comments=comments_in_false_statement,
                  false_expr,
                ),
              ]);
            },
          ])
        };

      let inner =
        Doc.concat([
          Doc.softLine,
          Comment_utils.inbetween_comments_to_docs(
            ~offset=false,
            cond_leading_comment,
          ),
          switch (cond_leading_comment) {
          | [] => Doc.nil
          | _ => Doc.ifBreaks(Doc.nil, Doc.space)
          },
          Doc.group(
            print_expression(
              ~expression_parent=InfixExpression,
              ~original_source,
              ~comments=commentsInCondition,
              condition,
            ),
          ),
          switch (same_line_comments) {
          | [] => Doc.nil
          | _ =>
            Doc.concat([
              Doc.concat(
                List.mapi(
                  (index, c) =>
                    Doc.concat([
                      Doc.space,
                      Comment_utils.nobreak_comment_to_doc(c),
                      switch (c) {
                      | Line(_) => Doc.breakParent
                      | _ => Doc.nil
                      },
                    ]),
                  same_line_comments,
                ),
              ),
            ])
          },
        ]);

      Doc.concat([
        Doc.group(
          Doc.concat([
            Doc.text("if"),
            Doc.space,
            Doc.group(
              Doc.concat([
                Doc.lparen,
                Doc.indent(inner),
                Doc.softLine,
                Doc.rparen,
              ]),
            ),
          ]),
        ),
        Doc.group(true_clause),
        Comment_utils.inbetween_comments_to_docs(
          ~offset=true,
          true_trailing_comment,
        ),
        Doc.group(false_clause),
      ]);
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
                  ~expression_parent=InfixExpression,
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
            ~expression_parent=GenericExpression,
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
      let comments_before_loop_expression =
        Comment_utils.get_comments_enclosed_and_before_location(
          ~loc1=expr.pexp_loc,
          ~loc2=expression4.pexp_loc,
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
                switch (optexpression1) {
                | Some(expr) =>
                  Doc.group(
                    print_expression(
                      ~expression_parent=InfixExpression,
                      ~original_source,
                      ~comments=comments_before_loop_expression,
                      expr,
                    ),
                  )
                | None => Doc.nil
                },
                Doc.text(";"),
                switch (optexpression2, optexpression3) {
                | (None, None) => Doc.nil
                | (None, Some(_)) => Doc.space
                | (Some(expr), _) =>
                  Doc.concat([
                    Doc.line,
                    Doc.group(
                      print_expression(
                        ~expression_parent=InfixExpression,
                        ~original_source,
                        ~comments=comments_before_loop_expression,
                        expr,
                      ),
                    ),
                  ])
                },
                Doc.text(";"),
                switch (optexpression3) {
                | Some(expr) =>
                  Doc.concat([
                    switch (expr.pexp_desc) {
                    | PExpBlock(_) => Doc.space
                    | _ => Doc.line
                    },
                    Doc.group(
                      print_expression(
                        ~expression_parent=InfixExpression,
                        ~original_source,
                        ~comments=comments_before_loop_expression,
                        expr,
                      ),
                    ),
                  ])
                | None => Doc.nil
                },
              ]),
            ),
            Doc.softLine,
            Doc.rparen,
          ]),
        ),
        Doc.space,
        Doc.group(
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments=comments_in_expression4,
            expression4,
          ),
        ),
      ]);
    | PExpContinue => Doc.text("continue")
    | PExpBreak => Doc.text("break")
    | PExpReturn(expr) =>
      Doc.concat([
        Doc.text("return"),
        switch (expr) {
        | Some(expr) =>
          Doc.concat([
            Doc.space,
            print_expression(
              ~expression_parent=GenericExpression,
              ~original_source,
              ~comments,
              expr,
            ),
          ])
        | None => Doc.nil
        },
      ])
    | PExpConstraint(expression, parsed_type) =>
      let comments_in_expression =
        Comment_utils.get_comments_inside_location(
          ~location=expression.pexp_loc,
          comments,
        );

      Doc.group(
        Doc.concat([
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments=comments_in_expression,
            expression,
          ),
          Doc.text(":"),
          Doc.space,
          print_type(
            ~original_source,
            ~comments=
              Comment_utils.get_comments_inside_location(
                ~location=parsed_type.ptyp_loc,
                comments,
              ),
            parsed_type,
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
          ~wrapper=expr.pexp_loc,
          ~next_loc=expression.pexp_loc,
          ~comments=patterns_comments,
          ~original_source,
          ~followed_by_arrow=true,
          patterns,
        );

      switch (expression.pexp_desc) {
      | PExpBlock(_)
      | PExpLambda(_) =>
        Doc.concat([
          Doc.group(
            Doc.concat([args, Doc.space, Doc.text("=>"), Doc.space]),
          ),
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments=comments_in_expression,
            expression,
          ),
        ])
      | PExpIf(_) =>
        let out =
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments=comments_in_expression,
            expression,
          );

        Doc.concat([
          Doc.group(
            Doc.concat([
              args,
              Doc.space,
              Doc.text("=>"),
              Doc.ifBreaks(Doc.space, Doc.line),
            ]),
          ),
          out,
        ]);
      | _ =>
        Doc.concat([
          args,
          Doc.space,
          Doc.text("=>"),
          Doc.indent(
            Doc.concat([
              Doc.line,
              print_expression(
                ~expression_parent=GenericExpression,
                ~original_source,
                ~comments=comments_in_expression,
                expression,
              ),
            ]),
          ),
        ])
      };

    | PExpApp(func, expressions) =>
      let comments_in_expression =
        Comment_utils.get_comments_inside_location(
          ~location=expr.pexp_loc,
          comments,
        );
      print_application(
        ~expression_parent,
        ~expressions,
        ~original_source,
        ~comments=comments_in_expression,
        func,
      );
    | PExpConstruct({txt: IdentName({txt: "[...]"})}, expressions) =>
      resugar_list(~original_source, ~comments, expressions)
    | PExpConstruct({txt: IdentName({txt: "[]"})}, expressions) =>
      Doc.text("[]")
    | PExpConstruct({txt: id}, []) => print_ident(id)
    | PExpConstruct(constr, expressions) =>
      let comments_in_expression =
        Comment_utils.get_comments_inside_location(
          ~location=expr.pexp_loc,
          comments,
        );
      print_application(
        ~expression_parent,
        ~expressions,
        ~original_source,
        ~comments=comments_in_expression,
        Ast_helper.Exp.ident(constr),
      );
    | PExpBlock(expressions) =>
      switch (expressions) {
      | [] =>
        // Not legal syntax so we shouldn't ever hit it, but we'll handle
        // it just in case.
        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat([Doc.lbrace, Doc.indent(Doc.line), Doc.rbrace]),
        )
      | _ =>
        let get_loc = (expr: Parsetree.expression) => {
          expr.pexp_loc;
        };

        let print_item = (~comments, expr: Parsetree.expression) => {
          Doc.group(
            print_expression(
              ~expression_parent=GenericExpression,
              ~original_source,
              ~comments,
              expr,
            ),
          );
        };

        let after_brace_comments =
          Comment_utils.get_after_brace_comments(
            ~loc=expr.pexp_loc,
            comments,
          );

        let cleaned_comments =
          remove_used_comments(
            ~remove_comments=after_brace_comments,
            comments,
          );

        let print_attribute = (expr: Parsetree.expression) =>
          print_attributes(expr.pexp_attributes);

        let printed_expressions =
          block_item_iterator(
            ~previous=Block(expr.pexp_loc),
            ~get_loc,
            ~print_item,
            ~comments=cleaned_comments,
            ~print_attribute,
            ~original_source,
            expressions,
          );

        let start_after_brace =
          Doc.concat([Doc.hardLine, printed_expressions]);

        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat([
            Doc.lbrace,
            Comment_utils.single_line_of_comments(after_brace_comments),
            Doc.indent(start_after_brace),
            Doc.hardLine,
            Doc.rbrace,
          ]),
        );
      }

    | PExpBoxAssign(expression, expression1) =>
      Doc.concat([
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
          expression,
        ),
        Doc.space,
        Doc.text(":="),
        Doc.space,
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
          expression1,
        ),
      ])
    | PExpAssign(expression, expression1) =>
      let left =
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
          expression,
        );
      print_assignment(~original_source, ~comments, left, expression1);
    | /** Used for modules without body expressions */ PExpNull => Doc.nil
    };

  expression_doc;
}

and is_grouped_access_expression = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpConstant(_)
  | PExpConstruct(_)
  | PExpTuple(_)
  | PExpId(_)
  | PExpArrayGet(_)
  | PExpArraySet(_)
  | PExpRecordGet(_)
  | PExpRecordSet(_)
  | PExpRecord(_)
  | PExpBlock(_)
  | PExpArray(_) => false
  | PExpApp(func, _) =>
    let func_name = get_function_name(func);
    infixop(func_name);
  | _ => true
  };
}

and print_expression =
    (
      ~expression_parent: expression_parent_type,
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      expr: Parsetree.expression,
    ) => {
  let printed_expr =
    print_expression_inner(
      ~expression_parent,
      ~original_source,
      ~comments,
      expr,
    );
  switch (expression_parent) {
  | AccessExpression =>
    if (is_grouped_access_expression(expr)) {
      Doc.concat([Doc.lparen, printed_expr, Doc.rparen]);
    } else {
      printed_expr;
    }

  | GenericExpression
  | InfixExpression => printed_expr
  };
}
and print_assignment = (~original_source, ~comments, left, value) => {
  switch (value.pexp_desc) {
  | PExpApp(func, expressions) =>
    let function_name = get_function_name(func);

    let trimmed_operator = String.trim(function_name);

    let left_matches_first =
      switch (expressions) {
      | [expr, ...remainder] =>
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
          expr,
        )
        == left
      | _ => false
      };

    if (left_matches_first) {
      // +=, -=, *=, /=, and %=
      switch (trimmed_operator) {
      | "+"
      | "-"
      | "*"
      | "/"
      | "%" =>
        let sugared_op = Doc.text(" " ++ trimmed_operator ++ "= ");
        Doc.concat([
          left,
          sugared_op,
          switch (expressions) {
          | [] =>
            raise(IllegalParse("Sugared op needs at least one expression"))
          | [expression] =>
            let expr =
              print_expression(
                ~expression_parent=GenericExpression,
                ~original_source,
                ~comments,
                expression,
              );
            switch (expression.pexp_desc) {
            | PExpIf(_) =>
              Doc.indent(
                print_expression(
                  ~expression_parent=GenericExpression,
                  ~original_source,
                  ~comments,
                  expression,
                ),
              )
            | _ => expr
            };
          | [expression1, expression2, ...rest] =>
            let expr =
              print_expression(
                ~expression_parent=GenericExpression,
                ~original_source,
                ~comments,
                expression2,
              );
            switch (expression2.pexp_desc) {
            | PExpIf(_) =>
              Doc.indent(
                print_expression(
                  ~expression_parent=GenericExpression,
                  ~original_source,
                  ~comments,
                  expression2,
                ),
              )
            | _ => expr
            };
          },
        ]);
      | _ =>
        Doc.concat([
          left,
          Doc.space,
          Doc.equal,
          Doc.space,
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments,
            value,
          ),
        ])
      };
    } else {
      Doc.concat([
        left,
        Doc.space,
        Doc.equal,
        Doc.space,
        print_expression(
          ~expression_parent=GenericExpression,
          ~original_source,
          ~comments,
          value,
        ),
      ]);
    };

  | _ =>
    Doc.concat([
      left,
      Doc.space,
      Doc.equal,
      Doc.space,
      print_expression(
        ~expression_parent=GenericExpression,
        ~original_source,
        ~comments,
        value,
      ),
    ])
  };
}
and print_value_bind =
    (
      ~export_flag: Asttypes.export_flag,
      ~rec_flag: Asttypes.rec_flag,
      ~mut_flag: Asttypes.mut_flag,
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
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

  let formatted_items =
    switch (vbs) {
    | [] => []
    | [first, ...rem] =>
      let get_loc = (vb: Parsetree.value_binding) => vb.pvb_loc;
      let print_item = (~comments, vb: Parsetree.value_binding) => {
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
            Comment_utils.inbetween_comments_to_docs(
              ~offset=false,
              after_let_comments,
            )
          };

        let expr_comments =
          Comment_utils.get_comments_inside_location(
            ~location=vb.pvb_expr.pexp_loc,
            comments,
          );
        let printed =
          print_expression(
            ~expression_parent=GenericExpression,
            ~original_source,
            ~comments=expr_comments,
            vb.pvb_expr,
          );

        let expression =
          switch (vb.pvb_expr.pexp_desc) {
          | PExpIf(_) =>
            if (Doc.willBreak(printed)) {
              Doc.concat([Doc.space, printed]);
            } else {
              Doc.indent(Doc.concat([Doc.line, printed]));
            }
          | _ => Doc.concat([Doc.space, printed])
          };

        let pattern_comments =
          Comment_utils.get_comments_enclosed_and_before_location(
            ~loc1=vb.pvb_loc,
            ~loc2=vb.pvb_expr.pexp_loc,
            comments,
          );

        Doc.concat([
          Doc.group(
            print_pattern(
              ~original_source,
              ~comments=pattern_comments,
              ~next_loc=vb.pvb_loc,
              vb.pvb_pat,
            ),
          ),
          after_let_comments_docs,
          switch (after_let_comments) {
          | [] => Doc.space
          | _ => Doc.nil
          },
          Doc.equal,
          expression,
        ]);
      };

      item_iterator(
        ~get_loc,
        ~print_item,
        ~comments,
        ~iterated_item=IteratedValueBindings,
        vbs,
      );
    };

  let value_bindings =
    List.mapi(
      (index, doc) => {
        let item =
          if (index > 0) {
            Doc.concat([Doc.line, doc]);
          } else {
            doc;
          };
        let vb = List.nth(vbs, index);
        switch (vb.pvb_expr.pexp_desc) {
        | PExpLambda(fn, _) => item
        | _ =>
          if (index > 0) {
            Doc.indent(item);
          } else {
            item;
          }
        };
      },
      formatted_items,
    );

  Doc.group(
    Doc.concat([
      exported,
      Doc.text("let"),
      Doc.space,
      recursive,
      mutble,
      Doc.concat(value_bindings),
    ]),
  );
};

let rec print_data =
        (
          ~original_source: array(string),
          ~comments: list(Parsetree.comment),
          data: Parsetree.data_declaration,
        ) => {
  let nameloc = data.pdata_name;
  switch (data.pdata_kind) {
  | PDataAbstract =>
    let get_loc = (t: Parsetree.parsed_type) => t.ptyp_loc;
    let print_item = (~comments, t: Parsetree.parsed_type) => {
      print_type(~original_source, ~comments, t);
    };

    let after_angle_cmts =
      switch (data.pdata_params) {
      | [hd, ...rem] =>
        Comment_utils.get_comments_before_location(
          ~location=get_loc(hd),
          comments,
        )
      | [] =>
        Comment_utils.get_comments_to_end_of_line(
          ~location=nameloc.loc,
          comments,
        )
      };

    switch (data.pdata_params) {
    | [] =>
      let type_comments =
        switch (after_angle_cmts) {
        | [] => Doc.nil
        | cmts => Comment_utils.single_line_of_comments(cmts)
        };
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
                Doc.indent(
                  Doc.concat([
                    Doc.line,
                    print_type(~original_source, ~comments, manifest),
                    type_comments,
                  ]),
                ),
              ])
            | None => Doc.nil
            },
          ]),
        ),
      ]);

    | [hd, ...rem] =>
      let after_angle_comments =
        switch (after_angle_cmts) {
        | [] => Doc.softLine
        | cmts =>
          Doc.concat([
            Doc.space,
            Comment_utils.new_comments_to_docs(cmts),
            Doc.ifBreaks(Doc.nil, Doc.space),
          ])
        };
      let type_comments =
        switch (data.pdata_manifest) {
        | None => comments
        | Some(manifest) =>
          let tcomments =
            Comment_utils.get_comments_between_locs(
              ~begin_loc=nameloc.loc,
              ~end_loc=manifest.ptyp_loc,
              comments,
            );

          remove_used_comments(~remove_comments=after_angle_cmts, tcomments);
        };

      let items =
        item_iterator(
          ~get_loc,
          ~print_item,
          ~comments=type_comments,
          ~iterated_item=IteratedTypeItems,
          data.pdata_params,
        );
      let printed_types = Doc.join(~sep=Doc.line, items);
      let printed_types_after_angle = printed_types;
      let params = [
        Doc.text("<"),
        Doc.indent(
          Doc.concat([after_angle_comments, printed_types_after_angle]),
        ),
        Doc.softLine,
        Doc.text(">"),
      ];

      Doc.concat([
        Doc.text("type"),
        Doc.space,
        Doc.group(Doc.concat([Doc.text(data.pdata_name.txt), ...params])),
        switch (data.pdata_manifest) {
        | Some(manifest) =>
          let manifest_comments =
            Comment_utils.get_comments_inside_location(
              ~location=manifest.ptyp_loc,
              comments,
            );

          Doc.group(
            Doc.concat([
              Doc.space,
              Doc.equal,
              Doc.space,
              print_type(
                ~original_source,
                ~comments=manifest_comments,
                manifest,
              ),
            ]),
          );
        | None => Doc.nil
        },
      ]);
    };

  | PDataVariant(constr_declarations) =>
    let get_loc = (lbl: Parsetree.constructor_declaration) => {
      lbl.pcd_loc;
    };

    let print_item = (~comments, d: Parsetree.constructor_declaration) => {
      Doc.group(
        Doc.concat([
          Doc.text(d.pcd_name.txt),
          switch (d.pcd_args) {
          | PConstrTuple(parsed_types) =>
            switch (parsed_types) {
            | [] => Doc.nil
            | [first, ...rem] =>
              let get_loc = (t: Parsetree.parsed_type) => t.ptyp_loc;
              let print_item = (~comments, t: Parsetree.parsed_type) => {
                Doc.concat([print_type(~original_source, ~comments, t)]);
              };

              let after_paren_comments =
                Comment_utils.get_after_brace_comments(
                  ~first=first.ptyp_loc,
                  ~loc=get_loc(first),
                  comments,
                );
              let cleaned_comments =
                remove_used_comments(
                  ~remove_comments=after_paren_comments,
                  comments,
                );

              let type_items =
                item_iterator(
                  ~get_loc,
                  ~print_item,
                  ~comments=cleaned_comments,
                  ~iterated_item=IteratedTupleConstructor,
                  parsed_types,
                );
              let printed_type_items = Doc.join(~sep=Doc.line, type_items);

              let printed_type_items_after_parens =
                Doc.concat([
                  force_break_if_line_comment(
                    ~separator=Doc.softLine,
                    after_paren_comments,
                  ),
                  printed_type_items,
                ]);

              Doc.group(
                Doc.concat([
                  Doc.lparen,
                  Comment_utils.single_line_of_comments(after_paren_comments),
                  Doc.indent(printed_type_items_after_parens),
                  Doc.softLine,
                  Doc.rparen,
                ]),
              );
            }
          | PConstrSingleton => Doc.nil
          },
        ]),
      );
    };

    let after_brace_comments =
      Comment_utils.get_after_brace_comments(~loc=data.pdata_loc, comments);
    let cleaned_comments =
      remove_used_comments(~remove_comments=after_brace_comments, comments);

    let decl_items =
      item_iterator(
        ~get_loc,
        ~print_item,
        ~comments=cleaned_comments,
        ~iterated_item=IteratedDataDeclarations,
        constr_declarations,
      );
    let printed_decls = Doc.join(~sep=Doc.hardLine, decl_items);

    let printed_decls_after_brace =
      Doc.concat([
        force_break_if_line_comment(
          ~separator=Doc.hardLine,
          after_brace_comments,
        ),
        printed_decls,
      ]);

    Doc.group(
      Doc.concat([
        Doc.text("enum"),
        Doc.space,
        Doc.text(nameloc.txt),
        switch (data.pdata_params) {
        | [] => Doc.space
        | [first, ...rem] =>
          let get_loc = (t: Parsetree.parsed_type) => t.ptyp_loc;
          let print_item = (~comments, t: Parsetree.parsed_type) => {
            print_type(~original_source, ~comments, t);
          };

          let after_angle_comments =
            Comment_utils.get_after_brace_comments(
              ~loc=get_loc(first),
              comments,
            );
          let cleaned_comments =
            remove_used_comments(
              ~remove_comments=after_angle_comments,
              comments,
            );

          let params =
            item_iterator(
              ~get_loc,
              ~print_item,
              ~comments=cleaned_comments,
              ~iterated_item=IteratedEnum,
              data.pdata_params,
            );

          let printed_data_params = Doc.join(~sep=Doc.line, params);

          let printed_data_params_after_angle =
            Doc.concat([
              force_break_if_line_comment(
                ~separator=Doc.softLine,
                after_angle_comments,
              ),
              printed_data_params,
            ]);

          Doc.group(
            Doc.concat([
              Doc.group(
                Doc.concat([
                  Doc.text("<"),
                  Comment_utils.single_line_of_comments(after_angle_comments),
                  Doc.indent(printed_data_params_after_angle),
                  Doc.softLine,
                  Doc.text(">"),
                ]),
              ),
              Doc.space,
            ]),
          );
        },
        Doc.lbrace,
        Comment_utils.single_line_of_comments(after_brace_comments),
        Doc.indent(printed_decls_after_brace),
        Doc.hardLine,
        Doc.rbrace,
      ]),
    );

  | PDataRecord(label_declarations) =>
    let get_loc = (lbl: Parsetree.label_declaration) => {
      lbl.pld_loc;
    };

    let print_item = (~comments, lbl: Parsetree.label_declaration) => {
      let is_mutable =
        switch (lbl.pld_mutable) {
        | Mutable => Doc.text("mut ")
        | Immutable => Doc.nil
        };
      Doc.concat([
        is_mutable,
        print_ident(lbl.pld_name.txt),
        Doc.text(":"),
        Doc.space,
        print_type(~original_source, ~comments, lbl.pld_type),
      ]);
    };

    let pre_brace_comments = []; // We can't determine from AST if comment comes before or after brace

    let remaining_comments =
      remove_used_comments(~remove_comments=pre_brace_comments, comments);

    let after_brace_comments =
      Comment_utils.get_after_brace_comments(
        ~loc=data.pdata_loc,
        remaining_comments,
      );

    let cleaned_comments =
      remove_used_comments(
        ~remove_comments=after_brace_comments,
        remaining_comments,
      );

    let decl_items =
      item_iterator(
        ~get_loc,
        ~print_item,
        ~comments=cleaned_comments,
        ~iterated_item=IteratedRecordLabels,
        label_declarations,
      );
    let printed_decls = Doc.join(~sep=Doc.hardLine, decl_items);
    let printed_decls_after_brace = Doc.concat([Doc.hardLine, printed_decls]);

    Doc.group(
      Doc.concat([
        Doc.text("record"),
        Doc.space,
        Doc.text(nameloc.txt),
        switch (data.pdata_params) {
        | [] => Doc.space
        | [first, ...rem] =>
          let get_loc = (t: Parsetree.parsed_type) => t.ptyp_loc;
          let print_item = (~comments, t: Parsetree.parsed_type) => {
            print_type(~original_source, ~comments, t);
          };

          let param_items =
            item_iterator(
              ~get_loc,
              ~print_item,
              ~comments=[],
              ~iterated_item=IteratedRecordData,
              data.pdata_params,
            );
          let printed_param_items = Doc.join(~sep=Doc.line, param_items);
          let printed_params_after_angle =
            Doc.concat([
              force_break_if_line_comment(
                ~separator=Doc.softLine,
                pre_brace_comments,
              ),
              printed_param_items,
            ]);
          Doc.group(
            Doc.concat([
              Doc.text("<"),
              Comment_utils.single_line_of_comments(pre_brace_comments),
              Doc.indent(printed_params_after_angle),
              Doc.softLine,
              Doc.text(">"),
              Doc.space,
            ]),
          );
        },
        Doc.concat([
          Doc.lbrace,
          Comment_utils.single_line_of_comments(after_brace_comments),
          Doc.indent(printed_decls_after_brace),
          Doc.hardLine,
          Doc.rbrace,
        ]),
      ]),
    );
  };
};
let data_print =
    (
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      datas: list((Parsetree.export_flag, Parsetree.data_declaration)),
    ) => {
  let previous_data: ref(option(Parsetree.data_declaration)) = ref(None);
  Doc.join(
    ~sep=Doc.concat([Doc.comma, Doc.hardLine]),
    List.map(
      data => {
        let (expt, decl: Parsetree.data_declaration) = data;

        let leading_comments =
          switch (previous_data^) {
          | None => []
          | Some(prev) =>
            Comment_utils.get_comments_between_locations(
              ~loc1=prev.pdata_loc,
              ~loc2=decl.pdata_loc,
              comments,
            )
          };

        let leading_comment_docs =
          Comment_utils.new_comments_to_docs(leading_comments);

        let data_comments =
          Comment_utils.get_comments_inside_location(
            ~location=decl.pdata_loc,
            comments,
          );

        previous_data := Some(decl);

        Doc.concat([
          leading_comment_docs,
          switch ((expt: Asttypes.export_flag)) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          },
          print_data(~original_source, ~comments=data_comments, decl),
        ]);
      },
      datas,
    ),
  );
};
let import_print =
    (
      ~comments: list(Parsetree.comment),
      ~original_source: array(string),
      imp: Parsetree.import_declaration,
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
              let get_loc = (identloc: Location.loc(Identifier.t)) => {
                identloc.loc;
              };

              let print_item =
                  (~comments, identloc: Location.loc(Identifier.t)) => {
                print_ident(identloc.txt);
              };

              let after_brace_comments =
                Comment_utils.get_after_brace_comments(
                  ~loc=imp.pimp_loc,
                  comments,
                );
              let cleaned_comments =
                remove_used_comments(
                  ~remove_comments=after_brace_comments,
                  comments,
                );

              let exceptions =
                block_item_iterator_line(
                  ~previous=Block(imp.pimp_loc),
                  ~get_loc,
                  ~print_item,
                  ~comments=cleaned_comments,
                  ~original_source,
                  ~separator=Doc.comma,
                  identlocs,
                );

              Doc.concat([
                Doc.space,
                Doc.text("except"),
                Doc.space,
                Doc.lbrace,
                Comment_utils.single_line_of_comments(after_brace_comments),
                Doc.indent(
                  Doc.concat([
                    force_break_if_line_comment(
                      ~separator=Doc.line,
                      after_brace_comments,
                    ),
                    exceptions,
                  ]),
                ),
                Doc.line,
                Doc.rbrace,
              ]);
            },
          ])
        | PImportValues(identlocsopts) =>
          switch (identlocsopts) {
          | [] => Doc.concat([Doc.lbrace, Doc.rbrace])

          | [first, ...rem] =>
            let get_loc =
                (
                  identlocopt: (
                    Parsetree.loc(Identifier.t),
                    option(Parsetree.loc(Identifier.t)),
                  ),
                ) => {
              let (loc, optloc) = identlocopt;
              loc.loc;
            };
            let after_brace_comments =
              Comment_utils.get_after_brace_comments(
                ~loc=imp.pimp_loc,
                comments,
              );
            let cleaned_comments =
              remove_used_comments(
                ~remove_comments=after_brace_comments,
                comments,
              );

            let print_item =
                (
                  ~comments,
                  identlocopt: (
                    Parsetree.loc(Identifier.t),
                    option(Parsetree.loc(Identifier.t)),
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
            };

            let printed_items =
              block_item_iterator_line(
                ~previous=Block(imp.pimp_loc),
                ~get_loc,
                ~print_item,
                ~comments=cleaned_comments,
                ~separator=Doc.comma,
                ~original_source,
                identlocsopts,
              );

            Doc.concat([
              Doc.lbrace,
              Comment_utils.single_line_of_comments(after_brace_comments),
              Doc.indent(
                Doc.concat([
                  force_break_if_line_comment(
                    ~separator=Doc.line,
                    after_brace_comments,
                  ),
                  printed_items,
                ]),
              ),
              Doc.line,
              Doc.rbrace,
            ]);
          }
        }
      },
      imp.pimp_val,
    );

  let path = imp.pimp_path.txt;

  Doc.group(
    Doc.concat([
      Doc.text("import "),
      Doc.join(~sep=Doc.concat([Doc.comma, Doc.space]), vals),
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

  let fixed_ident =
    if (infixop(ident) || prefixop(ident)) {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };
  Doc.concat([
    fixed_ident,
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
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      vd: Parsetree.value_description,
    ) => {
  let ident = vd.pval_name.txt;

  let fixed_ident =
    if (infixop(ident) || prefixop(ident)) {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };

  Doc.concat([
    fixed_ident,
    Doc.text(":"),
    Doc.space,
    print_type(~original_source, ~comments, vd.pval_type),
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
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      vd: Parsetree.value_description,
    ) => {
  let ident = vd.pval_name.txt;

  let fixed_ident =
    if (infixop(ident) || prefixop(ident)) {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };

  Doc.concat([
    fixed_ident,
    Doc.text(":"),
    Doc.space,
    print_type(~original_source, ~comments, vd.pval_type),
    Doc.space,
    Doc.equal,
    Doc.space,
    Doc.text("\""),
    Doc.join(~sep=Doc.text(","), List.map(p => Doc.text(p), vd.pval_prim)),
    Doc.text("\""),
  ]);
};

let toplevel_print =
    (
      ~original_source: array(string),
      ~comments: list(Parsetree.comment),
      data: Parsetree.toplevel_stmt,
    ) => {
  let without_comments =
    switch (data.ptop_desc) {
    | PTopImport(import_declaration) =>
      import_print(~comments, ~original_source, import_declaration)
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
          ~original_source,
          ~comments,
          value_description,
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
          ~original_source,
          ~comments,
          value_description,
        ),
      ]);
    | PTopData(data_declarations) =>
      data_print(~original_source, ~comments, data_declarations)

    | PTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
      print_value_bind(
        ~export_flag,
        ~rec_flag,
        ~mut_flag,
        ~original_source,
        ~comments,
        value_bindings,
      )
    | PTopExpr(expression) =>
      print_expression(
        ~expression_parent=GenericExpression,
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
                  ~sep=Doc.comma,
                  List.map(
                    t => print_type(~original_source, ~comments, t),
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
          ~sep=Doc.concat([Doc.comma, Doc.line]),
          List.map(print_export_declaration, export_declarations),
        ),
      ])

    | PTopExportAll(export_excepts) =>
      Doc.concat([
        Doc.text("export *"),
        if (List.length(export_excepts) > 0) {
          Doc.concat([
            Doc.space,
            Doc.text("except"),
            Doc.space,
            Doc.join(
              ~sep=Doc.comma,
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

let parse_source = (program_str: string) => {
  switch (
    {
      let lines = String.split_on_char('\n', program_str);
      let eol = Fs_access.determine_eol(List.nth_opt(lines, 0));
      let compile_state =
        Compile.compile_string(
          ~is_root_file=true,
          ~hook=stop_after_parse,
          ~name=?None,
          program_str,
        );

      (compile_state, lines, eol);
    }
  ) {
  | exception exn => Error(ParseError(exn))
  | ({cstate_desc: Parsed(parsed_program)}, lines, eol) =>
    Ok((parsed_program, Array.of_list(lines), eol))
  | _ => Error(InvalidCompilationState)
  };
};

let format_ast =
    (
      ~original_source: array(string),
      ~eol: Fs_access.eol,
      parsed_program: Parsetree.parsed_program,
    ) => {
  let get_loc = (stmt: Parsetree.toplevel_stmt) => {
    stmt.ptop_loc;
  };

  let print_item = (~comments, stmt: Parsetree.toplevel_stmt) => {
    toplevel_print(~original_source, ~comments, stmt);
  };

  let get_attributes = (stmt: Parsetree.toplevel_stmt) => {
    let attributes = stmt.ptop_attributes;
    print_attributes(attributes);
  };

  // special case where we have no code, we still format the comments

  let final_doc =
    switch (parsed_program.statements) {
    | [] => Comment_utils.new_comments_to_docs(parsed_program.comments)
    | _ =>
      let top_level_stmts =
        block_item_iterator(
          ~previous=TopOfFile,
          ~get_loc,
          ~print_item,
          ~comments=parsed_program.comments,
          ~print_attribute=get_attributes,
          ~original_source,
          parsed_program.statements,
        );
      Doc.concat([top_level_stmts, Doc.hardLine]);
    };

  Doc.toString(~width=80, ~eol, final_doc);
};
