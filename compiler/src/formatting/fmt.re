/**
  This module implements a formatter for Grain code. For information about the
  pretty-printing engine and specifics on its inner workings, see the Doc module.
*/
open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_diagnostics;
open Parsetree;
open Doc;
open Location;

exception FormatterError(string);

type compilation_error =
  | ParseError(exn)
  | InvalidCompilationState;

let parse_source = program_str => {
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
  | exception exn => Stdlib.Error(ParseError(exn))
  | ({cstate_desc: Parsed(parsed_program)}, lines, eol) =>
    Ok((parsed_program, Array.of_list(lines), eol))
  | _ => Error(InvalidCompilationState)
  };
};

type infix_grouping =
  | None
  | FormatterGrouping
  | ParenGrouping;

// As all (current) Grain infix operators have left-to-right associativity,
// operators with the same precedence do not need paren grouping on the left
// but do however need paren grouping on the right, as this indicates that the
// user grouped the operations in a particular manner.
// The only planned operator to have right-to-left associativity is
// exponentiation. When this is implemented, the logic is reversed.
type infix_side =
  | Left
  | Right;

// This takes a location and makes the loc_end the same as loc_start.
// Its main purpose is to find comments between the start of an enclosing location and the first item inside.
let enclosing_start_location = loc => {
  {...loc, loc_end: loc.loc_start};
};

// This takes a location and makes the loc_start the same as loc_end.
// Its main purpose is to find comments between the end of an enclosing location and the last item inside.
let enclosing_end_location = loc => {
  {...loc, loc_start: loc.loc_end};
};

let is_same_op = (expr1, expr2) =>
  switch (expr1.pexp_desc, expr2.pexp_desc) {
  | (
      PExpId({txt: Identifier.IdentName({txt: op1})}),
      PExpId({txt: Identifier.IdentName({txt: op2})}),
    ) =>
    op1 == op2
  | _ => false
  };

let is_shift_or_concat_op = expr =>
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) =>
    if (String.length(op) > 1) {
      switch (String.sub(op, 0, 2)) {
      | "<<"
      | ">>"
      | "++" => true
      | _ => false
      };
    } else {
      false;
    }
  | _ => false
  };

let is_logic_op = expr =>
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) =>
    if (String.length(op) > 1) {
      switch (String.sub(op, 0, 2)) {
      | "<="
      | ">="
      | "=="
      | "!="
      | "is"
      | "&&"
      | "||" => true
      | _ => false
      };
    } else {
      false;
    }
  | _ => false
  };

let is_math_op = expr =>
  if (is_logic_op(expr) || is_shift_or_concat_op(expr)) {
    false;
  } else {
    switch (expr.pexp_desc) {
    | PExpId({txt: Identifier.IdentName({txt: op})}) =>
      if (String.length(op) > 0) {
        switch (op.[0]) {
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
      }
    | _ => false
    };
  };

let op_precedence = startsWith =>
  switch (startsWith) {
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

let precedence = expr => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) =>
    if (String.length(op) > 1) {
      switch (String.sub(op, 0, 2)) {
      | "++" => 110
      | "<<"
      | ">>" => 100
      | "=="
      | "!="
      | "is" => 80
      | "&&" => 40
      | "||"
      | "??" => 30
      | _ => op_precedence(op.[0])
      };
    } else if (String.length(op) > 0) {
      op_precedence(op.[0]);
    } else {
      9999;
    }
  | _ => 9999
  };
};

let infixop = op => {
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
  | _
  | exception _ => false
  };
};

let is_infix_op = expr => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) => infixop(op)
  | _ => false
  };
};

let prefixop = op =>
  switch (op.[0]) {
  | '!' => true
  | _
  | exception _ => false
  };

let is_prefix_op = expr => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) => prefixop(op)
  | _ => false
  };
};

let is_keyword_function = expr => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: "assert" | "throw" | "fail"})}) =>
    true
  | _ => false
  };
};

let needs_grouping = (~parent, ~side: infix_side, expr) => {
  switch (expr.pexp_desc, side) {
  | (PExpIf(_), _) => ParenGrouping
  | (PExpApp(fn1, _), Left)
      when is_infix_op(fn1) && precedence(fn1) < precedence(parent) =>
    ParenGrouping
  | (PExpApp(fn1, _), Right)
      when is_infix_op(fn1) && precedence(fn1) <= precedence(parent) =>
    ParenGrouping
  | (PExpApp(fn1, _), _) =>
    if (is_infix_op(fn1)) {
      if ((!is_math_op(parent) && !is_logic_op(parent))
          && !is_same_op(fn1, parent)) {
        ParenGrouping;
      } else if (precedence(fn1) == precedence(parent)) {
        None;
      } else {
        FormatterGrouping;
      };
    } else {
      FormatterGrouping;
    }
  | (PExpConstant(PConstNumber(PConstNumberRational(_, _))), _)
      when op_precedence('/') <= precedence(parent) =>
    ParenGrouping
  | _ => FormatterGrouping
  };
};

let build_document = (~original_source, parsed_program) => {
  let comment_tree = Commenttree.from_comments(parsed_program.comments);

  let get_original_code = location => {
    let (_, start_line, startc, _) =
      Locations.get_raw_pos_info(location.loc_start);
    let (_, end_line, endc, _) =
      Locations.get_raw_pos_info(location.loc_end);

    let (++) = Stdlib.(++);

    if (Array.length(original_source) > end_line - 1) {
      if (start_line == end_line) {
        String_utils.Utf8.sub(
          original_source[start_line - 1],
          startc,
          endc - startc,
        );
      } else {
        let text = ref("");
        for (line in start_line - 1 to end_line - 1) {
          if (line + 1 == start_line) {
            text :=
              text^
              ++ String_utils.Utf8.string_after(original_source[line], startc)
              ++ "\n";
          } else if (line + 1 == end_line) {
            text :=
              text^ ++ String_utils.Utf8.sub(original_source[line], 0, endc);
          } else {
            text := text^ ++ original_source[line] ++ "\n";
          };
        };
        text^;
      };
    } else {
      raise(FormatterError("Requested beyond end of original source"));
    };
  };

  let has_disable_formatting_comment = loc => {
    switch (Commenttree.query_line(comment_tree, loc.loc_start.pos_lnum - 1)) {
    | Some(Line({cmt_content: "formatter-ignore"})) => true
    | _ => false
    };
  };

  let rec print_infix_prefix_op = expr => {
    switch (expr.pexp_desc) {
    | PExpId({txt: Identifier.IdentName({txt: op})}) => string(op)
    | _ => failwith("Impossible: non- prefix or infix op")
    };
  }
  and print_constant = (~loc, constant) => {
    string(get_original_code(loc));
  }
  and print_punnable_pattern =
      (
        ({txt: ident, loc: ident_loc}, pat): (
          Location.loc(Identifier.t),
          pattern,
        ),
      ) => {
    switch (pat.ppat_desc) {
    | PPatVar({txt: name}) when Identifier.string_of_ident(ident) == name =>
      // Don't forget the comments that could have been between a punnable name and value, e.g.
      // { foo: /* foo */ foo, }
      print_comment_range(~trail=space, ident_loc, pat.ppat_loc)
      ++ string(name)
    | _ =>
      print_identifier(ident)
      ++ string(":")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           ident_loc,
           pat.ppat_loc,
         )
      ++ print_pattern(pat)
    };
  }
  and print_lambda_argument = arg => {
    print_pattern(arg.pla_pattern)
    ++ (
      switch (arg.pla_default) {
      | Some(expr) =>
        string("=")
        ++ print_comment_range(arg.pla_pattern.ppat_loc, expr.pexp_loc)
        ++ print_expression(expr)
      | None => empty
      }
    );
  }
  and print_pattern = ({ppat_desc, ppat_loc}) => {
    switch (ppat_desc) {
    | PPatAny => string("_")
    | PPatVar({txt: name}) => print_ident_string(name)
    | PPatAlias(pat, {txt: alias, loc: alias_loc}) =>
      print_pattern(pat)
      ++ string(" as")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           pat.ppat_loc,
           alias_loc,
         )
      ++ string(alias)
    | PPatOr(lhs, rhs) =>
      print_pattern(lhs)
      ++ string(" |")
      ++ print_comment_range(
           ~none=breakable_space,
           ~lead=space,
           ~trail=breakable_space,
           lhs.ppat_loc,
           rhs.ppat_loc,
         )
      ++ print_pattern(rhs)
    | PPatConstruct({txt: ident, loc: ident_loc}, cstr_pat) =>
      print_identifier(ident)
      ++ (
        switch (cstr_pat) {
        | PPatConstrRecord([], closed_flag) =>
          braces(
            indent(
              breakable_space
              ++ (
                switch (closed_flag) {
                | Open => string("_")
                | Closed => empty
                }
              )
              ++ print_comment_range(
                   ident_loc,
                   enclosing_end_location(ppat_loc),
                 ),
            )
            ++ breakable_space,
          )
        | PPatConstrRecord(pats, closed_flag) =>
          braces(
            indent(
              concat_map(
                ~lead=
                  ((next_ident, _)) =>
                    print_comment_range(
                      ~none=breakable_space,
                      ~lead=space,
                      ~trail=breakable_space,
                      ident_loc,
                      next_ident.loc,
                    ),
                ~sep=
                  (({loc: prev_loc}, _), ({loc: next_loc}, _)) => {
                    print_comment_range(
                      ~none=breakable_space,
                      ~lead=space,
                      ~trail=breakable_space,
                      prev_loc,
                      next_loc,
                    )
                  },
                ~trail=
                  (({loc: prev_loc}, _)) =>
                    print_comment_range(
                      ~lead=space,
                      ~block_end=true,
                      prev_loc,
                      enclosing_end_location(ppat_loc),
                    ),
                ~f=
                  (~final, p) =>
                    if (final) {
                      group(print_punnable_pattern(p))
                      ++ (
                        switch (closed_flag) {
                        | Open => comma_breakable_space ++ string("_")
                        | Closed => trailing_comma
                        }
                      );
                    } else {
                      group(print_punnable_pattern(p) ++ comma);
                    },
                pats,
              ),
            )
            ++ breakable_space,
          )
        | PPatConstrSingleton => empty
        | PPatConstrTuple(pats) =>
          parens(
            indent(
              concat_map(
                ~lead=
                  ({ppat_loc: next}) =>
                    print_comment_range(
                      ~none=break,
                      ~lead=if_broken(space, empty),
                      ~trail=breakable_space,
                      ident_loc,
                      next,
                    ),
                ~sep=
                  ({ppat_loc: prev}, {ppat_loc: next}) => {
                    print_comment_range(
                      ~none=breakable_space,
                      ~lead=space,
                      ~trail=breakable_space,
                      prev,
                      next,
                    )
                  },
                ~trail=
                  ({ppat_loc: prev}) =>
                    print_comment_range(
                      ~lead=space,
                      ~block_end=true,
                      prev,
                      enclosing_end_location(ppat_loc),
                    ),
                ~f=
                  (~final, p) =>
                    if (final) {
                      group(print_pattern(p)) ++ trailing_comma;
                    } else {
                      group(print_pattern(p) ++ comma);
                    },
                pats,
              ),
            )
            ++ break,
          )
        }
      )
    | PPatConstraint(pat, typ) =>
      print_pattern(pat)
      ++ string(":")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           pat.ppat_loc,
           typ.ptyp_loc,
         )
      ++ print_type(typ)
    | PPatConstant(constant) => print_constant(~loc=ppat_loc, constant)
    | PPatRecord(pats, closed_flag) =>
      braces(
        indent(
          concat_map(
            ~lead=
              ((next_ident, _)) =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  enclosing_start_location(ppat_loc),
                  next_ident.loc,
                ),
            ~sep=
              ((_, {ppat_loc: prev_loc}), ({loc: next_loc}, _)) => {
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev_loc,
                  next_loc,
                )
              },
            ~trail=
              ((_, {ppat_loc: prev_loc})) =>
                print_comment_range(
                  ~lead=space,
                  ~block_end=true,
                  prev_loc,
                  enclosing_end_location(ppat_loc),
                ),
            ~f=
              (~final, p) =>
                if (final) {
                  group(print_punnable_pattern(p))
                  ++ (
                    switch (closed_flag) {
                    | Open when pats == [] => string("_")
                    | Open => comma_breakable_space ++ string("_")
                    | Closed => trailing_comma
                    }
                  );
                } else {
                  group(print_punnable_pattern(p) ++ comma);
                },
            pats,
          ),
        )
        ++ breakable_space,
      )
    | PPatArray([]) =>
      array_brackets(
        indent(
          print_comment_range(
            ~block_end=true,
            ~none=break,
            ~lead=space,
            enclosing_start_location(ppat_loc),
            enclosing_end_location(ppat_loc),
          ),
        )
        ++ break,
      )
    | PPatArray(pats) =>
      array_brackets(
        indent(
          concat_map(
            ~lead=
              next =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  enclosing_start_location(ppat_loc),
                  next.ppat_loc,
                ),
            ~sep=
              (prev, next) =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev.ppat_loc,
                  next.ppat_loc,
                ),
            ~trail=
              prev =>
                print_comment_range(
                  ~lead=space,
                  ~block_end=true,
                  prev.ppat_loc,
                  enclosing_end_location(ppat_loc),
                ),
            ~f=
              (~final, p) =>
                if (final) {
                  group(print_pattern(p)) ++ trailing_comma;
                } else {
                  group(print_pattern(p) ++ comma);
                },
            pats,
          ),
        )
        ++ break,
      )
    | PPatList([]) =>
      list_brackets(
        indent(
          print_comment_range(
            ~block_end=true,
            ~none=break,
            ~lead=if_broken(space, empty),
            enclosing_start_location(ppat_loc),
            enclosing_end_location(ppat_loc),
          ),
        )
        ++ break,
      )
    | PPatList(pats) =>
      list_brackets(
        indent(
          concat_map(
            ~lead=
              next =>
                print_comment_range(
                  ~none=break,
                  ~lead=if_broken(space, empty),
                  ~trail=breakable_space,
                  enclosing_start_location(ppat_loc),
                  switch (next) {
                  | ListItem(pat)
                  | ListSpread(pat, _) => pat.ppat_loc
                  },
                ),
            ~sep=
              (prev, next) =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  switch (prev) {
                  | ListItem(pat)
                  | ListSpread(pat, _) => pat.ppat_loc
                  },
                  switch (next) {
                  | ListItem(pat)
                  | ListSpread(pat, _) => pat.ppat_loc
                  },
                ),
            ~trail=
              prev =>
                print_comment_range(
                  ~lead=space,
                  ~block_end=true,
                  switch (prev) {
                  | ListItem(pat)
                  | ListSpread(pat, _) => pat.ppat_loc
                  },
                  enclosing_end_location(ppat_loc),
                ),
            ~f=
              (~final, item) => {
                switch (item) {
                | ListItem(pat) when final =>
                  group(print_pattern(pat)) ++ trailing_comma
                | ListItem(pat) => group(print_pattern(pat) ++ comma)
                | ListSpread(pat, _) when final =>
                  group(string("...") ++ print_pattern(pat))
                | ListSpread(pat, _) =>
                  group(string("...") ++ print_pattern(pat) ++ comma)
                }
              },
            pats,
          ),
        )
        ++ break,
      )
    | PPatTuple(pats) =>
      parens(
        indent(
          concat_map(
            ~lead=
              ({ppat_loc: next}) =>
                print_comment_range(
                  ~none=break,
                  ~lead=if_broken(space, empty),
                  ~trail=breakable_space,
                  enclosing_start_location(ppat_loc),
                  next,
                ),
            ~sep=
              ({ppat_loc: prev}, {ppat_loc: next}) => {
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev,
                  next,
                )
              },
            ~trail=
              ({ppat_loc: prev}) =>
                print_comment_range(
                  ~lead=space,
                  ~block_end=true,
                  prev,
                  enclosing_end_location(ppat_loc),
                ),
            ~f=
              (~final, p) =>
                if (final) {
                  group(print_pattern(p)) ++ trailing_comma;
                } else {
                  group(print_pattern(p) ++ comma);
                },
            pats,
          ),
        )
        ++ break,
      )
    };
  }
  and print_ident_string = ident =>
    if (infixop(ident) || prefixop(ident)) {
      parens(string(ident));
    } else {
      string(ident);
    }
  and print_identifier = ident => {
    print_ident_string(Identifier.string_of_ident(ident));
  }
  and print_punnable_expression = (({txt: ident, loc: ident_loc}, expr)) => {
    switch (expr.pexp_desc) {
    | PExpId({txt: name}) when Identifier.equal(ident, name) =>
      // Don't forget the comments that could have been between a punnable name and value, e.g.
      // { foo: /* foo */ foo, }
      print_comment_range(~trail=space, ident_loc, expr.pexp_loc)
      ++ print_identifier(name)
    | _ =>
      print_identifier(ident)
      ++ string(":")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           ident_loc,
           expr.pexp_loc,
         )
      ++ print_expression(expr)
    };
  }
  and print_grouped_access_expression = expr =>
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
    | PExpArray(_)
    | PExpList(_) => print_expression(expr)
    | PExpApp(func, _) when is_infix_op(func) =>
      parens(indent(break ++ print_expression(expr)) ++ break)
    | PExpApp(_) => print_expression(expr)
    | _ => parens(indent(break ++ print_expression(expr)) ++ break)
    }
  and print_use_item = use_item => {
    switch (use_item) {
    | PUseType({name, alias, loc}) =>
      string("type")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           enclosing_start_location(loc),
           name.loc,
         )
      ++ print_identifier(name.txt)
      ++ (
        switch (alias) {
        | None => empty
        | Some({txt: alias, loc: alias_loc}) =>
          string(" as")
          ++ print_comment_range(
               ~none=space,
               ~lead=space,
               ~trail=space,
               name.loc,
               alias_loc,
             )
          ++ print_identifier(alias)
        }
      )
    | PUseException({name, alias, loc}) =>
      string("exception")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           enclosing_start_location(loc),
           name.loc,
         )
      ++ print_identifier(name.txt)
      ++ (
        switch (alias) {
        | None => empty
        | Some({txt: alias, loc: alias_loc}) =>
          string(" as")
          ++ print_comment_range(
               ~none=space,
               ~lead=space,
               ~trail=space,
               name.loc,
               alias_loc,
             )
          ++ print_identifier(alias)
        }
      )
    | PUseModule({name, alias, loc}) =>
      string("module")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           enclosing_start_location(loc),
           name.loc,
         )
      ++ print_identifier(name.txt)
      ++ (
        switch (alias) {
        | None => empty
        | Some({txt: alias, loc: alias_loc}) =>
          string(" as")
          ++ print_comment_range(
               ~none=space,
               ~lead=space,
               ~trail=space,
               name.loc,
               alias_loc,
             )
          ++ print_identifier(alias)
        }
      )
    | PUseValue({name, alias}) =>
      print_identifier(name.txt)
      ++ (
        switch (alias) {
        | None => empty
        | Some({txt: alias, loc: alias_loc}) =>
          string(" as")
          ++ print_comment_range(
               ~none=space,
               ~lead=space,
               ~trail=space,
               name.loc,
               alias_loc,
             )
          ++ print_identifier(alias)
        }
      )
    };
  }
  and print_match_branch = ({pmb_pat, pmb_body, pmb_guard}) => {
    let space_type =
      switch (pmb_body.pexp_desc) {
      | PExpBlock(_) => space
      | _ => breakable_space
      };

    switch (pmb_guard) {
    | None =>
      group(print_pattern(pmb_pat) ++ string(" =>"))
      ++ group(
           indent(
             print_comment_range(
               ~none=space_type,
               ~lead=space,
               ~trail=space_type,
               pmb_pat.ppat_loc,
               pmb_body.pexp_loc,
             )
             ++ group(print_expression(pmb_body) ++ comma),
           ),
         )
    | Some(guard) =>
      group(print_pattern(pmb_pat))
      ++ group(
           ~kind=FitAll,
           indent(
             print_comment_range(
               ~none=breakable_space,
               ~lead=space,
               ~trail=breakable_space,
               pmb_pat.ppat_loc,
               guard.pexp_loc,
             )
             ++ string("when ")
             ++ group(print_expression(guard)),
           )
           ++ string(" =>"),
         )
      ++ group(
           indent(
             print_comment_range(
               ~none=space_type,
               ~lead=space,
               ~trail=space_type,
               guard.pexp_loc,
               pmb_body.pexp_loc,
             )
             ++ group(print_expression(pmb_body) ++ comma),
           ),
         )
    };
  }
  and print_attribute = attr => {
    switch (attr) {
    | Asttypes.{attr_name: {txt: attr_name}, attr_args: []} =>
      string("@") ++ string(attr_name)
    | {attr_name: {txt: attr_name, loc: attr_name_loc}, attr_args, attr_loc} =>
      string("@")
      ++ string(attr_name)
      ++ parens(
           indent(
             concat_map(
               ~lead=
                 next =>
                   print_comment_range(
                     ~none=break,
                     ~lead=if_broken(space, empty),
                     ~trail=breakable_space,
                     attr_name_loc,
                     next.loc,
                   ),
               ~sep=
                 (prev, next) =>
                   print_comment_range(
                     ~none=breakable_space,
                     ~lead=space,
                     ~trail=breakable_space,
                     prev.loc,
                     next.loc,
                   ),
               ~trail=
                 prev =>
                   print_comment_range(
                     ~block_end=true,
                     ~lead=space,
                     prev.loc,
                     enclosing_end_location(attr_loc),
                   ),
               ~f=
                 (~final, attr_arg) =>
                   if (final) {
                     double_quotes(string(attr_arg.txt)) ++ trailing_comma;
                   } else {
                     double_quotes(string(attr_arg.txt)) ++ comma;
                   },
               attr_args,
             ),
           )
           ++ break,
         )
    };
  }
  and print_application_argument = (~infix_wrap=?, arg) => {
    (
      switch (arg.paa_label) {
      | Unlabeled => empty
      | Labeled({txt: label, loc: label_loc})
      | Default({txt: label, loc: label_loc}) =>
        string(label)
        ++ string("=")
        ++ print_comment_range(label_loc, arg.paa_expr.pexp_loc)
      }
    )
    ++ print_expression(~infix_wrap?, arg.paa_expr);
  }
  and print_if =
      (~force_blocks=false, ~loc, condition, true_branch, false_branch) =>
    if (force_blocks) {
      let true_branch_doc =
        switch (true_branch.pexp_desc) {
        | PExpBlock(_) => print_expression(true_branch)
        | PExpIf(_) =>
          parens(indent(break ++ print_expression(true_branch)) ++ break)
        | _ =>
          braces(
            ~wrap=doc => group(~print_width=2, doc),
            indent(hardline ++ print_expression(true_branch)) ++ hardline,
          )
        };
      let false_branch_doc =
        switch (false_branch) {
        | Some({pexp_desc: PExpBlock(_)} as false_branch) =>
          Some(print_expression(false_branch))
        | Some({
            pexp_desc: PExpIf(condition, true_branch, false_branch),
            pexp_loc: loc,
          }) =>
          Some(
            print_if(
              ~loc,
              ~force_blocks,
              condition,
              true_branch,
              false_branch,
            ),
          )
        | Some(false_branch) =>
          Some(
            braces(
              ~wrap=doc => group(~print_width=2, doc),
              indent(hardline ++ print_expression(false_branch)) ++ hardline,
            ),
          )
        | None => None
        };
      group(
        string("if ")
        ++ parens(
             indent(
               print_comment_range(
                 ~none=break,
                 ~lead=if_broken(space, empty),
                 ~trail=breakable_space,
                 enclosing_start_location(loc),
                 condition.pexp_loc,
               )
               ++ print_expression(~infix_wrap=Fun.id, condition)
               ++ print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    condition.pexp_loc,
                    true_branch.pexp_loc,
                  ),
             )
             ++ break,
           )
        ++ space
        ++ true_branch_doc
        ++ (
          switch (false_branch_doc) {
          | Some(false_branch_doc) =>
            print_comment_range(
              ~none=space,
              ~lead=space,
              ~trail=space,
              true_branch.pexp_loc,
              Option.get(false_branch).pexp_loc,
            )
            ++ string("else ")
            ++ false_branch_doc
          | None => empty
          }
        ),
      );
    } else {
      switch (true_branch.pexp_desc, false_branch) {
      | (PExpBlock(_), _)
      | (_, Some({pexp_desc: PExpBlock(_) | PExpIf(_)})) =>
        print_if(
          ~loc,
          ~force_blocks=true,
          condition,
          true_branch,
          false_branch,
        )
      | (_, None) =>
        let true_branch_doc =
          switch (true_branch.pexp_desc) {
          | PExpIf(_) =>
            parens(indent(break ++ print_expression(true_branch)) ++ break)
          | _ => print_expression(true_branch)
          };
        group(
          string("if ")
          ++ parens(
               indent(
                 print_comment_range(
                   ~none=break,
                   ~lead=if_broken(space, empty),
                   ~trail=breakable_space,
                   enclosing_start_location(loc),
                   condition.pexp_loc,
                 )
                 ++ print_expression(~infix_wrap=Fun.id, condition)
                 ++ print_comment_range(
                      ~block_end=true,
                      ~lead=space,
                      condition.pexp_loc,
                      true_branch.pexp_loc,
                    ),
               )
               ++ break,
             )
          ++ indent(breakable_space ++ true_branch_doc),
        );
      | (_, Some(false_branch)) =>
        let true_branch_doc =
          switch (true_branch.pexp_desc) {
          | PExpIf(_) =>
            parens(indent(break ++ print_expression(true_branch)) ++ break)
          | _ => print_expression(true_branch)
          };
        group(
          string("if ")
          ++ parens(
               indent(
                 print_comment_range(
                   ~none=break,
                   ~lead=if_broken(space, empty),
                   ~trail=breakable_space,
                   enclosing_start_location(loc),
                   condition.pexp_loc,
                 )
                 ++ print_expression(~infix_wrap=Fun.id, condition)
                 ++ print_comment_range(
                      ~block_end=true,
                      ~lead=space,
                      condition.pexp_loc,
                      true_branch.pexp_loc,
                    ),
               )
               ++ break,
             )
          ++ indent(breakable_space ++ true_branch_doc)
          ++ print_comment_range(
               ~none=breakable_space,
               ~lead=space,
               ~trail=breakable_space,
               true_branch.pexp_loc,
               false_branch.pexp_loc,
             )
          ++ string("else")
          ++ indent(breakable_space ++ print_expression(false_branch)),
        );
      };
    }
  and print_assignment = (~collapsible, ~lhs_loc, new_value) => {
    switch (new_value.pexp_desc) {
    | PExpApp(
        {
          pexp_desc:
            PExpId({
              txt:
                Identifier.IdentName({
                  txt: ("+" | "-" | "*" | "/" | "%") as op,
                }),
            }),
        },
        [_arg1, arg2],
      )
        when collapsible =>
      space
      ++ string(op)
      ++ string("=")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           lhs_loc,
           // TODO(#1977): There appears to be a bug with the parser that the location of
           // paa_loc is further to the left than the underlying expression, so
           // here we just use the location of the expression directly.
           arg2.paa_expr.pexp_loc,
         )
      ++ print_application_argument(arg2)
    | _ =>
      string(" =")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           lhs_loc,
           new_value.pexp_loc,
         )
      ++ print_expression(new_value)
    };
  }
  and print_expression = (~infix_wrap=d => group(indent(d)), expr) => {
    group(
      concat_map(
        ~lead=_ => empty,
        ~sep=
          (prev, next) =>
            print_comment_range(
              ~none=hardline,
              ~lead=space,
              ~trail=hardline,
              prev.Asttypes.attr_loc,
              next.attr_loc,
            ),
        ~trail=
          prev =>
            print_comment_range(
              ~none=hardline,
              ~lead=space,
              ~trail=hardline,
              prev.Asttypes.attr_loc,
              expr.pexp_core_loc,
            ),
        ~f=(~final, a) => print_attribute(a),
        expr.pexp_attributes,
      ),
    )
    ++ (
      switch (expr.pexp_desc) {
      | PExpId({txt: ident}) => print_identifier(ident)
      | PExpConstant(constant) =>
        print_constant(~loc=expr.pexp_loc, constant)
      | PExpConstruct({txt: ident, loc: ident_loc}, cstr_expr) =>
        print_identifier(ident)
        ++ (
          switch (cstr_expr) {
          | PExpConstrSingleton => empty
          | PExpConstrTuple(exprs) =>
            parens(
              indent(
                concat_map(
                  ~lead=
                    next =>
                      print_comment_range(
                        ~none=break,
                        ~lead=if_broken(space, empty),
                        ~trail=breakable_space,
                        ident_loc,
                        next.pexp_loc,
                      ),
                  ~sep=
                    (prev, next) =>
                      print_comment_range(
                        ~none=breakable_space,
                        ~lead=space,
                        ~trail=breakable_space,
                        prev.pexp_loc,
                        next.pexp_loc,
                      ),
                  ~trail=
                    prev =>
                      print_comment_range(
                        ~lead=space,
                        ~block_end=true,
                        prev.pexp_loc,
                        enclosing_end_location(expr.pexp_loc),
                      ),
                  ~f=
                    (~final, e) =>
                      if (final) {
                        group(print_expression(e)) ++ trailing_comma;
                      } else {
                        group(print_expression(e) ++ comma);
                      },
                  exprs,
                ),
              )
              ++ break,
            )
          | PExpConstrRecord(exprs) =>
            braces(
              indent(
                concat_map(
                  ~lead=
                    ((next_ident, _)) =>
                      print_comment_range(
                        ~none=breakable_space,
                        ~lead=space,
                        ~trail=breakable_space,
                        ident_loc,
                        next_ident.loc,
                      ),
                  ~sep=
                    ((_, prev), (next, _)) =>
                      print_comment_range(
                        ~none=breakable_space,
                        ~lead=space,
                        ~trail=breakable_space,
                        prev.pexp_loc,
                        next.loc,
                      ),
                  ~trail=
                    ((_, prev)) =>
                      print_comment_range(
                        ~lead=space,
                        ~block_end=true,
                        prev.pexp_loc,
                        enclosing_end_location(expr.pexp_loc),
                      ),
                  ~f=
                    (~final, e) =>
                      if (final) {
                        group(print_punnable_expression(e));
                      } else {
                        group(print_punnable_expression(e) ++ comma);
                      },
                  exprs,
                ),
              )
              ++ breakable_space,
            )
          }
        )
      | PExpBlock(exprs) =>
        braces(
          ~wrap=doc => group(~print_width=2, doc),
          indent(
            concat_map(
              ~lead=
                first =>
                  print_comment_range(
                    ~none=hardline,
                    ~lead=space,
                    ~trail=hardline,
                    enclosing_start_location(expr.pexp_loc),
                    first.pexp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=
                      switch (
                        next.pexp_loc.loc_start.pos_lnum
                        - prev.pexp_loc.loc_end.pos_lnum
                      ) {
                      | 0
                      | 1 => hardline
                      | _ => hardline ++ hardline
                      },
                    ~lead=space,
                    ~trail=space,
                    prev.pexp_loc,
                    next.pexp_loc,
                  ),
              ~trail=
                last =>
                  print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    last.pexp_loc,
                    enclosing_end_location(expr.pexp_loc),
                  ),
              ~f=
                (~final, e) =>
                  if (has_disable_formatting_comment(e.pexp_loc)) {
                    string(get_original_code(e.pexp_loc));
                  } else {
                    print_expression(e);
                  },
              exprs,
            ),
          )
          ++ hardline,
        )
      | PExpLet(rec_flag, mut_flag, vbs) =>
        string("let ")
        ++ (
          switch (rec_flag) {
          | Nonrecursive => empty
          | Recursive => string("rec ")
          }
        )
        ++ (
          switch (mut_flag) {
          | Immutable => empty
          | Mutable => string("mut ")
          }
        )
        ++ print_comment_range(
             ~allow_breaks=false,
             ~trail=space,
             enclosing_start_location(expr.pexp_loc),
             List.hd(vbs).pvb_loc,
           )
        ++ group @@
        concat_map(
          ~lead=_ => empty,
          ~sep=
            (prev, next) =>
              print_comment_range(
                ~none=hardline,
                ~lead=space,
                ~trail=hardline,
                prev.pvb_loc,
                next.pvb_loc,
              )
              ++ string("and "),
          ~trail=_ => empty,
          ~f=(~final, vb) => print_value_binding(vb),
          vbs,
        )
      | PExpApp(fn, [arg]) when is_prefix_op(fn) =>
        print_infix_prefix_op(fn)
        ++ print_comment_range(fn.pexp_loc, arg.paa_loc)
        ++ (
          switch (needs_grouping(~parent=fn, ~side=Left, arg.paa_expr)) {
          | ParenGrouping =>
            parens(
              indent(break ++ print_application_argument(arg)) ++ break,
            )
          | FormatterGrouping => group(print_application_argument(arg))
          | None => print_application_argument(arg)
          }
        )
      | PExpApp(fn, [lhs, rhs]) when is_infix_op(fn) =>
        // To ensure adequate grouping/breaking of subexpressions, chains of
        // binops are included in a single Doc.group, with new groups inserted
        // where necessary. By default, this group indents when breaking. This
        // behavior is overridden by passing ~infix_wrap=Fun.id to
        // print_expression. This is particularly useful for things like the
        // condition of an `if` statement, where we don't need the additional
        // indenting.
        infix_wrap @@
        (
          switch (needs_grouping(~parent=fn, ~side=Left, lhs.paa_expr)) {
          | ParenGrouping =>
            parens(
              indent(
                break ++ print_application_argument(~infix_wrap=Fun.id, lhs),
              )
              ++ break,
            )
          | FormatterGrouping =>
            group(
              indent(print_application_argument(~infix_wrap=Fun.id, lhs)),
            )
          | None => print_application_argument(~infix_wrap=Fun.id, lhs)
          }
        )
        ++ print_comment_range(
             ~none=space,
             ~lead=space,
             ~trail=space,
             ~allow_breaks=false,
             lhs.paa_loc,
             fn.pexp_loc,
           )
        ++ print_infix_prefix_op(fn)
        ++ print_comment_range(
             ~none=breakable_space,
             ~lead=space,
             ~trail=breakable_space,
             fn.pexp_loc,
             rhs.paa_loc,
           )
        ++ (
          switch (needs_grouping(~parent=fn, ~side=Right, rhs.paa_expr)) {
          | ParenGrouping =>
            parens(
              indent(
                break ++ print_application_argument(~infix_wrap=Fun.id, rhs),
              )
              ++ break,
            )
          | FormatterGrouping =>
            group(
              indent(print_application_argument(~infix_wrap=Fun.id, rhs)),
            )
          | None => print_application_argument(~infix_wrap=Fun.id, rhs)
          }
        )
      | PExpApp(fn, [rhs]) when is_keyword_function(fn) =>
        print_expression(fn)
        ++ print_comment_range(
             ~none=space,
             ~lead=space,
             ~trail=space,
             fn.pexp_loc,
             rhs.paa_loc,
           )
        ++ print_expression(rhs.paa_expr)
      | PExpApp(fn, exprs) =>
        group(
          print_grouped_access_expression(fn)
          ++ parens(
               indent(
                 concat_map(
                   ~lead=
                     next =>
                       print_comment_range(
                         ~none=break,
                         ~lead=if_broken(space, empty),
                         ~trail=breakable_space,
                         fn.pexp_loc,
                         next.paa_loc,
                       ),
                   ~sep=
                     (prev, next) =>
                       print_comment_range(
                         ~none=breakable_space,
                         ~lead=space,
                         ~trail=breakable_space,
                         prev.paa_loc,
                         next.paa_loc,
                       ),
                   ~trail=
                     prev =>
                       print_comment_range(
                         ~block_end=true,
                         ~lead=space,
                         prev.paa_loc,
                         enclosing_end_location(expr.pexp_loc),
                       ),
                   ~f=
                     (~final, a) =>
                       if (final) {
                         group(print_application_argument(a));
                       } else {
                         group(print_application_argument(a) ++ comma);
                       },
                   exprs,
                 ),
               )
               ++ break,
             ),
        )
      | PExpLambda(
          [
            {
              pla_label: Labeled({txt: label, loc: label_loc}),
              pla_pattern: {ppat_desc: PPatVar({txt: var})},
            } as single_param,
          ],
          body,
        )
          when label == var =>
        print_lambda_argument(single_param)
        ++ string(" =>")
        ++ print_comment_range(~lead=space, label_loc, body.pexp_loc)
        ++ group(
             switch (body.pexp_desc) {
             | PExpBlock(_) => space ++ print_expression(body)
             | _ => indent(breakable_space ++ print_expression(body))
             },
           )
      | PExpLambda(params, body) =>
        parens(
          indent(
            concat_map(
              ~lead=
                next =>
                  print_comment_range(
                    ~none=break,
                    ~lead=if_broken(space, empty),
                    ~trail=breakable_space,
                    enclosing_start_location(expr.pexp_loc),
                    next.pla_loc,
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.pla_loc,
                    next.pla_loc,
                  ),
              ~trail=
                last =>
                  print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    last.pla_loc,
                    body.pexp_loc,
                  ),
              ~f=
                (~final, a) =>
                  if (final) {
                    group(print_lambda_argument(a)) ++ trailing_comma;
                  } else {
                    group(print_lambda_argument(a) ++ comma);
                  },
              params,
            ),
          )
          ++ break,
        )
        ++ string(" =>")
        ++ group(
             switch (body.pexp_desc) {
             | PExpBlock(_) => space ++ print_expression(body)
             | _ => indent(breakable_space ++ print_expression(body))
             },
           )
      | PExpContinue => string("continue")
      | PExpBreak => string("break")
      | PExpTuple(exprs) =>
        parens(
          indent(
            concat_map(
              ~lead=
                next =>
                  print_comment_range(
                    ~none=break,
                    ~lead=if_broken(space, empty),
                    ~trail=breakable_space,
                    enclosing_start_location(expr.pexp_loc),
                    next.pexp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.pexp_loc,
                    next.pexp_loc,
                  ),
              ~trail=
                last =>
                  print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    last.pexp_loc,
                    enclosing_end_location(expr.pexp_loc),
                  ),
              ~f=
                (~final, e) =>
                  if (final) {
                    group(print_expression(e)) ++ trailing_comma;
                  } else {
                    group(print_expression(e) ++ comma);
                  },
              exprs,
            ),
          )
          ++ break,
        )
      | PExpArray([]) =>
        array_brackets(
          indent(
            print_comment_range(
              ~block_end=true,
              ~none=break,
              ~lead=space,
              enclosing_start_location(expr.pexp_loc),
              enclosing_end_location(expr.pexp_loc),
            ),
          )
          ++ break,
        )
      | PExpArray(exprs) =>
        array_brackets(
          indent(
            concat_map(
              ~lead=
                next =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    enclosing_start_location(expr.pexp_loc),
                    next.pexp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.pexp_loc,
                    next.pexp_loc,
                  ),
              ~trail=
                prev =>
                  print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    prev.pexp_loc,
                    enclosing_end_location(expr.pexp_loc),
                  ),
              ~f=
                (~final, e) =>
                  if (final) {
                    group(print_expression(e)) ++ trailing_comma;
                  } else {
                    group(print_expression(e) ++ comma);
                  },
              exprs,
            ),
          )
          ++ break,
        )
      | PExpList([]) =>
        list_brackets(
          indent(
            print_comment_range(
              ~block_end=true,
              ~none=break,
              ~lead=if_broken(space, empty),
              enclosing_start_location(expr.pexp_loc),
              enclosing_end_location(expr.pexp_loc),
            ),
          )
          ++ break,
        )
      | PExpList(items) =>
        list_brackets(
          indent(
            concat_map(
              ~lead=
                next =>
                  print_comment_range(
                    ~none=break,
                    ~lead=if_broken(space, empty),
                    ~trail=breakable_space,
                    enclosing_start_location(expr.pexp_loc),
                    switch (next) {
                    | ListItem(expr)
                    | ListSpread(expr, _) => expr.pexp_loc
                    },
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    switch (prev) {
                    | ListItem(expr)
                    | ListSpread(expr, _) => expr.pexp_loc
                    },
                    switch (next) {
                    | ListItem(expr)
                    | ListSpread(expr, _) => expr.pexp_loc
                    },
                  ),
              ~trail=
                prev =>
                  print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    switch (prev) {
                    | ListItem(expr)
                    | ListSpread(expr, _) => expr.pexp_loc
                    },
                    enclosing_end_location(expr.pexp_loc),
                  ),
              ~f=
                (~final, item) => {
                  switch (item) {
                  | ListItem(expr) when final =>
                    group(print_expression(expr)) ++ trailing_comma
                  | ListItem(expr) => group(print_expression(expr) ++ comma)
                  | ListSpread(expr, _) when final =>
                    group(string("...") ++ print_expression(expr))
                  | ListSpread(expr, _) =>
                    group(string("...") ++ print_expression(expr) ++ comma)
                  }
                },
              items,
            ),
          )
          ++ break,
        )
      | PExpArrayGet(arr, elem) =>
        print_grouped_access_expression(arr)
        ++ print_comment_range(arr.pexp_loc, elem.pexp_loc)
        ++ list_brackets(
             indent(break ++ print_expression(~infix_wrap=Fun.id, elem))
             ++ break,
           )
      | PExpArraySet(arr, elem, new_value) =>
        print_grouped_access_expression(arr)
        ++ print_comment_range(arr.pexp_loc, elem.pexp_loc)
        ++ list_brackets(
             indent(break ++ print_expression(~infix_wrap=Fun.id, elem))
             ++ break,
           )
        ++ string(" =")
        ++ print_comment_range(
             ~none=space,
             ~lead=space,
             ~trail=space,
             elem.pexp_loc,
             new_value.pexp_loc,
           )
        ++ print_expression(new_value)
      | PExpRecord(base, labels) =>
        braces(
          indent(
            concat_map(
              ~lead=
                ((next_ident, _)) =>
                  switch (base) {
                  | None =>
                    print_comment_range(
                      ~none=breakable_space,
                      ~lead=space,
                      ~trail=breakable_space,
                      enclosing_start_location(expr.pexp_loc),
                      next_ident.loc,
                    )
                  | Some(base_expr) =>
                    print_comment_range(
                      ~none=breakable_space,
                      ~lead=space,
                      ~trail=breakable_space,
                      enclosing_start_location(expr.pexp_loc),
                      base_expr.pexp_loc,
                    )
                    ++ string("...")
                    ++ print_expression(base_expr)
                    ++ comma
                    ++ print_comment_range(
                         ~none=breakable_space,
                         ~lead=space,
                         ~trail=breakable_space,
                         base_expr.pexp_loc,
                         next_ident.loc,
                       )
                  },
              ~sep=
                ((_, {pexp_loc: prev_loc}), ({loc: next_loc}, _)) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev_loc,
                    next_loc,
                  ),
              ~trail=
                ((_, {pexp_loc: prev_loc})) =>
                  print_comment_range(
                    ~lead=space,
                    ~block_end=true,
                    prev_loc,
                    enclosing_end_location(expr.pexp_loc),
                  ),
              ~f=
                (~final, e) =>
                  if (final) {
                    group(print_punnable_expression(e))
                    ++ (
                      if (Option.is_none(base) && List.length(labels) == 1) {
                        comma;
                      } else {
                        trailing_comma;
                      }
                    );
                  } else {
                    group(print_punnable_expression(e) ++ comma);
                  },
              labels,
            ),
          )
          ++ breakable_space,
        )
      | PExpRecordGet(record, elem) =>
        print_grouped_access_expression(record)
        ++ string(".")
        ++ print_comment_range(record.pexp_loc, elem.loc)
        ++ print_identifier(elem.txt)
      | PExpRecordSet(
          {pexp_desc: PExpId({txt: IdentName({txt: name})})} as record,
          {txt: elem_name} as elem,
          {
            pexp_desc:
              PExpApp(
                _,
                [
                  {
                    paa_expr: {
                      pexp_desc:
                        PExpRecordGet(
                          {
                            pexp_desc:
                              PExpId({txt: IdentName({txt: new_name})}),
                          },
                          {txt: new_elem_name},
                        ),
                    },
                  },
                  _,
                ],
              ),
          } as new_value,
        )
          when name == new_name && elem_name == new_elem_name =>
        print_grouped_access_expression(record)
        ++ string(".")
        ++ print_comment_range(record.pexp_loc, elem.loc)
        ++ print_identifier(elem.txt)
        ++ print_assignment(~collapsible=true, ~lhs_loc=elem.loc, new_value)
      | PExpRecordSet(record, elem, new_value) =>
        print_grouped_access_expression(record)
        ++ string(".")
        ++ print_comment_range(record.pexp_loc, elem.loc)
        ++ print_identifier(elem.txt)
        ++ print_assignment(~collapsible=false, ~lhs_loc=elem.loc, new_value)
      | PExpPrim0(_) => failwith("Impossible: PExpPrim0 in parsetree")
      | PExpPrim1(_) => failwith("Impossible: PExpPrim1 in parsetree")
      | PExpPrim2(_) => failwith("Impossible: PExpPrim2 in parsetree")
      | PExpPrimN(_) => failwith("Impossible: PExpPrimN in parsetree")
      | PExpAssign(
          {pexp_desc: PExpId({txt: IdentName({txt: name})})} as binding,
          {
            pexp_desc:
              PExpApp(
                _,
                [
                  {
                    paa_expr: {
                      pexp_desc: PExpId({txt: IdentName({txt: new_name})}),
                    },
                  },
                  _,
                ],
              ),
          } as new_value,
        )
          when name == new_name =>
        print_expression(binding)
        ++ print_assignment(
             ~collapsible=true,
             ~lhs_loc=binding.pexp_loc,
             new_value,
           )
      | PExpAssign(binding, new_value) =>
        print_expression(binding)
        ++ print_assignment(
             ~collapsible=false,
             ~lhs_loc=binding.pexp_loc,
             new_value,
           )
      | PExpBoxAssign(binding, new_value) =>
        print_expression(binding)
        ++ string(" :=")
        ++ print_comment_range(
             ~none=space,
             ~lead=space,
             ~trail=space,
             binding.pexp_loc,
             new_value.pexp_loc,
           )
        ++ print_expression(new_value)
      | PExpReturn(return_expr) =>
        string("return")
        ++ group(
             switch (return_expr) {
             | None => empty
             | Some(return_expr) =>
               print_comment_range(
                 ~allow_breaks=false,
                 ~none=space,
                 ~lead=space,
                 ~trail=space,
                 enclosing_start_location(expr.pexp_loc),
                 return_expr.pexp_loc,
               )
               ++ print_expression(return_expr)
             },
           )
      | PExpUse(ident, use_items) =>
        string("from")
        ++ print_comment_range(
             ~allow_breaks=false,
             ~none=space,
             ~lead=space,
             ~trail=space,
             enclosing_start_location(expr.pexp_loc),
             ident.loc,
           )
        ++ print_identifier(ident.txt)
        ++ string(" use ")
        ++ (
          switch (use_items) {
          | PUseAll =>
            print_comment_range(
              ~allow_breaks=false,
              ~trail=space,
              ident.loc,
              enclosing_end_location(expr.pexp_loc),
            )
            ++ string("*")
          | PUseItems(items) =>
            braces(
              indent(
                concat_map(
                  ~lead=
                    next =>
                      print_comment_range(
                        ~none=breakable_space,
                        ~lead=space,
                        ~trail=breakable_space,
                        ident.loc,
                        switch (next) {
                        | PUseType({loc})
                        | PUseException({loc})
                        | PUseModule({loc})
                        | PUseValue({loc}) => loc
                        },
                      ),
                  ~sep=
                    (prev, next) =>
                      print_comment_range(
                        ~none=breakable_space,
                        ~lead=space,
                        ~trail=breakable_space,
                        switch (prev) {
                        | PUseType({loc})
                        | PUseException({loc})
                        | PUseModule({loc})
                        | PUseValue({loc}) => loc
                        },
                        switch (next) {
                        | PUseType({loc})
                        | PUseException({loc})
                        | PUseModule({loc})
                        | PUseValue({loc}) => loc
                        },
                      ),
                  ~trail=
                    prev =>
                      print_comment_range(
                        ~block_end=true,
                        ~lead=space,
                        switch (prev) {
                        | PUseType({loc})
                        | PUseException({loc})
                        | PUseModule({loc})
                        | PUseValue({loc}) => loc
                        },
                        enclosing_end_location(expr.pexp_loc),
                      ),
                  ~f=
                    (~final, u) =>
                      if (final) {
                        group(print_use_item(u)) ++ trailing_comma;
                      } else {
                        group(print_use_item(u) ++ comma);
                      },
                  items,
                ),
              )
              ++ breakable_space,
            )
          }
        )
      | PExpIf(cond, true_branch, false_branch) =>
        print_if(~loc=expr.pexp_loc, cond, true_branch, false_branch)
      | PExpWhile(cond, body) =>
        string("while ")
        ++ parens(
             indent(
               print_comment_range(
                 ~none=break,
                 ~lead=if_broken(space, empty),
                 ~trail=breakable_space,
                 enclosing_start_location(expr.pexp_loc),
                 cond.pexp_loc,
               )
               ++ print_expression(~infix_wrap=Fun.id, cond)
               ++ print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    cond.pexp_loc,
                    body.pexp_loc,
                  ),
             )
             ++ break,
           )
        ++ space
        ++ print_expression(body)
      | PExpFor(init, cond, inc, body) =>
        let start_location = enclosing_start_location(expr.pexp_loc);
        let cond_start_loc =
          switch (init) {
          | None => start_location
          | Some(init) => init.pexp_loc
          };
        let inc_start_loc =
          switch (cond) {
          | None => cond_start_loc
          | Some(cond) => cond.pexp_loc
          };
        group(
          string("for ")
          ++ parens(
               ~wrap=Fun.id,
               indent(
                 (
                   switch (init) {
                   | None => empty
                   | Some(init) =>
                     print_comment_range(
                       ~none=break,
                       ~lead=if_broken(space, empty),
                       ~trail=breakable_space,
                       enclosing_start_location(expr.pexp_loc),
                       init.pexp_loc,
                     )
                     ++ print_expression(init)
                   }
                 )
                 ++ string(";")
                 ++ (
                   switch (cond) {
                   | None => break
                   | Some(cond) =>
                     print_comment_range(
                       ~none=breakable_space,
                       ~lead=space,
                       ~trail=breakable_space,
                       cond_start_loc,
                       cond.pexp_loc,
                     )
                     ++ print_expression(cond)
                   }
                 )
                 ++ string(";")
                 ++ (
                   switch (inc) {
                   | None =>
                     print_comment_range(
                       ~block_end=true,
                       ~lead=space,
                       inc_start_loc,
                       body.pexp_loc,
                     )
                   | Some(inc) =>
                     print_comment_range(
                       ~none=breakable_space,
                       ~lead=space,
                       ~trail=breakable_space,
                       inc_start_loc,
                       inc.pexp_loc,
                     )
                     ++ print_expression(inc)
                     ++ print_comment_range(
                          ~block_end=true,
                          ~lead=space,
                          inc.pexp_loc,
                          body.pexp_loc,
                        )
                   }
                 ),
               )
               ++ break,
             )
          ++ space
          ++ print_expression(body),
        );
      | PExpMatch(value, {txt: branches, loc: branches_loc}) =>
        string("match ")
        ++ parens(
             indent(
               print_comment_range(
                 ~none=break,
                 ~lead=if_broken(space, empty),
                 ~trail=breakable_space,
                 enclosing_start_location(expr.pexp_loc),
                 value.pexp_loc,
               )
               ++ print_expression(~infix_wrap=Fun.id, value)
               ++ print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    value.pexp_loc,
                    branches_loc,
                  ),
             )
             ++ break,
           )
        ++ space
        ++ braces(
             ~wrap=doc => group(~print_width=2, doc),
             indent(
               concat_map(
                 ~lead=
                   next =>
                     print_comment_range(
                       ~none=hardline,
                       ~lead=space,
                       ~trail=hardline,
                       enclosing_start_location(branches_loc),
                       next.pmb_loc,
                     ),
                 ~sep=
                   (prev, next) =>
                     print_comment_range(
                       ~none=hardline,
                       ~lead=space,
                       ~trail=hardline,
                       prev.pmb_loc,
                       next.pmb_loc,
                     ),
                 ~trail=
                   last =>
                     print_comment_range(
                       ~block_end=true,
                       ~lead=space,
                       last.pmb_loc,
                       enclosing_end_location(expr.pexp_loc),
                     ),
                 ~f=(~final, b) => group(print_match_branch(b)),
                 branches,
               ),
             )
             ++ hardline,
           )
      | PExpConstraint(expr, typ) =>
        print_expression(expr)
        ++ string(":")
        ++ group(
             indent(
               print_comment_range(
                 ~none=breakable_space,
                 ~lead=space,
                 ~trail=breakable_space,
                 expr.pexp_loc,
                 typ.ptyp_loc,
               )
               ++ print_type(typ),
             ),
           )
      }
    );
  }
  and print_value_binding = ({pvb_pat, pvb_expr}) => {
    group(
      ~kind=FitAll,
      print_pattern(pvb_pat)
      ++ string(" =")
      ++ indent(
           print_comment_range(
             ~none=breakable_space,
             ~lead=space,
             ~trail=breakable_space,
             pvb_pat.ppat_loc,
             pvb_expr.pexp_loc,
           )
           ++ print_expression(pvb_expr),
         ),
    );
  }
  and print_parsed_type_argument = arg => {
    (
      switch (arg.ptyp_arg_label) {
      | Unlabeled => empty
      | Labeled({txt: label, loc: label_loc})
      | Default({txt: label, loc: label_loc}) =>
        string(label)
        ++ string(":")
        ++ print_comment_range(
             ~none=space,
             ~lead=space,
             ~trail=space,
             label_loc,
             arg.ptyp_arg_type.ptyp_loc,
           )
      }
    )
    ++ print_type(arg.ptyp_arg_type);
  }
  and print_type = ({ptyp_desc, ptyp_loc}) => {
    switch (ptyp_desc) {
    | PTyAny => string("_")
    | PTyVar(name) => string(name)
    | PTyConstr({txt: ident, loc: ident_loc}, params) =>
      let name = Identifier.string_of_ident(ident);
      string(name)
      ++ (
        switch (params) {
        | [] => empty
        | typs =>
          angle_brackets(
            concat_map(
              ~lead=
                next =>
                  print_comment_range(
                    ~block_start=true,
                    ~trail=space,
                    ident_loc,
                    next.ptyp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.ptyp_loc,
                    next.ptyp_loc,
                  ),
              ~trail=
                prev =>
                  print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    prev.ptyp_loc,
                    enclosing_end_location(ptyp_loc),
                  ),
              ~f=
                (~final, t) =>
                  if (final) {
                    group(print_type(t));
                  } else {
                    group(print_type(t) ++ comma);
                  },
              typs,
            ),
          )
        }
      );
    | PTyTuple(typs) =>
      parens(
        indent(
          concat_map(
            ~lead=
              next =>
                print_comment_range(
                  ~none=break,
                  ~lead=if_broken(space, empty),
                  ~trail=breakable_space,
                  enclosing_start_location(ptyp_loc),
                  next.ptyp_loc,
                ),
            ~sep=
              (prev, next) =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev.ptyp_loc,
                  next.ptyp_loc,
                ),
            ~trail=
              prev =>
                print_comment_range(
                  ~block_end=true,
                  ~lead=space,
                  prev.ptyp_loc,
                  enclosing_end_location(ptyp_loc),
                ),
            ~f=
              (~final, t) =>
                if (final) {
                  group(print_type(t));
                } else {
                  group(print_type(t) ++ comma);
                },
            typs,
          ),
        )
        ++ break,
      )
    | PTyArrow([{ptyp_arg_label: Unlabeled} as param], return) =>
      print_parsed_type_argument(param)
      ++ string(" =>")
      ++ print_comment_range(
           ~none=space,
           ~lead=space,
           ~trail=space,
           param.ptyp_arg_loc,
           return.ptyp_loc,
         )
      ++ print_type(return)
    | PTyArrow(params, return) =>
      parens(
        indent(
          concat_map(
            ~lead=
              next =>
                print_comment_range(
                  ~none=break,
                  ~lead=if_broken(space, empty),
                  ~trail=breakable_space,
                  enclosing_start_location(ptyp_loc),
                  next.ptyp_arg_loc,
                ),
            ~sep=
              (prev, next) =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev.ptyp_arg_loc,
                  next.ptyp_arg_loc,
                ),
            ~trail=
              prev =>
                print_comment_range(
                  ~block_end=true,
                  ~lead=space,
                  prev.ptyp_arg_loc,
                  return.ptyp_loc,
                ),
            ~f=
              (~final, a) =>
                if (final) {
                  group(print_parsed_type_argument(a)) ++ trailing_comma;
                } else {
                  group(print_parsed_type_argument(a) ++ comma);
                },
            params,
          ),
        )
        ++ break,
      )
      ++ string(" => ")
      ++ print_type(return)
    | PTyPoly(_) => failwith("Impossible: PTyPoly in the parsetree")
    };
  }
  and print_label_declaration = ({pld_name, pld_type, pld_mutable, pld_loc}) => {
    (
      switch (pld_mutable) {
      | Mutable => string("mut ")
      | Immutable => empty
      }
    )
    ++ print_identifier(pld_name.txt)
    ++ string(":")
    ++ print_comment_range(
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(pld_loc),
         pld_type.ptyp_loc,
       )
    ++ print_type(pld_type);
  }
  and print_constructor_arguments = args => {
    switch (args) {
    | PConstrTuple({txt: typs, loc: typs_loc}) =>
      parens(
        indent(
          concat_map(
            ~lead=
              first =>
                print_comment_range(
                  ~none=break,
                  ~lead=if_broken(space, empty),
                  ~trail=breakable_space,
                  enclosing_start_location(typs_loc),
                  first.ptyp_loc,
                ),
            ~sep=
              (prev, next) =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev.ptyp_loc,
                  next.ptyp_loc,
                ),
            ~trail=
              last =>
                print_comment_range(
                  ~lead=breakable_space,
                  last.ptyp_loc,
                  enclosing_end_location(typs_loc),
                ),
            ~f=
              (~final, t) =>
                if (final) {
                  group(print_type(t));
                } else {
                  group(print_type(t) ++ comma);
                },
            typs,
          ),
        )
        ++ break,
      )
    | PConstrRecord({txt: labels, loc: labels_loc}) =>
      braces(
        indent(
          concat_map(
            ~lead=
              next =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  enclosing_start_location(labels_loc),
                  next.pld_loc,
                ),
            ~sep=
              (prev, next) =>
                print_comment_range(
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev.pld_loc,
                  next.pld_loc,
                ),
            ~trail=
              last =>
                print_comment_range(
                  ~block_end=true,
                  ~lead=space,
                  last.pld_loc,
                  enclosing_end_location(labels_loc),
                ),
            ~f=
              (~final, ld) =>
                if (final) {
                  group(print_label_declaration(ld))
                  ++ (
                    switch (labels) {
                    | [_single_element] => comma
                    | _ => trailing_comma
                    }
                  );
                } else {
                  group(print_label_declaration(ld) ++ comma);
                },
            labels,
          ),
        )
        ++ breakable_space,
      )
    | PConstrSingleton => empty
    };
  }
  and print_exception = ({ptyexn_constructor, ptyexn_loc}) => {
    string("exception")
    ++ print_comment_range(
         ~allow_breaks=false,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(ptyexn_loc),
         ptyexn_constructor.pext_name.loc,
       )
    ++ string(ptyexn_constructor.pext_name.txt)
    ++ (
      switch (ptyexn_constructor.pext_kind) {
      | PExtDecl((PConstrTuple({loc}) | PConstrRecord({loc})) as args) =>
        print_comment_range(ptyexn_constructor.pext_name.loc, loc)
        ++ print_constructor_arguments(args)
      | PExtDecl(PConstrSingleton)
      | PExtRebind(_) => empty
      }
    );
  }
  and print_constructor_declaration = ({pcd_name, pcd_args}) => {
    string(pcd_name.txt)
    ++ (
      switch (pcd_args) {
      | PConstrTuple({loc})
      | PConstrRecord({loc}) => print_comment_range(pcd_name.loc, loc)
      | PConstrSingleton => empty
      }
    )
    ++ print_constructor_arguments(pcd_args);
  }
  and print_data_declaration = decl => {
    switch (decl) {
    | {
        pdata_name,
        pdata_params,
        pdata_manifest,
        pdata_kind: PDataAbstract,
        pdata_rec,
        pdata_loc,
      } =>
      string("type ")
      ++ (
        switch (pdata_rec) {
        | Recursive => string("rec ")
        | Nonrecursive => empty
        }
      )
      ++ print_comment_range(
           ~allow_breaks=false,
           ~trail=space,
           enclosing_start_location(pdata_loc),
           pdata_name.loc,
         )
      ++ string(pdata_name.txt)
      ++ (
        switch (pdata_params) {
        | [] => empty
        | typs =>
          angle_brackets(
            concat_map(
              ~lead=
                next =>
                  print_comment_range(
                    ~block_start=true,
                    ~trail=space,
                    pdata_name.loc,
                    next.ptyp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.ptyp_loc,
                    next.ptyp_loc,
                  ),
              ~trail=
                prev =>
                  print_comment_range(
                    ~block_end=true,
                    ~lead=space,
                    prev.ptyp_loc,
                    switch (pdata_manifest) {
                    | None => enclosing_end_location(pdata_loc)
                    | Some(typ) => typ.ptyp_loc
                    },
                  ),
              ~f=
                (~final, t) =>
                  if (final) {
                    group(print_type(t));
                  } else {
                    group(print_type(t) ++ comma);
                  },
              pdata_params,
            ),
          )
        }
      )
      ++ (
        switch (pdata_manifest) {
        | None => empty
        | Some(typ) =>
          group(
            ~kind=FitAll,
            string(" =")
            ++ indent(
                 (
                   switch (pdata_params) {
                   | [] =>
                     print_comment_range(
                       ~none=breakable_space,
                       ~lead=space,
                       ~trail=breakable_space,
                       pdata_name.loc,
                       typ.ptyp_loc,
                     )
                   | _ => breakable_space
                   }
                 )
                 ++ print_type(typ),
               ),
          )
        }
      )
    | {
        pdata_name,
        pdata_params,
        pdata_kind: PDataVariant(cstr_decls),
        pdata_rec,
        pdata_loc,
      } =>
      string("enum ")
      ++ (
        switch (pdata_rec) {
        | Recursive => string("rec ")
        | Nonrecursive => empty
        }
      )
      ++ print_comment_range(
           ~allow_breaks=false,
           ~trail=space,
           enclosing_start_location(pdata_loc),
           pdata_name.loc,
         )
      ++ string(pdata_name.txt)
      ++ (
        switch (pdata_params) {
        | [] => empty
        | typs =>
          angle_brackets(
            concat_map(
              ~lead=
                next =>
                  print_comment_range(
                    ~block_start=true,
                    ~trail=space,
                    pdata_name.loc,
                    next.ptyp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.ptyp_loc,
                    next.ptyp_loc,
                  ),
              ~trail=_ => empty,
              ~f=
                (~final, t) =>
                  if (final) {
                    group(print_type(t));
                  } else {
                    group(print_type(t) ++ comma);
                  },
              pdata_params,
            ),
          )
        }
      )
      ++ space
      ++ braces(
           ~wrap=doc => group(~print_width=2, doc),
           indent(
             concat_map(
               ~lead=
                 next =>
                   print_comment_range(
                     ~none=hardline,
                     ~lead=space,
                     ~trail=hardline,
                     List.fold_left(
                       (_, param) => param.ptyp_loc,
                       pdata_name.loc,
                       pdata_params,
                     ),
                     next.pcd_loc,
                   ),
               ~sep=
                 (prev, next) =>
                   print_comment_range(
                     ~none=hardline,
                     ~lead=space,
                     ~trail=hardline,
                     prev.pcd_loc,
                     next.pcd_loc,
                   ),
               ~trail=
                 last =>
                   print_comment_range(
                     ~block_end=true,
                     ~lead=space,
                     last.pcd_loc,
                     enclosing_end_location(pdata_loc),
                   ),
               ~f=
                 (~final, cd) =>
                   group(print_constructor_declaration(cd) ++ comma),
               cstr_decls,
             ),
           )
           ++ hardline,
         )
    | {
        pdata_name,
        pdata_params,
        pdata_kind: PDataRecord(labels),
        pdata_rec,
        pdata_loc,
      } =>
      string("record ")
      ++ (
        switch (pdata_rec) {
        | Recursive => string("rec ")
        | Nonrecursive => empty
        }
      )
      ++ print_comment_range(
           ~allow_breaks=false,
           ~trail=space,
           enclosing_start_location(pdata_loc),
           pdata_name.loc,
         )
      ++ string(pdata_name.txt)
      ++ (
        switch (pdata_params) {
        | [] => empty
        | typs =>
          angle_brackets(
            concat_map(
              ~lead=
                next =>
                  print_comment_range(
                    ~block_start=true,
                    ~trail=space,
                    pdata_name.loc,
                    next.ptyp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  print_comment_range(
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.ptyp_loc,
                    next.ptyp_loc,
                  ),
              ~trail=_ => empty,
              ~f=
                (~final, t) =>
                  if (final) {
                    group(print_type(t));
                  } else {
                    group(print_type(t) ++ comma);
                  },
              pdata_params,
            ),
          )
        }
      )
      ++ space
      ++ braces(
           ~wrap=doc => group(~print_width=2, doc),
           indent(
             concat_map(
               ~lead=
                 next =>
                   print_comment_range(
                     ~none=hardline,
                     ~lead=space,
                     ~trail=hardline,
                     List.fold_left(
                       (_, param) => param.ptyp_loc,
                       pdata_name.loc,
                       pdata_params,
                     ),
                     next.pld_loc,
                   ),
               ~sep=
                 (prev, next) =>
                   print_comment_range(
                     ~none=hardline,
                     ~lead=space,
                     ~trail=hardline,
                     prev.pld_loc,
                     next.pld_loc,
                   ),
               ~trail=
                 last =>
                   print_comment_range(
                     ~block_end=true,
                     ~lead=space,
                     last.pld_loc,
                     enclosing_end_location(pdata_loc),
                   ),
               ~f=(~final, l) => group(print_label_declaration(l) ++ comma),
               labels,
             ),
           )
           ++ hardline,
         )
    };
  }
  and print_primitive_description = ({pprim_ident, pprim_name, pprim_loc}) => {
    string("primitive")
    ++ print_comment_range(
         ~allow_breaks=false,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(pprim_loc),
         pprim_ident.loc,
       )
    ++ print_ident_string(pprim_ident.txt)
    ++ string(" =")
    ++ print_comment_range(
         ~allow_breaks=false,
         ~none=space,
         ~lead=space,
         ~trail=space,
         pprim_ident.loc,
         pprim_name.loc,
       )
    ++ double_quotes(string(pprim_name.txt));
  }
  and print_include_declaration = ({pinc_path, pinc_alias, pinc_loc}) => {
    string("include")
    ++ print_comment_range(
         ~allow_breaks=false,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(pinc_loc),
         pinc_path.loc,
       )
    ++ double_quotes(string(pinc_path.txt))
    ++ (
      switch (pinc_alias) {
      | None => empty
      | Some({txt: alias, loc: alias_loc}) =>
        string(" as")
        ++ print_comment_range(
             ~allow_breaks=false,
             ~none=space,
             ~lead=space,
             ~trail=space,
             pinc_path.loc,
             alias_loc,
           )
        ++ string(alias)
      }
    );
  }
  and print_module_declaration = ({pmod_name, pmod_stmts, pmod_loc}) => {
    string("module")
    ++ print_comment_range(
         ~allow_breaks=false,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(pmod_loc),
         pmod_name.loc,
       )
    ++ string(pmod_name.txt)
    ++ space
    ++ braces(
         ~wrap=doc => group(~print_width=2, doc),
         indent(
           concat_map(
             ~lead=
               next =>
                 print_comment_range(
                   ~none=hardline,
                   ~lead=space,
                   ~trail=hardline,
                   pmod_name.loc,
                   next.ptop_loc,
                 ),
             ~sep=
               (prev, next) =>
                 print_comment_range(
                   ~none=
                     switch (
                       next.ptop_loc.loc_start.pos_lnum
                       - prev.ptop_loc.loc_end.pos_lnum
                     ) {
                     | 0
                     | 1 => hardline
                     | _ => hardline ++ hardline
                     },
                   ~lead=space,
                   ~trail=hardline,
                   prev.ptop_loc,
                   next.ptop_loc,
                 ),
             ~trail=
               prev =>
                 print_comment_range(
                   ~lead=space,
                   prev.ptop_loc,
                   enclosing_end_location(pmod_loc),
                 ),
             ~f=
               (~final, s) =>
                 if (has_disable_formatting_comment(s.ptop_loc)) {
                   string(get_original_code(s.ptop_loc));
                 } else {
                   print_toplevel_stmt(s);
                 },
             pmod_stmts,
           ),
         )
         ++ hardline,
       );
  }
  and print_value_description =
      ({pval_mod, pval_name, pval_name_alias, pval_type, pval_loc}) => {
    group @@
    string(pval_name.txt)
    ++ string(":")
    ++ indent(
         print_comment_range(
           ~none=breakable_space,
           ~lead=space,
           ~trail=breakable_space,
           pval_name.loc,
           pval_type.ptyp_loc,
         )
         ++ print_type(pval_type)
         ++ (
           switch (pval_name_alias) {
           | None => empty
           | Some(alias) =>
             string(" as")
             ++ print_comment_range(
                  ~allow_breaks=false,
                  ~none=space,
                  ~lead=space,
                  ~trail=space,
                  pval_type.ptyp_loc,
                  alias.loc,
                )
             ++ string(alias.txt)
           }
         )
         ++ string(" from")
         ++ print_comment_range(
              ~allow_breaks=false,
              ~none=space,
              ~lead=space,
              ~trail=space,
              Option.fold(
                ~none=pval_type.ptyp_loc,
                ~some=alias => alias.loc,
                pval_name_alias,
              ),
              enclosing_end_location(pval_loc),
            )
         ++ double_quotes(string(pval_mod.txt)),
       );
  }
  and print_provide_item = provide_item => {
    switch (provide_item) {
    | PProvideType({name, alias, loc}) =>
      string("type")
      ++ print_comment_range(
           ~allow_breaks=false,
           ~none=space,
           ~lead=space,
           ~trail=space,
           enclosing_start_location(loc),
           name.loc,
         )
      ++ print_identifier(name.txt)
      ++ (
        switch (alias) {
        | None => empty
        | Some(alias) =>
          string(" as")
          ++ print_comment_range(
               ~allow_breaks=false,
               ~none=space,
               ~lead=space,
               ~trail=space,
               name.loc,
               enclosing_end_location(loc),
             )
          ++ print_identifier(alias.txt)
        }
      )
    | PProvideException({name, alias, loc}) =>
      string("exception")
      ++ print_comment_range(
           ~allow_breaks=false,
           ~none=space,
           ~lead=space,
           ~trail=space,
           enclosing_start_location(loc),
           name.loc,
         )
      ++ print_identifier(name.txt)
      ++ (
        switch (alias) {
        | None => empty
        | Some(alias) =>
          string(" as")
          ++ print_comment_range(
               ~allow_breaks=false,
               ~none=space,
               ~lead=space,
               ~trail=space,
               name.loc,
               enclosing_end_location(loc),
             )
          ++ print_identifier(alias.txt)
        }
      )
    | PProvideModule({name, alias, loc}) =>
      string("module")
      ++ print_comment_range(
           ~allow_breaks=false,
           ~none=space,
           ~lead=space,
           ~trail=space,
           enclosing_start_location(loc),
           name.loc,
         )
      ++ print_identifier(name.txt)
      ++ (
        switch (alias) {
        | None => empty
        | Some(alias) =>
          string(" as")
          ++ print_comment_range(
               ~allow_breaks=false,
               ~none=space,
               ~lead=space,
               ~trail=space,
               name.loc,
               enclosing_end_location(loc),
             )
          ++ print_identifier(alias.txt)
        }
      )
    | PProvideValue({name, alias, loc}) =>
      print_identifier(name.txt)
      ++ (
        switch (alias) {
        | None => empty
        | Some(alias) =>
          string(" as")
          ++ print_comment_range(
               ~allow_breaks=false,
               ~none=space,
               ~lead=space,
               ~trail=space,
               name.loc,
               enclosing_end_location(loc),
             )
          ++ print_identifier(alias.txt)
        }
      )
    };
  }
  and print_toplevel_stmt = stmt => {
    group(
      concat_map(
        ~lead=_ => empty,
        ~sep=
          (prev, next) =>
            print_comment_range(
              ~none=hardline,
              ~lead=space,
              ~trail=hardline,
              prev.Asttypes.attr_loc,
              next.attr_loc,
            ),
        ~trail=
          prev =>
            print_comment_range(
              ~none=hardline,
              ~lead=space,
              ~trail=hardline,
              prev.Asttypes.attr_loc,
              stmt.ptop_core_loc,
            ),
        ~f=(~final, a) => print_attribute(a),
        stmt.ptop_attributes,
      ),
    )
    ++ group(
         switch (stmt.ptop_desc) {
         | PTopExpr(expr) => print_expression(expr)
         | PTopException(provide_flag, ex) =>
           (
             switch (provide_flag) {
             | Asttypes.NotProvided => empty
             | Asttypes.Abstract =>
               string("abstract")
               ++ print_comment_range(
                    ~allow_breaks=false,
                    ~none=space,
                    ~lead=space,
                    ~trail=space,
                    enclosing_start_location(stmt.ptop_core_loc),
                    ex.ptyexn_loc,
                  )
             | Asttypes.Provided =>
               string("provide")
               ++ print_comment_range(
                    ~allow_breaks=false,
                    ~none=space,
                    ~lead=space,
                    ~trail=space,
                    enclosing_start_location(stmt.ptop_core_loc),
                    ex.ptyexn_loc,
                  )
             }
           )
           ++ print_exception(ex)
         | PTopData(datas) =>
           group @@
           concat_map(
             ~lead=_ => empty,
             ~sep=
               ((_, _, prev), (_, _, next)) => {
                 print_comment_range(
                   ~none=hardline,
                   ~lead=space,
                   ~trail=hardline,
                   prev,
                   next,
                 )
                 ++ string("and")
                 ++ space
               },
             ~trail=_ => empty,
             ~f=
               (~final, (provide_flag, decl, decl_loc)) =>
                 group(
                   (
                     switch (provide_flag) {
                     | Asttypes.NotProvided => empty
                     | Asttypes.Abstract =>
                       string("abstract")
                       ++ print_comment_range(
                            ~none=space,
                            ~lead=space,
                            ~trail=space,
                            enclosing_start_location(decl_loc),
                            decl.pdata_loc,
                          )
                     | Asttypes.Provided =>
                       string("provide")
                       ++ print_comment_range(
                            ~none=space,
                            ~lead=space,
                            ~trail=space,
                            enclosing_start_location(decl_loc),
                            decl.pdata_loc,
                          )
                     }
                   )
                   ++ print_data_declaration(decl),
                 ),
             datas,
           )
         | PTopLet(provide_flag, rec_flag, mut_flag, vbs) =>
           group @@
           (
             switch (provide_flag) {
             | NotProvided => empty
             | Abstract => string("abstract ")
             | Provided => string("provide ")
             }
           )
           ++ string("let ")
           ++ (
             switch (rec_flag) {
             | Nonrecursive => empty
             | Recursive => string("rec ")
             }
           )
           ++ (
             switch (mut_flag) {
             | Immutable => empty
             | Mutable => string("mut ")
             }
           )
           ++ concat_map(
                ~lead=
                  next =>
                    print_comment_range(
                      ~allow_breaks=false,
                      ~trail=space,
                      enclosing_start_location(stmt.ptop_core_loc),
                      next.pvb_loc,
                    ),
                ~sep=
                  (prev, next) =>
                    print_comment_range(
                      ~none=hardline,
                      ~lead=space,
                      ~trail=hardline,
                      prev.pvb_loc,
                      next.pvb_loc,
                    )
                    ++ string("and")
                    ++ space,
                ~trail=_ => empty,
                ~f=(~final, vb) => group(print_value_binding(vb)),
                vbs,
              )
         | PTopPrimitive(provide_flag, prim_desc) =>
           (
             switch (provide_flag) {
             | NotProvided => empty
             | Abstract => string("abstract ")
             | Provided => string("provide ")
             }
           )
           ++ print_comment_range(
                ~allow_breaks=false,
                ~trail=space,
                enclosing_start_location(stmt.ptop_core_loc),
                prim_desc.pprim_loc,
              )
           ++ print_primitive_description(prim_desc)
         | PTopInclude(include_decl) =>
           print_include_declaration(include_decl)
         | PTopForeign(provide_flag, value_desc) =>
           (
             switch (provide_flag) {
             | NotProvided => empty
             | Abstract => string("abstract ")
             | Provided => string("provide ")
             }
           )
           ++ string("foreign wasm ")
           ++ print_comment_range(
                ~allow_breaks=false,
                ~trail=space,
                enclosing_start_location(stmt.ptop_core_loc),
                value_desc.pval_loc,
              )
           ++ print_value_description(value_desc)
         | PTopModule(provide_flag, module_decl) =>
           (
             switch (provide_flag) {
             | NotProvided => empty
             | Abstract => string("abstract ")
             | Provided => string("provide ")
             }
           )
           ++ print_comment_range(
                ~allow_breaks=false,
                ~trail=space,
                enclosing_start_location(stmt.ptop_core_loc),
                module_decl.pmod_loc,
              )
           ++ print_module_declaration(module_decl)
         | PTopProvide(provide_items) =>
           string("provide ")
           ++ braces(
                indent(
                  concat_map(
                    ~lead=
                      next =>
                        print_comment_range(
                          ~none=breakable_space,
                          ~lead=space,
                          ~trail=breakable_space,
                          enclosing_start_location(stmt.ptop_core_loc),
                          switch (next) {
                          | PProvideType({loc})
                          | PProvideException({loc})
                          | PProvideModule({loc})
                          | PProvideValue({loc}) => loc
                          },
                        ),
                    ~sep=
                      (prev, next) =>
                        print_comment_range(
                          ~none=breakable_space,
                          ~lead=space,
                          ~trail=breakable_space,
                          switch (prev) {
                          | PProvideType({loc})
                          | PProvideException({loc})
                          | PProvideModule({loc})
                          | PProvideValue({loc}) => loc
                          },
                          switch (next) {
                          | PProvideType({loc})
                          | PProvideException({loc})
                          | PProvideModule({loc})
                          | PProvideValue({loc}) => loc
                          },
                        ),
                    ~trail=
                      prev =>
                        print_comment_range(
                          ~block_end=true,
                          ~lead=space,
                          switch (prev) {
                          | PProvideType({loc})
                          | PProvideException({loc})
                          | PProvideModule({loc})
                          | PProvideValue({loc}) => loc
                          },
                          enclosing_end_location(stmt.ptop_core_loc),
                        ),
                    ~f=
                      (~final, p) =>
                        if (final) {
                          group(print_provide_item(p)) ++ trailing_comma;
                        } else {
                          group(print_provide_item(p) ++ comma);
                        },
                    provide_items,
                  ),
                )
                ++ breakable_space,
              )
         },
       );
  }

  and print_comment_range =
      (
        ~none=empty,
        ~lead=empty,
        ~trail=empty,
        ~allow_breaks=true,
        ~block_start=false,
        ~block_end=false,
        prev,
        next,
      ) => {
    // This function prints all comments between two given locations, while
    // preserving line breaks and structure. It includes a number of optional
    // parameters to assist in this goal:
    //   ~none:
    //     Printed when no comments are in this range
    //   ~lead:
    //     Printed before any comments. This parameter is ignored if there are
    //     no comments or the first comment is not on the same line as the
    //     start location.
    //   ~trail:
    //     Printed after any comments. This parameter is ignored if there are
    //     no comments or the last comment is not on the same line as the end
    //     location.
    //   ~allow_breaks:
    //     A flag indicating if we can add additional line breaks, e.g. no line
    //     breaks are allowed between most keywords, such as `let` and `mut`.
    //   ~block_start:
    //     A flag indicating these comments are to be printed at the start of
    //     curly braces, brackets, parens, or carets, that when broken would
    //     introduce a newline which would interfere with this function's
    //     whitespace calculations. Setting this parameter to `true` will
    //     prevent extra whitespace from appearing at the start of that block.
    //   ~block_end:
    //     A flag indicating these comments are to be printed at the end of
    //     curly braces, brackets, parens, or carets, that when broken would
    //     introduce a newline which would interfere with this function's
    //     whitespace calculations. Setting this parameter to `true` will
    //     prevent extra whitespace from appearing at the end of that block.
    // Useful tips and tricks:
    //  • Use the ~none, ~lead, and ~trail parameters to handle whitespace
    //    between entities instead of trying to guess the output of this
    //    function. I.e. prefer `print_comment_range(~trail=space, ...)` to
    //    `print_comment_range(...) ++ space`, as this function will handle
    //    a trailing line comment for you.
    //  • Generally, your ~none and ~trail parameters should match.
    //  • The ~lead parameter should almost never contain a break, as this
    //    could prevent a comment from returning to the line it was originally
    //    on.
    // Happy formatting!

    let between_loc = {
      loc_start: prev.loc_end,
      loc_end: next.loc_start,
      loc_ghost: true,
    };

    let comments = Commenttree.query(comment_tree, between_loc);
    switch (comments) {
    | [] => none // bail out quickly for the most common case
    | _ =>
      let loc = cmt => {
        switch (cmt) {
        | Doc({cmt_loc})
        | Block({cmt_loc})
        | Line({cmt_loc})
        | Shebang({cmt_loc}) => cmt_loc
        };
      };

      let print_comment = cmt => {
        switch (cmt) {
        | Doc({cmt_source})
        | Block({cmt_source}) => string(cmt_source)
        | Line({cmt_source})
        | Shebang({cmt_source}) =>
          // When a line comment appears anywhere, we force the surrounding
          // group to break. Note that the hardline that must follow a line
          // comment is not included here, but instead included later to
          // account for constructs (like blocks) that may include their own
          // newline character.
          group_breaker ++ string(String.trim(cmt_source))
        };
      };

      // We use phantom hardlines here to prevent comment line breaks from
      // disrupting the formatting engine's width calculations. This means that
      // the engine guarantees we get a break for line comments, but it
      // considers the following code as just one really long line:
      //     func(foo, // comment
      //       bar, baz, // comment
      //       qux // comment
      //     )
      // This gives us a balance between comments completely changing the
      // formatting of an expression versus the engine just being "mindful"
      // about the comments.

      switch (comments) {
      | [] => empty
      | comments =>
        concat_map(
          ~lead=
            next =>
              switch (loc(next).loc_start.pos_lnum - prev.loc_end.pos_lnum) {
              | _ when block_start => lead
              | 0 => lead
              | 1 when allow_breaks => phantom_hardline
              | _ when allow_breaks => phantom_hardline ++ phantom_hardline
              | _ => lead
              },
          ~sep=
            (prev, next) => {
              let line_delta =
                loc(next).loc_start.pos_lnum - loc(prev).loc_end.pos_lnum;
              if (allow_breaks) {
                switch (prev) {
                | Block(_) when line_delta == 0 => breakable_space
                | _ when line_delta <= 1 => phantom_hardline
                | _ => phantom_hardline ++ phantom_hardline
                };
              } else {
                switch (prev) {
                | Line(_)
                | Shebang(_) => phantom_hardline // required for line comments no matter what
                | _ => space
                };
              };
            },
          ~trail=
            prev => {
              let line_delta =
                next.loc_start.pos_lnum - loc(prev).loc_end.pos_lnum;
              switch (prev) {
              // prevent double spacing of comments at the end of a block
              | Line(_)
              | Shebang(_) when block_end =>
                if_broken(empty, phantom_hardline)
              | _ when block_end => empty
              | Line(_)
              | Shebang(_) when line_delta <= 1 => phantom_hardline
              | Line(_)
              | Shebang(_) when line_delta > 1 =>
                phantom_hardline ++ phantom_hardline
              | Doc(_) when allow_breaks => phantom_hardline
              | _ when allow_breaks && line_delta == 1 => phantom_hardline
              | _ when allow_breaks && line_delta > 1 =>
                phantom_hardline ++ phantom_hardline
              | _ => trail
              };
            },
          ~f=(~final, c) => print_comment(c),
          comments,
        )
      };
    };
  };

  let toplevel =
    switch (parsed_program.statements) {
    | [] =>
      print_comment_range(
        ~none=hardline,
        ~lead=space,
        ~trail=hardline,
        parsed_program.module_name.loc,
        enclosing_end_location(parsed_program.prog_loc),
      )
    | _ =>
      concat_map(
        ~lead=
          first =>
            print_comment_range(
              ~none=hardline ++ hardline,
              ~lead=space,
              ~trail=hardline ++ hardline,
              parsed_program.module_name.loc,
              first.ptop_loc,
            ),
        ~sep=
          (prev, next) => {
            print_comment_range(
              ~none=
                switch (
                  next.ptop_loc.loc_start.pos_lnum
                  - prev.ptop_loc.loc_end.pos_lnum
                ) {
                | 0
                | 1 => hardline
                | _ => hardline ++ hardline
                },
              ~lead=space,
              ~trail=space,
              prev.ptop_loc,
              next.ptop_loc,
            )
          },
        ~trail=
          last =>
            print_comment_range(
              ~block_end=true,
              ~lead=space,
              last.ptop_loc,
              enclosing_end_location(parsed_program.prog_loc),
            )
            ++ hardline,
        ~f=
          (~final, s) =>
            if (has_disable_formatting_comment(s.ptop_loc)) {
              string(get_original_code(s.ptop_loc));
            } else {
              print_toplevel_stmt(s);
            },
        parsed_program.statements,
      )
    };

  group @@
  print_comment_range(
    enclosing_start_location(parsed_program.prog_loc),
    parsed_program.module_name.loc,
  )
  ++ string("module ")
  ++ string(parsed_program.module_name.txt)
  ++ toplevel;
};

let format = (~write, ~original_source, ~eol, parsed_program) => {
  Engine.print(
    ~write,
    ~eol,
    ~line_width=80,
    build_document(~original_source, parsed_program),
  );
};

let format_to_string = (~original_source, ~eol, parsed_program) => {
  Engine.to_string(
    ~eol,
    ~line_width=80,
    build_document(~original_source, parsed_program),
  );
};
