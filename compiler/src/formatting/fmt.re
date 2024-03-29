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
  Location.{...loc, loc_end: loc.loc_start};
};

// This takes a location and makes the loc_start the same as loc_end.
// Its main purpose is to find comments between the end of an enclosing location and the last item inside.
let enclosing_end_location = loc => {
  Location.{...loc, loc_start: loc.loc_end};
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
  | PExpConstraint(_) => 140
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
    } else if (is_keyword_function(fn1)) {
      ParenGrouping;
    } else {
      FormatterGrouping;
    }
  | (PExpConstant(PConstNumber(PConstNumberRational(_))), _)
      when op_precedence('/') <= precedence(parent) =>
    ParenGrouping
  | _ => FormatterGrouping
  };
};

let has_disable_formatting_comment = (comments, loc: Location.t) => {
  switch (Commenttree.query_line(comments, loc.loc_start.pos_lnum - 1)) {
  | Some(Line({cmt_content: "formatter-ignore"})) => true
  | _ => false
  };
};

type formatter = {
  comments: Commenttree.t,
  source: array(string),
  print_original_code: (formatter, Location.t) => Doc.t,
  print_infix_prefix_op: (formatter, expression) => Doc.t,
  print_constant: (formatter, constant) => Doc.t,
  print_punnable_pattern: (formatter, (loc(Identifier.t), pattern)) => Doc.t,
  print_lambda_argument: (formatter, lambda_argument) => Doc.t,
  print_pattern: (formatter, pattern) => Doc.t,
  print_ident_string: (formatter, string) => Doc.t,
  print_identifier: (formatter, Identifier.t) => Doc.t,
  print_punnable_expression:
    (formatter, (loc(Identifier.t), expression)) => Doc.t,
  print_grouped_access_expression: (formatter, expression) => Doc.t,
  print_use_item: (formatter, use_item) => Doc.t,
  print_match_branch: (formatter, match_branch) => Doc.t,
  print_attribute: (formatter, attribute) => Doc.t,
  print_application_argument:
    (formatter, ~infix_wrap: t => t=?, application_argument) => Doc.t,
  print_if:
    (
      formatter,
      ~force_blocks: bool=?,
      ~loc: Location.t,
      expression,
      expression,
      option(expression)
    ) =>
    Doc.t,
  print_assignment:
    (formatter, ~collapsible: bool, ~lhs_loc: Location.t, expression) => Doc.t,
  print_expression: (formatter, ~infix_wrap: t => t=?, expression) => Doc.t,
  print_value_binding: (formatter, value_binding) => Doc.t,
  print_parsed_type_argument: (formatter, parsed_type_argument) => Doc.t,
  print_type: (formatter, parsed_type) => Doc.t,
  print_label_declaration: (formatter, label_declaration) => Doc.t,
  print_constructor_arguments: (formatter, constructor_arguments) => Doc.t,
  print_exception: (formatter, type_exception) => Doc.t,
  print_constructor_declaration: (formatter, constructor_declaration) => Doc.t,
  print_data_declaration: (formatter, data_declaration) => Doc.t,
  print_primitive_description: (formatter, primitive_description) => Doc.t,
  print_include_declaration: (formatter, include_declaration) => Doc.t,
  print_module_declaration: (formatter, module_declaration) => Doc.t,
  print_value_description: (formatter, value_description) => Doc.t,
  print_provide_item: (formatter, provide_item) => Doc.t,
  print_toplevel_stmt: (formatter, toplevel_stmt) => Doc.t,
  print_comment_range:
    (
      formatter,
      ~none: t=?,
      ~lead: t=?,
      ~trail: t=?,
      ~allow_breaks: bool=?,
      ~block_start: bool=?,
      ~block_end: bool=?,
      Location.t,
      Location.t
    ) =>
    Doc.t,
  print_program: (formatter, parsed_program) => Doc.t,
};

let print_original_code = (fmt, location: Location.t) => {
  let (_, start_line, startc, _) =
    Locations.get_raw_pos_info(location.loc_start);
  let (_, end_line, endc, _) = Locations.get_raw_pos_info(location.loc_end);

  let (++) = Stdlib.(++);

  let str =
    if (Array.length(fmt.source) > end_line - 1) {
      if (start_line == end_line) {
        String_utils.Utf8.sub(
          fmt.source[start_line - 1],
          startc,
          endc - startc,
        );
      } else {
        let text = ref("");
        for (line in start_line - 1 to end_line - 1) {
          if (line + 1 == start_line) {
            text :=
              text^
              ++ String_utils.Utf8.string_after(fmt.source[line], startc)
              ++ "\n";
          } else if (line + 1 == end_line) {
            text := text^ ++ String_utils.Utf8.sub(fmt.source[line], 0, endc);
          } else {
            text := text^ ++ fmt.source[line] ++ "\n";
          };
        };
        text^;
      };
    } else {
      raise(FormatterError("Requested beyond end of original source"));
    };

  string(str);
};

let print_infix_prefix_op = (fmt, expr) => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) => string(op)
  | _ => failwith("Impossible: non- prefix or infix op")
  };
};

let print_constant = (fmt, constant) => {
  switch (constant) {
  | PConstNumber(PConstNumberInt({txt: value})) => string(value)
  | PConstNumber(PConstNumberFloat({txt: value})) => string(value)
  | PConstNumber(PConstNumberRational({numerator, slash, denominator})) =>
    string(numerator.txt)
    ++ fmt.print_comment_range(
         fmt,
         ~lead=space,
         ~trail=space,
         numerator.loc,
         slash,
       )
    ++ string("/")
    ++ fmt.print_comment_range(
         fmt,
         ~lead=space,
         ~trail=space,
         slash,
         denominator.loc,
       )
    ++ string(denominator.txt)
  | PConstInt8({txt: value})
  | PConstUint8({txt: value})
  | PConstInt16({txt: value})
  | PConstUint16({txt: value})
  | PConstInt32({txt: value})
  | PConstUint32({txt: value})
  | PConstInt64({txt: value})
  | PConstUint64({txt: value})
  | PConstFloat32({txt: value})
  | PConstFloat64({txt: value})
  | PConstWasmI32({txt: value})
  | PConstWasmI64({txt: value})
  | PConstWasmF32({txt: value})
  | PConstWasmF64({txt: value})
  | PConstBigInt({txt: value})
  | PConstRational({txt: value})
  | PConstBytes({txt: value})
  | PConstString({txt: value})
  | PConstChar({txt: value}) => string(value)
  | PConstBool(value) => string(value ? "true" : "false")
  | PConstVoid => string("void")
  };
};

let print_punnable_pattern =
    (
      fmt,
      ({txt: ident, loc: ident_loc}, pat): (
        Location.loc(Identifier.t),
        pattern,
      ),
    ) => {
  switch (pat.ppat_desc) {
  | PPatVar({txt: name}) when Identifier.string_of_ident(ident) == name =>
    // Don't forget the comments that could have been between a punnable name and value, e.g.
    // { foo: /* foo */ foo, }
    fmt.print_comment_range(fmt, ~trail=space, ident_loc, pat.ppat_loc)
    ++ string(name)
  | _ =>
    fmt.print_identifier(fmt, ident)
    ++ string(":")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         ident_loc,
         pat.ppat_loc,
       )
    ++ fmt.print_pattern(fmt, pat)
  };
};

let print_lambda_argument = (fmt, arg) => {
  fmt.print_pattern(fmt, arg.pla_pattern)
  ++ (
    switch (arg.pla_default) {
    | Some(expr) =>
      string("=")
      ++ fmt.print_comment_range(
           fmt,
           arg.pla_pattern.ppat_loc,
           expr.pexp_loc,
         )
      ++ fmt.print_expression(fmt, expr)
    | None => empty
    }
  );
};

let print_pattern = (fmt, {ppat_desc, ppat_loc}) => {
  switch (ppat_desc) {
  | PPatAny => string("_")
  | PPatVar({txt: name}) => fmt.print_ident_string(fmt, name)
  | PPatAlias(pat, {txt: alias, loc: alias_loc}) =>
    fmt.print_pattern(fmt, pat)
    ++ string(" as")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         pat.ppat_loc,
         alias_loc,
       )
    ++ string(alias)
  | PPatOr(lhs, rhs) =>
    fmt.print_pattern(fmt, lhs)
    ++ string(" |")
    ++ fmt.print_comment_range(
         fmt,
         ~none=breakable_space,
         ~lead=space,
         ~trail=breakable_space,
         lhs.ppat_loc,
         rhs.ppat_loc,
       )
    ++ fmt.print_pattern(fmt, rhs)
  | PPatConstruct({txt: ident, loc: ident_loc}, cstr_pat) =>
    fmt.print_identifier(fmt, ident)
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
            ++ fmt.print_comment_range(
                 fmt,
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
                  fmt.print_comment_range(
                    fmt,
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    ident_loc,
                    next_ident.loc,
                  ),
              ~sep=
                (({loc: prev_loc}, _), ({loc: next_loc}, _)) => {
                  fmt.print_comment_range(
                    fmt,
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev_loc,
                    next_loc,
                  )
                },
              ~trail=
                (({loc: prev_loc}, _)) =>
                  fmt.print_comment_range(
                    fmt,
                    ~lead=space,
                    ~block_end=true,
                    prev_loc,
                    enclosing_end_location(ppat_loc),
                  ),
              ~f=
                (~final, p) =>
                  if (final) {
                    group(fmt.print_punnable_pattern(fmt, p))
                    ++ (
                      switch (closed_flag) {
                      | Open => comma_breakable_space ++ string("_")
                      | Closed => trailing_comma
                      }
                    );
                  } else {
                    group(fmt.print_punnable_pattern(fmt, p) ++ comma);
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
                  fmt.print_comment_range(
                    fmt,
                    ~none=break,
                    ~lead=if_broken(space, empty),
                    ~trail=breakable_space,
                    ident_loc,
                    next,
                  ),
              ~sep=
                ({ppat_loc: prev}, {ppat_loc: next}) => {
                  fmt.print_comment_range(
                    fmt,
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev,
                    next,
                  )
                },
              ~trail=
                ({ppat_loc: prev}) =>
                  fmt.print_comment_range(
                    fmt,
                    ~lead=space,
                    ~block_end=true,
                    prev,
                    enclosing_end_location(ppat_loc),
                  ),
              ~f=
                (~final, p) =>
                  if (final) {
                    group(fmt.print_pattern(fmt, p)) ++ trailing_comma;
                  } else {
                    group(fmt.print_pattern(fmt, p) ++ comma);
                  },
              pats,
            ),
          )
          ++ break,
        )
      }
    )
  | PPatConstraint(pat, typ) =>
    fmt.print_pattern(fmt, pat)
    ++ string(":")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         pat.ppat_loc,
         typ.ptyp_loc,
       )
    ++ fmt.print_type(fmt, typ)
  | PPatConstant(constant) => fmt.print_constant(fmt, constant)
  | PPatRecord(pats, closed_flag) =>
    braces(
      indent(
        concat_map(
          ~lead=
            ((next_ident, _)) =>
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                enclosing_start_location(ppat_loc),
                next_ident.loc,
              ),
          ~sep=
            ((_, {ppat_loc: prev_loc}), ({loc: next_loc}, _)) => {
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                prev_loc,
                next_loc,
              )
            },
          ~trail=
            ((_, {ppat_loc: prev_loc})) =>
              fmt.print_comment_range(
                fmt,
                ~lead=space,
                ~block_end=true,
                prev_loc,
                enclosing_end_location(ppat_loc),
              ),
          ~f=
            (~final, p) =>
              if (final) {
                group(fmt.print_punnable_pattern(fmt, p))
                ++ (
                  switch (closed_flag) {
                  | Open when pats == [] => string("_")
                  | Open => comma_breakable_space ++ string("_")
                  | Closed => trailing_comma
                  }
                );
              } else {
                group(fmt.print_punnable_pattern(fmt, p) ++ comma);
              },
          pats,
        ),
      )
      ++ breakable_space,
    )
  | PPatArray([]) =>
    array_brackets(
      indent(
        fmt.print_comment_range(
          fmt,
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
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                enclosing_start_location(ppat_loc),
                next.ppat_loc,
              ),
          ~sep=
            (prev, next) =>
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                prev.ppat_loc,
                next.ppat_loc,
              ),
          ~trail=
            prev =>
              fmt.print_comment_range(
                fmt,
                ~lead=space,
                ~block_end=true,
                prev.ppat_loc,
                enclosing_end_location(ppat_loc),
              ),
          ~f=
            (~final, p) =>
              if (final) {
                group(fmt.print_pattern(fmt, p)) ++ trailing_comma;
              } else {
                group(fmt.print_pattern(fmt, p) ++ comma);
              },
          pats,
        ),
      )
      ++ break,
    )
  | PPatList([]) =>
    list_brackets(
      indent(
        fmt.print_comment_range(
          fmt,
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
              fmt.print_comment_range(
                fmt,
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
              fmt.print_comment_range(
                fmt,
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
              fmt.print_comment_range(
                fmt,
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
                group(fmt.print_pattern(fmt, pat)) ++ trailing_comma
              | ListItem(pat) => group(fmt.print_pattern(fmt, pat) ++ comma)
              | ListSpread(pat, _) when final =>
                group(string("...") ++ fmt.print_pattern(fmt, pat))
              | ListSpread(pat, _) =>
                group(string("...") ++ fmt.print_pattern(fmt, pat) ++ comma)
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
              fmt.print_comment_range(
                fmt,
                ~none=break,
                ~lead=if_broken(space, empty),
                ~trail=breakable_space,
                enclosing_start_location(ppat_loc),
                next,
              ),
          ~sep=
            ({ppat_loc: prev}, {ppat_loc: next}) => {
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                prev,
                next,
              )
            },
          ~trail=
            ({ppat_loc: prev}) =>
              fmt.print_comment_range(
                fmt,
                ~lead=space,
                ~block_end=true,
                prev,
                enclosing_end_location(ppat_loc),
              ),
          ~f=
            (~final, p) =>
              if (final) {
                group(fmt.print_pattern(fmt, p)) ++ trailing_comma;
              } else {
                group(fmt.print_pattern(fmt, p) ++ comma);
              },
          pats,
        ),
      )
      ++ break,
    )
  };
};

let print_ident_string = (fmt, ident) =>
  if (infixop(ident) || prefixop(ident)) {
    parens(string(ident));
  } else {
    string(ident);
  };

let print_identifier = (fmt, ident) => {
  switch (ident) {
  | Identifier.IdentName({txt: ident}) => fmt.print_ident_string(fmt, ident)
  | IdentExternal(mod_, {txt: ident}) =>
    fmt.print_identifier(fmt, mod_)
    ++ string(".")
    ++ fmt.print_ident_string(fmt, ident)
  };
};

let print_punnable_expression = (fmt, ({txt: ident, loc: ident_loc}, expr)) => {
  switch (expr.pexp_desc) {
  | PExpId({txt: name}) when Identifier.equal(ident, name) =>
    // Don't forget the comments that could have been between a punnable name and value, e.g.
    // { foo: /* foo */ foo, }
    fmt.print_comment_range(fmt, ~trail=space, ident_loc, expr.pexp_loc)
    ++ fmt.print_identifier(fmt, name)
  | _ =>
    fmt.print_identifier(fmt, ident)
    ++ string(":")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         ident_loc,
         expr.pexp_loc,
       )
    ++ fmt.print_expression(fmt, expr)
  };
};

let print_grouped_access_expression = (fmt, expr) =>
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
  | PExpList(_) => fmt.print_expression(fmt, expr)
  | PExpApp(func, _) when is_infix_op(func) =>
    parens(indent(break ++ fmt.print_expression(fmt, expr)) ++ break)
  | PExpApp(_) => fmt.print_expression(fmt, expr)
  | _ => parens(indent(break ++ fmt.print_expression(fmt, expr)) ++ break)
  };

let print_use_item = (fmt, use_item) => {
  switch (use_item) {
  | PUseType({name, alias, loc}) =>
    string("type")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(loc),
         name.loc,
       )
    ++ fmt.print_identifier(fmt, name.txt)
    ++ (
      switch (alias) {
      | None => empty
      | Some({txt: alias, loc: alias_loc}) =>
        string(" as")
        ++ fmt.print_comment_range(
             fmt,
             ~none=space,
             ~lead=space,
             ~trail=space,
             name.loc,
             alias_loc,
           )
        ++ fmt.print_identifier(fmt, alias)
      }
    )
  | PUseException({name, alias, loc}) =>
    string("exception")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(loc),
         name.loc,
       )
    ++ fmt.print_identifier(fmt, name.txt)
    ++ (
      switch (alias) {
      | None => empty
      | Some({txt: alias, loc: alias_loc}) =>
        string(" as")
        ++ fmt.print_comment_range(
             fmt,
             ~none=space,
             ~lead=space,
             ~trail=space,
             name.loc,
             alias_loc,
           )
        ++ fmt.print_identifier(fmt, alias)
      }
    )
  | PUseModule({name, alias, loc}) =>
    string("module")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(loc),
         name.loc,
       )
    ++ fmt.print_identifier(fmt, name.txt)
    ++ (
      switch (alias) {
      | None => empty
      | Some({txt: alias, loc: alias_loc}) =>
        string(" as")
        ++ fmt.print_comment_range(
             fmt,
             ~none=space,
             ~lead=space,
             ~trail=space,
             name.loc,
             alias_loc,
           )
        ++ fmt.print_identifier(fmt, alias)
      }
    )
  | PUseValue({name, alias}) =>
    fmt.print_identifier(fmt, name.txt)
    ++ (
      switch (alias) {
      | None => empty
      | Some({txt: alias, loc: alias_loc}) =>
        string(" as")
        ++ fmt.print_comment_range(
             fmt,
             ~none=space,
             ~lead=space,
             ~trail=space,
             name.loc,
             alias_loc,
           )
        ++ fmt.print_identifier(fmt, alias)
      }
    )
  };
};

let print_match_branch = (fmt, {pmb_pat, pmb_body, pmb_guard}) => {
  let space_type =
    switch (pmb_body.pexp_desc) {
    | PExpBlock(_) => space
    | _ => breakable_space
    };
  switch (pmb_guard) {
  | None =>
    group(fmt.print_pattern(fmt, pmb_pat) ++ string(" =>"))
    ++ group(
         indent(
           fmt.print_comment_range(
             fmt,
             ~none=space_type,
             ~lead=space,
             ~trail=space_type,
             pmb_pat.ppat_loc,
             pmb_body.pexp_loc,
           )
           ++ group(fmt.print_expression(fmt, pmb_body) ++ comma),
         ),
       )
  | Some(guard) =>
    group(print_pattern(fmt, pmb_pat))
    ++ group(
         ~kind=FitAll,
         indent(
           fmt.print_comment_range(
             fmt,
             ~none=breakable_space,
             ~lead=space,
             ~trail=breakable_space,
             pmb_pat.ppat_loc,
             guard.pexp_loc,
           )
           ++ string("when ")
           ++ group(fmt.print_expression(fmt, guard)),
         )
         ++ string(" =>"),
       )
    ++ group(
         indent(
           fmt.print_comment_range(
             fmt,
             ~none=space_type,
             ~lead=space,
             ~trail=space_type,
             guard.pexp_loc,
             pmb_body.pexp_loc,
           )
           ++ group(fmt.print_expression(fmt, pmb_body) ++ comma),
         ),
       )
  };
};

let print_attribute = (fmt, attr) => {
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
                 fmt.print_comment_range(
                   fmt,
                   ~none=break,
                   ~lead=if_broken(space, empty),
                   ~trail=breakable_space,
                   attr_name_loc,
                   next.loc,
                 ),
             ~sep=
               (prev, next) =>
                 fmt.print_comment_range(
                   fmt,
                   ~none=breakable_space,
                   ~lead=space,
                   ~trail=breakable_space,
                   prev.loc,
                   next.loc,
                 ),
             ~trail=
               prev =>
                 fmt.print_comment_range(
                   fmt,
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
};

let print_application_argument = (fmt, ~infix_wrap=?, arg) => {
  (
    switch (arg.paa_label) {
    | Unlabeled => empty
    | Labeled({txt: label, loc: label_loc})
    | Default({txt: label, loc: label_loc}) =>
      string(label)
      ++ string("=")
      ++ fmt.print_comment_range(fmt, label_loc, arg.paa_expr.pexp_loc)
    }
  )
  ++ fmt.print_expression(fmt, ~infix_wrap?, arg.paa_expr);
};

let print_if =
    (fmt, ~force_blocks=false, ~loc, condition, true_branch, false_branch) =>
  if (force_blocks) {
    let true_branch_doc =
      switch (true_branch.pexp_desc) {
      | PExpBlock(_) => fmt.print_expression(fmt, true_branch)
      | PExpIf(_) =>
        parens(
          indent(break ++ fmt.print_expression(fmt, true_branch)) ++ break,
        )
      | _ =>
        braces(
          ~wrap=doc => group(~print_width=2, doc),
          indent(hardline ++ fmt.print_expression(fmt, true_branch))
          ++ hardline,
        )
      };
    let false_branch_doc =
      switch (false_branch) {
      | Some({pexp_desc: PExpBlock(_)} as false_branch) =>
        Some(fmt.print_expression(fmt, false_branch))
      | Some({
          pexp_desc: PExpIf(condition, true_branch, false_branch),
          pexp_loc: loc,
        }) =>
        Some(
          fmt.print_if(
            fmt,
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
            indent(hardline ++ fmt.print_expression(fmt, false_branch))
            ++ hardline,
          ),
        )
      | None => None
      };
    group(
      string("if ")
      ++ parens(
           indent(
             fmt.print_comment_range(
               fmt,
               ~none=break,
               ~lead=if_broken(space, empty),
               ~trail=breakable_space,
               enclosing_start_location(loc),
               condition.pexp_loc,
             )
             ++ fmt.print_expression(fmt, ~infix_wrap=Fun.id, condition)
             ++ fmt.print_comment_range(
                  fmt,
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
          fmt.print_comment_range(
            fmt,
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
      fmt.print_if(
        fmt,
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
          parens(
            indent(break ++ fmt.print_expression(fmt, true_branch)) ++ break,
          )
        | _ => fmt.print_expression(fmt, true_branch)
        };
      group(
        string("if ")
        ++ parens(
             indent(
               fmt.print_comment_range(
                 fmt,
                 ~none=break,
                 ~lead=if_broken(space, empty),
                 ~trail=breakable_space,
                 enclosing_start_location(loc),
                 condition.pexp_loc,
               )
               ++ fmt.print_expression(fmt, ~infix_wrap=Fun.id, condition)
               ++ fmt.print_comment_range(
                    fmt,
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
          parens(
            indent(break ++ fmt.print_expression(fmt, true_branch)) ++ break,
          )
        | _ => fmt.print_expression(fmt, true_branch)
        };
      group(
        string("if ")
        ++ parens(
             indent(
               fmt.print_comment_range(
                 fmt,
                 ~none=break,
                 ~lead=if_broken(space, empty),
                 ~trail=breakable_space,
                 enclosing_start_location(loc),
                 condition.pexp_loc,
               )
               ++ fmt.print_expression(fmt, ~infix_wrap=Fun.id, condition)
               ++ fmt.print_comment_range(
                    fmt,
                    ~block_end=true,
                    ~lead=space,
                    condition.pexp_loc,
                    true_branch.pexp_loc,
                  ),
             )
             ++ break,
           )
        ++ indent(breakable_space ++ true_branch_doc)
        ++ fmt.print_comment_range(
             fmt,
             ~none=breakable_space,
             ~lead=space,
             ~trail=breakable_space,
             true_branch.pexp_loc,
             false_branch.pexp_loc,
           )
        ++ string("else")
        ++ indent(breakable_space ++ fmt.print_expression(fmt, false_branch)),
      );
    };
  };

let print_assignment = (fmt, ~collapsible, ~lhs_loc, new_value) => {
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
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         lhs_loc,
         // TODO(#1977): There appears to be a bug with the parser that the location of
         // paa_loc is further to the left than the underlying expression, so
         // here we just use the location of the expression directly.
         arg2.paa_expr.pexp_loc,
       )
    ++ fmt.print_application_argument(fmt, arg2)
  | _ =>
    string(" =")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         lhs_loc,
         new_value.pexp_loc,
       )
    ++ fmt.print_expression(fmt, new_value)
  };
};
let print_expression = (fmt, ~infix_wrap=d => group(indent(d)), expr) => {
  group(
    concat_map(
      ~lead=_ => empty,
      ~sep=
        (prev, next) =>
          fmt.print_comment_range(
            fmt,
            ~none=hardline,
            ~lead=space,
            ~trail=hardline,
            prev.Asttypes.attr_loc,
            next.attr_loc,
          ),
      ~trail=
        prev =>
          fmt.print_comment_range(
            fmt,
            ~none=hardline,
            ~lead=space,
            ~trail=hardline,
            prev.Asttypes.attr_loc,
            expr.pexp_core_loc,
          ),
      ~f=(~final, a) => fmt.print_attribute(fmt, a),
      expr.pexp_attributes,
    ),
  )
  ++ (
    switch (expr.pexp_desc) {
    | PExpId({txt: ident}) => fmt.print_identifier(fmt, ident)
    | PExpConstant(constant) => fmt.print_constant(fmt, constant)
    | PExpConstruct({txt: ident, loc: ident_loc}, cstr_expr) =>
      fmt.print_identifier(fmt, ident)
      ++ (
        switch (cstr_expr) {
        | PExpConstrSingleton => empty
        | PExpConstrTuple(exprs) =>
          parens(
            indent(
              concat_map(
                ~lead=
                  next =>
                    fmt.print_comment_range(
                      fmt,
                      ~none=break,
                      ~lead=if_broken(space, empty),
                      ~trail=breakable_space,
                      ident_loc,
                      next.pexp_loc,
                    ),
                ~sep=
                  (prev, next) =>
                    fmt.print_comment_range(
                      fmt,
                      ~none=breakable_space,
                      ~lead=space,
                      ~trail=breakable_space,
                      prev.pexp_loc,
                      next.pexp_loc,
                    ),
                ~trail=
                  prev =>
                    fmt.print_comment_range(
                      fmt,
                      ~lead=space,
                      ~block_end=true,
                      prev.pexp_loc,
                      enclosing_end_location(expr.pexp_loc),
                    ),
                ~f=
                  (~final, e) =>
                    if (final) {
                      group(fmt.print_expression(fmt, e)) ++ trailing_comma;
                    } else {
                      group(fmt.print_expression(fmt, e) ++ comma);
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
                    fmt.print_comment_range(
                      fmt,
                      ~none=breakable_space,
                      ~lead=space,
                      ~trail=breakable_space,
                      ident_loc,
                      next_ident.loc,
                    ),
                ~sep=
                  ((_, prev), (next, _)) =>
                    fmt.print_comment_range(
                      fmt,
                      ~none=breakable_space,
                      ~lead=space,
                      ~trail=breakable_space,
                      prev.pexp_loc,
                      next.loc,
                    ),
                ~trail=
                  ((_, prev)) =>
                    fmt.print_comment_range(
                      fmt,
                      ~lead=space,
                      ~block_end=true,
                      prev.pexp_loc,
                      enclosing_end_location(expr.pexp_loc),
                    ),
                ~f=
                  (~final, e) =>
                    if (final) {
                      group(fmt.print_punnable_expression(fmt, e));
                    } else {
                      group(fmt.print_punnable_expression(fmt, e) ++ comma);
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
                fmt.print_comment_range(
                  fmt,
                  ~none=hardline,
                  ~lead=space,
                  ~trail=hardline,
                  enclosing_start_location(expr.pexp_loc),
                  first.pexp_loc,
                ),
            ~sep=
              (prev, next) =>
                fmt.print_comment_range(
                  fmt,
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
                fmt.print_comment_range(
                  fmt,
                  ~block_end=true,
                  ~lead=space,
                  last.pexp_loc,
                  enclosing_end_location(expr.pexp_loc),
                ),
            ~f=
              (~final, e) =>
                if (has_disable_formatting_comment(fmt.comments, e.pexp_loc)) {
                  fmt.print_original_code(fmt, e.pexp_loc);
                } else {
                  fmt.print_expression(fmt, e);
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
      ++ fmt.print_comment_range(
           fmt,
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
            fmt.print_comment_range(
              fmt,
              ~none=hardline,
              ~lead=space,
              ~trail=hardline,
              prev.pvb_loc,
              next.pvb_loc,
            )
            ++ string("and "),
        ~trail=_ => empty,
        ~f=(~final, vb) => fmt.print_value_binding(fmt, vb),
        vbs,
      )
    | PExpApp(fn, [arg]) when is_prefix_op(fn) =>
      fmt.print_infix_prefix_op(fmt, fn)
      ++ fmt.print_comment_range(fmt, fn.pexp_loc, arg.paa_loc)
      ++ (
        switch (needs_grouping(~parent=fn, ~side=Left, arg.paa_expr)) {
        | ParenGrouping =>
          parens(
            indent(break ++ fmt.print_application_argument(fmt, arg))
            ++ break,
          )
        | FormatterGrouping =>
          group(fmt.print_application_argument(fmt, arg))
        | None => fmt.print_application_argument(fmt, arg)
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
              break
              ++ fmt.print_application_argument(fmt, ~infix_wrap=Fun.id, lhs),
            )
            ++ break,
          )
        | FormatterGrouping =>
          group(
            indent(
              fmt.print_application_argument(fmt, ~infix_wrap=Fun.id, lhs),
            ),
          )
        | None => fmt.print_application_argument(fmt, ~infix_wrap=Fun.id, lhs)
        }
      )
      ++ fmt.print_comment_range(
           fmt,
           ~none=space,
           ~lead=space,
           ~trail=space,
           ~allow_breaks=false,
           lhs.paa_loc,
           fn.pexp_loc,
         )
      ++ fmt.print_infix_prefix_op(fmt, fn)
      ++ fmt.print_comment_range(
           fmt,
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
              break
              ++ fmt.print_application_argument(fmt, ~infix_wrap=Fun.id, rhs),
            )
            ++ break,
          )
        | FormatterGrouping =>
          group(
            indent(
              fmt.print_application_argument(fmt, ~infix_wrap=Fun.id, rhs),
            ),
          )
        | None => fmt.print_application_argument(fmt, ~infix_wrap=Fun.id, rhs)
        }
      )
    | PExpApp(fn, [rhs]) when is_keyword_function(fn) =>
      fmt.print_expression(fmt, fn)
      ++ fmt.print_comment_range(
           fmt,
           ~none=space,
           ~lead=space,
           ~trail=space,
           fn.pexp_loc,
           rhs.paa_loc,
         )
      ++ fmt.print_expression(fmt, rhs.paa_expr)
    | PExpApp(fn, exprs) =>
      group(
        fmt.print_grouped_access_expression(fmt, fn)
        ++ parens(
             indent(
               concat_map(
                 ~lead=
                   next =>
                     fmt.print_comment_range(
                       fmt,
                       ~none=break,
                       ~lead=if_broken(space, empty),
                       ~trail=breakable_space,
                       fn.pexp_loc,
                       next.paa_loc,
                     ),
                 ~sep=
                   (prev, next) =>
                     fmt.print_comment_range(
                       fmt,
                       ~none=breakable_space,
                       ~lead=space,
                       ~trail=breakable_space,
                       prev.paa_loc,
                       next.paa_loc,
                     ),
                 ~trail=
                   prev =>
                     fmt.print_comment_range(
                       fmt,
                       ~block_end=true,
                       ~lead=space,
                       prev.paa_loc,
                       enclosing_end_location(expr.pexp_loc),
                     ),
                 ~f=
                   (~final, a) =>
                     if (final) {
                       group(fmt.print_application_argument(fmt, a));
                     } else {
                       group(
                         fmt.print_application_argument(fmt, a) ++ comma,
                       );
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
      fmt.print_lambda_argument(fmt, single_param)
      ++ string(" =>")
      ++ fmt.print_comment_range(fmt, ~lead=space, label_loc, body.pexp_loc)
      ++ group(
           switch (body.pexp_desc) {
           | PExpBlock(_) => space ++ fmt.print_expression(fmt, body)
           | _ => indent(breakable_space ++ fmt.print_expression(fmt, body))
           },
         )
    | PExpLambda(params, body) =>
      parens(
        indent(
          concat_map(
            ~lead=
              next =>
                fmt.print_comment_range(
                  fmt,
                  ~none=break,
                  ~lead=if_broken(space, empty),
                  ~trail=breakable_space,
                  enclosing_start_location(expr.pexp_loc),
                  next.pla_loc,
                ),
            ~sep=
              (prev, next) =>
                fmt.print_comment_range(
                  fmt,
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev.pla_loc,
                  next.pla_loc,
                ),
            ~trail=
              last =>
                fmt.print_comment_range(
                  fmt,
                  ~block_end=true,
                  ~lead=space,
                  last.pla_loc,
                  body.pexp_loc,
                ),
            ~f=
              (~final, a) =>
                if (final) {
                  group(fmt.print_lambda_argument(fmt, a)) ++ trailing_comma;
                } else {
                  group(fmt.print_lambda_argument(fmt, a) ++ comma);
                },
            params,
          ),
        )
        ++ break,
      )
      ++ string(" =>")
      ++ group(
           switch (body.pexp_desc) {
           | PExpBlock(_) => space ++ fmt.print_expression(fmt, body)
           | _ => indent(breakable_space ++ fmt.print_expression(fmt, body))
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
                fmt.print_comment_range(
                  fmt,
                  ~none=break,
                  ~lead=if_broken(space, empty),
                  ~trail=breakable_space,
                  enclosing_start_location(expr.pexp_loc),
                  next.pexp_loc,
                ),
            ~sep=
              (prev, next) =>
                fmt.print_comment_range(
                  fmt,
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev.pexp_loc,
                  next.pexp_loc,
                ),
            ~trail=
              last =>
                fmt.print_comment_range(
                  fmt,
                  ~block_end=true,
                  ~lead=space,
                  last.pexp_loc,
                  enclosing_end_location(expr.pexp_loc),
                ),
            ~f=
              (~final, e) =>
                if (final) {
                  group(fmt.print_expression(fmt, e)) ++ trailing_comma;
                } else {
                  group(fmt.print_expression(fmt, e) ++ comma);
                },
            exprs,
          ),
        )
        ++ break,
      )
    | PExpArray([]) =>
      array_brackets(
        indent(
          fmt.print_comment_range(
            fmt,
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
                fmt.print_comment_range(
                  fmt,
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  enclosing_start_location(expr.pexp_loc),
                  next.pexp_loc,
                ),
            ~sep=
              (prev, next) =>
                fmt.print_comment_range(
                  fmt,
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev.pexp_loc,
                  next.pexp_loc,
                ),
            ~trail=
              prev =>
                fmt.print_comment_range(
                  fmt,
                  ~block_end=true,
                  ~lead=space,
                  prev.pexp_loc,
                  enclosing_end_location(expr.pexp_loc),
                ),
            ~f=
              (~final, e) =>
                if (final) {
                  group(fmt.print_expression(fmt, e)) ++ trailing_comma;
                } else {
                  group(fmt.print_expression(fmt, e) ++ comma);
                },
            exprs,
          ),
        )
        ++ break,
      )
    | PExpList([]) =>
      list_brackets(
        indent(
          fmt.print_comment_range(
            fmt,
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
                fmt.print_comment_range(
                  fmt,
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
                fmt.print_comment_range(
                  fmt,
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
                fmt.print_comment_range(
                  fmt,
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
                  group(fmt.print_expression(fmt, expr)) ++ trailing_comma
                | ListItem(expr) =>
                  group(fmt.print_expression(fmt, expr) ++ comma)
                | ListSpread(expr, _) when final =>
                  group(string("...") ++ fmt.print_expression(fmt, expr))
                | ListSpread(expr, _) =>
                  group(
                    string("...") ++ fmt.print_expression(fmt, expr) ++ comma,
                  )
                }
              },
            items,
          ),
        )
        ++ break,
      )
    | PExpArrayGet(arr, elem) =>
      fmt.print_grouped_access_expression(fmt, arr)
      ++ fmt.print_comment_range(fmt, arr.pexp_loc, elem.pexp_loc)
      ++ list_brackets(
           indent(
             break ++ fmt.print_expression(fmt, ~infix_wrap=Fun.id, elem),
           )
           ++ break,
         )
    | PExpArraySet({array, index, value, infix_op: None}) =>
      fmt.print_grouped_access_expression(fmt, array)
      ++ fmt.print_comment_range(fmt, array.pexp_loc, index.pexp_loc)
      ++ list_brackets(
           indent(
             break ++ fmt.print_expression(fmt, ~infix_wrap=Fun.id, index),
           )
           ++ break,
         )
      ++ string(" =")
      ++ fmt.print_comment_range(
           fmt,
           ~none=space,
           ~lead=space,
           ~trail=space,
           index.pexp_loc,
           value.pexp_loc,
         )
      ++ fmt.print_expression(fmt, value)
    | PExpArraySet({array, index, value, infix_op: Some(infix)}) =>
      fmt.print_grouped_access_expression(fmt, array)
      ++ fmt.print_comment_range(fmt, array.pexp_loc, index.pexp_loc)
      ++ list_brackets(
           indent(
             break ++ fmt.print_expression(fmt, ~infix_wrap=Fun.id, index),
           )
           ++ break,
         )
      ++ fmt.print_comment_range(
           fmt,
           ~none=space,
           ~lead=space,
           ~trail=space,
           index.pexp_loc,
           infix.pexp_loc,
         )
      ++ fmt.print_infix_prefix_op(fmt, infix)
      ++ string("=")
      ++ fmt.print_comment_range(
           fmt,
           ~none=space,
           ~lead=space,
           ~trail=space,
           infix.pexp_loc,
           value.pexp_loc,
         )
      ++ fmt.print_expression(fmt, value)
    | PExpRecord(base, labels) =>
      braces(
        indent(
          concat_map(
            ~lead=
              ((next_ident, _)) =>
                switch (base) {
                | None =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    enclosing_start_location(expr.pexp_loc),
                    next_ident.loc,
                  )
                | Some(base_expr) =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    enclosing_start_location(expr.pexp_loc),
                    base_expr.pexp_loc,
                  )
                  ++ string("...")
                  ++ fmt.print_expression(fmt, base_expr)
                  ++ comma
                  ++ fmt.print_comment_range(
                       fmt,
                       ~none=breakable_space,
                       ~lead=space,
                       ~trail=breakable_space,
                       base_expr.pexp_loc,
                       next_ident.loc,
                     )
                },
            ~sep=
              ((_, {pexp_loc: prev_loc}), ({loc: next_loc}, _)) =>
                fmt.print_comment_range(
                  fmt,
                  ~none=breakable_space,
                  ~lead=space,
                  ~trail=breakable_space,
                  prev_loc,
                  next_loc,
                ),
            ~trail=
              ((_, {pexp_loc: prev_loc})) =>
                fmt.print_comment_range(
                  fmt,
                  ~lead=space,
                  ~block_end=true,
                  prev_loc,
                  enclosing_end_location(expr.pexp_loc),
                ),
            ~f=
              (~final, e) =>
                if (final) {
                  group(fmt.print_punnable_expression(fmt, e))
                  ++ (
                    if (Option.is_none(base) && List.length(labels) == 1) {
                      comma;
                    } else {
                      trailing_comma;
                    }
                  );
                } else {
                  group(fmt.print_punnable_expression(fmt, e) ++ comma);
                },
            labels,
          ),
        )
        ++ breakable_space,
      )
    | PExpRecordGet(record, elem) =>
      fmt.print_grouped_access_expression(fmt, record)
      ++ string(".")
      ++ fmt.print_comment_range(fmt, record.pexp_loc, elem.loc)
      ++ fmt.print_identifier(fmt, elem.txt)
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
      fmt.print_grouped_access_expression(fmt, record)
      ++ string(".")
      ++ fmt.print_comment_range(fmt, record.pexp_loc, elem.loc)
      ++ fmt.print_identifier(fmt, elem.txt)
      ++ fmt.print_assignment(
           fmt,
           ~collapsible=true,
           ~lhs_loc=elem.loc,
           new_value,
         )
    | PExpRecordSet(record, elem, new_value) =>
      fmt.print_grouped_access_expression(fmt, record)
      ++ string(".")
      ++ fmt.print_comment_range(fmt, record.pexp_loc, elem.loc)
      ++ fmt.print_identifier(fmt, elem.txt)
      ++ fmt.print_assignment(
           fmt,
           ~collapsible=false,
           ~lhs_loc=elem.loc,
           new_value,
         )
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
      fmt.print_expression(fmt, binding)
      ++ fmt.print_assignment(
           fmt,
           ~collapsible=true,
           ~lhs_loc=binding.pexp_loc,
           new_value,
         )
    | PExpAssign(binding, new_value) =>
      fmt.print_expression(fmt, binding)
      ++ fmt.print_assignment(
           fmt,
           ~collapsible=false,
           ~lhs_loc=binding.pexp_loc,
           new_value,
         )
    | PExpBoxAssign(binding, new_value) =>
      fmt.print_expression(fmt, binding)
      ++ string(" :=")
      ++ fmt.print_comment_range(
           fmt,
           ~none=space,
           ~lead=space,
           ~trail=space,
           binding.pexp_loc,
           new_value.pexp_loc,
         )
      ++ fmt.print_expression(fmt, new_value)
    | PExpReturn(return_expr) =>
      string("return")
      ++ group(
           switch (return_expr) {
           | None => empty
           | Some(return_expr) =>
             fmt.print_comment_range(
               fmt,
               ~allow_breaks=false,
               ~none=space,
               ~lead=space,
               ~trail=space,
               enclosing_start_location(expr.pexp_loc),
               return_expr.pexp_loc,
             )
             ++ fmt.print_expression(fmt, return_expr)
           },
         )
    | PExpUse(ident, use_items) =>
      string("use")
      ++ fmt.print_comment_range(
           fmt,
           ~allow_breaks=false,
           ~none=space,
           ~lead=space,
           ~trail=space,
           enclosing_start_location(expr.pexp_loc),
           ident.loc,
         )
      ++ fmt.print_identifier(fmt, ident.txt)
      ++ string(".")
      ++ (
        switch (use_items) {
        | PUseAll =>
          fmt.print_comment_range(
            fmt,
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
                    fmt.print_comment_range(
                      fmt,
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
                    fmt.print_comment_range(
                      fmt,
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
                    fmt.print_comment_range(
                      fmt,
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
                      group(fmt.print_use_item(fmt, u)) ++ trailing_comma;
                    } else {
                      group(fmt.print_use_item(fmt, u) ++ comma);
                    },
                items,
              ),
            )
            ++ breakable_space,
          )
        }
      )
    | PExpIf(cond, true_branch, false_branch) =>
      fmt.print_if(fmt, ~loc=expr.pexp_loc, cond, true_branch, false_branch)
    | PExpWhile(cond, body) =>
      string("while ")
      ++ parens(
           indent(
             fmt.print_comment_range(
               fmt,
               ~none=break,
               ~lead=if_broken(space, empty),
               ~trail=breakable_space,
               enclosing_start_location(expr.pexp_loc),
               cond.pexp_loc,
             )
             ++ fmt.print_expression(fmt, ~infix_wrap=Fun.id, cond)
             ++ fmt.print_comment_range(
                  fmt,
                  ~block_end=true,
                  ~lead=space,
                  cond.pexp_loc,
                  body.pexp_loc,
                ),
           )
           ++ break,
         )
      ++ space
      ++ fmt.print_expression(fmt, body)
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
                   fmt.print_comment_range(
                     fmt,
                     ~none=break,
                     ~lead=if_broken(space, empty),
                     ~trail=breakable_space,
                     enclosing_start_location(expr.pexp_loc),
                     init.pexp_loc,
                   )
                   ++ fmt.print_expression(fmt, init)
                 }
               )
               ++ string(";")
               ++ (
                 switch (cond) {
                 | None => break
                 | Some(cond) =>
                   fmt.print_comment_range(
                     fmt,
                     ~none=breakable_space,
                     ~lead=space,
                     ~trail=breakable_space,
                     cond_start_loc,
                     cond.pexp_loc,
                   )
                   ++ fmt.print_expression(fmt, cond)
                 }
               )
               ++ string(";")
               ++ (
                 switch (inc) {
                 | None =>
                   fmt.print_comment_range(
                     fmt,
                     ~block_end=true,
                     ~lead=space,
                     inc_start_loc,
                     body.pexp_loc,
                   )
                 | Some(inc) =>
                   fmt.print_comment_range(
                     fmt,
                     ~none=breakable_space,
                     ~lead=space,
                     ~trail=breakable_space,
                     inc_start_loc,
                     inc.pexp_loc,
                   )
                   ++ fmt.print_expression(fmt, inc)
                   ++ fmt.print_comment_range(
                        fmt,
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
        ++ fmt.print_expression(fmt, body),
      );
    | PExpMatch(value, {txt: branches, loc: branches_loc}) =>
      string("match ")
      ++ parens(
           indent(
             fmt.print_comment_range(
               fmt,
               ~none=break,
               ~lead=if_broken(space, empty),
               ~trail=breakable_space,
               enclosing_start_location(expr.pexp_loc),
               value.pexp_loc,
             )
             ++ fmt.print_expression(fmt, ~infix_wrap=Fun.id, value)
             ++ fmt.print_comment_range(
                  fmt,
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
                   fmt.print_comment_range(
                     fmt,
                     ~none=hardline,
                     ~lead=space,
                     ~trail=hardline,
                     enclosing_start_location(branches_loc),
                     next.pmb_loc,
                   ),
               ~sep=
                 (prev, next) =>
                   fmt.print_comment_range(
                     fmt,
                     ~none=hardline,
                     ~lead=space,
                     ~trail=hardline,
                     prev.pmb_loc,
                     next.pmb_loc,
                   ),
               ~trail=
                 last =>
                   fmt.print_comment_range(
                     fmt,
                     ~block_end=true,
                     ~lead=space,
                     last.pmb_loc,
                     enclosing_end_location(expr.pexp_loc),
                   ),
               ~f=(~final, b) => group(fmt.print_match_branch(fmt, b)),
               branches,
             ),
           )
           ++ hardline,
         )
    | PExpConstraint(inner_expr, typ) =>
      (
        switch (needs_grouping(~parent=expr, ~side=Left, inner_expr)) {
        | ParenGrouping =>
          parens(
            indent(break ++ fmt.print_expression(fmt, inner_expr)) ++ break,
          )
        | FormatterGrouping => group(fmt.print_expression(fmt, inner_expr))
        | None => fmt.print_expression(fmt, inner_expr)
        }
      )
      ++ string(":")
      ++ group(
           indent(
             fmt.print_comment_range(
               fmt,
               ~none=breakable_space,
               ~lead=space,
               ~trail=breakable_space,
               inner_expr.pexp_loc,
               typ.ptyp_loc,
             )
             ++ fmt.print_type(fmt, typ),
           ),
         )
    }
  );
};

let print_value_binding = (fmt, {pvb_pat, pvb_expr}) => {
  group(
    ~kind=FitAll,
    fmt.print_pattern(fmt, pvb_pat)
    ++ string(" =")
    ++ indent(
         fmt.print_comment_range(
           fmt,
           ~none=breakable_space,
           ~lead=space,
           ~trail=breakable_space,
           pvb_pat.ppat_loc,
           pvb_expr.pexp_loc,
         )
         ++ fmt.print_expression(fmt, pvb_expr),
       ),
  );
};

let print_parsed_type_argument = (fmt, arg) => {
  (
    switch (arg.ptyp_arg_label) {
    | Unlabeled => empty
    | Labeled({txt: label, loc: label_loc}) =>
      string(label)
      ++ string(":")
      ++ fmt.print_comment_range(
           fmt,
           ~none=space,
           ~lead=space,
           ~trail=space,
           label_loc,
           arg.ptyp_arg_type.ptyp_loc,
         )
    | Default({txt: label, loc: label_loc}) =>
      string("?")
      ++ string(label)
      ++ string(":")
      ++ fmt.print_comment_range(
           fmt,
           ~none=space,
           ~lead=space,
           ~trail=space,
           label_loc,
           arg.ptyp_arg_type.ptyp_loc,
         )
    }
  )
  ++ fmt.print_type(fmt, arg.ptyp_arg_type);
};

let print_type = (fmt, {ptyp_desc, ptyp_loc}) => {
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
          indent(
            concat_map(
              ~lead=
                next =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=break,
                    ~lead=if_broken(space, empty),
                    ~trail=breakable_space,
                    ident_loc,
                    next.ptyp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.ptyp_loc,
                    next.ptyp_loc,
                  ),
              ~trail=
                prev =>
                  fmt.print_comment_range(
                    fmt,
                    ~block_end=true,
                    ~lead=space,
                    prev.ptyp_loc,
                    enclosing_end_location(ptyp_loc),
                  ),
              ~f=
                (~final, t) =>
                  if (final) {
                    group(fmt.print_type(fmt, t));
                  } else {
                    group(fmt.print_type(fmt, t) ++ comma);
                  },
              typs,
            ),
          )
          ++ break,
        )
      }
    );
  | PTyTuple(typs) =>
    parens(
      indent(
        concat_map(
          ~lead=
            next =>
              fmt.print_comment_range(
                fmt,
                ~none=break,
                ~lead=if_broken(space, empty),
                ~trail=breakable_space,
                enclosing_start_location(ptyp_loc),
                next.ptyp_loc,
              ),
          ~sep=
            (prev, next) =>
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                prev.ptyp_loc,
                next.ptyp_loc,
              ),
          ~trail=
            prev =>
              fmt.print_comment_range(
                fmt,
                ~block_end=true,
                ~lead=space,
                prev.ptyp_loc,
                enclosing_end_location(ptyp_loc),
              ),
          ~f=
            (~final, t) =>
              if (final) {
                group(fmt.print_type(fmt, t));
              } else {
                group(fmt.print_type(fmt, t) ++ comma);
              },
          typs,
        ),
      )
      ++ break,
    )
  | PTyArrow([{ptyp_arg_label: Unlabeled} as param], return) =>
    fmt.print_parsed_type_argument(fmt, param)
    ++ string(" =>")
    ++ fmt.print_comment_range(
         fmt,
         ~none=space,
         ~lead=space,
         ~trail=space,
         param.ptyp_arg_loc,
         return.ptyp_loc,
       )
    ++ fmt.print_type(fmt, return)
  | PTyArrow(params, return) =>
    parens(
      indent(
        concat_map(
          ~lead=
            next =>
              fmt.print_comment_range(
                fmt,
                ~none=break,
                ~lead=if_broken(space, empty),
                ~trail=breakable_space,
                enclosing_start_location(ptyp_loc),
                next.ptyp_arg_loc,
              ),
          ~sep=
            (prev, next) =>
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                prev.ptyp_arg_loc,
                next.ptyp_arg_loc,
              ),
          ~trail=
            prev =>
              fmt.print_comment_range(
                fmt,
                ~block_end=true,
                ~lead=space,
                prev.ptyp_arg_loc,
                return.ptyp_loc,
              ),
          ~f=
            (~final, a) =>
              if (final) {
                group(fmt.print_parsed_type_argument(fmt, a))
                ++ trailing_comma;
              } else {
                group(fmt.print_parsed_type_argument(fmt, a) ++ comma);
              },
          params,
        ),
      )
      ++ break,
    )
    ++ string(" => ")
    ++ fmt.print_type(fmt, return)
  | PTyPoly(_) => failwith("Impossible: PTyPoly in the parsetree")
  };
};

let print_label_declaration =
    (fmt, {pld_name, pld_type, pld_mutable, pld_loc}) => {
  (
    switch (pld_mutable) {
    | Mutable => string("mut ")
    | Immutable => empty
    }
  )
  ++ fmt.print_identifier(fmt, pld_name.txt)
  ++ string(":")
  ++ fmt.print_comment_range(
       fmt,
       ~none=space,
       ~lead=space,
       ~trail=space,
       enclosing_start_location(pld_loc),
       pld_type.ptyp_loc,
     )
  ++ fmt.print_type(fmt, pld_type);
};
let print_constructor_arguments = (fmt, args) => {
  switch (args) {
  | PConstrTuple({txt: typs, loc: typs_loc}) =>
    parens(
      indent(
        concat_map(
          ~lead=
            first =>
              fmt.print_comment_range(
                fmt,
                ~none=break,
                ~lead=if_broken(space, empty),
                ~trail=breakable_space,
                enclosing_start_location(typs_loc),
                first.ptyp_loc,
              ),
          ~sep=
            (prev, next) =>
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                prev.ptyp_loc,
                next.ptyp_loc,
              ),
          ~trail=
            last =>
              fmt.print_comment_range(
                fmt,
                ~lead=breakable_space,
                last.ptyp_loc,
                enclosing_end_location(typs_loc),
              ),
          ~f=
            (~final, t) =>
              if (final) {
                group(fmt.print_type(fmt, t));
              } else {
                group(fmt.print_type(fmt, t) ++ comma);
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
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                enclosing_start_location(labels_loc),
                next.pld_loc,
              ),
          ~sep=
            (prev, next) =>
              fmt.print_comment_range(
                fmt,
                ~none=breakable_space,
                ~lead=space,
                ~trail=breakable_space,
                prev.pld_loc,
                next.pld_loc,
              ),
          ~trail=
            last =>
              fmt.print_comment_range(
                fmt,
                ~block_end=true,
                ~lead=space,
                last.pld_loc,
                enclosing_end_location(labels_loc),
              ),
          ~f=
            (~final, ld) =>
              if (final) {
                group(fmt.print_label_declaration(fmt, ld))
                ++ (
                  switch (labels) {
                  | [_single_element] => comma
                  | _ => trailing_comma
                  }
                );
              } else {
                group(fmt.print_label_declaration(fmt, ld) ++ comma);
              },
          labels,
        ),
      )
      ++ breakable_space,
    )
  | PConstrSingleton => empty
  };
};

let print_exception = (fmt, {ptyexn_constructor, ptyexn_loc}) => {
  string("exception")
  ++ fmt.print_comment_range(
       fmt,
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
      fmt.print_comment_range(fmt, ptyexn_constructor.pext_name.loc, loc)
      ++ fmt.print_constructor_arguments(fmt, args)
    | PExtDecl(PConstrSingleton)
    | PExtRebind(_) => empty
    }
  );
};

let print_constructor_declaration = (fmt, {pcd_name, pcd_args}) => {
  string(pcd_name.txt)
  ++ (
    switch (pcd_args) {
    | PConstrTuple({loc})
    | PConstrRecord({loc}) => fmt.print_comment_range(fmt, pcd_name.loc, loc)
    | PConstrSingleton => empty
    }
  )
  ++ fmt.print_constructor_arguments(fmt, pcd_args);
};

let print_data_declaration = (fmt, decl) => {
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
    ++ fmt.print_comment_range(
         fmt,
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
          indent(
            concat_map(
              ~lead=
                next =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=break,
                    ~lead=if_broken(space, empty),
                    ~trail=breakable_space,
                    pdata_name.loc,
                    next.ptyp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=breakable_space,
                    ~lead=space,
                    ~trail=breakable_space,
                    prev.ptyp_loc,
                    next.ptyp_loc,
                  ),
              ~trail=
                prev =>
                  fmt.print_comment_range(
                    fmt,
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
                    group(fmt.print_type(fmt, t));
                  } else {
                    group(fmt.print_type(fmt, t) ++ comma);
                  },
              pdata_params,
            ),
          )
          ++ break,
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
                   fmt.print_comment_range(
                     fmt,
                     ~none=breakable_space,
                     ~lead=space,
                     ~trail=breakable_space,
                     pdata_name.loc,
                     typ.ptyp_loc,
                   )
                 | _ => breakable_space
                 }
               )
               ++ fmt.print_type(fmt, typ),
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
    ++ fmt.print_comment_range(
         fmt,
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
          indent(
            concat_map(
              ~lead=
                next =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=break,
                    ~lead=if_broken(space, empty),
                    ~trail=breakable_space,
                    pdata_name.loc,
                    next.ptyp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  fmt.print_comment_range(
                    fmt,
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
                    group(fmt.print_type(fmt, t));
                  } else {
                    group(fmt.print_type(fmt, t) ++ comma);
                  },
              pdata_params,
            ),
          )
          ++ break,
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
                 fmt.print_comment_range(
                   fmt,
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
                 fmt.print_comment_range(
                   fmt,
                   ~none=hardline,
                   ~lead=space,
                   ~trail=hardline,
                   prev.pcd_loc,
                   next.pcd_loc,
                 ),
             ~trail=
               last =>
                 fmt.print_comment_range(
                   fmt,
                   ~block_end=true,
                   ~lead=space,
                   last.pcd_loc,
                   enclosing_end_location(pdata_loc),
                 ),
             ~f=
               (~final, cd) =>
                 group(fmt.print_constructor_declaration(fmt, cd) ++ comma),
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
    ++ fmt.print_comment_range(
         fmt,
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
          indent(
            concat_map(
              ~lead=
                next =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=break,
                    ~lead=if_broken(space, empty),
                    ~trail=breakable_space,
                    pdata_name.loc,
                    next.ptyp_loc,
                  ),
              ~sep=
                (prev, next) =>
                  fmt.print_comment_range(
                    fmt,
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
                    group(fmt.print_type(fmt, t));
                  } else {
                    group(fmt.print_type(fmt, t) ++ comma);
                  },
              pdata_params,
            ),
          )
          ++ break,
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
                 fmt.print_comment_range(
                   fmt,
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
                 fmt.print_comment_range(
                   fmt,
                   ~none=hardline,
                   ~lead=space,
                   ~trail=hardline,
                   prev.pld_loc,
                   next.pld_loc,
                 ),
             ~trail=
               last =>
                 fmt.print_comment_range(
                   fmt,
                   ~block_end=true,
                   ~lead=space,
                   last.pld_loc,
                   enclosing_end_location(pdata_loc),
                 ),
             ~f=
               (~final, l) =>
                 group(fmt.print_label_declaration(fmt, l) ++ comma),
             labels,
           ),
         )
         ++ hardline,
       )
  };
};

let print_primitive_description = (fmt, {pprim_ident, pprim_name, pprim_loc}) => {
  string("primitive")
  ++ fmt.print_comment_range(
       fmt,
       ~allow_breaks=false,
       ~none=space,
       ~lead=space,
       ~trail=space,
       enclosing_start_location(pprim_loc),
       pprim_ident.loc,
     )
  ++ fmt.print_ident_string(fmt, pprim_ident.txt)
  ++ string(" =")
  ++ fmt.print_comment_range(
       fmt,
       ~allow_breaks=false,
       ~none=space,
       ~lead=space,
       ~trail=space,
       pprim_ident.loc,
       pprim_name.loc,
     )
  ++ double_quotes(string(pprim_name.txt));
};

let print_include_declaration =
    (fmt, {pinc_path, pinc_module, pinc_alias, pinc_loc}) => {
  open Filepath.String;
  let path =
    if (!is_relpath(pinc_path.txt) && check_suffix(pinc_path.txt, ".gr")) {
      chop_suffix(pinc_path.txt, ".gr");
    } else {
      pinc_path.txt;
    };
  string("from")
  ++ fmt.print_comment_range(
       fmt,
       ~allow_breaks=false,
       ~none=space,
       ~lead=space,
       ~trail=space,
       enclosing_start_location(pinc_loc),
       pinc_path.loc,
     )
  ++ double_quotes(string(path))
  ++ fmt.print_comment_range(
       fmt,
       ~allow_breaks=false,
       ~none=space,
       ~lead=space,
       ~trail=space,
       pinc_path.loc,
       pinc_module.loc,
     )
  ++ string("include ")
  ++ string(pinc_module.txt)
  ++ (
    switch (pinc_alias) {
    | None => empty
    | Some({txt: alias, loc: alias_loc}) =>
      string(" as")
      ++ fmt.print_comment_range(
           fmt,
           ~allow_breaks=false,
           ~none=space,
           ~lead=space,
           ~trail=space,
           pinc_module.loc,
           alias_loc,
         )
      ++ string(alias)
    }
  );
};

let print_module_declaration = (fmt, {pmod_name, pmod_stmts, pmod_loc}) => {
  string("module")
  ++ fmt.print_comment_range(
       fmt,
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
               fmt.print_comment_range(
                 fmt,
                 ~none=hardline,
                 ~lead=space,
                 ~trail=hardline,
                 pmod_name.loc,
                 next.ptop_loc,
               ),
           ~sep=
             (prev, next) =>
               fmt.print_comment_range(
                 fmt,
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
               fmt.print_comment_range(
                 fmt,
                 ~lead=space,
                 prev.ptop_loc,
                 enclosing_end_location(pmod_loc),
               ),
           ~f=
             (~final, s) =>
               if (has_disable_formatting_comment(fmt.comments, s.ptop_loc)) {
                 fmt.print_original_code(fmt, s.ptop_loc);
               } else {
                 fmt.print_toplevel_stmt(fmt, s);
               },
           pmod_stmts,
         ),
       )
       ++ hardline,
     );
};

let print_value_description =
    (fmt, {pval_mod, pval_name, pval_name_alias, pval_type, pval_loc}) => {
  group @@
  string(pval_name.txt)
  ++ string(":")
  ++ indent(
       fmt.print_comment_range(
         fmt,
         ~none=breakable_space,
         ~lead=space,
         ~trail=breakable_space,
         pval_name.loc,
         pval_type.ptyp_loc,
       )
       ++ fmt.print_type(fmt, pval_type)
       ++ (
         switch (pval_name_alias) {
         | None => empty
         | Some(alias) =>
           string(" as")
           ++ fmt.print_comment_range(
                fmt,
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
       ++ fmt.print_comment_range(
            fmt,
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
};

let print_provide_item = (fmt, provide_item) => {
  switch (provide_item) {
  | PProvideType({name, alias, loc}) =>
    string("type")
    ++ fmt.print_comment_range(
         fmt,
         ~allow_breaks=false,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(loc),
         name.loc,
       )
    ++ fmt.print_identifier(fmt, name.txt)
    ++ (
      switch (alias) {
      | None => empty
      | Some(alias) =>
        string(" as")
        ++ fmt.print_comment_range(
             fmt,
             ~allow_breaks=false,
             ~none=space,
             ~lead=space,
             ~trail=space,
             name.loc,
             enclosing_end_location(loc),
           )
        ++ fmt.print_identifier(fmt, alias.txt)
      }
    )
  | PProvideException({name, alias, loc}) =>
    string("exception")
    ++ fmt.print_comment_range(
         fmt,
         ~allow_breaks=false,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(loc),
         name.loc,
       )
    ++ fmt.print_identifier(fmt, name.txt)
    ++ (
      switch (alias) {
      | None => empty
      | Some(alias) =>
        string(" as")
        ++ fmt.print_comment_range(
             fmt,
             ~allow_breaks=false,
             ~none=space,
             ~lead=space,
             ~trail=space,
             name.loc,
             enclosing_end_location(loc),
           )
        ++ fmt.print_identifier(fmt, alias.txt)
      }
    )
  | PProvideModule({name, alias, loc}) =>
    string("module")
    ++ fmt.print_comment_range(
         fmt,
         ~allow_breaks=false,
         ~none=space,
         ~lead=space,
         ~trail=space,
         enclosing_start_location(loc),
         name.loc,
       )
    ++ fmt.print_identifier(fmt, name.txt)
    ++ (
      switch (alias) {
      | None => empty
      | Some(alias) =>
        string(" as")
        ++ fmt.print_comment_range(
             fmt,
             ~allow_breaks=false,
             ~none=space,
             ~lead=space,
             ~trail=space,
             name.loc,
             enclosing_end_location(loc),
           )
        ++ fmt.print_identifier(fmt, alias.txt)
      }
    )
  | PProvideValue({name, alias, loc}) =>
    fmt.print_identifier(fmt, name.txt)
    ++ (
      switch (alias) {
      | None => empty
      | Some(alias) =>
        string(" as")
        ++ fmt.print_comment_range(
             fmt,
             ~allow_breaks=false,
             ~none=space,
             ~lead=space,
             ~trail=space,
             name.loc,
             enclosing_end_location(loc),
           )
        ++ fmt.print_identifier(fmt, alias.txt)
      }
    )
  };
};

let print_toplevel_stmt = (fmt, stmt) => {
  group(
    concat_map(
      ~lead=_ => empty,
      ~sep=
        (prev, next) =>
          fmt.print_comment_range(
            fmt,
            ~none=hardline,
            ~lead=space,
            ~trail=hardline,
            prev.Asttypes.attr_loc,
            next.attr_loc,
          ),
      ~trail=
        prev =>
          fmt.print_comment_range(
            fmt,
            ~none=hardline,
            ~lead=space,
            ~trail=hardline,
            prev.Asttypes.attr_loc,
            stmt.ptop_core_loc,
          ),
      ~f=(~final, a) => fmt.print_attribute(fmt, a),
      stmt.ptop_attributes,
    ),
  )
  ++ group(
       switch (stmt.ptop_desc) {
       | PTopExpr(expr) => fmt.print_expression(fmt, expr)
       | PTopException(provide_flag, ex) =>
         (
           switch (provide_flag) {
           | Asttypes.NotProvided => empty
           | Asttypes.Abstract =>
             string("abstract")
             ++ fmt.print_comment_range(
                  fmt,
                  ~allow_breaks=false,
                  ~none=space,
                  ~lead=space,
                  ~trail=space,
                  enclosing_start_location(stmt.ptop_core_loc),
                  ex.ptyexn_loc,
                )
           | Asttypes.Provided =>
             string("provide")
             ++ fmt.print_comment_range(
                  fmt,
                  ~allow_breaks=false,
                  ~none=space,
                  ~lead=space,
                  ~trail=space,
                  enclosing_start_location(stmt.ptop_core_loc),
                  ex.ptyexn_loc,
                )
           }
         )
         ++ fmt.print_exception(fmt, ex)
       | PTopData(datas) =>
         group @@
         concat_map(
           ~lead=_ => empty,
           ~sep=
             ((_, _, prev), (_, _, next)) => {
               fmt.print_comment_range(
                 fmt,
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
                     ++ fmt.print_comment_range(
                          fmt,
                          ~none=space,
                          ~lead=space,
                          ~trail=space,
                          enclosing_start_location(decl_loc),
                          decl.pdata_loc,
                        )
                   | Asttypes.Provided =>
                     string("provide")
                     ++ fmt.print_comment_range(
                          fmt,
                          ~none=space,
                          ~lead=space,
                          ~trail=space,
                          enclosing_start_location(decl_loc),
                          decl.pdata_loc,
                        )
                   }
                 )
                 ++ fmt.print_data_declaration(fmt, decl),
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
                  fmt.print_comment_range(
                    fmt,
                    ~allow_breaks=false,
                    ~trail=space,
                    enclosing_start_location(stmt.ptop_core_loc),
                    next.pvb_loc,
                  ),
              ~sep=
                (prev, next) =>
                  fmt.print_comment_range(
                    fmt,
                    ~none=hardline,
                    ~lead=space,
                    ~trail=hardline,
                    prev.pvb_loc,
                    next.pvb_loc,
                  )
                  ++ string("and")
                  ++ space,
              ~trail=_ => empty,
              ~f=(~final, vb) => group(fmt.print_value_binding(fmt, vb)),
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
         ++ fmt.print_comment_range(
              fmt,
              ~allow_breaks=false,
              ~trail=space,
              enclosing_start_location(stmt.ptop_core_loc),
              prim_desc.pprim_loc,
            )
         ++ fmt.print_primitive_description(fmt, prim_desc)
       | PTopInclude(include_decl) =>
         fmt.print_include_declaration(fmt, include_decl)
       | PTopForeign(provide_flag, value_desc) =>
         (
           switch (provide_flag) {
           | NotProvided => empty
           | Abstract => string("abstract ")
           | Provided => string("provide ")
           }
         )
         ++ string("foreign wasm ")
         ++ fmt.print_comment_range(
              fmt,
              ~allow_breaks=false,
              ~trail=space,
              enclosing_start_location(stmt.ptop_core_loc),
              value_desc.pval_loc,
            )
         ++ fmt.print_value_description(fmt, value_desc)
       | PTopModule(provide_flag, module_decl) =>
         (
           switch (provide_flag) {
           | NotProvided => empty
           | Abstract => string("abstract ")
           | Provided => string("provide ")
           }
         )
         ++ fmt.print_comment_range(
              fmt,
              ~allow_breaks=false,
              ~trail=space,
              enclosing_start_location(stmt.ptop_core_loc),
              module_decl.pmod_loc,
            )
         ++ fmt.print_module_declaration(fmt, module_decl)
       | PTopProvide(provide_items) =>
         string("provide ")
         ++ braces(
              indent(
                concat_map(
                  ~lead=
                    next =>
                      fmt.print_comment_range(
                        fmt,
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
                      fmt.print_comment_range(
                        fmt,
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
                      fmt.print_comment_range(
                        fmt,
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
                        group(fmt.print_provide_item(fmt, p))
                        ++ trailing_comma;
                      } else {
                        group(fmt.print_provide_item(fmt, p) ++ comma);
                      },
                  provide_items,
                ),
              )
              ++ breakable_space,
            )
       },
     );
};

let print_comment_range =
    (
      fmt,
      ~none=empty,
      ~lead=empty,
      ~trail=empty,
      ~allow_breaks=true,
      ~block_start=false,
      ~block_end=false,
      prev: Location.t,
      next: Location.t,
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

  let between_loc: Location.t = {
    loc_start: prev.loc_end,
    loc_end: next.loc_start,
    loc_ghost: true,
  };

  let comments = Commenttree.query(fmt.comments, between_loc);
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
            | Shebang(_) when block_end => if_broken(empty, phantom_hardline)
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

let print_program = (fmt, parsed_program) => {
  let toplevel =
    switch (parsed_program.statements) {
    | [] =>
      fmt.print_comment_range(
        fmt,
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
            fmt.print_comment_range(
              fmt,
              ~none=hardline ++ hardline,
              ~lead=space,
              ~trail=hardline ++ hardline,
              parsed_program.module_name.loc,
              first.ptop_loc,
            ),
        ~sep=
          (prev, next) => {
            fmt.print_comment_range(
              fmt,
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
            fmt.print_comment_range(
              fmt,
              ~block_end=true,
              ~lead=space,
              last.ptop_loc,
              enclosing_end_location(parsed_program.prog_loc),
            )
            ++ hardline,
        ~f=
          (~final, s) =>
            if (has_disable_formatting_comment(fmt.comments, s.ptop_loc)) {
              fmt.print_original_code(fmt, s.ptop_loc);
            } else {
              fmt.print_toplevel_stmt(fmt, s);
            },
        parsed_program.statements,
      )
    };

  let attributes =
    switch (parsed_program.attributes) {
    | [] =>
      group(
        fmt.print_comment_range(
          fmt,
          enclosing_start_location(parsed_program.prog_loc),
          parsed_program.module_name.loc,
        ),
      )
    | _ =>
      group @@
      concat_map(
        ~lead=
          first =>
            fmt.print_comment_range(
              fmt,
              ~trail=hardline,
              enclosing_start_location(parsed_program.prog_loc),
              first.attr_loc,
            ),
        ~sep=
          (prev, next) =>
            fmt.print_comment_range(
              fmt,
              ~none=hardline,
              ~lead=space,
              ~trail=hardline,
              prev.Asttypes.attr_loc,
              next.attr_loc,
            ),
        ~trail=
          prev =>
            fmt.print_comment_range(
              fmt,
              ~none=hardline,
              ~lead=space,
              ~trail=hardline,
              prev.Asttypes.attr_loc,
              parsed_program.prog_core_loc,
            ),
        ~f=(~final, a) => fmt.print_attribute(fmt, a),
        parsed_program.attributes,
      )
    };

  attributes
  ++ string("module ")
  ++ string(parsed_program.module_name.txt)
  ++ toplevel;
};

// The default_formatter cannot look up original source code or comments.
// You must override `comments` and `source` fields on the record
let default_formatter: formatter = {
  comments: Commenttree.empty,
  source: [||],
  print_original_code,
  print_infix_prefix_op,
  print_constant,
  print_punnable_pattern,
  print_lambda_argument,
  print_pattern,
  print_ident_string,
  print_identifier,
  print_punnable_expression,
  print_grouped_access_expression,
  print_use_item,
  print_match_branch,
  print_attribute,
  print_application_argument,
  print_if,
  print_assignment,
  print_expression,
  print_value_binding,
  print_parsed_type_argument,
  print_type,
  print_label_declaration,
  print_constructor_arguments,
  print_exception,
  print_constructor_declaration,
  print_data_declaration,
  print_primitive_description,
  print_include_declaration,
  print_module_declaration,
  print_value_description,
  print_provide_item,
  print_toplevel_stmt,
  print_comment_range,
  print_program,
};

let format = (~write, ~source, ~eol, parsed_program: parsed_program) => {
  let comments = Commenttree.from_comments(parsed_program.comments);
  let formatter = {...default_formatter, comments, source};
  Engine.print(
    ~write,
    ~eol,
    ~line_width=80,
    formatter.print_program(formatter, parsed_program),
  );
};

let format_to_string = (~source, ~eol, parsed_program: parsed_program) => {
  let comments = Commenttree.from_comments(parsed_program.comments);
  let formatter = {...default_formatter, comments, source};
  Engine.to_string(
    ~eol,
    ~line_width=80,
    formatter.print_program(formatter, parsed_program),
  );
};
