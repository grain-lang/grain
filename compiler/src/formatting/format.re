open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_diagnostics;

/**
  This takes a location and makes the loc_end the same as loc_start.
  Its main purpose is to find comments between the start of an enclosing location and the first item inside.
*/
let enclosing_start_location = (loc: Location.t) => {
  {...loc, loc_end: loc.loc_start};
};

/**
  This takes a location and makes the loc_start the same as loc_end.
  Its main purpose is to find comments between the end of an enclosing location and the last item inside.
*/
let enclosing_end_location = (loc: Location.t) => {
  {...loc, loc_start: loc.loc_end};
};

let is_same_op = (expr1: Parsetree.expression, expr2: Parsetree.expression) =>
  switch (expr1.pexp_desc, expr2.pexp_desc) {
  | (
      PExpId({txt: Identifier.IdentName({txt: op1})}),
      PExpId({txt: Identifier.IdentName({txt: op2})}),
    ) =>
    op1 == op2
  | _ => false
  };

let is_shift_or_concat_op = (expr: Parsetree.expression) =>
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) =>
    if (String.length(op) > 1) {
      switch (String.sub(op, 0, 2)) {
      | "<<"
      | ">>"
      | "++"
      | "||" => true
      | _ => false
      };
    } else {
      false;
    }
  | _ => false
  };

let is_logic_op = (expr: Parsetree.expression) =>
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) =>
    if (String.length(op) > 1) {
      switch (String.sub(op, 0, 2)) {
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
    }
  | _ => false
  };

let is_math_op = (expr: Parsetree.expression) =>
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

let precedence = (expr: Parsetree.expression) => {
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
  | _ => false
  | exception _ => false
  };
};

let is_infix = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) => infixop(op)
  | _ => false
  };
};

let is_prefix = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) =>
    switch (op.[0]) {
    | '!' => true
    | _ => false
    | exception _ => false
    }
  | _ => false
  };
};

let is_keyword = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: "assert"})})
  | PExpId({txt: Identifier.IdentName({txt: "throw"})})
  | PExpId({txt: Identifier.IdentName({txt: "fail"})}) => true
  | _ => false
  };
};

type compilation_error =
  | ParseError(exn)
  | InvalidCompilationState;

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

open Doc;

let comment_tree = ref(Commenttree.from_comments([]));

let needs_grouping =
    (~parent: Parsetree.expression, expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpIf(_) => true
  | PExpApp(fn1, _) when precedence(fn1) < precedence(parent) => true
  | PExpApp(fn1, _) =>
    if (is_infix(fn1)) {
      (!is_math_op(parent) && !is_logic_op(parent))
      && !is_same_op(fn1, parent);
    } else {
      false;
    }
  | PExpConstant(PConstNumber(PConstNumberRational(_, _))) =>
    op_precedence('/') < precedence(parent)
  | _ => false
  };
};

let rec print_constant = (constant: Parsetree.constant) => {
  // TODO: Our parsetree is bad and we don't have written values for constants
  switch (constant) {
  | PConstNumber(PConstNumberInt(value)) => string(value)
  | PConstNumber(PConstNumberFloat(value)) => string(value)
  | PConstNumber(PConstNumberRational(numerator, denominator)) =>
    string(numerator) ++ string("/") ++ string(denominator)
  | PConstInt8(value) => empty
  | PConstUint8(_, value) => empty
  | PConstInt16(value) => empty
  | PConstUint16(_, value) => empty
  | PConstInt32(value) => string(value) ++ string("l")
  | PConstUint32(_, value) => string(value) ++ string("ul")
  | PConstInt64(value) => string(value) ++ string("L")
  | PConstUint64(_, value) => string(value) ++ string("uL")
  | PConstFloat32(value) => string(value) ++ string("f")
  | PConstFloat64(value) => string(value) ++ string("d")
  | PConstWasmI32(value) => string(value) ++ string("n")
  | PConstWasmI64(value) => string(value) ++ string("N")
  | PConstWasmF32(value) => string(value) ++ string("w")
  | PConstWasmF64(value) => string(value) ++ string("W")
  | PConstBigInt(value) => string(value) ++ string("t")
  | PConstRational(numerator, denominator) => empty
  | PConstBool(value) => string(value ? "true" : "false")
  | PConstVoid => string("void")
  | PConstBytes(value) =>
    string("b") ++ string("\"") ++ string(value) ++ string("\"")
  | PConstString(value) => string("\"") ++ string(value) ++ string("\"")
  | PConstChar(value) => string("\'") ++ string(value) ++ string("\'")
  };
}
and print_punnable_pattern =
    (({txt: ident}, pat): (Location.loc(Identifier.t), Parsetree.pattern)) => {
  switch (pat.ppat_desc) {
  | PPatVar({txt: name}) when Identifier.string_of_ident(ident) == name =>
    string(name)
  | _ =>
    concat([print_identifier(ident), string(": "), print_pattern(pat)])
  };
}
and print_lambda_argument = (arg: Parsetree.lambda_argument) => {
  concat([
    // TODO: Figure out how to print these
    // switch (arg.pla_label) {
    // | Unlabeled => string("_") // TODO: is this correct?
    // | Labeled({txt: label})
    // | Default({txt: label}) => string(label)
    // },
    // string(":"),
    // space,
    print_pattern(arg.pla_pattern),
    switch (arg.pla_default) {
    | Some(expr) => concat([string("="), print_expression(expr)])
    | None => empty
    },
  ]);
}
and print_pattern = ({ppat_desc}: Parsetree.pattern) => {
  switch (ppat_desc) {
  | PPatAny => string("_")
  | PPatVar({txt: name}) when infixop(name) => parens(string(name))
  | PPatVar({txt: name}) => string(name)
  | PPatAlias(pat, {txt: alias}) =>
    print_pattern(pat) ++ string(" as ") ++ string(alias)
  | PPatOr(lhs, rhs) =>
    concat([
      print_pattern(lhs),
      string(" |"),
      breakable_space,
      print_pattern(rhs),
    ])
  | PPatConstruct({txt: ident}, cstr_pat) =>
    concat([
      print_identifier(ident),
      switch (cstr_pat) {
      | PPatConstrRecord([], closed_flag) =>
        braces(
          switch (closed_flag) {
          | Open => string("_")
          | Closed => empty
          },
        )
      | PPatConstrRecord(pats, closed_flag) =>
        braces(
          concat_map(
            ~sep=
              ((_, prev_pat), (_, next_pat)) => {
                concat([
                  comma_breakable_space,
                  print_comments(
                    prev_pat.Parsetree.ppat_loc,
                    next_pat.ppat_loc,
                  ),
                ])
              },
            ~trail=
              last =>
                switch (closed_flag) {
                | Open => comma_breakable_space ++ string("_")
                | Closed => empty
                },
            ~f=print_punnable_pattern,
            pats,
          ),
        )
      | PPatConstrSingleton => empty
      | PPatConstrTuple(pats) =>
        parens(
          concat_map(
            ~sep=(prev, next) => comma_breakable_space,
            ~f=print_pattern,
            pats,
          ),
        )
      },
    ])
  | PPatConstraint(pat, typ) =>
    concat([print_pattern(pat), string(": "), print_type(typ)])
  | PPatConstant(constant) => print_constant(constant)
  | PPatRecord(pats, closed_flag) =>
    braces(
      concat_map(
        ~sep=(prev, next) => comma_breakable_space,
        ~trail=
          last =>
            switch (closed_flag) {
            | Open when pats == [] => string("_")
            | Open => comma_breakable_space ++ string("_")
            | Closed => empty
            },
        ~f=print_punnable_pattern,
        pats,
      ),
    )
  | PPatArray(pats) =>
    array_brakets(
      concat_map(
        ~sep=(prev, next) => comma_breakable_space,
        ~f=print_pattern,
        pats,
      ),
    )
  | PPatList(pats) =>
    list_brakets(
      concat_map(
        ~sep=(prev, next) => comma_breakable_space,
        ~trail=last => ifBreaks(","),
        ~f=
          (item: Parsetree.list_item(Parsetree.pattern)) => {
            switch (item) {
            | ListItem(pat) => print_pattern(pat)
            | ListSpread(pat, _) => string("...") ++ print_pattern(pat)
            }
          },
        pats,
      ),
    )
  | PPatTuple(pats) =>
    parens(
      concat_map(
        ~sep=(prev, next) => comma_breakable_space,
        ~f=print_pattern,
        pats,
      ),
    )
  };
}
and print_identifier = (ident: Identifier.t) => {
  let name = Identifier.string_of_ident(ident);
  string(name);
}
and print_punnable_expression =
    (
      ({txt: ident}, expr): (
        Location.loc(Identifier.t),
        Parsetree.expression,
      ),
    ) => {
  switch (expr.pexp_desc) {
  | PExpId({txt: name}) when Identifier.equal(ident, name) =>
    print_identifier(name)
  | _ =>
    concat([print_identifier(ident), string(": "), print_expression(expr)])
  };
}
and print_grouped_access_expression = (expr: Parsetree.expression) =>
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
  | PExpApp(func, _) when is_infix(func) => parens(print_expression(expr))
  | _ => parens(print_expression(expr))
  }
and print_use_item = (use_item: Parsetree.use_item) => {
  switch (use_item) {
  | PUseType({name, alias}) =>
    concat([
      string("type "),
      print_identifier(name.txt),
      switch (alias) {
      | None => empty
      | Some({txt: alias}) => string(" as ") ++ print_identifier(alias)
      },
    ])
  | PUseModule({name, alias}) =>
    concat([
      print_identifier(name.txt),
      switch (alias) {
      | None => empty
      | Some({txt: alias}) => string(" as ") ++ print_identifier(alias)
      },
    ])
  | PUseValue({name, alias}) =>
    concat([
      print_identifier(name.txt),
      switch (alias) {
      | None => empty
      | Some({txt: alias}) => string(" as ") ++ print_identifier(alias)
      },
    ])
  };
}
and print_match_branch =
    ({pmb_pat, pmb_body, pmb_guard}: Parsetree.match_branch) => {
  concat([
    group_all(print_pattern(pmb_pat)),
    switch (pmb_guard) {
    | None => empty
    | Some(guard) => string(" when ") ++ print_expression(guard)
    },
    string(" =>"),
    breakable_space,
    print_expression(pmb_body),
  ]);
}
and print_attribute = (attr: Asttypes.attribute) => {
  switch (attr) {
  | ({txt: attr_name}, []) => concat([string("@"), string(attr_name)])
  | ({txt: attr_name}, attr_args) =>
    concat([
      string("@"),
      string(attr_name),
      parens(
        concat_map(
          ~sep=(prev, next) => comma_breakable_space,
          ~f=attr_arg => string(attr_arg.Location.txt),
          attr_args,
        ),
      ),
    ])
  };
}
and print_application_argument = (arg: Parsetree.application_argument) => {
  concat([
    switch (arg.paa_label) {
    | Unlabeled => empty
    | Labeled({txt: label})
    | Default({txt: label}) => concat([string(label), string("=")])
    },
    print_expression(arg.paa_expr),
  ]);
}
and print_expression = (expr: Parsetree.expression) => {
  concat([
    // print_comments(expr.pexp_loc),
    concat_map(
      ~sep=(prev, next) => hardline,
      ~trail=last => hardline,
      ~f=print_attribute,
      expr.pexp_attributes,
    ),
    switch (expr.pexp_desc) {
    | PExpId({txt: ident}) => print_identifier(ident)
    | PExpConstant(constant) => print_constant(constant)
    | PExpConstruct({txt: ident}, cstr_expr) =>
      concat([
        print_identifier(ident),
        switch (cstr_expr) {
        | PExpConstrSingleton => empty
        | PExpConstrTuple(exprs) =>
          parens(
            concat_map(
              ~sep=(prev, next) => comma_breakable_space,
              ~f=print_expression,
              exprs,
            ),
          )
        | PExpConstrRecord(exprs) =>
          braces(
            concat_map(
              ~sep=(prev, next) => comma_breakable_space,
              ~f=print_punnable_expression,
              exprs,
            ),
          )
        },
      ])
    | PExpBlock(exprs) =>
      block_braces(
        concat_map(
          ~lead=
            first =>
              print_comments(
                enclosing_start_location(expr.pexp_loc),
                first.pexp_loc,
              ),
          ~sep=
            (prev: Parsetree.expression, next: Parsetree.expression) =>
              switch (print_comments(prev.pexp_loc, next.pexp_loc)) {
              | Empty =>
                switch (
                  next.pexp_loc.loc_start.pos_lnum
                  - prev.pexp_loc.loc_end.pos_lnum
                ) {
                | 0 => string(";") ++ breakable_space
                | 1 => hardline
                | _ => hardline ++ hardline
                }
              | docs => docs
              },
          ~trail=
            last =>
              concat([
                breakable_space,
                print_comments(
                  last.pexp_loc,
                  enclosing_end_location(expr.pexp_loc),
                ),
              ]),
          ~f=print_expression,
          exprs,
        ),
      )
    | PExpLet(rec_flag, mut_flag, vbs) =>
      concat([
        string("let "),
        switch (rec_flag) {
        | Nonrecursive => empty
        | Recursive => string("rec ")
        },
        switch (mut_flag) {
        | Immutable => empty
        | Mutable => string("mut ")
        },
        concat_map(
          ~sep=(prev, next) => comma_breakable_space,
          ~f=print_value_binding,
          vbs,
        ),
      ])
    | PExpApp(fn, [lhs, rhs]) when is_infix(fn) =>
      concat([
        print_application_argument(lhs)
        |> (
          // TODO: fix this
          if (needs_grouping(~parent=fn, lhs.paa_expr)) {
            parens;
          } else {
            Fun.id;
          }
        ),
        space,
        print_expression(fn),
        breakable_space,
        print_application_argument(rhs)
        |> (
          // TODO: fix this
          if (needs_grouping(~parent=fn, rhs.paa_expr)) {
            parens;
          } else {
            Fun.id;
          }
        ),
      ])
    | PExpApp(fn, [rhs]) when is_keyword(fn) =>
      concat([print_expression(fn), space, print_expression(rhs.paa_expr)])
    | PExpApp(fn, exprs) =>
      group(
        concat([
          print_grouped_access_expression(fn),
          parens(
            concat_map(
              ~sep=(prev, next) => comma_breakable_space,
              ~f=print_application_argument,
              exprs,
            ),
          ),
        ]),
      )
    | PExpLambda([single_param], body) =>
      concat([
        print_lambda_argument(single_param),
        string(" =>"),
        nest_all(
          (
            // TODO: Is there a better way to do this?
            switch (body.pexp_desc) {
            | PExpBlock(_) => space
            | _ => breakable_space
            }
          )
          ++ print_expression(body),
        ),
      ])
    | PExpLambda(params, body) =>
      concat([
        parens(
          concat_map(
            ~sep=(prev, next) => comma_breakable_space,
            ~f=print_lambda_argument,
            params,
          ),
        ),
        string(" =>"),
        nest_all(
          (
            // TODO: Is there a better way to do this?
            switch (body.pexp_desc) {
            | PExpBlock(_) => space
            | _ => breakable_space
            }
          )
          ++ print_expression(body),
        ),
      ])
    | PExpContinue => string("continue")
    | PExpBreak => string("break")
    | PExpTuple(exprs) =>
      parens(
        concat_map(
          ~sep=(prev, next) => comma_breakable_space,
          ~f=print_expression,
          exprs,
        ),
      )
    | PExpArray(exprs) =>
      array_brakets(
        concat_map(
          ~sep=(prev, next) => comma_breakable_space,
          ~f=print_expression,
          exprs,
        ),
      )
    | PExpList(items) =>
      list_brakets(
        concat_map(
          ~sep=(prev, next) => comma_breakable_space,
          ~f=
            (item: Parsetree.list_item(Parsetree.expression)) => {
              switch (item) {
              | ListItem(expr) => print_expression(expr)
              | ListSpread(expr, _) =>
                string("...") ++ print_expression(expr)
              }
            },
          items,
        ),
      )
    | PExpArrayGet(arr, elem) =>
      concat([
        print_grouped_access_expression(arr),
        brackets(print_expression(elem)),
      ])
    | PExpArraySet(arr, elem, new_value) =>
      concat([
        print_grouped_access_expression(arr),
        brackets(print_expression(elem)),
        string(" = "),
        print_expression(new_value),
      ])
    | PExpRecord(base, labels) =>
      braces(
        concat([
          switch (base) {
          | None => empty
          | Some(expr) => string("...") ++ print_expression(expr)
          },
          concat_map(
            ~sep=(prev, next) => comma_breakable_space,
            ~f=print_punnable_expression,
            labels,
          ),
        ]),
      )
    | PExpRecordGet(record, elem) =>
      concat([
        print_grouped_access_expression(record),
        string("."),
        print_identifier(elem.txt),
      ])
    | PExpRecordSet(record, elem, new_value) =>
      concat([
        print_grouped_access_expression(record),
        string("."),
        print_identifier(elem.txt),
        string(" = "),
        print_expression(new_value),
      ])
    // Ignore the prims because they shouldn't be in the parsetree
    | PExpPrim0(_) => failwith("Impossible: PExpPrim0 in parsetree")
    | PExpPrim1(_) => failwith("Impossible: PExpPrim1 in parsetree")
    | PExpPrim2(_) => failwith("Impossible: PExpPrim2 in parsetree")
    | PExpPrimN(_) => failwith("Impossible: PExpPrimN in parsetree")
    | PExpAssign(binding, new_value) =>
      concat([
        print_expression(binding),
        switch (new_value.pexp_desc) {
        | PExpApp(
            {pexp_desc: PExpId({txt: Identifier.IdentName({txt: op})})},
            [lhs, rhs],
          ) =>
          switch (op) {
          | "+" => string(" += ") ++ print_application_argument(rhs)
          | "-" => string(" -= ") ++ print_application_argument(rhs)
          | "*" => string(" *= ") ++ print_application_argument(rhs)
          | "/" => string(" /= ") ++ print_application_argument(rhs)
          | "%" => string(" %= ") ++ print_application_argument(rhs)
          | _ => string(" = ") ++ print_expression(new_value)
          }
        | _ => string(" = ") ++ print_expression(new_value)
        },
      ])
    | PExpBoxAssign(binding, new_value) =>
      concat([
        print_expression(binding),
        string(" := "),
        print_expression(new_value),
      ])
    | PExpReturn(expr) =>
      concat([
        string("return"),
        switch (expr) {
        | None => empty
        | Some(expr) => breakable_space ++ print_expression(expr)
        },
      ])
    | PExpUse(ident, use_items) =>
      concat([
        string("from "),
        print_identifier(ident.txt),
        string(" use "),
        switch (use_items) {
        | PUseAll => string("*")
        | PUseItems(items) =>
          braces(
            concat_map(
              ~sep=(prev, next) => comma_breakable_space,
              ~f=print_use_item,
              items,
            ),
          )
        },
      ])
    | PExpIf(cond, if_true, if_false) =>
      concat([
        string("if "),
        parens(print_expression(cond)),
        space,
        print_expression(if_true),
        switch (if_false) {
        | None => empty
        | Some(if_false) => string(" else ") ++ print_expression(if_false)
        },
      ])
    | PExpWhile(cond, body) =>
      concat([
        string("while "),
        parens(print_expression(cond)),
        breakable_space,
        print_expression(body),
      ])
    | PExpFor(init, cond, inc, body) =>
      concat([
        string("for "),
        parens(
          concat([
            switch (init) {
            | None => empty
            | Some(init) => print_expression(init)
            },
            string("; "),
            switch (cond) {
            | None => empty
            | Some(cond) => print_expression(cond)
            },
            string("; "),
            switch (inc) {
            | None => empty
            | Some(inc) => print_expression(inc)
            },
            braces(print_expression(body)),
          ]),
        ),
      ])
    | PExpMatch(value, branches) =>
      concat([
        string("match "),
        parens(print_expression(value)),
        space,
        block_braces(
          concat_map(
            ~sep=(prev, next) => comma_hardline,
            ~trail=last => comma,
            ~f=print_match_branch,
            branches,
          ),
        ),
      ])
    | PExpConstraint(expr, typ) =>
      concat([print_expression(expr), string(": "), print_type(typ)])
    },
  ]);
}
and print_value_binding = ({pvb_pat, pvb_expr}: Parsetree.value_binding) => {
  concat([
    print_pattern(pvb_pat),
    string(" = "),
    print_expression(pvb_expr)
    |> (is_infix(pvb_expr) || is_prefix(pvb_expr) ? parens : Fun.id),
  ]);
}
and print_parsed_type_argument = (arg: Parsetree.parsed_type_argument) => {
  concat([
    switch (arg.ptyp_arg_label) {
    | Unlabeled => string("_") // TODO: is this correct?
    | Labeled({txt: label})
    | Default({txt: label}) => string(label)
    },
    string(":"),
    space,
    print_type(arg.ptyp_arg_type),
  ]);
}
and print_type = ({ptyp_desc}: Parsetree.parsed_type) => {
  switch (ptyp_desc) {
  | PTyAny => string("_")
  | PTyVar(name) => string(name)
  | PTyConstr({txt: ident}, params) =>
    let name = Identifier.string_of_ident(ident);
    concat([
      string(name),
      switch (params) {
      | [] => empty
      | typs =>
        angle_brakets(
          concat_map(
            ~sep=(prev, next) => comma_breakable_space,
            ~f=print_type,
            typs,
          ),
        )
      },
    ]);
  | PTyTuple(typs) =>
    parens(
      concat_map(
        ~sep=(prev, next) => comma_breakable_space,
        ~f=print_type,
        typs,
      ),
    )
  | PTyArrow(params, return) =>
    concat([
      parens(
        concat_map(
          ~sep=(prev, next) => comma_breakable_space,
          ~f=print_parsed_type_argument,
          params,
        ),
      ),
      string(" -> "),
      print_type(return),
    ])
  | PTyPoly(_) => failwith("Impossible: PTyPoly in the parsetree")
  };
}
and print_label_declaration =
    ({pld_name, pld_type, pld_mutable}: Parsetree.label_declaration) => {
  concat([
    switch (pld_mutable) {
    | Mutable => string("mut ")
    | Immutable => empty
    },
    print_identifier(pld_name.txt),
    string(": "),
    print_type(pld_type),
  ]);
}
and print_constructor_arguments = (args: Parsetree.constructor_arguments) => {
  switch (args) {
  | PConstrTuple({txt: typs, loc}) =>
    parens(
      concat_map(
        ~lead=
          first =>
            switch (
              print_comments(enclosing_start_location(loc), first.ptyp_loc)
            ) {
            | Empty => empty
            | comments => concat([breakable_space, comments, breakable_space])
            },
        ~sep=
          (prev: Parsetree.parsed_type, next: Parsetree.parsed_type) =>
            concat([
              comma_breakable_space,
              print_comments(prev.ptyp_loc, next.ptyp_loc),
              breakable_space,
            ]),
        ~trail=
          last =>
            concat([
              switch (
                print_comments(last.ptyp_loc, enclosing_end_location(loc))
              ) {
              | Empty => empty
              | comments =>
                concat([breakable_space, comments, breakable_space])
              },
            ]),
        ~f=print_type,
        typs,
      ),
    )
  | PConstrRecord({txt: labels, loc}) =>
    braces(
      concat_map(
        ~lead=
          first =>
            concat([
              print_comments(enclosing_start_location(loc), first.pld_loc),
              breakable_space,
            ]),
        ~sep=
          (
            prev: Parsetree.label_declaration,
            next: Parsetree.label_declaration,
          ) =>
            concat([
              comma_breakable_space,
              print_comments(prev.pld_loc, next.pld_loc),
              breakable_space,
            ]),
        ~trail=
          last =>
            concat([
              comma_breakable_space,
              print_comments(last.pld_loc, enclosing_end_location(loc)),
            ]),
        ~f=print_label_declaration,
        labels,
      ),
    )
  | PConstrSingleton => empty
  };
}
and print_exception = ({ptyexn_constructor}: Parsetree.type_exception) => {
  concat([
    string("exception "),
    string(ptyexn_constructor.pext_name.txt),
    switch (ptyexn_constructor.pext_kind) {
    | PExtDecl(args) => print_constructor_arguments(args)
    | PExtRebind(_) => empty
    },
  ]);
}
and print_constructor_declaration =
    ({pcd_name, pcd_args}: Parsetree.constructor_declaration) => {
  concat([
    string(pcd_name.txt),
    switch (pcd_args) {
    | PConstrTuple({loc})
    | PConstrRecord({loc}) =>
      switch (print_comments(pcd_name.loc, loc)) {
      | SmartPrint.Empty => empty
      | comments => concat([space, comments, space])
      }
    | PConstrSingleton => empty
    },
    print_constructor_arguments(pcd_args),
  ]);
}
and print_data_declaration = (decl: Parsetree.data_declaration) => {
  switch (decl) {
  | {pdata_name, pdata_params, pdata_manifest, pdata_kind: PDataAbstract} =>
    concat([
      string("type "),
      string(pdata_name.txt),
      switch (pdata_params) {
      | [] => empty
      | typs =>
        angle_brakets(
          concat_map(
            ~sep=(prev, next) => comma_breakable_space,
            ~f=print_type,
            pdata_params,
          ),
        )
      },
      switch (pdata_manifest) {
      | None => empty
      | Some(typ) => string(" = ") ++ print_type(typ)
      },
    ])
  | {
      pdata_name,
      pdata_params,
      pdata_kind: PDataVariant(cstr_decls),
      pdata_loc,
    } =>
    concat([
      string("enum "),
      string(pdata_name.txt),
      switch (pdata_params) {
      | [] => empty
      | typs =>
        angle_brakets(
          concat_map(
            ~sep=(prev, next) => comma_breakable_space,
            ~f=print_type,
            pdata_params,
          ),
        )
      },
      space,
      braces(
        concat_map(
          ~sep=
            (
              prev: Parsetree.constructor_declaration,
              next: Parsetree.constructor_declaration,
            ) =>
              concat([
                comma_breakable_space,
                print_comments(prev.pcd_loc, next.pcd_loc),
                breakable_space,
              ]),
          ~trail=
            last =>
              concat([
                // TODO: get ifBreaks working in all situations
                ifBreaks(","),
                breakable_space,
                print_comments(
                  last.pcd_loc,
                  enclosing_end_location(pdata_loc),
                ),
                breakable_space,
              ]),
          ~f=print_constructor_declaration,
          cstr_decls,
        ),
      ),
    ])
  | {pdata_name, pdata_params, pdata_kind: PDataRecord(labels)} =>
    concat([
      string("record "),
      string(pdata_name.txt),
      switch (pdata_params) {
      | [] => empty
      | typs =>
        angle_brakets(
          concat_map(
            ~sep=(prev, next) => comma_breakable_space,
            ~f=print_type,
            pdata_params,
          ),
        )
      },
      block_braces(
        concat_map(
          ~sep=(prev, next) => comma_hardline,
          ~f=print_label_declaration,
          labels,
        ),
      ),
    ])
  };
}
and print_primitive_description =
    ({pprim_ident, pprim_name}: Parsetree.primitive_description) => {
  concat([
    string("primitive "),
    string(pprim_ident.txt),
    string(" = "),
    double_quotes(string(pprim_name.txt)),
  ]);
}
and print_include_declaration =
    ({pinc_path, pinc_alias}: Parsetree.include_declaration) => {
  concat([
    string("include "),
    double_quotes(string(pinc_path.txt)),
    switch (pinc_alias) {
    | None => empty
    | Some({txt: alias}) => string(" as ") ++ string(alias)
    },
  ]);
}
and print_module_declaration =
    ({pmod_name, pmod_stmts}: Parsetree.module_declaration) => {
  concat([
    string("module "),
    string(pmod_name.txt),
    block_braces(
      concat_map(
        ~sep=(prev, next) => hardline ++ hardline,
        ~f=print_toplevel_stmt,
        pmod_stmts,
      ),
    ),
  ]);
}
and print_value_description =
    (
      {pval_mod, pval_name, pval_name_alias, pval_type}: Parsetree.value_description,
    ) => {
  concat([
    string(pval_name.txt),
    string(": "),
    print_type(pval_type),
    switch (pval_name_alias) {
    | None => empty
    | Some(alias) => string(" as ") ++ string(alias.txt)
    },
    string(" from "),
    string(pval_mod.txt),
  ]);
}
and print_provide_item = (provide_item: Parsetree.provide_item) => {
  switch (provide_item) {
  | PProvideType({name, alias}) =>
    concat([
      string("type "),
      print_identifier(name.txt),
      switch (alias) {
      | None => empty
      | Some(alias) => print_identifier(alias.txt)
      },
    ])
  | PProvideModule({name, alias}) =>
    concat([
      print_identifier(name.txt),
      switch (alias) {
      | None => empty
      | Some(alias) => print_identifier(alias.txt)
      },
    ])
  | PProvideValue({name, alias}) =>
    concat([
      print_identifier(name.txt),
      switch (alias) {
      | None => empty
      | Some(alias) => print_identifier(alias.txt)
      },
    ])
  };
}
and print_toplevel_stmt = (stmt: Parsetree.toplevel_stmt) => {
  concat([
    concat_map(
      ~sep=(prev, next) => hardline,
      ~trail=last => hardline,
      ~f=print_attribute,
      stmt.ptop_attributes,
    ),
    switch (stmt.ptop_desc) {
    | PTopExpr(expr) => group(print_expression(expr))
    | PTopException(provide_flag, ex) => print_exception(ex)
    | PTopData(datas) =>
      concat_map(
        ~sep=
          ((_, prev), (_, next)) => {
            concat([
              switch (
                next.Parsetree.pdata_loc.loc_start.pos_lnum
                - prev.pdata_loc.loc_end.pos_lnum
              ) {
              | 0 => comma ++ space
              | _ => comma ++ hardline
              },
              print_comments(prev.Parsetree.pdata_loc, next.pdata_loc),
            ])
          },
        ~f=
          ((provide_flag, decl)) =>
            concat([
              switch (provide_flag) {
              | Asttypes.NotProvided => empty
              | Asttypes.Abstract => string("abstract ")
              | Asttypes.Provided => string("provide ")
              },
              print_data_declaration(decl),
            ]),
        datas,
      )
    | PTopLet(provide_flag, rec_flag, mut_flag, vbs) =>
      concat([
        switch (provide_flag) {
        | NotProvided => empty
        | Abstract => string("abstract ")
        | Provided => string("provide ")
        },
        string("let "),
        switch (rec_flag) {
        | Nonrecursive => empty
        | Recursive => string("rec ")
        },
        switch (mut_flag) {
        | Immutable => empty
        | Mutable => string("mut ")
        },
        nest(
          concat_map(
            ~sep=
              (prev: Parsetree.value_binding, next: Parsetree.value_binding) =>
                concat([
                  comma,
                  breakable_space,
                  print_comments(prev.pvb_loc, next.pvb_loc),
                ]),
            ~f=print_value_binding,
            vbs,
          ),
        ),
      ])
    | PTopPrimitive(provide_flag, prim_desc) =>
      concat([
        switch (provide_flag) {
        | NotProvided => empty
        | Abstract => string("abstract ")
        | Provided => string("provide ")
        },
        print_primitive_description(prim_desc),
      ])
    | PTopInclude(include_decl) => print_include_declaration(include_decl)
    | PTopForeign(provide_flag, value_desc) =>
      concat([
        switch (provide_flag) {
        | NotProvided => empty
        | Abstract => string("abstract ")
        | Provided => string("provide ")
        },
        string("foreign wasm "),
        print_value_description(value_desc),
      ])
    | PTopModule(provide_flag, module_decl) =>
      concat([
        switch (provide_flag) {
        | NotProvided => empty
        | Abstract => string("abstract ")
        | Provided => string("provide ")
        },
        print_module_declaration(module_decl),
      ])
    | PTopProvide(provide_items) =>
      concat([
        string("provide "),
        braces(
          concat_map(
            ~sep=(prev, next) => comma_breakable_space,
            ~f=print_provide_item,
            provide_items,
          ),
        ),
      ])
    },
  ]);
}
and print_comments = (prev: Location.t, next: Location.t) => {
  // prerr_endline(
  //   String.cat(
  //     "prev loc",
  //     Location.to_yojson(prev) |> Yojson.Safe.to_string,
  //   ),
  // );
  // prerr_endline(
  //   String.cat(
  //     "next loc",
  //     Location.to_yojson(next) |> Yojson.Safe.to_string,
  //   ),
  // );
  let between_loc =
    Location.{
      loc_start: prev.loc_end,
      loc_end: next.loc_start,
      loc_ghost: true,
    };

  let print_comment = (cmt: Parsetree.comment) => {
    switch (cmt) {
    | Doc({cmt_source})
    | Block({cmt_source})
    | Line({cmt_source})
    | Shebang({cmt_source}) => string(cmt_source)
    };
  };

  // let print_leading = (cmt: Parsetree.comment) => {
  //   switch (cmt) {
  //   | Doc({cmt_loc})
  //   | Block({cmt_loc})
  //   | Line({cmt_loc})
  //   | Shebang({cmt_loc}) =>
  //     switch (prev.loc_end.pos_lnum - cmt_loc.loc_start.pos_lnum) {
  //     // | 0 => breakable_space
  //     | _ => empty
  //     }
  //   };
  // };

  let print_trailing = (cmt: Parsetree.comment) => {
    switch (cmt) {
    | Block({cmt_loc}) =>
      switch (next.loc_start.pos_lnum - cmt_loc.loc_end.pos_lnum) {
      // | 0 => breakable_space
      | 0 => empty
      | 1 => hardline
      | _ => hardline ++ hardline
      }
    | Doc({cmt_loc}) => hardline
    | Line({cmt_loc})
    | Shebang({cmt_loc}) =>
      switch (next.loc_start.pos_lnum - cmt_loc.loc_end.pos_lnum) {
      | 0 => hardline
      | 1 => hardline
      | _ => hardline ++ hardline
      }
    };
  };

  switch (Commenttree.query(comment_tree^, between_loc)) {
  | [] => empty
  | comments =>
    concat_map(
      // ~lead=print_leadsing,
      ~sep=
        (prev_cmt: Parsetree.comment, next_cmt: Parsetree.comment) => {
          switch (prev_cmt, next_cmt) {
          | (Block(prev_cmt), Doc(next_cmt))
          | (Block(prev_cmt), Block(next_cmt))
          | (Block(prev_cmt), Line(next_cmt))
          | (Block(prev_cmt), Shebang(next_cmt))
          | (Line(prev_cmt), Doc(next_cmt))
          | (Line(prev_cmt), Block(next_cmt))
          | (Line(prev_cmt), Line(next_cmt))
          | (Line(prev_cmt), Shebang(next_cmt))
          | (Doc(prev_cmt), Doc(next_cmt))
          | (Doc(prev_cmt), Block(next_cmt))
          | (Doc(prev_cmt), Line(next_cmt))
          | (Doc(prev_cmt), Shebang(next_cmt))
          | (Shebang(prev_cmt), Doc(next_cmt))
          | (Shebang(prev_cmt), Block(next_cmt))
          | (Shebang(prev_cmt), Line(next_cmt))
          | (Shebang(prev_cmt), Shebang(next_cmt)) =>
            switch (
              next_cmt.cmt_loc.loc_start.pos_lnum
              - prev_cmt.cmt_loc.loc_end.pos_lnum
            ) {
            | 0 => breakable_space
            | 1 => hardline
            | _ => hardline ++ hardline
            }
          }
        },
      ~trail=print_trailing,
      ~f=print_comment,
      comments,
    )
  };
};

let format_ast =
    (
      ~original_source: array(string),
      ~eol: Fs_access.eol,
      parsed_program: Parsetree.parsed_program,
    ) => {
  comment_tree := Commenttree.from_comments(parsed_program.comments);

  let final_doc =
    concat([
      print_comments(
        {
          loc_start: parsed_program.prog_loc.loc_start,
          loc_end: parsed_program.prog_loc.loc_start,
          loc_ghost: true,
        },
        parsed_program.module_name.loc,
      ),
      string("module "),
      string(parsed_program.module_name.txt),
      hardline,
      hardline,
      concat_map(
        ~lead=
          first =>
            print_comments(parsed_program.module_name.loc, first.ptop_loc),
        ~sep=
          (prev: Parsetree.toplevel_stmt, next: Parsetree.toplevel_stmt) => {
            concat([
              switch (
                next.ptop_loc.loc_start.pos_lnum
                - prev.ptop_loc.loc_end.pos_lnum
              ) {
              | 0 => space
              | 1 => hardline
              | _ => hardline ++ hardline
              },
              print_comments(prev.ptop_loc, next.ptop_loc),
            ])
          },
        ~trail=
          last =>
            switch (
              // TODO: Not sure I like this type of switch
              print_comments(
                last.ptop_loc,
                {
                  loc_start: parsed_program.prog_loc.loc_end,
                  loc_end: parsed_program.prog_loc.loc_end,
                  loc_ghost: true,
                },
              )
            ) {
            | Empty => hardline
            | comments => concat([hardline, hardline, comments])
            },
        ~f=print_toplevel_stmt,
        parsed_program.statements,
      ),
    ]);

  let newline =
    switch (eol) {
    | CRLF => "\r\n"
    | LF => "\n"
    };
  to_string(~width=80, ~tab=2, ~newline, final_doc);
};
