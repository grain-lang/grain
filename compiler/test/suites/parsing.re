open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_tests.Test_utils;
open Grain_parsing;
open Grain_parsing.Ast_helper;
open Grain_parsing.Parsetree;

describe("parsing", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;
  let assertParse = makeParseRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertFileRun = makeFileRunner(test_or_skip);

  // operators
  assertFileRun(
    "custom_operator",
    "customOperator",
    "Ok(3)\nErr(\"Division by zero!\")\n",
  );
  let a =
    Expression.ident(
      ~loc=Location.dummy_loc,
      ~core_loc=Location.dummy_loc,
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("a"))),
    );
  let b =
    Expression.ident(
      ~loc=Location.dummy_loc,
      ~core_loc=Location.dummy_loc,
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("b"))),
    );
  let c =
    Expression.ident(
      ~loc=Location.dummy_loc,
      ~core_loc=Location.dummy_loc,
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("c"))),
    );
  let unlabled_expr = expr => {
    paa_label: Unlabeled,
    paa_expr: expr,
    paa_loc: Location.dummy_loc,
  };
  let testOp = op =>
    assertParse(
      op,
      "module Test; a " ++ op ++ " b",
      {
        attributes: [],
        module_name: Location.mknoloc("Test"),
        statements: [
          Toplevel.expr(
            ~loc=Location.dummy_loc,
            ~core_loc=Location.dummy_loc,
            Expression.apply(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              Expression.ident(
                ~loc=Location.dummy_loc,
                ~core_loc=Location.dummy_loc,
                Location.mknoloc(
                  Identifier.IdentName(Location.mknoloc(op)),
                ),
              ),
              [unlabled_expr(a), unlabled_expr(b)],
            ),
          ),
        ],
        comments: [],
        prog_loc: Location.dummy_loc,
        prog_core_loc: Location.dummy_loc,
      },
    );

  let ops = [
    // Smoketest of operators which should all work
    "||+",
    "||^",
    "&&*&^%",
    "|*",
    "^^^",
    "&-",
    "==!",
    "==$",
    "==*==",
    "!==^",
    "<<<<<",
    "<%>",
    "<=>",
    ">>>>",
    ">>>>>>>>",
    "><><><",
    "+==",
    "+!",
    "++!",
    "+-+",
    "**//**",
    "**",
    // verify that common fan favorites work
    "??",
    "???",
    "+.",
    "-.",
    "*.",
    "/.",
    ">>=",
    "|>",
    ">:",
    "%%",
    "===",
    "!==",
    "==?",
    "&?",
    "++",
    "--",
    "^*^",
    "^-^",
  ];
  List.iter(testOp, ops);

  // verify precedence is maintained
  assertParse(
    "custom_op_precedence_1",
    "module Test; a +++ b *** c",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          Expression.apply(
            ~loc=Location.dummy_loc,
            ~core_loc=Location.dummy_loc,
            Expression.ident(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("+++")),
              ),
            ),
            [
              unlabled_expr(a),
              unlabled_expr(
                Expression.apply(
                  ~loc=Location.dummy_loc,
                  ~core_loc=Location.dummy_loc,
                  Expression.ident(
                    ~loc=Location.dummy_loc,
                    ~core_loc=Location.dummy_loc,
                    Location.mknoloc(
                      Identifier.IdentName(Location.mknoloc("***")),
                    ),
                  ),
                  [unlabled_expr(b), unlabled_expr(c)],
                ),
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "custom_op_precedence_2",
    "module Test; a &&-- b &-- c",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          Expression.apply(
            ~loc=Location.dummy_loc,
            ~core_loc=Location.dummy_loc,
            Expression.ident(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("&&--")),
              ),
            ),
            [
              unlabled_expr(a),
              unlabled_expr(
                Expression.apply(
                  ~loc=Location.dummy_loc,
                  ~core_loc=Location.dummy_loc,
                  Expression.ident(
                    ~loc=Location.dummy_loc,
                    ~core_loc=Location.dummy_loc,
                    Location.mknoloc(
                      Identifier.IdentName(Location.mknoloc("&--")),
                    ),
                  ),
                  [unlabled_expr(b), unlabled_expr(c)],
                ),
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "custom_op_precedence_3",
    "module Test; a ||-- b |-- c",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          Expression.apply(
            ~loc=Location.dummy_loc,
            ~core_loc=Location.dummy_loc,
            Expression.ident(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("||--")),
              ),
            ),
            [
              unlabled_expr(a),
              unlabled_expr(
                Expression.apply(
                  ~loc=Location.dummy_loc,
                  ~core_loc=Location.dummy_loc,
                  Expression.ident(
                    ~loc=Location.dummy_loc,
                    ~core_loc=Location.dummy_loc,
                    Location.mknoloc(
                      Identifier.IdentName(Location.mknoloc("|--")),
                    ),
                  ),
                  [unlabled_expr(b), unlabled_expr(c)],
                ),
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "regression_issue_1473",
    "module Test; a << b >> c",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          Expression.apply(
            ~loc=Location.dummy_loc,
            ~core_loc=Location.dummy_loc,
            Expression.ident(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc(">>")),
              ),
            ),
            [
              unlabled_expr(
                Expression.apply(
                  ~loc=Location.dummy_loc,
                  ~core_loc=Location.dummy_loc,
                  Expression.ident(
                    ~loc=Location.dummy_loc,
                    ~core_loc=Location.dummy_loc,
                    Location.mknoloc(
                      Identifier.IdentName(Location.mknoloc("<<")),
                    ),
                  ),
                  [unlabled_expr(a), unlabled_expr(b)],
                ),
              ),
              unlabled_expr(c),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "regression_issue_1609",
    "module Test; return -1",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          Expression.return(
            ~loc=Location.dummy_loc,
            ~core_loc=Location.dummy_loc,
            Some(
              Expression.constant(
                ~loc=Location.dummy_loc,
                ~core_loc=Location.dummy_loc,
                PConstNumber(
                  PConstNumberInt({
                    txt: "-1",
                    loc:
                      mk_loc(
                        "regression_issue_1609",
                        (1, 20, 0),
                        (1, 22, 0),
                      ),
                  }),
                ),
              ),
            ),
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );

  // Whitespace tests

  // Reason does not support OCaml's Unicode escapes, which is why these are
  // UTF-8 byte sequences instead of pretty Unicode escapes

  assertParse(
    "whitespace_1",
    // In order,
    // HORIZONTAL TABULATION
    // VERTICAL TABULATION
    // SPACE
    // LEFT-TO-RIGHT MARK
    // RIGHT-TO-LEFT MARK
    // LINE FEED
    // FORM FEED
    // CARRIAGE RETURN
    // NEXT LINE
    // LINE SEPARATOR
    // PARAGRAPH SEPARATOR
    "
    module Test
    \x09
    \x0b
    \x20
    \xe2\x80\x8e
    \xe2\x80\x8f
    \x0a
    \x0c
    \x0d
    \xc2\x85
    \xe2\x80\xa8
    \xe2\x80\xa9
    ",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );

  assertCompileError(
    "invalid_whitespace_nbsp",
    "\xc2\xa0",
    "Grain lexer doesn't recognize this token",
  );
  assertCompileError(
    "invalid_whitespace_emspace",
    "\xe2\x80\x83",
    "Grain lexer doesn't recognize this token",
  );
  assertCompileError(
    "invalid_whitespace_hairspace",
    "\xe2\x80\x8a",
    "Grain lexer doesn't recognize this token",
  );
  assertCompileError(
    "invalid_whitespace_ideographicspace",
    "\xe3\x80\x80",
    "Grain lexer doesn't recognize this token",
  );

  assertParse(
    "end_of_statement_linefeed",
    "module Test; a\x0ab",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          a,
        ),
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          b,
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_formfeed",
    "module Test; a\x0cb",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          a,
        ),
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          b,
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_carriagereturn",
    "module Test; a\x0db",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          a,
        ),
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          b,
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_crlf",
    "module Test; a\x0d\x0ab",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          a,
        ),
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          b,
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_nextline",
    "module Test; a\xc2\x85b",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          a,
        ),
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          b,
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_lineseparator",
    "module Test; a\xe2\x80\xa8b",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          a,
        ),
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          b,
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_paragraphseparator",
    "module Test; a\xe2\x80\xa9b",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          a,
        ),
        Toplevel.expr(
          ~loc=Location.dummy_loc,
          ~core_loc=Location.dummy_loc,
          b,
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
});
