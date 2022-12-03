open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_parsing;
open Grain_parsing.Ast_helper;

describe("parsing", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;
  let assertParse = makeParseRunner(test);
  let assertFileRun = makeFileRunner(test_or_skip);

  // operators
  assertFileRun(
    "custom_operator",
    "customOperator",
    "Ok(3)\nErr(\"Division by zero!\")\n",
  );
  let a =
    Exp.ident(
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("a"))),
    );
  let b =
    Exp.ident(
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("b"))),
    );
  let c =
    Exp.ident(
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("c"))),
    );
  let testOp = op =>
    assertParse(
      op,
      "a " ++ op ++ " b",
      {
        statements: [
          Top.expr(
            Exp.apply(
              Exp.ident(
                Location.mknoloc(
                  Identifier.IdentName(Location.mknoloc(op)),
                ),
              ),
              [a, b],
            ),
          ),
        ],
        comments: [],
        prog_loc: Location.dummy_loc,
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
    "a +++ b *** c",
    {
      statements: [
        Top.expr(
          Exp.apply(
            Exp.ident(
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("+++")),
              ),
            ),
            [
              a,
              Exp.apply(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("***")),
                  ),
                ),
                [b, c],
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "custom_op_precedence_2",
    "a &&-- b &-- c",
    {
      statements: [
        Top.expr(
          Exp.apply(
            Exp.ident(
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("&&--")),
              ),
            ),
            [
              a,
              Exp.apply(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("&--")),
                  ),
                ),
                [b, c],
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "custom_op_precedence_3",
    "a ||-- b |-- c",
    {
      statements: [
        Top.expr(
          Exp.apply(
            Exp.ident(
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("||--")),
              ),
            ),
            [
              a,
              Exp.apply(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("|--")),
                  ),
                ),
                [b, c],
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "regression_issue_1473",
    "a << b >> c",
    {
      statements: [
        Top.expr(
          Exp.apply(
            Exp.ident(
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc(">>")),
              ),
            ),
            [
              Exp.apply(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("<<")),
                  ),
                ),
                [a, b],
              ),
              c,
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
});
