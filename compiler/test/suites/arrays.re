open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("arrays", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertRunError = makeErrorRunner(test_or_skip);
  let assertParse = makeParseRunner(test);

  assertRun("array1", "print([> 1, 2, 3])", "[> 1, 2, 3]\n");
  assertRun("array2", "print([>])", "[> ]\n");
  assertSnapshot("array3", "[>\n1, 2, 3]");
  assertCompileError("array_error", "[> 1, false, 2]", "has type Bool but");
  assertSnapshot("array_access", "let x = [> 1, 2, 3]; x[0]");
  assertSnapshot("array_access2", "let x = [> 1, 2, 3]; x[1]");
  assertSnapshot("array_access3", "let x = [> 1, 2, 3]; x[2]");
  assertSnapshot("array_access4", "let x = [> 1, 2, 3]; x[-2]");
  assertSnapshot("array_access5", "let x = [> 1, 2, 3]; x[-3]");
  assertSnapshot("array_access5", "[> 1, 2, 3][-3]");
  assertRunError(
    "array_access_err",
    "let x = [> 1, 2, 3]; x[3]",
    "Index out of bounds",
  );
  assertRunError(
    "array_access_err2",
    "let x = [> 1, 2, 3]; x[-4]",
    "Index out of bounds",
  );
  assertRunError(
    "array_access_err3",
    "let x = [> 1, 2, 3]; x[99]",
    "Index out of bounds",
  );
  assertRunError(
    "array_access_err4",
    "let x = [> 1, 2, 3]; x[-99]",
    "Index out of bounds",
  );
  assertRunError(
    "array_access_err5",
    "let x = [> 1, 2, 3]; x[1.5]",
    "Index not an integer",
  );
  assertRunError(
    "array_access_err6",
    "let x = [> 1, 2, 3]; x[1/3]",
    "Index not an integer",
  );
  assertRunError(
    "array_access_err7",
    "let x = [> 1, 2, 3]; x[987654321987654321]",
    "Index out of bounds",
  );
  assertCompileError(
    "array_access_err8",
    "let x = [> 1, 2, 3]; x[false]",
    "has type Bool but",
  );
  assertRun(
    "array_set",
    "let x = [> 1, 2, 3]; x[0] = 4; print(x)",
    "[> 4, 2, 3]\n",
  );
  assertRun(
    "array_set2",
    "let x = [> 1, 2, 3]; x[-2] = 4; print(x)",
    "[> 1, 4, 3]\n",
  );
  assertCompileError(
    "array_set_err",
    "let x = [> 1, 2, 3]; x[-2] = false",
    "has type Bool but",
  );
  assertRunError(
    "array_set_err2",
    "let x = [> 1, 2, 3]; x[-12] = 4",
    "Index out of bounds",
  );
  assertRunError(
    "array_set_err3",
    "let x = [> 1, 2, 3]; x[1.5] = 4",
    "Index not an integer",
  );
  assertRunError(
    "array_set_err4",
    "let x = [> 1, 2, 3]; x[1/3] = 4",
    "Index not an integer",
  );
  assertRunError(
    "array_set_err5",
    "let x = [> 1, 2, 3]; x[987654321987654321] = 4",
    "Index out of bounds",
  );
  assertCompileError(
    "array_type",
    "let x = [> true, false, false]; x[1] + 3",
    "has type Bool but",
  );
  assertCompileError(
    "array_type2",
    "let x = [> true, false, false]; (x[1] = true) + 3",
    "has type Void but",
  );
  // trailing commas
  assertSnapshot("array1_trailing", "[> 1, 2, 3,]");
  assertSnapshot("array1_trailing_space", "[> 1, 2, 3, ]");
  assertCompileError(
    "invalid_empty_trailing",
    "[> ,]",
    "Error: Syntax error",
  );
  // parsing
  Grain_parsing.(
    Ast_helper.(
      assertParse(
        "issue_925_parse_array_set_newline",
        "state[0] =
           5",
        {
          statements: [
            Top.expr(
              Exp.array_set(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("state")),
                  ),
                ),
                Exp.constant(Const.number(PConstNumberInt("0"))),
                Exp.constant(Const.number(PConstNumberInt("5"))),
              ),
            ),
          ],
          comments: [],
          prog_loc: Location.dummy_loc,
        },
      )
    )
  );
});
