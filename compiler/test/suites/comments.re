open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_parsing;
open Ast_helper;

describe("comments", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertParse = makeParseRunner(test);

  let str = s => Top.expr @@ Exp.constant(Const.string(s));

  assertParse(
    "comment_parse_1",
    "// Test\n\"foo\"",
    {
      statements: [str("foo")],
      comments: [
        Parsetree.Line({
          cmt_content: "Test",
          cmt_source: "// Test\n",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_2",
    "/* Test */\"foo\"",
    {
      statements: [str("foo")],
      comments: [
        Parsetree.Block({
          cmt_content: "Test",
          cmt_source: "/* Test */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_3",
    "/** Test */\"foo\"",
    {
      statements: [str("foo")],
      comments: [
        Parsetree.Doc({
          cmt_content: "Test",
          cmt_source: "/** Test */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_4",
    "#!/bin/grain\n\"foo\"",
    {
      statements: [str("foo")],
      comments: [
        Parsetree.Shebang({
          cmt_content: "/bin/grain",
          cmt_source: "#!/bin/grain\n",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
    },
  );
  assertCompileError(
    "comment_line_numbers_1",
    "//comment\n//comment\n5 + 5L",
    "line 3, characters 4-6",
  );
  assertCompileError(
    "comment_line_numbers_2",
    "//comment\r\n//comment\r\n5 + 5L",
    "line 3, characters 4-6",
  );
  assertCompileError(
    "comment_line_numbers_3",
    "//comment\n//comment\r\n5 + 5L",
    "line 3, characters 4-6",
  );
  assertSnapshot("comment_alone", "//\nlet x = 10\nx");
  assertSnapshot("comment_block", "/* block 1 */let x = 10/* block 2 */\nx");
  assertSnapshot("comment_doc", "/** doc 1 */let x = 10/** doc 2 */\nx");
  assertSnapshot("comment_shebang", "#!/bin/grain\nlet x = 10\nx");
});
