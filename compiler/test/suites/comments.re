open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_parsing;
open Ast_helper;

describe("comments", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertParse = makeParseRunner(test);

  assertParse(
    "comment_parse_1",
    "// Test\nmodule Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Line({
          cmt_content: "Test",
          cmt_source: "// Test",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_2",
    "/* Test */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Block({
          cmt_content: "Test",
          cmt_source: "/* Test */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_block_multiline_trim",
    "/* Test\n    Weird indent\n  Normal indent */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Block({
          cmt_content: "Test\nWeird indent\nNormal indent",
          cmt_source: "/* Test\n    Weird indent\n  Normal indent */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_block_multiline_trim2",
    "/* Test\r\n    Weird indent\r\n  Normal indent */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Block({
          cmt_content: "Test\nWeird indent\nNormal indent",
          cmt_source: "/* Test\r\n    Weird indent\r\n  Normal indent */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_3",
    "/** Test */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Doc({
          cmt_content: "Test",
          cmt_source: "/** Test */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_doc_multiline_trim_all_same_indent",
    "/**\n  Test\n  Weird indent\n  Normal indent */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Doc({
          cmt_content: "Test\nWeird indent\nNormal indent",
          cmt_source: "/**\n  Test\n  Weird indent\n  Normal indent */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_doc_multiline_trim_keeps_differnt_indent",
    "/** Test\n    Weird indent\n  Normal indent */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Doc({
          cmt_content: "Test\n   Weird indent\n Normal indent",
          cmt_source: "/** Test\n    Weird indent\n  Normal indent */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_doc_multiline_trim_normalizes_tabs",
    // Note: There are explicit tab characters in this string to test them
    "/**\n		Test\r\n	 Weird indent\r\n  Normal indent */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Doc({
          cmt_content: "Test\nWeird indent\nNormal indent",
          cmt_source: "/**\n		Test\r\n	 Weird indent\r\n  Normal indent */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_4",
    "#!/bin/grain\nmodule Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Shebang({
          cmt_content: "/bin/grain",
          cmt_source: "#!/bin/grain",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_block_deasterisk",
    "/* Test\n* no space before\n * space before\n  * tab before\n *no space after */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Block({
          cmt_content: "Test\nno space before\nspace before\ntab before\nno space after",
          cmt_source: "/* Test\n* no space before\n * space before\n  * tab before\n *no space after */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_doc_deasterisk",
    "/** Test\n* no space before\n * space before\n  * tab before\n *no space after */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Doc({
          cmt_content: " Test\n no space before\n space before\n tab before\nno space after",
          cmt_source: "/** Test\n* no space before\n * space before\n  * tab before\n *no space after */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "comment_parse_doc_deasterisk2",
    "/** Test\n* no space before\n * space before\n  * tab before\n * trailing space after */module Test",
    {
      attributes: [],
      module_name: Location.mknoloc("Test"),
      statements: [],
      comments: [
        Parsetree.Doc({
          cmt_content: "Test\nno space before\nspace before\ntab before\ntrailing space after",
          cmt_source: "/** Test\n* no space before\n * space before\n  * tab before\n * trailing space after */",
          cmt_loc: Location.dummy_loc,
        }),
      ],
      prog_loc: Location.dummy_loc,
      prog_core_loc: Location.dummy_loc,
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
