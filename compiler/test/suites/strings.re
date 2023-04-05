open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;
open Grain_utils.Warnings;

describe("strings", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertParse = makeParseRunner(test);
  let assertParseWithLocs = makeParseRunner(~keep_locs=true, test);
  open Grain_parsing;
  open Ast_helper;
  let mk_loc =
      (
        file,
        (start_line, start_col, start_bol),
        (end_line, end_col, end_bol),
      ) => {
    loc_start: {
      pos_fname: file,
      pos_lnum: start_line,
      pos_bol: start_bol,
      pos_cnum: start_col,
    },
    loc_end: {
      pos_fname: file,
      pos_lnum: end_line,
      pos_bol: end_bol,
      pos_cnum: end_col,
    },
    loc_ghost: false,
  };
  let str = (~loc=?, s) =>
    Toplevel.expr(~loc?) @@ Expression.constant(~loc?, Constant.string(s));
  assertParse(
    "string_parse_dqs1",
    "module Test; \"foo\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("foo")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_dqs2",
    "module Test; \"bar\\nbaz\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("bar\nbaz")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_sqs1",
    "module Test; \"foobar\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("foobar")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_sqs2",
    "module Test; \"bar\\u{41}\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("barA")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_sqs3",
    "module Test; \"bar\\x41\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("barA")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_sqs4",
    "module Test; \"bar\\101\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("barA")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_sqs5",
    "module Test; \"bar\\u0041\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("barA")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_emoji_escape",
    "module Test; \"😂\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("😂")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_emoji_literal",
    "module Test; \"💯\"",
    {
      module_name: Location.mknoloc("Test"),
      statements: [str("💯")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  /* String parse locations */
  assertParseWithLocs(
    "string_loc_single_line",
    "module Test\n\"foo\"",
    {
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_loc_single_line", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_loc_single_line", (2, 12, 12), (2, 17, 12)),
          "foo",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_loc_single_line", (1, 0, 0), (2, 17, 12)),
    },
  );
  assertParseWithLocs(
    "string_loc_multi_line",
    "module Test\n\"foo\nbar\nbaz\nqux\nquux\"",
    {
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_loc_multi_line", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_loc_multi_line", (2, 12, 12), (6, 34, 29)),
          "foo\nbar\nbaz\nqux\nquux",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_loc_multi_line", (1, 0, 0), (6, 34, 29)),
    },
  );
  assertParseWithLocs(
    "string_loc_single_line_emoji",
    "module Test\n\"💯\"",
    {
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_loc_single_line_emoji", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=
            mk_loc(
              "string_loc_single_line_emoji",
              (2, 12, 12),
              (2, 15, 12),
            ),
          "💯",
        ),
      ],
      comments: [],
      prog_loc:
        mk_loc("string_loc_single_line_emoji", (1, 0, 0), (2, 15, 12)),
    },
  );
  assertSnapshot("string1", "\"foo\"");
  assertSnapshot("string2", "\"💯\"");
  assertSnapshot("string3", "\"making my way downtown, walking fast\"");
  assertSnapshot("concat", "\"foo\" ++ \"bar\"");
  assertRun(
    "toString_escape1",
    {|print(("foo\\bar", 1))|},
    "(\"foo\\\\bar\", 1)\n",
  );
  assertRun(
    "toString_escape2",
    {|print(("foo\bbar", 1))|},
    "(\"foo\\bbar\", 1)\n",
  );
  assertRun(
    "toString_escape3",
    {|print(("foo\fbar", 1))|},
    "(\"foo\\fbar\", 1)\n",
  );
  assertRun(
    "toString_escape4",
    {|print(("foo\nbar", 1))|},
    "(\"foo\\nbar\", 1)\n",
  );
  assertRun(
    "toString_escape5",
    {|print(("foo
bar", 1))|},
    "(\"foo\\nbar\", 1)\n",
  );
  assertRun(
    "toString_escape6",
    {|print(("foo\rbar", 1))|},
    "(\"foo\\rbar\", 1)\n",
  );
  assertRun(
    "toString_escape7",
    {|print(("foo\tbar",1 ))|},
    "(\"foo\\tbar\", 1)\n",
  );
  assertRun(
    "toString_escape8",
    {|print(("foo\vbar", 1))|},
    "(\"foo\\vbar\", 1)\n",
  );
  assertRun(
    "toString_escape9",
    {|print(("foo\"bar", 1))|},
    "(\"foo\\\"bar\", 1)\n",
  );
  assertRun("toString_boxing1", {|print(box(1))|}, "box(1)\n");
  assertRun("toString_boxing2", {|print(box(true))|}, "box(true)\n");
  assertCompileError(
    "string_err",
    "let x = \"hello\"; x + \", world\"",
    "type",
  );
  assertCompileError(
    "unicode_err1",
    "let x = \"\\u{d800}\"",
    "Illegal unicode code point",
  );
  assertCompileError(
    "unicode_err2",
    "let x = \"\\u{dfff}\"",
    "Illegal unicode code point",
  );
  assertCompileError(
    "unicode_err3",
    "let x = \"\\u{110000}\"",
    "Illegal unicode code point",
  );
  assertRun(
    "string_float1",
    {|include "float32"; from Float32 use *; print(div(0.0f, 0.0f))|},
    "NaN\n",
  );
  assertRun(
    "string_float2",
    {|include "float32"; from Float32 use *; print(div(1.0f, 0.0f))|},
    "Infinity\n",
  );
  assertRun(
    "string_float3",
    {|include "float32"; from Float32 use *; print(div(-1.0f, 0.0f))|},
    "-Infinity\n",
  );
  assertRun(
    "string_float4",
    {|include "float64"; from Float64 use *; print(div(0.0d, 0.0d))|},
    "NaN\n",
  );
  assertRun(
    "string_float5",
    {|include "float64"; from Float64 use *; print(div(1.0d, 0.0d))|},
    "Infinity\n",
  );
  assertRun(
    "string_float6",
    {|include "float64"; from Float64 use *; print(div(-1.0d, 0.0d))|},
    "-Infinity\n",
  );

  // Bytes literals
  assertRun("bytes_literal", {|print(b"abc")|}, "<bytes: 61 62 63>\n");
  assertRun(
    "bytes_literal_long",
    {|print(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg")|},
    "<bytes: 41 42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f 50 51 52 53 54 55 56 57 58 59 5a 61 62 63 64 65 66...>\n",
  );
  assertCompileError(
    "bytes_literal_err1",
    {|print(b"abc\u1234")|},
    "Byte strings may not contain unicode escapes",
  );
  assertCompileError(
    "bytes_literal_err2",
    {|print(b"abc\u{1234}")|},
    "Byte strings may not contain unicode escapes",
  );
  assertCompileError(
    "bytes_literal_err3",
    {|print(b"abc😂")|},
    "Byte strings may not contain non-ascii unicode characters",
  );
});
