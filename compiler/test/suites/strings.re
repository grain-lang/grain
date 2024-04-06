open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_tests.Test_utils;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;
open Grain_utils.Warnings;

describe("strings", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertParseWithLocs = makeParseRunner(~keep_locs=true, test);
  open Grain_parsing;
  open Ast_helper;
  let str = (~loc, s) => {
    Toplevel.expr(~loc, ~core_loc=loc) @@
    Expression.constant(~loc, ~core_loc=loc, Constant.string({txt: s, loc}));
  };
  assertParseWithLocs(
    "string_parse_dqs1",
    "module Test; \"foo\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_dqs1", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_dqs1", (1, 13, 0), (1, 18, 0)),
          "\"foo\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_dqs1", (1, 0, 0), (1, 18, 0)),
      prog_core_loc: mk_loc("string_parse_dqs1", (1, 0, 0), (1, 18, 0)),
    },
  );
  assertParseWithLocs(
    "string_parse_dqs2",
    "module Test; \"bar\\nbaz\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_dqs2", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_dqs2", (1, 13, 0), (1, 23, 0)),
          "\"bar\\nbaz\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_dqs2", (1, 0, 0), (1, 23, 0)),
      prog_core_loc: mk_loc("string_parse_dqs2", (1, 0, 0), (1, 23, 0)),
    },
  );
  assertParseWithLocs(
    "string_parse_sqs1",
    "module Test; \"foobar\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_sqs1", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_sqs1", (1, 13, 0), (1, 21, 0)),
          "\"foobar\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_sqs1", (1, 0, 0), (1, 21, 0)),
      prog_core_loc: mk_loc("string_parse_sqs1", (1, 0, 0), (1, 21, 0)),
    },
  );
  assertParseWithLocs(
    "string_parse_sqs2",
    "module Test; \"bar\\u{41}\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_sqs2", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_sqs2", (1, 13, 0), (1, 24, 0)),
          "\"bar\\u{41}\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_sqs2", (1, 0, 0), (1, 24, 0)),
      prog_core_loc: mk_loc("string_parse_sqs2", (1, 0, 0), (1, 24, 0)),
    },
  );
  assertParseWithLocs(
    "string_parse_sqs3",
    "module Test; \"bar\\x41\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_sqs3", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_sqs3", (1, 13, 0), (1, 22, 0)),
          "\"bar\\x41\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_sqs3", (1, 0, 0), (1, 22, 0)),
      prog_core_loc: mk_loc("string_parse_sqs3", (1, 0, 0), (1, 22, 0)),
    },
  );
  assertParseWithLocs(
    "string_parse_sqs4",
    "module Test; \"bar\\101\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_sqs4", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_sqs4", (1, 13, 0), (1, 22, 0)),
          "\"bar\\101\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_sqs4", (1, 0, 0), (1, 22, 0)),
      prog_core_loc: mk_loc("string_parse_sqs4", (1, 0, 0), (1, 22, 0)),
    },
  );
  assertParseWithLocs(
    "string_parse_sqs5",
    "module Test; \"bar\\u0041\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_sqs5", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_sqs5", (1, 13, 0), (1, 24, 0)),
          "\"bar\\u0041\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_sqs5", (1, 0, 0), (1, 24, 0)),
      prog_core_loc: mk_loc("string_parse_sqs5", (1, 0, 0), (1, 24, 0)),
    },
  );
  assertParseWithLocs(
    "string_parse_emoji_escape",
    "module Test; \"😂\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_emoji_escape", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_emoji_escape", (1, 13, 0), (1, 16, 0)),
          "\"😂\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_emoji_escape", (1, 0, 0), (1, 16, 0)),
      prog_core_loc:
        mk_loc("string_parse_emoji_escape", (1, 0, 0), (1, 16, 0)),
    },
  );
  assertParseWithLocs(
    "string_parse_emoji_literal",
    "module Test; \"💯\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_parse_emoji_literal", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_parse_emoji_literal", (1, 13, 0), (1, 16, 0)),
          "\"💯\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_parse_emoji_literal", (1, 0, 0), (1, 16, 0)),
      prog_core_loc:
        mk_loc("string_parse_emoji_literal", (1, 0, 0), (1, 16, 0)),
    },
  );
  /* String parse locations */
  assertParseWithLocs(
    "string_loc_single_line",
    "module Test\n\"foo\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_loc_single_line", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_loc_single_line", (2, 12, 12), (2, 17, 12)),
          "\"foo\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_loc_single_line", (1, 0, 0), (2, 17, 12)),
      prog_core_loc:
        mk_loc("string_loc_single_line", (1, 0, 0), (2, 17, 12)),
    },
  );
  assertParseWithLocs(
    "string_loc_multi_line",
    "module Test\n\"foo\nbar\nbaz\nqux\nquux\"",
    {
      attributes: [],
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("string_loc_multi_line", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        str(
          ~loc=mk_loc("string_loc_multi_line", (2, 12, 12), (6, 34, 29)),
          "\"foo\nbar\nbaz\nqux\nquux\"",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("string_loc_multi_line", (1, 0, 0), (6, 34, 29)),
      prog_core_loc:
        mk_loc("string_loc_multi_line", (1, 0, 0), (6, 34, 29)),
    },
  );
  assertParseWithLocs(
    "string_loc_single_line_emoji",
    "module Test\n\"💯\"",
    {
      attributes: [],
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
          "\"💯\"",
        ),
      ],
      comments: [],
      prog_loc:
        mk_loc("string_loc_single_line_emoji", (1, 0, 0), (2, 15, 12)),
      prog_core_loc:
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
  assertRun("string_float1", {|print(NaNf)|}, "NaN\n");
  assertRun("string_float2", {|print(Infinityf)|}, "Infinity\n");
  assertRun("string_float3", {|print(-Infinityf)|}, "-Infinity\n");
  assertRun(
    "string_float4",
    {|from "float64" include Float64; use Float64.*; print(0.0d / 0.0d)|},
    "NaN\n",
  );
  assertRun(
    "string_float5",
    {|from "float64" include Float64; use Float64.*; print(1.0d / 0.0d)|},
    "Infinity\n",
  );
  assertRun(
    "string_float6",
    {|from "float64" include Float64; use Float64.*; print(-1.0d / 0.0d)|},
    "-Infinity\n",
  );

  // Bytes literals
  assertRun("bytes_literal", {|print(b"abc")|}, "<bytes: 61 62 63>\n");
  assertRun(
    "bytes_literal_hex",
    {|print(b"\xc3\x81\x24\x24\x00\x24\x99\xc3")|},
    "<bytes: c3 81 24 24 00 24 99 c3>\n",
  );
  assertRun(
    "bytes_literal_long",
    {|print(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg")|},
    "<bytes: 41 42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f 50 51 52 53 54 55 56 57 58 59 5a 61 62 63 64 65 66...>\n",
  );
  assertCompileError(
    "bytes_literal_err1",
    {|print(b"abc\u1234")|},
    "Byte literals may not contain unicode escapes",
  );
  assertCompileError(
    "bytes_literal_err2",
    {|print(b"abc\u{1234}")|},
    "Byte literals may not contain unicode escapes",
  );
  assertCompileError(
    "bytes_literal_err3",
    {|print(b"abc😂")|},
    "Byte literals may not contain non-ascii unicode characters",
  );
});
