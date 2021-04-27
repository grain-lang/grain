open TestFramework;
open Runner;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;

describe("strings", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);
  let assertParse = makeParseRunner(test);
  open Grain_parsing;
  open Ast_helper;
  let str = s => Top.expr @@ Exp.constant(Const.string(s));
  assertParse(
    "string_parse_dqs1",
    "\"foo\"",
    {statements: [str("foo")], comments: [], prog_loc: Location.dummy_loc},
  );
  assertParse(
    "string_parse_dqs2",
    "\"bar\\nbaz\"",
    {
      statements: [str("bar\nbaz")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_sqs1",
    "\"foobar\"",
    {
      statements: [str("foobar")],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "string_parse_sqs2",
    "\"bar\\u{41}\"",
    {statements: [str("barA")], comments: [], prog_loc: Location.dummy_loc},
  );
  assertParse(
    "string_parse_sqs3",
    "\"bar\\x41\"",
    {statements: [str("barA")], comments: [], prog_loc: Location.dummy_loc},
  );
  assertParse(
    "string_parse_sqs4",
    "\"bar\\101\"",
    {statements: [str("barA")], comments: [], prog_loc: Location.dummy_loc},
  );
  assertParse(
    "string_parse_sqs5",
    "\"bar\\u0041\"",
    {statements: [str("barA")], comments: [], prog_loc: Location.dummy_loc},
  );
  assertParse(
    "string_parse_emoji_escape",
    "\"😂\"",
    {statements: [str("😂")], comments: [], prog_loc: Location.dummy_loc},
  );
  assertParse(
    "string_parse_emoji_literal",
    "\"💯\"",
    {statements: [str("💯")], comments: [], prog_loc: Location.dummy_loc},
  );
  assertSnapshot("string1", "\"foo\"");
  assertSnapshot("string2", "\"💯\"");
  assertSnapshot("string3", "\"making my way downtown, walking fast\"");
  assertSnapshot("concat", "\"foo\" ++ \"bar\"");
  assertRun(
    "toString_escape1",
    {|print(("foo\\bar",))|},
    "(\"foo\\\\bar\",)\n",
  );
  assertRun(
    "toString_escape2",
    {|print(("foo\bbar",))|},
    "(\"foo\\bbar\",)\n",
  );
  assertRun(
    "toString_escape3",
    {|print(("foo\fbar",))|},
    "(\"foo\\fbar\",)\n",
  );
  assertRun(
    "toString_escape4",
    {|print(("foo\nbar",))|},
    "(\"foo\\nbar\",)\n",
  );
  assertRun(
    "toString_escape5",
    {|print(("foo
bar",))|},
    "(\"foo\\nbar\",)\n",
  );
  assertRun(
    "toString_escape6",
    {|print(("foo\rbar",))|},
    "(\"foo\\rbar\",)\n",
  );
  assertRun(
    "toString_escape7",
    {|print(("foo\tbar",))|},
    "(\"foo\\tbar\",)\n",
  );
  assertRun(
    "toString_escape8",
    {|print(("foo\vbar",))|},
    "(\"foo\\vbar\",)\n",
  );
  assertRun(
    "toString_escape9",
    {|print(("foo\"bar",))|},
    "(\"foo\\\"bar\",)\n",
  );
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
    {|import * from "float32"; print(div(0.0f, 0.0f))|},
    "NaN\n",
  );
  assertRun(
    "string_float2",
    {|import * from "float32"; print(div(1.0f, 0.0f))|},
    "Infinity\n",
  );
  assertRun(
    "string_float3",
    {|import * from "float32"; print(div(-1.0f, 0.0f))|},
    "-Infinity\n",
  );
  assertRun(
    "string_float4",
    {|import * from "float64"; print(div(0.0d, 0.0d))|},
    "NaN\n",
  );
  assertRun(
    "string_float5",
    {|import * from "float64"; print(div(1.0d, 0.0d))|},
    "Infinity\n",
  );
  assertRun(
    "string_float6",
    {|import * from "float64"; print(div(-1.0d, 0.0d))|},
    "-Infinity\n",
  );
});
