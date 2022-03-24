open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_parsing.Location;
open Grain_middle_end.Anf_helper;

describe("chars", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
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
  let char = (~loc=?, s) =>
    Top.expr(~loc?) @@ Exp.constant(~loc?, Const.char(s));

  assertRun("char1", "print('A')", "A\n");
  assertSnapshot("char2", "'\\x41'");
  assertSnapshot("char3", "'\\101'");
  assertSnapshot("char4", "'\\u0041'");
  assertSnapshot("char5", "'\\u{41}'");
  assertSnapshot("char6", "'💯'");
  assertSnapshot("char7", "'\\u{1F33E}'");
  assertSnapshot("char8", "'\\u2728'");
  assertRun("char_eq1", "print('🌾' == '🌾')", "true\n");
  assertRun("char_eq2", "print('🌾' == '💯')", "false\n");
  assertRun(
    "char_eq3",
    "import Char from \"char\"; print(Char.fromCode(0x1F33E) == '🌾')",
    "true\n",
  );
  assertRun(
    "char_eq4",
    "import Char from \"char\"; print(Char.fromCode(0x1F33E) == '💯')",
    "false\n",
  );
  assertRun("char_toString_escape1", {|print(('\\',))|}, "('\\\\',)\n");
  assertRun("char_toString_escape2", {|print(('\b',))|}, "('\\b',)\n");
  assertRun("char_toString_escape3", {|print(('\f',))|}, "('\\f',)\n");
  assertRun("char_toString_escape4", {|print(('\n',))|}, "('\\n',)\n");
  assertRun("char_toString_escape5", {|print(('
',))|}, "('\\n',)\n");
  assertRun("char_toString_escape6", {|print(('\r',))|}, "('\\r',)\n");
  assertRun("char_toString_escape7", {|print(('\t',))|}, "('\\t',)\n");
  assertRun("char_toString_escape8", {|print(('\v',))|}, "('\\v',)\n");
  assertRun("char_toString_escape9", {|print(('\'',))|}, "('\\'',)\n");
  assertCompileError(
    "char_illegal",
    "'abc'",
    "This character literal contains multiple characters: 'abc'\nDid you mean to create the string \"abc\" instead?",
  );
  assertCompileError(
    "unicode_err1",
    "let x = '\\u{d800}'",
    "Illegal unicode code point",
  );
  assertCompileError(
    "unicode_err2",
    "let x = '\\u{dfff}'",
    "Illegal unicode code point",
  );
  assertCompileError(
    "unicode_err3",
    "let x = '\\u{110000}'",
    "Illegal unicode code point",
  );
  // parse locations
  assertParseWithLocs(
    "char_loc_simple",
    "'a'",
    {
      statements: [
        char(~loc=mk_loc("char_loc_simple", (1, 0, 0), (1, 3, 0)), "a"),
      ],
      comments: [],
      prog_loc: mk_loc("char_loc_simple", (1, 0, 0), (1, 3, 0)),
    },
  );
  assertParseWithLocs(
    "char_loc_code",
    "'\\u{1F3F4}'",
    {
      statements: [
        char(~loc=mk_loc("char_loc_code", (1, 0, 0), (1, 11, 0)), "🏴"),
      ],
      comments: [],
      prog_loc: mk_loc("char_loc_code", (1, 0, 0), (1, 11, 0)),
    },
  );
  // note that the char length is calculated as having 4 bytes, not 1 codepoint
  assertParseWithLocs(
    "char_loc_emoji",
    "'💯'",
    {
      statements: [
        char(~loc=mk_loc("char_loc_emoji", (1, 0, 0), (1, 6, 0)), "💯"),
      ],
      comments: [],
      prog_loc: mk_loc("char_loc_emoji", (1, 0, 0), (1, 6, 0)),
    },
  );
});
