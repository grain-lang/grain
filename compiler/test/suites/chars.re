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
    Toplevel.expr(~loc?) @@ Expression.constant(~loc?, Constant.char(s));

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
    "include \"char\" as Char; print(Char.fromCode(0x1F33E) == '🌾')",
    "true\n",
  );
  assertRun(
    "char_eq4",
    "include \"char\" as Char; print(Char.fromCode(0x1F33E) == '💯')",
    "false\n",
  );
  assertRun("char_toString_escape1", {|print(('\\', 1))|}, "('\\\\', 1)\n");
  assertRun("char_toString_escape2", {|print(('\b', 1))|}, "('\\b', 1)\n");
  assertRun("char_toString_escape3", {|print(('\f', 1))|}, "('\\f', 1)\n");
  assertRun("char_toString_escape4", {|print(('\n', 1))|}, "('\\n', 1)\n");
  assertRun("char_toString_escape5", {|print(('
', 1))|}, "('\\n', 1)\n");
  assertRun("char_toString_escape6", {|print(('\r', 1))|}, "('\\r', 1)\n");
  assertRun("char_toString_escape7", {|print(('\t', 1))|}, "('\\t', 1)\n");
  assertRun("char_toString_escape8", {|print(('\v', 1))|}, "('\\v', 1)\n");
  assertRun("char_toString_escape9", {|print(('\'', 1))|}, "('\\'', 1)\n");
  assertCompileError(
    "char_illegal1",
    "'abc'",
    "This character literal contains multiple characters: 'abc'\nDid you mean to create the string \"abc\" instead?",
  );
  assertCompileError(
    "char_illegal2",
    {|'{"test": 1}'|},
    {|This character literal contains multiple characters: '\{"test": 1\}'
Did you mean to create the string "\{\\"test\\": 1\}" instead?|},
  );
  assertCompileError(
    "char_illegal3",
    "''",
    "This character literal contains no character. Did you mean to create an empty string \"\" instead?",
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
    "module Test\n'a'",
    {
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("char_loc_simple", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        char(
          ~loc=mk_loc("char_loc_simple", (2, 12, 12), (2, 15, 12)),
          "a",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("char_loc_simple", (1, 0, 0), (2, 15, 12)),
    },
  );
  assertParseWithLocs(
    "char_loc_code",
    "module Test\n'\\u{1F3F4}'",
    {
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("char_loc_code", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        char(
          ~loc=mk_loc("char_loc_code", (2, 12, 12), (2, 23, 12)),
          "🏴",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("char_loc_code", (1, 0, 0), (2, 23, 12)),
    },
  );
  assertParseWithLocs(
    "char_loc_emoji",
    "module Test\n'💯'",
    {
      module_name:
        Location.mkloc(
          "Test",
          mk_loc("char_loc_emoji", (1, 7, 0), (1, 11, 0)),
        ),
      statements: [
        char(
          ~loc=mk_loc("char_loc_emoji", (2, 12, 12), (2, 15, 12)),
          "💯",
        ),
      ],
      comments: [],
      prog_loc: mk_loc("char_loc_emoji", (1, 0, 0), (2, 15, 12)),
    },
  );
});
