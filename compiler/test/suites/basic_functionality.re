open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_tests.Test_utils;

describe("basic functionality", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertSnapshotFile = makeSnapshotFileRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertFilesize = makeFilesizeRunner(test);
  let assertParse = makeParseRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertRunError = makeErrorRunner(test_or_skip);
  let smallestFileConfig = () => {
    Grain_utils.Config.elide_type_info := true;
    Grain_utils.Config.profile := Some(Grain_utils.Config.Release);
  };

  assertSnapshot("nil", "");
  assertSnapshot("forty", "let x = 40; x");
  assertSnapshot("neg", "-40");
  assertSnapshot("simple_min", "-1073741824");
  assertSnapshot("simple_max", "1073741823");
  assertSnapshot("bigint_start_neg", "-0xffff_ffff_ffff_ffff");
  assertSnapshot("bigint_start_pos", "0xffff_ffff_ffff_ffff");
  assertSnapshot("heap_number_i32_wrapper", "1073741824");
  assertSnapshot("heap_number_i32_wrapper_max", "2147483647");
  assertSnapshot("heap_number_i64_wrapper", "2147483648");
  assertSnapshot("hex_dec_exp1", "0x1p5");
  assertSnapshot("hex_dec_exp2", "0x1.4p5");
  assertSnapshot("hex_dec_exp3", "0x1p-5");
  assertSnapshot("hex_dec_exp4", "0x1Ap5");
  assertSnapshot("hex_dec_exp5", "0x1A.4p5");
  assertSnapshot("hex_dec_exp5", "0x1A.4Ap5");
  assertSnapshot("hex", "0xff");
  assertSnapshot("hex_neg", "-0xff");
  assertSnapshot("bin", "0b1010");
  assertSnapshot("bin_neg", "-0b1010");
  assertSnapshot("oct", "0o77");
  assertSnapshot("oct_neg", "-0o77");
  assertSnapshot("fals", "let x = false; x");
  assertSnapshot("tru", "let x = true; x");
  assertSnapshot("infinity", "let x = Infinity; x");
  assertRun("infinity_2", "assert Infinity == 1.0 / 0.0", "");
  assertRun("infinity_3", "assert Infinity == Infinity", "");
  assertSnapshot("infinity_neg", "let x = -Infinity; x");
  assertRun("infinity_neg_2", "assert -Infinity == -1.0 / 0.0", "");
  assertRun("infinity_neg_3", "assert -Infinity == -Infinity", "");
  assertSnapshot("nan", "let x = NaN; x");
  assertRun("nan_2", "assert NaN != NaN", "");

  assertSnapshot("print_line_ending1", "print(1, suffix=\"\")");
  assertRun("print_line_ending2", "print(123, suffix=\"\")", "123");
  assertRun(
    "print_line_ending3",
    "print(1, suffix=\"\"); print(2, suffix=\"    \"); print(\"3\", suffix=\"end\")",
    "12    3end",
  );

  assertSnapshot(
    "complex1",
    "\n    let x = 2 and y = 3 and z = if (true) { 4 } else { 5 };\n    if (true) {\n      print(y)\n      y - (z + x)\n    } else {\n      print(8)\n      8\n    }\n    ",
  );
  assertSnapshot("complex2", "print(2 + 3)");
  assertSnapshot("binop1", "2 + 2");
  assertSnapshot("binop2", "2 - 2");
  assertSnapshot("binop2.1", "2-2");
  assertSnapshot("binop2.2", "2 -
                 2");
  assertSnapshot("binop2.3", "2
                 - 2");
  assertSnapshot("binop2.4", "- 2");
  assertSnapshot("binop3", "2 - 4");
  assertSnapshot("binop4", "2 * 3");
  assertSnapshot("binop5", "10 / 5");
  assertSnapshot("binop6", "9 % 5");
  assertRunError(
    "division_by_zero",
    "let nine = 9; nine / 0",
    "Division by zero",
  );
  assertRunError("modulo_by_zero", "9 % 0", "Modulo by zero");
  assertSnapshot("division1", "5 / 2");
  assertSnapshot("modulo1", "-17 % 4");
  assertSnapshot("modulo2", "17 % -4");
  assertSnapshot("modulo3", "-17 % -4");
  assertSnapshot("modulo4", "-17 % 17");
  assertSnapshot("modulo5", "17 % -17");
  assertSnapshot("modulo6", "17 % 17");
  assertSnapshot("and1", "true && true");
  assertSnapshot("and2", "true && false");
  assertSnapshot("and3", "false && true");
  assertSnapshot("and4", "false && false");
  assertSnapshot("andshort1", "true && {print(1); false}");
  assertSnapshot("andshort2", "false && {print(1); false}");
  assertSnapshot("andshadow", "let (&&) = (+); 1 && 2");
  assertSnapshot("or1", "true || true");
  assertSnapshot("or2", "true || false");
  assertSnapshot("or3", "false || true");
  assertSnapshot("or4", "false || false");
  assertSnapshot("orshort1", "true || {print(1); false}");
  assertSnapshot("orshort2", "false || {print(1); false}");
  assertSnapshot("orshadow", "let (||) = (+); 1 || 2");
  assertSnapshot("land1", "1 & 1");
  assertSnapshot("land2", "1 & 0");
  assertSnapshot("land3", "0 & 1");
  assertSnapshot("land4", "0 & 0");
  assertSnapshot("lor1", "1 | 1");
  assertSnapshot("lor2", "1 | 0");
  assertSnapshot("lor3", "0 | 1");
  assertSnapshot("lor4", "0 | 0");
  assertSnapshot("lxor1", "1 ^ 1");
  assertSnapshot("lxor2", "1 ^ 0");
  assertSnapshot("lxor3", "0 ^ 1");
  assertSnapshot("lxor4", "0 ^ 0");
  assertSnapshot("lsl1", "7 << 1");
  assertSnapshot("lsl2", "0 << 1");
  assertSnapshot("lsr1", "7 >>> 1");
  assertSnapshot("lsr2", "0 >>> 1");
  assertSnapshot("asr1", "179 >> 1");
  assertSnapshot("asr2", "0 >> 1");
  assertSnapshot("comp1", "if (2 < 3) {true} else {false}");
  assertCompileError("comp1e", "if (2 < 3) {true} else {3}", "type");
  assertSnapshot("comp2", "if (2 <= 3) {true} else {false}");
  assertCompileError("comp2e", "if (2 <= 3) {true} else {3}", "type");
  assertSnapshot("comp3", "if (2 >= 3) {4} else {5}");
  assertSnapshot("comp4", "if (2 > 3) {4} else {5}");
  assertSnapshot("comp5", "if (2 < 3) {4} else {5}");
  assertSnapshot("comp6", "if (2 == 3) {8} else {9}");
  assertSnapshot("comp7", "if (2 == 2) {8} else {9}");
  assertSnapshot("comp8", "if (2 <= 2) {10} else {11}");
  assertSnapshot("comp9", "if (2 >= 2) {10} else {11}");
  assertSnapshot("comp10", "let x = 2 and y = 4; (y - 2) == x");
  assertCompileError("comp11", "true == 2", "has type Number but");
  assertCompileError("comp12", "2 == false", "has type Bool but");
  assertSnapshot("comp13", "true == true");
  assertSnapshot("comp14", "true == false");
  assertSnapshot("comp15", "false == true");
  assertSnapshot("comp16", "false == false");
  assertSnapshot("comp17", "false isnt true");
  assertSnapshot("comp18", "4 isnt 1");
  assertSnapshot("comp19", "[1, 2] is [1, 2]");
  assertSnapshot("comp20", "[1, 2] isnt [1, 2]");
  assertSnapshot("assignment1", "let mut t = 2; t = 1;");
  assertSnapshot("assignment1", "let mut t = 1; t += 2;");
  assertSnapshot("assignment1", "let mut t = 2; t *= 2;");
  assertSnapshot("assignment1", "let mut t = 2; t /= 2;");
  assertSnapshot("assignment1", "let mut t = 2; t -= 2;");
  // These are not optimized into the same instance (boxes are mutable)
  assertSnapshot("comp21", "[box(1)] is [box(1)]");
  assertSnapshot("comp22", "[box(1)] isnt [box(1)]");
  assertSnapshot("precedence1", "3 + 4 * 6");
  assertSnapshot("precedence2", "4 * 6 + 3");
  assertSnapshot("precedence3", "3 + 4 % 6");
  assertSnapshot("precedence4", "4 % 6 + 3");
  assertSnapshot("precedence5", "4 > 3 && 9 < 13");
  assertSnapshot("not1", "!true");
  assertSnapshot("not2", "!false");
  assertSnapshot("incr_1", "incr(2)");
  assertSnapshot("incr_2", "incr(5)");
  assertSnapshot("incr_3", "incr(-1)");
  assertSnapshot("decr_1", "decr(2)");
  assertSnapshot("decr_2", "decr(5)");
  assertSnapshot("decr_3", "decr(0)");
  assertCompileError("comp_bool1", "if (2 < true) {3} else {4}", "type");
  assertCompileError("comp_bool2", "if (2 > true) {3} else {4}", "type");
  assertCompileError("comp_bool3", "if (true >= 4) {3} else {4}", "type");
  assertCompileError(
    "comp_bool4",
    "let x = true; if (x < 4) {3} else {5}",
    "type",
  );
  assertSnapshot("void", "{\"foo\";}");
  assertCompileError("arith1", "2 + true", "type");
  assertCompileError("arith2", "true + 4", "type");
  assertCompileError("arith3", "false - 5", "type");
  assertCompileError("arith4", "4 - true", "type");
  assertCompileError("arith5", "let x = true; x * 4", "type");
  assertCompileError("arith6", "let x = false; 4 * x", "type");
  assertCompileError("if1", "if (2) {5} else {6}", "type");
  assertCompileError("if2", "let y = 0; if (y) {5} else {6}", "type");
  assertCompileError("if3", "if (decr(1)) {2} else {5}", "type");
  assertSnapshot("if4", "if (false) 1 else if (false) 2 else {3}");
  assertSnapshot("if_one_sided", "if (3 < 4) print(5)");
  assertSnapshot("if_one_sided2", "if (3 > 4) print(5)");
  assertSnapshot("if_one_sided3", "let mut x = 1; if (3 < 4) x = 2");
  assertSnapshot("if_one_sided4", "let mut x = 1; if (3 < 4) x = 2; x");
  assertSnapshot("if_one_sided5", "let mut x = 1; if (3 < 4) x = 2 + 3");
  assertSnapshot("if_one_sided6", "let mut x = 1; if (3 < 4) x = 2 + 3; x");
  assertCompileError(
    "if_empty_block",
    "if (false) 1 else {}",
    "Syntax error",
  );
  assertCompileError(
    "if_one_sided_type_err",
    "let foo = (if (false) { ignore(5) }); let bar = foo + 5; bar",
    "has type Void but",
  );
  assertCompileError(
    "value_restriction",
    {|let f = () => x => x; let id = f(); let a = id(1); let b = id("a")|},
    "has type String but",
  );
  assertCompileError(
    "exports_weak_types",
    {|provide let f = box(x => 0)|},
    "type variables that cannot be generalized",
  );
  assertSnapshot("int32_1", "42l");
  assertSnapshot("int64_1", "99999999999999999L");
  assertSnapshot("uint32_1", "42ul");
  assertSnapshot("uint64_1", "99999999999999999uL");
  assertSnapshot("int64_pun_1", "9999999 * 99999999");
  assertSnapshot("int64_pun_2", "-99999999 - 999999999");
  assertSnapshot("bigint_1", "9223372036854775807 + 1");
  assertSnapshot("bigint_2", "0x42355430892589308190890313423");
  assertSnapshot("block_no_expression", "let f = () => { let x = 5 }; f()");
  assertSnapshotFile("func_shadow", "funcShadow");
  assertSnapshotFile(
    "func_shadow_and_indirect_call",
    "funcShadowAndIndirect",
  );
  /* Assertions */
  assertSnapshot("assert1", "assert true");
  assertSnapshot("assert2", "assert 3 + 3 == 6");
  assertRunError(
    "assert3",
    "assert false",
    "AssertionError: Assertion failed in assert3, line 1",
  );
  assertRunError(
    "assert4",
    "assert 4 - 1 == 14",
    "AssertionError: Assertion failed in assert4, line 1",
  );
  /* Failures */
  assertRunError("fail1", "ignore(fail \"boo\")", "Failure: boo");
  assertRunError(
    "fail2",
    "if (false) { 3 } else { fail \"boo\" }",
    "Failure: boo",
  );
  assertSnapshotFile("toplevel_statements", "toplevelStatements");
  assertSnapshotFile(
    ~config_fn=() => {Grain_utils.Config.no_gc := true},
    "unsafe_wasm_globals",
    "unsafeWasmGlobals",
  );
  assertSnapshotFile("pattern_match_unsafe_wasm", "patternMatchUnsafeWasm");
  /* Unicode support */
  Grain_parsing.(
    Ast_helper.(
      assertParse(
        "unicode_identifiers",
        {|
          module Test

          enum Caipirinha {
            Cachaça,
            Sugar,
            Lime,
          }

          let pokémon = "pikachu"

          type Über = Number
        |},
        {
          attributes: [],
          module_name: Location.mknoloc("Test"),
          statements: [
            Toplevel.data(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              [
                (
                  Asttypes.NotProvided,
                  DataDeclaration.variant(
                    ~loc=Location.dummy_loc,
                    Location.mknoloc("Caipirinha"),
                    [],
                    [
                      ConstructorDeclaration.singleton(
                        ~loc=Location.dummy_loc,
                        Location.mknoloc("Cachaça"),
                      ),
                      ConstructorDeclaration.singleton(
                        ~loc=Location.dummy_loc,
                        Location.mknoloc("Sugar"),
                      ),
                      ConstructorDeclaration.singleton(
                        ~loc=Location.dummy_loc,
                        Location.mknoloc("Lime"),
                      ),
                    ],
                  ),
                  Location.dummy_loc,
                ),
              ],
            ),
            Toplevel.let_(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              Asttypes.NotProvided,
              Asttypes.Nonrecursive,
              Asttypes.Immutable,
              [
                ValueBinding.mk(
                  ~loc=Location.dummy_loc,
                  Pattern.var(
                    ~loc=Location.dummy_loc,
                    Location.mknoloc("pokémon"),
                  ),
                  Expression.constant(
                    ~loc=Location.dummy_loc,
                    ~core_loc=Location.dummy_loc,
                    Constant.string({
                      txt: "\"pikachu\"",
                      loc:
                        mk_loc(
                          "unicode_identifiers",
                          (10, 147, 123),
                          (10, 156, 123),
                        ),
                    }),
                  ),
                ),
              ],
            ),
            Toplevel.data(
              ~loc=Location.dummy_loc,
              ~core_loc=Location.dummy_loc,
              [
                (
                  Asttypes.NotProvided,
                  DataDeclaration.abstract(
                    ~loc=Location.dummy_loc,
                    Location.mknoloc("Über"),
                    [],
                    Some(
                      Type.constr(
                        ~loc=Location.dummy_loc,
                        Location.mknoloc(
                          Identifier.IdentName(Location.mknoloc("Number")),
                        ),
                        [],
                      ),
                    ),
                  ),
                  Location.dummy_loc,
                ),
              ],
            ),
          ],
          comments: [],
          prog_loc: Location.dummy_loc,
          prog_core_loc: Location.dummy_loc,
        },
      )
    )
  );

  assertRun(
    "magic",
    {|
      primitive magic = "@magic"
      let helloBytes = b"hello"
      print(magic(helloBytes) ++ " world")
    |},
    "hello world\n",
  );

  assertFilesize(
    ~config_fn=smallestFileConfig,
    "smallest_grain_program",
    "",
    5165,
  );
});
