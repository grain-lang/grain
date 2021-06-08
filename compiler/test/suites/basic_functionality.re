open TestFramework;
open Runner;

describe("basic functionality", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertSnapshotFile = makeSnapshotFileRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRunError = makeErrorRunner(test);

  assertSnapshot("forty", "let x = 40; x");
  assertSnapshot("neg", "-1073741824");
  assertSnapshot("hex", "0xff");
  assertSnapshot("hex_neg", "-0xff");
  assertSnapshot("bin", "0b1010");
  assertSnapshot("bin_neg", "-0b1010");
  assertSnapshot("oct", "0o77");
  assertSnapshot("oct_neg", "-0o77");
  assertSnapshot("fals", "let x = false; x");
  assertSnapshot("tru", "let x = true; x");
  assertSnapshot(
    "complex1",
    "\n    let x = 2, y = 3, z = if (true) { 4 } else { 5 };\n    if (true) {\n      print(y)\n      y - (z + x)\n    } else {\n      print(8)\n      8\n    }\n    ",
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
  assertSnapshot("comp10", "let x = 2, y = 4; (y - 2) == x");
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
    "if_one_sided_type_err",
    "let foo = (if (false) { 5; }); let bar = foo + 5; bar",
    "has type Void but",
  );
  assertSnapshot("int32_1", "42l");
  assertSnapshot("int64_1", "99999999999999999L");
  assertSnapshot("int64_pun_1", "9999999 * 99999999");
  assertSnapshot("int64_pun_2", "-99999999 - 999999999");
  assertRunError("overflow1", "9223372036854775807 + 1", "Overflow");
  assertSnapshot("block_no_expression", "let f = () => { let x = 5 }; f()");
  /* Assertions */
  assertSnapshot("assert1", "assert true");
  assertSnapshot("assert2", "assert 3 + 3 == 6");
  assertRunError("assert3", "assert false", "AssertionError");
  assertRunError("assert4", "assert 4 - 1 == 14", "AssertionError");
  /* Failures */
  assertRunError("fail1", "ignore(fail \"boo\")", "Failure: boo");
  assertRunError(
    "fail2",
    "if (false) { 3 } else { fail \"boo\" }",
    "Failure: boo",
  );
  assertSnapshotFile("toplevel_statements", "toplevelStatements");
  assertSnapshotFile("unsafe_wasm_globals", "unsafeWasmGlobals");
  assertSnapshotFile("pattern_match_unsafe_wasm", "patternMatchUnsafeWasm");
});
