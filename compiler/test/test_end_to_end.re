open Grain.Compile;
open Runner;
open Grain_utils;
open Printf;
open OUnit2;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;

let wrap_todo = (todo, f, x) =>
  switch (todo) {
  | Some(msg) => OUnit2.todo(msg)
  | None => f(x)
  };

let exists = (check, result) =>
  try(Str.search_forward(Str.regexp_string(check), result, 0) >= 0) {
  | Not_found => false
  };

let t = (~todo=?, name, program, expected) =>
  name >:: wrap_todo(todo) @@ test_run(program, name, expected);
let tc = (~todo=?, name, program, expected) =>
  name >:: wrap_todo(todo) @@ test_run(~cmp=exists, program, name, expected);
let tgc = (~todo=?, name, heap_size, program, expected) =>
  name >:: wrap_todo(todo) @@ test_run(~heap_size, program, name, expected);
let terr = (~todo=?, name, program, expected) =>
  name >:: wrap_todo(todo) @@ test_err(program, name, expected);
let tgcerr = (~todo=?, name, heap_size, program, expected) =>
  name >:: wrap_todo(todo) @@ test_err(~heap_size, program, name, expected);

let te = (~todo=?, name, program, expected) =>
  name >:: wrap_todo(todo) @@ test_err(program, name, expected);

/** Tests that the file input/`input_file`.gr produces the given output */

let tfile = (~todo=?, name, input_file, expected) =>
  name >:: wrap_todo(todo) @@ test_run_file(input_file, name, expected);
let tefile = (~todo=?, name, input_file, expected) =>
  name >:: wrap_todo(todo) @@ test_run_file_err(input_file, name, expected);

/** Tests that the file stdlib/`input_file`.gr produces the given output */

let tlib = (~todo=?, ~returns=?, ~code=?, input_file) =>
  input_file
  >:: wrap_todo(todo) @@
  test_run_stdlib(~returns?, ~code?, input_file);

let tgcfile = (~todo=?, name, heap_size, input_file, expected) =>
  name
  >:: wrap_todo(todo) @@
  test_run_file(~heap_size, input_file, name, expected);

let test_final_anf =
    (
      program_str,
      outfile,
      expected: Grain_middle_end.Anftree.anf_expression,
      test_ctxt,
    ) => {
  open Grain_middle_end;
  let final_anf =
    Anf_utils.clear_locations @@
    compile_string_to_final_anf(outfile, program_str);
  let saved_disabled = Grain_typed.Ident.disable_stamps^;
  let (result, expected) =
    try(
      {
        Grain_typed.Ident.disable_stamps := true;
        let result =
          Sexplib.Sexp.to_string_hum @@
          Anftree.sexp_of_anf_expression(final_anf.body);
        let expected =
          Sexplib.Sexp.to_string_hum @@
          Anftree.sexp_of_anf_expression(expected);
        (result, expected);
      }
    ) {
    | e =>
      Grain_typed.Ident.disable_stamps := saved_disabled;
      raise(e);
    };

  assert_equal(~printer=x => x, expected, result);
};

let tfinalanf =
    (
      name,
      ~todo=?,
      program: string,
      expected: Grain_middle_end.Anftree.anf_expression,
    ) =>
  name >:: wrap_todo(todo) @@ test_final_anf(program, name, expected);

let tsound = (~todo=?, name, prog, expected) =>
  name >:: wrap_todo(todo) @@ test_optimizations_sound(prog, name, expected);
let tfsound = (~todo=?, name, filename, expected) =>
  name
  >:: wrap_todo(todo) @@
  test_file_optimizations_sound(filename, name, expected);

let test_parse =
    (
      ~todo=?,
      name,
      input,
      expected: Grain_parsing.Parsetree.parsed_program,
      test_ctxt,
    ) => {
  switch (todo) {
  | Some(msg) => OUnit2.todo(msg)
  | _ => ()
  };
  open Grain_parsing;
  let location_stripper = {
    ...Ast_mapper.default_mapper,
    location: (_, _) => Location.dummy_loc,
  };
  let comment_loc_stripper: Parsetree.comment => Parsetree.comment =
    comment => {
      switch (comment) {
      | Line(desc) => Line({...desc, cmt_loc: Location.dummy_loc})
      | Shebang(desc) => Shebang({...desc, cmt_loc: Location.dummy_loc})
      | Block(desc) => Block({...desc, cmt_loc: Location.dummy_loc})
      | Doc(desc) => Doc({...desc, cmt_loc: Location.dummy_loc})
      };
    };
  let strip_locs = ({statements, comments}: Parsetree.parsed_program) =>
    Parsetree.{
      statements:
        List.map(location_stripper.toplevel(location_stripper), statements),
      comments: List.map(comment_loc_stripper, comments),
      prog_loc: Location.dummy_loc,
    };
  let parsed = strip_locs @@ parse_string(name, input);
  let untagged = strip_locs @@ parsed;
  assert_equal(expected, untagged, ~printer=p =>
    Sexplib.Sexp.to_string_hum @@
    Grain_parsing.Parsetree.sexp_of_parsed_program(p)
  );
};

let tparse = (~todo=?, name, input, expected) =>
  name >:: wrap_todo(todo) @@ test_parse(name, input, expected);

/* Tests for constants, basic bindings, binops, and conditionals. */
let basic_functionality_tests = [
  t("forty", "let x = 40; x", "40"),
  t("neg", "-1073741824", "-1073741824"),
  t("hex", "0xff", "255"),
  t("hex_neg", "-0xff", "-255"),
  t("bin", "0b1010", "10"),
  t("bin_neg", "-0b1010", "-10"),
  t("oct", "0o77", "63"),
  t("oct_neg", "-0o77", "-63"),
  t("fals", "let x = false; x", "false"),
  t("tru", "let x = true; x", "true"),
  t(
    "complex1",
    "\n    let x = 2, y = 3, z = if (true) { 4 } else { 5 };\n    if (true) {\n      print(y)\n      y - (z + x)\n    } else {\n      print(8)\n      8\n    }\n    ",
    "3\n-3",
  ),
  t("complex2", "print(2 + 3)", "5\nvoid"),
  t("binop1", "2 + 2", "4"),
  t("binop2", "2 - 2", "0"),
  t("binop2.1", "2-2", "0"),
  t("binop2.2", "2 -
                 2", "0"),
  t("binop2.3", "2
                 - 2", "-2"),
  t("binop2.4", "- 2", "-2"),
  t("binop3", "2 - 4", "-2"),
  t("binop4", "2 * 3", "6"),
  t("binop5", "10 / 5", "2"),
  t("binop6", "9 % 5", "4"),
  te("division_by_zero", "let nine = 9; nine / 0", "division by zero"),
  te("modulo_by_zero", "9 % 0", "modulo by zero"),
  t("division1", "5 / 2", "5/2"),
  t("modulo1", "-17 % 4", "3"),
  t("modulo2", "17 % -4", "-3"),
  t("modulo3", "-17 % -4", "-1"),
  t("modulo4", "-17 % 17", "0"),
  t("modulo5", "17 % -17", "0"),
  t("modulo6", "17 % 17", "0"),
  t("and1", "true && true", "true"),
  t("and2", "true && false", "false"),
  t("and3", "false && true", "false"),
  t("and4", "false && false", "false"),
  t("or1", "true || true", "true"),
  t("or2", "true || false", "true"),
  t("or3", "false || true", "true"),
  t("or4", "false || false", "false"),
  t("land1", "1 & 1", "1"),
  t("land2", "1 & 0", "0"),
  t("land3", "0 & 1", "0"),
  t("land4", "0 & 0", "0"),
  t("lor1", "1 | 1", "1"),
  t("lor2", "1 | 0", "1"),
  t("lor3", "0 | 1", "1"),
  t("lor4", "0 | 0", "0"),
  t("lxor1", "1 ^ 1", "0"),
  t("lxor2", "1 ^ 0", "1"),
  t("lxor3", "0 ^ 1", "1"),
  t("lxor4", "0 ^ 0", "0"),
  t("lsl1", "7 << 1", "14"),
  t("lsl2", "0 << 1", "0"),
  t("lsr1", "7 >>> 1", "3"),
  t("lsr2", "0 >>> 1", "0"),
  t("asr1", "179 >> 1", "89"),
  t("asr2", "0 >> 1", "0"),
  t("comp1", "if (2 < 3) {true} else {false}", "true"),
  te("comp1e", "if (2 < 3) {true} else {3}", "type"),
  t("comp2", "if (2 <= 3) {true} else {false}", "true"),
  te("comp2e", "if (2 <= 3) {true} else {3}", "type"),
  t("comp3", "if (2 >= 3) {4} else {5}", "5"),
  t("comp4", "if (2 > 3) {4} else {5}", "5"),
  t("comp5", "if (2 < 3) {4} else {5}", "4"),
  t("comp6", "if (2 == 3) {8} else {9}", "9"),
  t("comp7", "if (2 == 2) {8} else {9}", "8"),
  t("comp8", "if (2 <= 2) {10} else {11}", "10"),
  t("comp9", "if (2 >= 2) {10} else {11}", "10"),
  t("comp10", "let x = 2, y = 4; (y - 2) == x", "true"),
  te("comp11", "true == 2", "has type Number but"),
  te("comp12", "2 == false", "has type Bool but"),
  t("comp13", "true == true", "true"),
  t("comp14", "true == false", "false"),
  t("comp15", "false == true", "false"),
  t("comp16", "false == false", "true"),
  t("comp17", "false isnt true", "true"),
  t("comp18", "4 isnt 1", "true"),
  t("comp19", "[1, 2] is [1, 2]", "false"),
  t("comp20", "[1, 2] isnt [1, 2]", "true"),
  // These are not optimized into the same instance (boxes are mutable)
  t("comp21", "[box(1)] is [box(1)]", "false"),
  t("comp22", "[box(1)] isnt [box(1)]", "true"),
  t("precedence1", "3 + 4 * 6", "27"),
  t("precedence2", "4 * 6 + 3", "27"),
  t("precedence3", "3 + 4 % 6", "7"),
  t("precedence4", "4 % 6 + 3", "7"),
  t("precedence5", "4 > 3 && 9 < 13", "true"),
  t("not1", "!true", "false"),
  t("not2", "!false", "true"),
  t("incr_1", "incr(2)", "3"),
  t("incr_2", "incr(5)", "6"),
  t("incr_3", "incr(-1)", "0"),
  t("decr_1", "decr(2)", "1"),
  t("decr_2", "decr(5)", "4"),
  t("decr_3", "decr(0)", "-1"),
  te("comp_bool1", "if (2 < true) {3} else {4}", "type"),
  te("comp_bool2", "if (2 > true) {3} else {4}", "type"),
  te("comp_bool3", "if (true >= 4) {3} else {4}", "type"),
  te("comp_bool4", "let x = true; if (x < 4) {3} else {5}", "type"),
  t("void", "{\"foo\";}", "void"),
  te("arith1", "2 + true", "type"),
  te("arith2", "true + 4", "type"),
  te("arith3", "false - 5", "type"),
  te("arith4", "4 - true", "type"),
  te("arith5", "let x = true; x * 4", "type"),
  te("arith6", "let x = false; 4 * x", "type"),
  te("if1", "if (2) {5} else {6}", "type"),
  te("if2", "let y = 0; if (y) {5} else {6}", "type"),
  te("if3", "if (decr(1)) {2} else {5}", "type"),
  t("if4", "if (false) 1 else if (false) 2 else {3}", "3"),
  t("if_one_sided", "if (3 < 4) print(5)", "5\nvoid"),
  t("if_one_sided2", "if (3 > 4) print(5)", "void"),
  t("int32_1", "42l", "42"),
  t("int64_1", "99999999999999999L", "99999999999999999"),
  t("int64_pun_1", "9999999 * 99999999", "999999890000001"),
  t("int64_pun_2", "-99999999 - 999999999", "-1099999998"),
  te("overflow1", "9223372036854775807 + 1", "overflow"),
  t("block_no_expression", "let f = () => { let x = 5 }; f()", "void"),
  /* Assertions */
  t("assert1", "assert true", "void"),
  t("assert2", "assert 3 + 3 == 6", "void"),
  te("assert3", "assert false", "Assertion error"),
  te("assert4", "assert 4 - 1 == 14", "Assertion error"),
  /* Failures */
  te("fail1", "ignore(fail \"boo\")", "Failure: boo"),
  te("fail2", "if (false) { 3 } else { fail \"boo\" }", "Failure: boo"),
  tfile("toplevel_statements", "toplevelStatements", "1\n2\n3\n4\n5\nfoo"),
  tfile(
    "unsafe_wasm_globals",
    "unsafeWasmGlobals",
    "42n\n42N\n42.0w\n42.0W\nvoid",
  ),
];

/* Tests for functions: basic, directly-recursive, and mutually-recursive. */
let function_tests = [
  tfile("fib1", "fib", "55"),
  tfile("fib2", "fib-better", "75025"),
  tfile("indirect", "indirect-tail", "10"),
  /* NOTE: This file also will test that we're doing tail calls
     and mutual recursion properly (should stack overflow otherwise) */
  /* Tests tail calls on only on one branch */
  tfile("one_branch_tail_call", "oneBranchTail", "[2]"),
  tfile("forward_decl", "forward-decl", "true"),
  /* This will test that we are doing tail calls for arbitrary-arity
     functions correctly */
  tfile("sinister_tail_call", "sinister-tail-call", "true"),
  tefile("fib_big", "too-much-fib", "overflow"),
  t("func_no_args", "let foo = (() => {print(5)});\nfoo()", "5\nvoid"),
  te("multi_bind", "let x = 2, y = x + 1; y", "Unbound value x"),
  t("multi_bind2", "let x = 2, y = 3; y", "3"),
  t("curried_func", "let add = a => b => a + b; add(2)(3)", "5"),
  te("unbound_fun", "2 + foo()", "unbound"),
  te("unbound_id_simple", "5 - x", "unbound"),
  te("unbound_id_let", "let x = x; 2 + 2", "unbound"),
  te(
    "shadow_multi",
    "let x = 12, x = 14; x",
    "Variable x is bound several times",
  ),
  t(
    "dup_func",
    "let rec foo = (() => {5});\nlet bar = (() => { 7 });\nlet rec foo = (() => {9});\nfoo()",
    "9",
  ),
  te("arity_1", "let foo = (() => {5});\nfoo(6)", "type"),
  te("arity_2", "let foo = ((x) => {x + 5});\nfoo()", "type"),
  te("arity_3", "let foo = ((x) => {x});\nfoo(1, 2, 3)", "type"),
  t("shorthand_1", "let foo = (x) => x; foo(1)", "1"),
  t("shorthand_2", "let foo = (x) => x + 3; foo(1)", "4"),
  t("shorthand_3", "let foo = x => x; foo(1)", "1"),
  t("shorthand_4", "let foo = x => x + 3; foo(1)", "4"),
  // Trailing commas
  t("fn_trailing_comma", "let testFn = (x, y,) => x + y; testFn(2, 3,)", "5"),
  t(
    "adt_trailing_comma",
    "enum Topping { Cheese, Pepperoni, Peppers, Pineapple, }
     enum Dough { WholeWheat, GlutenFree }
     enum Menu { Pizza(Topping,Dough,), Calzone(Topping,Dough,) }
     let item = Calzone(Peppers, WholeWheat,)
     item
    ",
    "<adt value>",
  ),
  t("lam_destructure_1", "((_) => 5)(\"foo\")", "5"),
  t("lam_destructure_2", "let foo = (_) => 5; foo(\"foo\")", "5"),
  t("lam_destructure_3", "(((a, b, c)) => a + b + c)((1, 2, 3))", "6"),
  t(
    "lam_destructure_4",
    "let foo = ((a, b, c)) => a + b + c; foo((1, 2, 3))",
    "6",
  ),
  t(
    "lam_destructure_5",
    "(((a, b, c), (x, y)) => a + b + c + x + y)((1, 2, 3), (4, 5))",
    "15",
  ),
  t(
    "lam_destructure_6",
    "let foo = ((a, b, c), (x, y)) => a + b + c + x + y; foo((1, 2, 3), (4, 5))",
    "15",
  ),
  t(
    "lam_destructure_7",
    "(((a, b, (c, d))) => a + b + c + d)((1, 2, (3, 4)))",
    "10",
  ),
  t(
    "lam_destructure_8",
    "let foo = ((a, b, (c, d))) => a + b + c + d; foo((1, 2, (3, 4)))",
    "10",
  ),
  t("lambda_1", "print((x) => {x})", "<lambda>\nvoid"),
  t("app_1", "((x) => {x})(1)", "1"),
  t(
    "letrec_1",
    "let rec x = ((n) => {if (n > 3) {n} else {x(n + 2)}}),\n                        y = ((n) => {x(n + 1)});\n                 y(2)",
    "5",
  ),
  /* Check that recursion is order-independent */
  t(
    "letrec_2",
    "let rec y = ((n) => {x(n + 1)}),\n                        x = ((n) => {if (n > 3) {n} else {x(n + 2)}});\n                 y(2)",
    "5",
  ),
  t(
    "let_1",
    "let rec x = ((n) => {n + 1}),\n                     y = (() => x(3)),\n                     z = ((n) => {x(n) + y()});\n               z(5)",
    "10",
  ),
  te(
    "let_norec_1",
    "let x = ((n) => {if (n > 3) {n} else {x(n + 2)}}),\n                        y = ((n) => {x(n + 1)});\n                 y(2)",
    "Unbound value x.",
  ),
  te(
    "lambda_dup_args",
    "((x, y, x) => {5})",
    "Variable x is bound several times",
  ),
  te("lambda_arity_1", "((x) => {6})()", "type"),
  te("lambda_arity_2", "((x) => {5})(1, 2)", "type"),
  te(
    "letrec_nonstatic_const",
    "let rec x = 5; x",
    "let rec may only be used with recursive function definitions",
  ),
  te(
    "letrec_nonstatic_same",
    "let x = x; x",
    "Unbound value x.\n       Hint: You are probably missing the `rec' keyword on line 1.",
  ),
  te(
    "letrec_nonstatic_other",
    "let rec x = ((z) => {z + 1}), y = x; y(2)",
    "let rec may only be used with recursive function definitions",
  ),
  te("nonfunction_1", "let x = 5; x(3)", "type"),
  t("lambda_pat_any", "let x = (_) => 5; x(\"foo\")", "5"),
  te("unknown_attribute", "@unknown let x = () => 5", "Unknown attribute"),
];

let mylist = "[1, 2, 3]";

let tuple_tests = [
  t("print_tup", "print((1, 2))", "(1, 2)\nvoid"),
  t("big_tup", "print((1, 2, 3, 4))", "(1, 2, 3, 4)\nvoid"),
  t("big_tup_access", "let (a, b, c, d) = (1, 2, 3, 4); c", "3"),
  t("nested_tup_1", "let (a, b) = ((1, 2), (3, 4)); a", "(1, 2)"),
  t("nested_tup_2", "let (a, b) = ((1, 2), (3, 4)); let (c, d) = b; d", "4"),
  t("nested_tup_3", "let (x, y) = ((1, 2), (3, 4)); let (a, b) = y; a", "3"),
  t("no_singleton_tup", "(1)", "1"),
  // trailing commas
  t("tup1_trailing", "(1, 2, 3,)", "(1, 2, 3)"),
  t("tup1_trailing_space", "(1, 2, 3, )", "(1, 2, 3)"),
  te("invalid_empty_trailing", "(,)", "Error: Syntax error"),
];

let list_tests = [
  t("list1", "[1, 2, 3]", "[1, 2, 3]"),
  t("list2", "[]", "[]"),
  te("list_heterogeneous", "[1, false, 2]", "type"),
  t("list_spread", "let a = [3, 4]; [1, 2, ...a]", "[1, 2, 3, 4]"),
  te(
    "invalid_list_no_comma_before_spread",
    "let a = [3, 4]; [1, 2 ...a]",
    "Error: Syntax error",
  ),
  // trailing commas
  t("list1_trailing", "[1, 2, 3,]", "[1, 2, 3]"),
  t("list1_trailing_space", "[1, 2, 3, ]", "[1, 2, 3]"),
  te("invalid_empty_trailing", "[,]", "Error: Syntax error"),
  te(
    "invalid_list_spread_trailing",
    "let a = [3, 4]; [1, 2, ...a,]",
    "Error: Syntax error",
  ),
];

let array_tests = [
  t("array1", "[> 1, 2, 3]", "[> 1, 2, 3]"),
  t("array2", "[>]", "[> ]"),
  t("array3", "[>\n1, 2, 3]", "[> 1, 2, 3]"),
  te("array_error", "[> 1, false, 2]", "has type Bool but"),
  t("array_access", "let x = [> 1, 2, 3]; x[0]", "1"),
  t("array_access2", "let x = [> 1, 2, 3]; x[1]", "2"),
  t("array_access3", "let x = [> 1, 2, 3]; x[2]", "3"),
  t("array_access4", "let x = [> 1, 2, 3]; x[-2]", "2"),
  t("array_access5", "let x = [> 1, 2, 3]; x[-3]", "1"),
  te(
    "array_access_err",
    "let x = [> 1, 2, 3]; x[3]",
    "array index out of bounds",
  ),
  te(
    "array_access_err2",
    "let x = [> 1, 2, 3]; x[-4]",
    "array index out of bounds",
  ),
  te(
    "array_access_err3",
    "let x = [> 1, 2, 3]; x[99]",
    "array index out of bounds",
  ),
  te(
    "array_access_err4",
    "let x = [> 1, 2, 3]; x[-99]",
    "array index out of bounds",
  ),
  te(
    "array_access_err5",
    "let x = [> 1, 2, 3]; x[false]",
    "has type Bool but",
  ),
  t("array_set", "let x = [> 1, 2, 3]; x[0] := 4; x", "[> 4, 2, 3]"),
  t("array_set2", "let x = [> 1, 2, 3]; x[-2] := 4; x", "[> 1, 4, 3]"),
  te(
    "array_set_err",
    "let x = [> 1, 2, 3]; x[-2] := false",
    "has type Bool but",
  ),
  te(
    "array_set_err2",
    "let x = [> 1, 2, 3]; x[-12] := 4",
    "array index out of bounds",
  ),
  te(
    "array_type",
    "let x = [> true, false, false]; x[1] + 3",
    "has type Bool but",
  ),
  te(
    "array_type2",
    "let x = [> true, false, false]; (x[1] := true) + 3",
    "has type Bool but",
  ),
  // trailing commas
  t("array1_trailing", "[> 1, 2, 3,]", "[> 1, 2, 3]"),
  t("array1_trailing_space", "[> 1, 2, 3, ]", "[> 1, 2, 3]"),
  te("invalid_empty_trailing", "[> ,]", "Error: Syntax error"),
];

let record_tests = [
  t("record_1", "record Rec {foo: Number}; {foo: 4}", "<record value>"),
  t(
    "record_2",
    "export record Rec {foo: Number}; {foo: 4}",
    "{\n  foo: 4\n}",
  ),
  t(
    "record_multiple",
    "export record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true}",
    "{\n  foo: 4,\n  bar: \"boo\",\n  baz: true\n}",
  ),
  t(
    "record_pun",
    "export record Rec {foo: Number}; let foo = 4; {foo}",
    "{\n  foo: 4\n}",
  ),
  t(
    "record_pun_multiple",
    "export record Rec {foo: Number, bar: Bool}; let foo = 4; let bar = false; {foo, bar}",
    "{\n  foo: 4,\n  bar: false\n}",
  ),
  t(
    "record_pun_mixed",
    "export record Rec {foo: Number, bar: Bool}; let foo = 4; {foo, bar: false}",
    "{\n  foo: 4,\n  bar: false\n}",
  ),
  t(
    "record_pun_mixed_2",
    "export record Rec {foo: Number, bar: Bool}; let bar = false; {foo: 4, bar}",
    "{\n  foo: 4,\n  bar: false\n}",
  ),
  te("record_err_1", "{foo: 4}", "Unbound record label foo"),
  te(
    "record_err_2",
    "record Rec {foo: Number}; {foo: 4, bar: 4}",
    "Unbound record label bar",
  ),
  t(
    "record_get_1",
    "record Rec {foo: Number}; let bar = {foo: 4}; bar.foo",
    "4",
  ),
  t("record_get_2", "record Rec {foo: Number}; {foo: 4}.foo", "4"),
  t(
    "record_get_multiple",
    "record Rec {foo: Number, bar: Number}; let x = {foo: 4, bar: 9}; x.foo + x.bar",
    "13",
  ),
  t(
    "record_get_multilevel",
    "record Rec1 {foo: Number, bar: Number}; record Rec2 {baz: Rec1}; let x = {baz: {foo: 4, bar: 9}}; x.baz.bar",
    "9",
  ),
  te(
    "record_get_err",
    "record Rec1 {foo: Number, bar: Number}; let x = {foo: 4, bar: 9}; x.baz",
    "The field baz does not belong to type Rec1",
  ),
  /* mutable record fields */
  t(
    "record_mut_1",
    "record Rec {foo: Number, mut bar: String, baz: Bool}; let a = {foo: 4, bar: \"boo\", baz: true}; a.bar = \"hoo\"; a.bar",
    "hoo",
  ),
  te(
    "record_mut_1",
    "record Rec {foo: Number, mut bar: String, baz: Bool}; let a = {foo: 4, bar: \"boo\", baz: true}; a.foo = 5; a.foo",
    "The record field foo is not mutable",
  ),
  /* record destructured assignment */
  t(
    "record_destruct_1",
    "record Rec {foo: Number, bar: String, baz: Bool}; let { foo, _ } = {foo: 4, bar: \"boo\", baz: true}; foo",
    "4",
  ),
  t(
    "record_destruct_2",
    "record Rec {foo: Number, bar: String, baz: Bool}; let { bar, _ } = {foo: 4, bar: \"boo\", baz: true}; bar",
    "boo",
  ),
  t(
    "record_destruct_3",
    "record Rec {foo: Number, bar: Number, baz: Number}; let { foo, bar, _ } = {foo: 4, bar: 5, baz: 6}; foo + bar",
    "9",
  ),
  t(
    "record_destruct_4",
    "record Rec {foo: Number, bar: Number, baz: Number}; let { foo, bar, baz } = {foo: 4, bar: 5, baz: 6}; foo + bar + baz",
    "15",
  ),
  t(
    "record_destruct_deep",
    "record Rec {foo: Number}; record Rec2 {bar: Rec}; let { bar: { foo } } = {bar: {foo: 4}}; foo",
    "4",
  ),
  te(
    "record_destruct_deep_alias",
    "record Rec {foo: Number}; record Rec2 {bar: Rec}; let { bar: { foo } } = {bar: {foo: 4}}; bar",
    "Unbound value bar",
  ),
  // Record trailing commas
  t(
    "record_definition_trailing",
    "export record Rec {foo: Number,}; {foo: 4}",
    "{\n  foo: 4\n}",
  ),
  t(
    "record_value_trailing",
    "export record Rec {foo: Number}; {foo: 4,}",
    "{\n  foo: 4\n}",
  ),
  t(
    "record_both_trailing",
    "export record Rec {foo: Number,}; {foo: 4,}",
    "{\n  foo: 4\n}",
  ),
  t(
    "record_multiple_fields_definition_trailing",
    "export record Rec {foo: Number, bar: String, baz: Bool,}; {foo: 4, bar: \"boo\", baz: true}",
    "{\n  foo: 4,\n  bar: \"boo\",\n  baz: true\n}",
  ),
  t(
    "record_multiple_fields_value_trailing",
    "export record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true,}",
    "{\n  foo: 4,\n  bar: \"boo\",\n  baz: true\n}",
  ),
  t(
    "record_multiple_fields_both_trailing",
    "export record Rec {foo: Number, bar: String, baz: Bool,}; {foo: 4, bar: \"boo\", baz: true,}",
    "{\n  foo: 4,\n  bar: \"boo\",\n  baz: true\n}",
  ),
  t(
    "record_pun_trailing",
    "export record Rec {foo: Number}; let foo = 4; {foo,}",
    "{\n  foo: 4\n}",
  ),
  t(
    "record_pun_multiple_trailing",
    "export record Rec {foo: Number, bar: Bool}; let foo = 4; let bar = false; {foo, bar,}",
    "{\n  foo: 4,\n  bar: false\n}",
  ),
  t(
    "record_pun_mixed_trailing",
    "export record Rec {foo: Number, bar: Bool}; let foo = 4; {foo, bar: false,}",
    "{\n  foo: 4,\n  bar: false\n}",
  ),
  t(
    "record_pun_mixed_2_trailing",
    "export record Rec {foo: Number, bar: Bool}; let bar = false; {foo: 4, bar,}",
    "{\n  foo: 4,\n  bar: false\n}",
  ),
];

let stdlib_tests = [
  t("stdlib_cons", mylist, "[1, 2, 3]"),
  t("stdlib_equal_1", "import * from \"list\"; (1, 2) is (1, 2)", "false"),
  t(
    "stdlib_equal_2",
    "import * from \"pervasives\"; (1, 2) == (1, 2)",
    "true",
  ),
  t(
    "stdlib_equal_3",
    "import * from \"list\"; [1, 2, 3] == [1, 2, 3]",
    "true",
  ),
  t("stdlib_equal_4", "import * from \"list\"; 1 == 1", "true"),
  t("stdlib_equal_5", "import * from \"list\"; 1 == 2", "false"),
  t("stdlib_equal_6", "import * from \"list\"; true == true", "true"),
  t("stdlib_equal_7", "import * from \"list\"; true == false", "false"),
  t("stdlib_equal_8", "import * from \"list\"; [>] == [>]", "true"),
  t("stdlib_equal_9", "import * from \"list\"; [>] == [> 1]", "false"),
  t("stdlib_equal_10", "import * from \"list\"; [> 1] == [> 1]", "true"),
  t("stdlib_equal_11", "import * from \"list\"; [> 1, 2] == [> 1]", "false"),
  t(
    "stdlib_equal_12",
    "import * from \"list\"; [> 1, 2, 3, 4] == [> 1, 2, 3, 4]",
    "true",
  ),
  t("stdlib_equal_13", "import * from \"list\"; \"\" == \"\"", "true"),
  t("stdlib_equal_14", "import * from \"list\"; \" \" == \"\"", "false"),
  t("stdlib_equal_15", "import * from \"list\"; \"f\" == \"\"", "false"),
  t("stdlib_equal_16", "import * from \"list\"; \"foo\" == \"foo\"", "true"),
  t(
    "stdlib_equal_17",
    "import * from \"list\"; \"foo 😂\" == \"foo 😂\"",
    "true",
  ),
  t(
    "stdlib_equal_18",
    "import * from \"list\"; \"foo 😂\" == \"foo 🙄\"",
    "false",
  ),
  t(
    "stdlib_equal_19",
    "record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true} == {foo: 4, bar: \"boo\", baz: true}",
    "true",
  ),
  t(
    "stdlib_equal_20",
    "record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true} == {foo: 4, bar: \"bar\", baz: true}",
    "false",
  ),
  t(
    "stdlib_equal_21",
    "record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true} == {foo: 78, bar: \"boo\", baz: true}",
    "false",
  ),
  t(
    "stdlib_equal_22",
    "record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true} == {foo: 4, bar: \"boo\", baz: false}",
    "false",
  ),
  tfile("recursive_equal_box", "recursive-equal-box", "void"),
  tfile("recursive_equal_mut", "recursive-equal-mut", "void"),
  /* te "stdlib_sum_err" "import * from \"list\"; sum([true, false])" "This expression has type Bool but"; */
  te(
    "stdlib_length_err",
    "import * from \"list\"; length(true)",
    "This expression has type Bool but",
  ),
  te(
    "stdlib_reverse_err",
    "import * from \"list\"; reverse(1)",
    "This expression has type Number but",
  ),
  tlib("array.test"),
  tlib("list.test"),
  tlib("option.test"),
  tlib("hash.test"),
  tlib("int64.test"),
  tlib("int32.test"),
  tlib("float32.test"),
  tlib("float64.test"),
  tlib("wasmi32.test"),
  tlib("wasmi64.test"),
  tlib("wasmf32.test"),
  tlib("wasmf64.test"),
  tlib("range.test"),
  tlib("string.test"),
  tlib("sys.file.test"),
  tlib("map.test"),
  tlib("set.test"),
  tlib("result.test"),
  tlib("queue.test"),
  tlib("stack.test"),
  tlib(~returns="", ~code=5, "sys.process.test"),
];

let box_tests = [
  t(
    "box1",
    "let b = box(4);\n            {\n              unbox(b)\n            }",
    "4",
  ),
  t(
    "box2",
    "let b = box((4, (5, 6)));\n            {\n              unbox(b)\n            }",
    "(4, (5, 6))",
  ),
  t(
    "box3",
    "let b = box(box(4));\n            {\n              unbox(unbox(b))\n            }",
    "4",
  ),
  t(
    "box4",
    "let b = box(4);\n            {\n              b := 3;\n              unbox(b)\n            }",
    "3",
  ),
  t(
    "box5",
    "let b = box(4);\n            {\n              b := unbox(b) - 1;\n              unbox(b)\n            }",
    "3",
  ),
  t("test_set_extra1", "box(1) := 2", "2"),
  tfile("counter-box", "counter-box", "1\n2\n3\nvoid"),
  te("test_unbox_err", "unbox(5)", "Box"),
  te(
    "test_box_typing",
    "unbox(box(false)) + 4",
    "expression has type Bool but",
  ),
  /* Operations on Box<Number> */
  t("box_addition1", "let b = box(4); b := unbox(b) + 19", "23"),
  t("box_addition2", "let b = box(4); b := unbox(b) + 19; unbox(b)", "23"),
  t("box_subtraction1", "let b = box(4); b := unbox(b) - 19", "-15"),
  t(
    "box_subtraction2",
    "let b = box(4); b := unbox(b) - 19; unbox(b)",
    "-15",
  ),
  t("box_multiplication1", "let b = box(4); b := unbox(b) * 19", "76"),
  t(
    "box_multiplication2",
    "let b = box(4); b := unbox(b) * 19; unbox(b)",
    "76",
  ),
  t("box_division1", "let b = box(76); b := unbox(b) / 19", "4"),
  t("box_division2", "let b = box(76); b := unbox(b) / 19; unbox(b)", "4"),
];

let let_mut_tests = [
  t("let-mut1", "let mut b = 4;b", "4"),
  t("let-mut2", "let mut b = (4, (5, 6));b", "(4, (5, 6))"),
  t("let-mut3", "let mut b = box(4);unbox(b)", "4"),
  t("let-mut4", "let mut b = 4;b = 3;b", "3"),
  t("let-mut5", "let mut b = 4;b = b - 1;b", "3"),
  tfile("counter-mut", "counter-mut", "1\n2\n3\nvoid"),
  te(
    "test_mut_typing",
    "let mut a = false; a + 4",
    "expression has type Bool but",
  ),
  // let mut destructure tests
  t(
    "let-mut_destructure1",
    "let mut (x, y, z) = (5, false, \"foo\"); x = 6; y = true; z = \"bar\"; print(x); print(y); print(z)",
    "6\ntrue\nbar\nvoid",
  ),
  t(
    "let-mut_destructure2",
    "{let mut (x, y, z) = (5, false, \"foo\"); x = 6; y = true; z = \"bar\"; print(x); print(y); print(z)}",
    "6\ntrue\nbar\nvoid",
  ),
  t(
    "let-mut_destructure3",
    "record Rec {foo: Number, bar: Bool}; let mut {foo, bar} = {foo: 5, bar: false}; foo = 6; bar = true; print(foo); print(bar)",
    "6\ntrue\nvoid",
  ),
  t(
    "let-mut_destructure4",
    "record Rec {foo: Number, bar: Bool}; {let mut {foo, bar} = {foo: 5, bar: false}; foo = 6; bar = true; print(foo); print(bar)}",
    "6\ntrue\nvoid",
  ),
  // not-mut let errors
  te(
    "let-mut_err1",
    "let x = 5; x = 6",
    "The identifier x was not declared mutable",
  ),
  te(
    "let-mut_err2",
    "let (x, y) = (1, 2); x = 6",
    "The identifier x was not declared mutable",
  ),
  te(
    "let-mut_err3",
    "let (x, y) = (1, 2); y = 6",
    "The identifier y was not declared mutable",
  ),
  te(
    "let-mut_err4",
    "record Rec {foo: Number, bar: Bool}; let {foo, bar} = {foo: 1, bar: false}; foo = 6",
    "The identifier foo was not declared mutable",
  ),
  /* Operations on mutable `Number`s */
  t("let-mut_addition1", "let mut b = 4; b = b + 19", "23"),
  t("let-mut_addition2", "let mut b = 4; b = b + 19; b", "23"),
  t("let-mut_addition3", "let mut b = 4; b += 19; b", "23"),
  t("let-mut_subtraction1", "let mut b = 4; b = b - 19", "-15"),
  t("let-mut_subtraction2", "let mut b = 4; b = b - 19; b", "-15"),
  t("let-mut_subtraction3", "let mut b = 4; b -= 19; b", "-15"),
  t("let-mut_multiplication1", "let mut b = 4; b = b * 19", "76"),
  t("let-mut_multiplication2", "let mut b = 4; b = b * 19; b", "76"),
  t("let-mut_multiplication3", "let mut b = 4; b *= 19; b", "76"),
  t("let-mut_division1", "let mut b = 76; b = b / 19", "4"),
  t("let-mut_division2", "let mut b = 76; b = b / 19; b", "4"),
  t("let-mut_division3", "let mut b = 76; b /= 19; b", "4"),
];

let loop_tests = [
  t(
    "loop1",
    "let b = box(3);\n            {\n              while (unbox(b) > 0) {\n                b := unbox(b) - 1\n              };\n              unbox(b)\n            }",
    "0",
  ),
  t(
    "loop2",
    "let b = box(12);\n             let count = box(0);\n            {\n              while (unbox(b) > 0) {\n                b := unbox(b) - 1;\n                count := unbox(count) + 1\n              };\n              unbox(count)\n            }",
    "12",
  ),
  t("loop3", "let mut b = 3; while (b > 0) { b = b - 1 }; b ", "0"),
  t(
    "loop4",
    "let mut b = 12; let mut count = 0; while (b > 0) { b = b - 1; count = count + 1 }; count",
    "12",
  ),
  t(
    "loop5",
    "let mut b = 12; let mut count = 0; while ((b -= 1) >= 0) { count += 1 }; count",
    "12",
  ),
];

let oom = [
  tgcerr("oomgc1", 70, "(1, (3, 4))", "Out of memory"),
  tgc("oomgc2", 356, "(1, (3, 4))", "(1, (3, 4))"),
  tgc("oomgc3", 256, "(3, 4)", "(3, 4)"),
];

let gc = [
  tgc(
    "gc1",
    512,
    "let f = (() => (1, 2));\n       {\n         f();\n         f();\n         f();\n         f()\n       }",
    "(1, 2)",
  ),
  /* Test that cyclic tuples are GC'd properly */
  tgc(
    "gc2",
    2560,
    "enum Opt<x> { None, Some(x) };\n     let f = (() => {\n      let x = (box(None), 2);\n      let (fst, _) = x\n      fst := Some(x)\n      });\n      {\n        f();\n        let x = (1, 2);\n        x\n      }",
    "(1, 2)",
  ),
  tgcfile("fib_gc_err", 1024, "fib-gc", "Out of memory"),
  tgcfile("fib_gc", 3424, "fib-gc", "832040"),
  /* tgcfile "fib_gc_bigger" 3072 "fib-gc" "832040";
     tgcfile "fib_gc_biggest" 512 "fib-gc" "832040"; */
  /* I've manually tested this test, but see TODO for automated testing */
  /* tgcfile ~todo:"Need to figure out how to disable dead assignment elimination to make sure this test is actually checking what we want" "sinister_gc" 3072 "sinister-tail-call-gc" "true"; */
  tgcfile("long_lists", 70000, "long_lists", "true"),
];

let match_tests = [
  /* Pattern matching on tuples */
  t("tuple_match_1", "match ((1,)) { (a,) => a }", "1"),
  t("tuple_match_2", "match ((1, 2, 3)) { (a, b, c) => a + b + c }", "6"),
  t(
    "tuple_match_3",
    "match ((1, \"boop\", false)) { (a, b, c) => (a, b, c) }",
    "(1, \"boop\", false)",
  ),
  t(
    "tuple_match_deep",
    "match ((1, (4, 5), 3)) { (a, (d, e), c) => a + c + d + e }",
    "13",
  ),
  t(
    "tuple_match_deep2",
    "match ((1, (2, (3, (4, 5, (6, 7)))))) { (a, (b, (c, (d, e, (f, g))))) => a + b + c + d + e + f + g }",
    "28",
  ),
  t(
    "tuple_match_deep3",
    "match ((1, [])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
    "1",
  ),
  t(
    "tuple_match_deep4",
    "match ((1, [2])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
    "3",
  ),
  t(
    "tuple_match_deep5",
    "match ((1, [4, 5])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
    "10",
  ),
  t(
    "tuple_match_deep6",
    "match ((1, [4, 5, 6])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
    "16",
  ),
  t(
    "tuple_match_deep7",
    "match ((1, [4, 5, 6, 7])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
    "999",
  ),
  /* Pattern matching on records */
  t(
    "record_match_1",
    "record Rec {foo: Number, bar: String, baz: Bool}; match ({foo: 4, bar: \"boo\", baz: true}) { { foo, _ } => foo }",
    "4",
  ),
  t(
    "record_match_2",
    "record Rec {foo: Number, bar: String, baz: Bool}; match ({foo: 4, bar: \"boo\", baz: true}) { { bar, _ } => bar }",
    "boo",
  ),
  t(
    "record_match_3",
    "record Rec {foo: Number, bar: Number, baz: Number}; match ({foo: 4, bar: 5, baz: 6}) { { foo, bar, _ } => foo + bar }",
    "9",
  ),
  t(
    "record_match_4",
    "record Rec {foo: Number, bar: Number, baz: Number}; match ({foo: 4, bar: 5, baz: 6}) { { foo, bar, baz } => foo + bar + baz}",
    "15",
  ),
  t(
    "record_match_deep",
    "record Rec {foo: Number}; record Rec2 {bar: Rec}; match ({bar: {foo: 4}}) { { bar: { foo } } => foo }",
    "4",
  ),
  te(
    "record_match_deep_alias",
    "record Rec {foo: Number}; record Rec2 {bar: Rec}; match ({bar: {foo: 4}}) { { bar: { foo } } => bar }",
    "Unbound value bar",
  ),
  /* Pattern matching on ADTs */
  t(
    "adt_match_1",
    "match ([]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
    "0",
  ),
  t(
    "adt_match_2",
    "match ([2]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
    "2",
  ),
  t(
    "adt_match_3",
    "match ([4, 5]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
    "9",
  ),
  t(
    "adt_match_4",
    "match ([4, 5, 6]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
    "15",
  ),
  t(
    "adt_match_5",
    "match ([4, 5, 6, 7]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
    "999",
  ),
  t(
    "adt_match_deep",
    "record Rec {foo: Number}; match ([{foo: 5}]) { [] => 999, [{foo}, ..._] => foo }",
    "5",
  ),
  // Guarded branches
  t(
    "guarded_match_1",
    "match ((1, 2, 3)) { (a, b, c) when a == 1 => 42, _ => 99 }",
    "42",
  ),
  t(
    "guarded_match_2",
    "match ((2, 2, 3)) { (a, b, c) when a == 1 => 42, _ => 99 }",
    "99",
  ),
  t(
    "guarded_match_3",
    "match ((2, 2, 3)) { (a, b, c) when (a == 2) && (c == 3) => 42, _ => 99 }",
    "42",
  ),
  t(
    "guarded_match_4",
    "match ((8, 2, 3)) { (a, b, c) when (a == 2) && (c == 3) => 42, _ => 99 }",
    "99",
  ),
  t(
    "guarded_match_5",
    "enum ADT { Foo((String, Number)), Bar }
     match (Foo((\"abcd\", 4))) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }",
    "42",
  ),
  t(
    "guarded_match_6",
    "enum ADT { Foo((String, Number)), Bar }
     match (Foo((\"abcd\", 3))) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }",
    "89",
  ),
  t(
    "guarded_match_7",
    "enum ADT { Foo((String, Number)), Bar }
     match (Foo((\"wxyz\", 4))) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }",
    "99",
  ),
  t(
    "guarded_match_8",
    "enum ADT { Foo((String, Number)), Bar }
     match (Foo((\"wxyz\", 15))) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }",
    "89",
  ),
  t(
    "guarded_match_9",
    "enum ADT { Foo((String, Number)), Bar }
     match (Bar) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }",
    "79",
  ),
  // Constant patterns
  t(
    "constant_match_1",
    "match (1/3) { 5 => false, 2/6 => true, _ => false }",
    "true",
  ),
  t(
    "constant_match_2",
    "match ((\"foo\", 5, false)) { (\"bar\", 5, false) => false, (\"foo\", _, true) => false, (\"foo\", _, false) => true, _ => false }",
    "true",
  ),
  t(
    "constant_match_3",
    "match (\"foo\") { \"foo\" when false => false, \"foo\" when true => true, _ => false }",
    "true",
  ),
  t(
    "constant_match_4",
    "match ((\"foo\", 5)) { (\"foo\", n) when n == 7 => false, (\"foo\", 9) when true => false, (\"foo\", n) when n == 5 => true, _ => false }",
    "true",
  ),
  tfile("mixed_matching", "mixedPatternMatching", "true"),
];

let import_tests = [
  /* import * tests */
  t(
    "import_all",
    "import * from \"exportStar\"; {print(x); print(y(4)); z}",
    "5\n4\nfoo",
  ),
  t(
    "import_all_except",
    "import * except {y} from \"exportStar\"; {print(x); z}",
    "5\nfoo",
  ),
  t(
    "import_all_except_multiple",
    "import * except {x, y} from \"exportStar\"; z",
    "foo",
  ),
  t(
    "import_all_constructor",
    "import * from \"tlists\"; Cons(2, Empty)",
    "Cons(2, Empty)",
  ),
  t(
    "import_all_except_constructor",
    "import * except {Cons} from \"tlists\"; Empty",
    "Empty",
  ),
  t(
    "import_all_except_multiple_constructor",
    "import * except {Cons, append} from \"tlists\"; sum(Empty)",
    "0",
  ),
  t(
    "import_with_export_multiple",
    "import * from \"sameExport\"; foo()",
    "6",
  ),
  /* import * errors */
  te(
    "import_all_except_error",
    "import * except {y} from \"exportStar\"; {print(x); print(y); z}",
    "Unbound value y",
  ),
  te(
    "import_all_except_multiple_error",
    "import * except {x, y} from \"exportStar\"; {print(x); z}",
    "Unbound value x",
  ),
  te(
    "import_all_except_multiple_error2",
    "import * except {x, y} from \"exportStar\"; {print(x); print(y); z}",
    "Unbound value x",
  ),
  te(
    "import_all_except_error_constructor",
    "import * except {Cons} from \"tlists\"; Cons(2, Empty)",
    "Unbound value Cons",
  ),
  te(
    "import_all_except_multiple_error_constructor",
    "import * except {Cons, append} from \"tlists\"; append(Empty, Empty)",
    "Unbound value append",
  ),
  te(
    "import_all_except_multiple_error2_constructor",
    "import * except {Cons, append} from \"tlists\"; let x = Cons(2, Empty); append(x, Empty)",
    "Unbound value Cons",
  ),
  /* import {} tests */
  t("import_some", "import {x} from \"exportStar\"; x", "5"),
  t("import_some_multiple", "import {x, y} from \"exportStar\"; y(x)", "5"),
  t(
    "import_some_constructor",
    "import {Cons, Empty} from \"tlists\"; Cons(5, Empty)",
    "Cons(5, Empty)",
  ),
  t(
    "import_some_mixed",
    "import {Cons, Empty, sum} from \"tlists\"; sum(Cons(5, Empty))",
    "5",
  ),
  t("import_alias", "import {x as y} from \"exportStar\"; y", "5"),
  t(
    "import_alias_multiple",
    "import {x as y, y as x} from \"exportStar\"; x(y)",
    "5",
  ),
  t(
    "import_alias_constructor",
    "import {Empty as None, sum} from \"tlists\"; sum(None)",
    "0",
  ),
  t(
    "import_alias_multiple_constructor",
    "import {Cons as Add, Empty as None, sum} from \"tlists\"; sum(Add(1, None))",
    "1",
  ),
  /* import {} errors */
  te(
    "import_some_error",
    "import {a} from \"exportStar\"; a",
    "Export \"a\" was not found in \"exportStar\"",
  ),
  te(
    "import_some_error2",
    "import {x, a} from \"exportStar\"; a",
    "Export \"a\" was not found in \"exportStar\"",
  ),
  te(
    "import_some_error3",
    "import {Foo} from \"exportStar\"; a",
    "Export \"Foo\" was not found in \"exportStar\"",
  ),
  te(
    "import_some_error3",
    "import {x, Foo} from \"exportStar\"; a",
    "Export \"Foo\" was not found in \"exportStar\"",
  ),
  /* import module tests */
  t("import_module", "import Foo from \"exportStar\"; Foo.x", "5"),
  t("import_module2", "import Foo from \"exportStar\"; Foo.y(Foo.x)", "5"),
  /* import module errors */
  te(
    "import_module_error",
    "import Foo from \"exportStar\"; Foo.foo",
    "Unbound value foo in module Foo",
  ),
  /* import well-formedness errors */
  te(
    "import_alias_illegal_renaming",
    "import {Cons as cons, Empty} from \"list\"; cons(3, Empty)",
    "Alias 'cons' should have proper casing",
  ),
  te(
    "import_alias_illegal_renaming2",
    "import {sum as Sum, Empty} from \"list\"; sum(Empty)",
    "Alias 'Sum' should have proper casing",
  ),
  te(
    "import_module_illegal_name",
    "import foo from \"list\";",
    "Module 'foo' should have an uppercase name",
  ),
  te(
    "import_module_not_external",
    "import Foo.Foo from \"list\";",
    "Module name 'Foo.Foo' should contain only one module.",
  ),
  te(
    "import_value_not_external",
    "import {foo as Foo.foo} from \"list\";",
    "Alias 'Foo.foo' should be at most one level deep",
  ),
  /* import multiple modules tests */
  t(
    "import_muliple_modules",
    "import * from \"tlists\"; import * from \"exportStar\"; Cons(x, Empty)",
    "Cons(5, Empty)",
  ),
  /* import same module tests */
  t(
    "import_same_module_unify",
    "import * from \"tlists\"; import TList from \"tlists\"; Cons(5, TList.Empty)",
    "Cons(5, Empty)",
  ),
  t(
    "import_same_module_unify2",
    "import *, TList from \"tlists\"; Cons(5, TList.Empty)",
    "Cons(5, Empty)",
  ),
  /* import filepath tests */
  t("import_relative_path", "import * from \"./exportStar\"; x", "5"),
  t(
    "import_relative_path2",
    "import * from \"../test-libs/exportStar\"; x",
    "5",
  ),
  t("import_relative_path3", "import * from \"nested/nested\"; j", "j"),
  te(
    "import_missing_file",
    "import * from \"foo\"; 2",
    "Missing file for module foo",
  ),
  /* Unbound module tests */
  te(
    "test_unbound_module",
    "String.concat(\"hello \", \"world\")",
    "Unbound module String",
  ),
  /* Misc import tests */
  te(
    "test_bad_import",
    "{let x = (1, 2); import * from \"tlists\"; x}",
    "error",
  ),
  tfile("test_file_same_name", "list", "OK\nvoid"),
  t(
    "annotation_across_import",
    "import TList, { Empty } from \"tlists\"; let foo : TList.TList<String> = Empty; foo",
    "Empty",
  ),
];

/* Note that optimizations are on by default, so all of the above tests
   check that it works correctly as well */
let optimization_tests = [
  /* note on resolve-scope test: (tags are not checked) */
  t(
    "trs1",
    "let f1 = ((x, y) => {x}),\n         f2 = ((x, y) => {y});\n       f1(1, 2)",
    "1",
  ),
  t(
    "regression_no_elim_impure_call",
    "let foo = (f) => { let g = f(5); 5 }; foo(print)",
    "5\n5",
  ),
  tfinalanf(
    "test_dead_branch_elimination_1",
    "{ if (true) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  ),
  tfinalanf(
    "test_dead_branch_elimination_2",
    "{ if (false) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(5L))),
      ),
    ),
  ),
  tfinalanf(
    "test_dead_branch_elimination_3",
    "{ let x = true; if (x) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  ),
  tfinalanf(
    "test_dead_branch_elimination_4",
    "{let x = if (true) {4} else {5}; x}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  ),
  t(
    "test_dead_branch_elimination_5",
    "\n      let x = box(1);\n      let y = box(2);\n      let z =\n        if (true) {\n          x := 3;\n          y := 4\n        } else {\n          x := 5;\n          y := 6\n        };\n      unbox(x) + unbox(y)",
    "7",
  ),
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  tfinalanf(
    "test_const_propagation",
    "{\n    let x = 4;\n    let y = x;\n    x}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  ),
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  tfinalanf(
    "test_const_propagation2",
    "((x) => {\n    let x = 4;\n    let y = x;\n    x})",
    {
      open Grain_typed;
      let x = Ident.create("lambda_arg");
      AExp.comp(
        Comp.lambda(
          [(x, HeapAllocated)],
          (
            AExp.comp(
              Comp.imm(
                ~allocation_type=HeapAllocated,
                Imm.const(Const_number(Const_number_int(4L))),
              ),
            ),
            HeapAllocated,
          ),
        ),
      );
    },
  ),
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  tfinalanf(
    "test_const_propagation_shadowing",
    "{\n  let x = 5;\n  let y = 12;\n  let z = y;\n  {\n    let y = x;\n    x\n  }\n  x + y}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(17L))),
      ),
    ),
  ),
  /* Primarily a constant-folding test, but DAE removes the let bindings as well */
  tfinalanf(
    "test_const_folding",
    "{\n    let x = 4 + 5;\n    let y = x * 2;\n    let z = y - x;\n    let a = x + 7;\n    let b = 14;\n    a + b}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(30L))),
      ),
    ),
  ),
  tfinalanf(
    "test_dae",
    "((x) => {let a = (x, 1); let b = (x, 1); (x, 1)})",
    {
      open Grain_typed;
      let arg = Ident.create("lambda_arg");
      let x = Ident.create("x");
      AExp.comp(
        Comp.lambda([(arg, HeapAllocated)]) @@
        (
          AExp.let_(
            Nonrecursive,
            [(x, Comp.imm(~allocation_type=HeapAllocated, Imm.id(arg)))],
          ) @@
          AExp.comp @@
          Comp.tuple([
            Imm.id(x),
            Imm.const(Const_number(Const_number_int(1L))),
          ]),
          HeapAllocated,
        ),
      );
    },
  ),
  tfinalanf(
    "test_dae_lambda_unused",
    "((x) => {1})",
    {
      open Grain_typed;
      let x = Ident.create("lambda_arg");
      AExp.comp(
        Comp.lambda(
          [(x, HeapAllocated)],
          (
            AExp.comp(
              Comp.imm(
                ~allocation_type=HeapAllocated,
                Imm.const(Const_number(Const_number_int(1L))),
              ),
            ),
            HeapAllocated,
          ),
        ),
      );
    },
  ),
  /* All optimizations are needed to work completely on this input */
  tfinalanf(
    "test_optimizations_work_together",
    "{\n    let x = 5;\n    let foo = ((y) => {y});\n    let y = (3, 5);\n    foo(3) + x}",
    {
      open Grain_typed;
      let plus = Ident.create("+");
      let foo = Ident.create("foo");
      let arg = Ident.create("lambda_arg");
      let app = Ident.create("app");
      AExp.let_(
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda([(arg, HeapAllocated)]) @@
            (
              AExp.comp @@
              Comp.imm(~allocation_type=HeapAllocated) @@
              Imm.id(arg),
              HeapAllocated,
            ),
          ),
        ],
      ) @@
      AExp.let_(
        Nonrecursive,
        [
          (
            app,
            Comp.app(
              ~allocation_type=HeapAllocated,
              (Imm.id(foo), ([HeapAllocated], HeapAllocated)),
              [Imm.const(Const_number(Const_number_int(3L)))],
            ),
          ),
        ],
      ) @@
      AExp.comp @@
      Comp.app(
        ~allocation_type=HeapAllocated,
        ~tail=true,
        (Imm.id(plus), ([HeapAllocated, HeapAllocated], HeapAllocated)),
        [Imm.id(app), Imm.const(Const_number(Const_number_int(5L)))],
      );
    },
  ),
  tfsound("test_counter-mut_sound", "counter-mut", "1\n2\n3\nvoid"),
  tfsound("test_counter-box_sound", "counter-box", "1\n2\n3\nvoid"),
  te("test_dae_sound", "let x = 2 + false; 3", "type"),
  te(
    "test_const_fold_times_zero_sound",
    "let f = ((x) => {x * 0}); f(false)",
    "Number",
  ),
  te(
    "test_const_fold_or_sound",
    "let f = ((x) => {x || true}); f(1)",
    "Bool",
  ),
  te(
    "test_const_fold_and_sound",
    "let f = ((x) => {false && x}); f(1)",
    "Bool",
  ),
  te(
    "test_const_fold_plus_sound",
    "let f = ((x) => {0 + x}); f(true)",
    "Number",
  ),
  te(
    "test_const_fold_times_one_sound",
    "let f = ((x) => {x * 1}); f(true)",
    "Number",
  ),
  /* te ~opts:{default_compile_options with sound_optimizations=false}
       "test_unsound_dae" "let x = 2 + false; 3" "type";
     t ~opts:{default_compile_options with sound_optimizations=false}
       "test_unsound_const_fold_times_zero" "let f = ((x) => {x * 0}); f(false)" "0";
     t ~opts:{default_compile_options with sound_optimizations=false}
       "test_unsound_const_fold_or" "let f = ((x) => {x or true}); f(1)" "true";
     t ~opts:{default_compile_options with sound_optimizations=false}
       "test_unsound_const_fold_and" "let f = ((x) => {false and x}); f(1)" "false";
     t ~opts:{default_compile_options with sound_optimizations=false}
       "test_unsound_const_fold_plus" "let f = ((x) => {0 + x}); f(true)" "true";
     t ~opts:{default_compile_options with sound_optimizations=false}
       "test_unsound_const_fold_times_one" "let f = ((x) => {x * 1}); f(true)" "true"; */
];

let string_tests = {
  open Grain_parsing;
  open Ast_helper;
  let str = s => Top.expr @@ Exp.constant(Const.string(s));
  [
    tparse(
      "string_parse_dqs1",
      "\"foo\"",
      {
        statements: [str("foo")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    tparse(
      "string_parse_dqs2",
      "\"bar\\nbaz\"",
      {
        statements: [str("bar\nbaz")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    tparse(
      "string_parse_sqs1",
      "\"foobar\"",
      {
        statements: [str("foobar")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    tparse(
      "string_parse_sqs2",
      "\"bar\\u{41}\"",
      {
        statements: [str("barA")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    tparse(
      "string_parse_sqs3",
      "\"bar\\x41\"",
      {
        statements: [str("barA")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    tparse(
      "string_parse_sqs4",
      "\"bar\\101\"",
      {
        statements: [str("barA")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    tparse(
      "string_parse_sqs5",
      "\"bar\\u0041\"",
      {
        statements: [str("barA")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    tparse(
      "string_parse_emoji_escape",
      "\"😂\"",
      {
        statements: [str("😂")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    tparse(
      "string_parse_emoji_literal",
      "\"💯\"",
      {
        statements: [str("💯")],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    ),
    t("string1", "\"foo\"", "foo"),
    t("string2", "\"💯\"", "💯"),
    t(
      "string3",
      "\"making my way downtown, walking fast\"",
      "making my way downtown, walking fast",
    ),
    t("concat", "\"foo\" ++ \"bar\"", "foobar"),
    te("string_err", "let x = \"hello\"; x + \", world\"", "type"),
    te("unicode_err1", "let x = \"\\u{d800}\"", "Illegal unicode code point"),
    te("unicode_err2", "let x = \"\\u{dfff}\"", "Illegal unicode code point"),
    te(
      "unicode_err3",
      "let x = \"\\u{110000}\"",
      "Illegal unicode code point",
    ),
  ];
};

let char_tests = [
  t("char1", "'A'", "A"),
  t("char2", "'\\x41'", "A"),
  t("char3", "'\\101'", "A"),
  t("char4", "'\\u0041'", "A"),
  t("char5", "'\\u{41}'", "A"),
  t("char6", "'💯'", "💯"),
  t("char7", "'\\u{1F33E}'", "🌾"),
  t("char_eq1", "'🌾' == '🌾'", "true"),
  t("char_eq2", "'🌾' == '💯'", "false"),
  t(
    "char_eq3",
    "import Char from \"char\"; Char.fromCode(0x1F33E) == '🌾'",
    "true",
  ),
  t(
    "char_eq4",
    "import Char from \"char\"; Char.fromCode(0x1F33E) == '💯'",
    "false",
  ),
  te(
    "char_illegal",
    "'abc'",
    "This character literal contains multiple characters: 'abc'\nDid you mean to create the string \"abc\" instead?",
  ),
  te("unicode_err1", "let x = '\\u{d800}'", "Illegal unicode code point"),
  te("unicode_err2", "let x = '\\u{dfff}'", "Illegal unicode code point"),
  te("unicode_err3", "let x = '\\u{110000}'", "Illegal unicode code point"),
];

let exception_tests = [
  t("exception_1", "exception Foo; Foo", "<adt value>"),
  t("exception_2", "export exception Foo; Foo", "Foo"),
  t(
    "exception_3",
    "export exception Foo(Bool, Number); Foo(false, 6)",
    "Foo(false, 6)",
  ),
  t(
    "exception_4",
    "export exception Foo(Bool, Number); export exception Bar; Bar",
    "Bar",
  ),
];

let enum_tests = [
  tfile("basicenum", "basicenum", "(false, true, true)"),
  tfile(
    "adtprint",
    "adtprint",
    "Foo\nBar\nBaz(\"baz\")\nQux(5, \"qux\", false)\nQuux\nFlip(\"flip\")\nvoid",
  ),
  t("adtprint_nonexported", "enum Foo { Foo }; Foo", "<adt value>"),
  t(
    "adt_trailing",
    "enum Topping { Cheese(Bool,), Pepperoni }; Pepperoni",
    "<adt value>",
  ),
];

let export_tests = [
  te("export1", "import * from \"noExports\"; x", "Unbound value x"),
  te("export2", "import * from \"noExports\"; y", "Unbound value y"),
  te("export3", "import * from \"noExports\"; z", "Unbound value z"),
  t("export4", "import * from \"onlyXExported\"; x", "4"),
  te("export5", "import * from \"onlyXExported\"; y", "Unbound value y"),
  te("export6", "import * from \"onlyXExported\"; z", "Unbound value z"),
  t("export7", "import * from \"exportStar\"; x", "5"),
  t("export8", "import * from \"exportStar\"; x + y(4)", "9"),
  t("export9", "import * from \"exportStar\"; y(z)", "foo"),
  te(
    "export10",
    "import * from \"exportStar\"; y(secret)",
    "Unbound value secret",
  ),
];

let number_tests = [
  t("numbers1", "0.3333 + (1 / 3)", "0.6666333333333333"),
  t("numbers2", "0.3333 - (1 / 3)", "-0.00003333333333332966"),
  t("numbers3", "0.0 + ((1 / 3) * (1 / 3))", "0.1111111111111111"),
  t("numbers4", "1 / 3", "1/3"),
  t("numbers5", "1.0 / 3", "0.3333333333333333"),
  t("numbers6", "2 / 6", "1/3"),
  t("numbers7", "(1 / 3) + (1 / 6)", "1/2"),
  t("numbers8", "(1 / 3) * (1 / 3)", "1/9"),
  t("numbers9", "(1 / 3) / (1 / 3)", "1"),
  t("numbers10", "-2 / 4", "-1/2"),
  t("numbers11", "2 / -4", "-1/2"),
  t("numbers12", "-2 / -4", "1/2"),
  te("numbers13", "9 / 0", "denominator of zero"),
  // basic syntax tests
  t("number_syntax1", "1.2", "1.2"),
  t("number_syntax2", "1.", "1.0"),
  t("number_syntax3", ".2", "0.2"),
  te("number_syntax4", ".", "Syntax error"),
  t("number_syntax5", "1.2d", "1.2"),
  t("number_syntax6", "1.2f", "1.2000000476837159"),
  t("number_syntax7", "1e2", "100.0"),
  t("number_syntax8", "1.2e2", "120.0"),
  t("number_syntax9", "1l", "1"),
  t("number_syntax10", "1L", "1"),
  // syntax errors
  te(
    "number_syntax_err1",
    "987654321987654321987654321",
    "outside of the integer range",
  ),
  te(
    "number_syntax_err2",
    "987654321987654321987654321l",
    "representable 32-bit",
  ),
  te(
    "number_syntax_err3",
    "987654321987654321987654321L",
    "representable 64-bit",
  ),
];

let comment_tests = {
  open Grain_parsing;
  open Ast_helper;
  let str = s => Top.expr @@ Exp.constant(Const.string(s));

  [
    tparse(
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
    ),
    tparse(
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
    ),
    tparse(
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
    ),
    tparse(
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
    ),
    te(
      "comment_line_numbers_1",
      "//comment\n//comment\n5 + 5L",
      "line 3, characters 4-6",
    ),
    t("comment_lone_//", "//\nlet x = 10\nx", "10"),
    t("comment_block", "/* block 1 */let x = 10/* block 2 */\nx", "10"),
    t("comment_doc", "/** doc 1 */let x = 10/** doc 2 */\nx", "10"),
    t("comment_shebang", "#!/bin/grain\nlet x = 10\nx", "10"),
  ];
};

let tests =
  "End to end"
  >::: basic_functionality_tests
  @ function_tests
  @ tuple_tests
  @ list_tests
  @ array_tests
  @ record_tests
  @ stdlib_tests
  @ box_tests
  @ let_mut_tests
  @ loop_tests
  @ oom
  @ gc
  @ match_tests
  @ import_tests
  @ optimization_tests
  @ char_tests
  @ string_tests
  @ enum_tests
  @ export_tests
  @ comment_tests
  @ number_tests
  @ exception_tests
  @ export_tests;
