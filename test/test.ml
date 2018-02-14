open Grain.Compile
open Grain.Runner
open Printf
open OUnit2
open ExtLib
open Grain.Legacy_types
open Grain.Expr
open Grain.Pretty

let () =
  Printexc.register_printer (fun exc ->
      match Grain_parsing.Location.error_of_exn exc with
      | None -> None
      | Some `Already_displayed -> None
      | Some (`Ok err) ->
        let buf = Buffer.create 512 in
        let formatter = Format.formatter_of_buffer buf in
        Format.fprintf formatter "@[%a@]@." Grain_parsing.Location.report_error err;
        Format.pp_flush_formatter formatter;
        let s = Buffer.contents buf in
        Buffer.reset buf;
        Some (s))

let t ?opts:(opts=default_compile_options) name program expected = name>::test_run opts [] program name expected;;
let tlib name program expected = name>::test_run default_compile_options [] program name expected;;
let tgc name heap_size program expected = name>::test_run default_compile_options [string_of_int heap_size] program name expected;;
let terr name program expected = name>::test_err default_compile_options [] program name expected;;
let tgcerr name heap_size program expected = name>::test_err default_compile_options [string_of_int heap_size] program name expected;;

let te ?opts:(opts=default_compile_options) name program expected = name>::test_err default_compile_options ["1000"] program name expected;;
let telib name program expected = name>::test_err default_compile_options ["10000"] program name expected;;

let tfvs name program expected = name>::
  (fun _ ->
    try
      let ast = parse_string name program in
      let typed_tree, _, _ = Grain_typed.Typemod.type_module Grain_typed.Env.empty ast in
      let anfed = anf typed_tree in
      let vars = free_vars anfed in
      let c = Pervasives.compare in
      let str_list_print strs = "[" ^ (ExtString.String.join ", " strs) ^ "]" in
      assert_equal (List.sort ~cmp:c vars) (List.sort ~cmp:c expected) ~printer:str_list_print
    with x ->
      (*Grain_parsing.Location.report_exception Format.err_formatter x;*)
      raise x)
;;

let test_input_file filename include_stdlib heap_size name expected test_ctxt =
  try
    let input_filename = "input/" ^ filename ^ ".gr" in
    let input_channel = open_in input_filename in
    let full_outfile = "output/" ^ name in
    let program = parse_file filename input_channel in
    let result = run include_stdlib program full_outfile run_no_vg [string_of_int heap_size] in
    assert_equal (expected ^ "\n") result
  with x ->
    (*Grain_parsing.Location.report_exception Format.err_formatter x;*)
    raise x

let test_err_input_file filename include_stdlib heap_size name errmsg test_ctxt =
  try
    let input_filename = "input/" ^ filename ^ ".gr" in
    let input_channel = open_in input_filename in
    let full_outfile = "output/" ^ name in
    let program = parse_file filename input_channel in
    let result = run include_stdlib program full_outfile run_no_vg [string_of_int heap_size] in
    assert_equal
      (errmsg)
      result
      ~cmp: (fun check result -> String.exists result check)
  with x ->
    (*Grain_parsing.Location.report_exception Format.err_formatter x;*)
    raise x

let test_optimizations_sound program_str opts heap_size name expected test_ctxt =
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in
  let program = parse_string name program_str in
  let result_unoptimized =
    run
      {opts with optimizations_enabled=false}
      program
      full_outfile_unoptimized
      run_no_vg
      [string_of_int heap_size] in
  let result_optimized =
    run
      {opts with optimizations_enabled=true}
      program
      full_outfile_optimized
      run_no_vg
      [string_of_int heap_size] in
  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (expected ^ "\n") result_optimized

let test_optimizations_sound_err program_str opts heap_size name errmsg test_ctxt =
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in
  let program = parse_string name program_str in
  let result_unoptimized =
    run
      {opts with optimizations_enabled=false}
      program
      full_outfile_unoptimized
      run_no_vg
      [string_of_int heap_size] in
  let result_optimized =
    run
      {opts with optimizations_enabled=true}
      program
      full_outfile_optimized
      run_no_vg
      [string_of_int heap_size] in
  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (errmsg) result_optimized
    ~cmp: (fun check result -> String.exists result check)

let test_file_optimizations_sound filename opts heap_size name expected test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let input_channel = open_in input_filename in
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in
  let program = parse_file filename input_channel in
  let result_unoptimized =
    run
      {opts with optimizations_enabled=false}
      program
      full_outfile_unoptimized
      run_no_vg
      [string_of_int heap_size] in
  let result_optimized =
    run
      {opts with optimizations_enabled=true}
      program
      full_outfile_optimized
      run_no_vg
      [string_of_int heap_size] in
  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (expected ^ "\n") result_optimized

let test_file_optimizations_sound_err filename opts heap_size name errmsg test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let input_channel = open_in input_filename in
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in
  let program = parse_file filename input_channel in
  let result_unoptimized =
    run
      {opts with optimizations_enabled=false}
      program
      full_outfile_unoptimized
      run_no_vg
      [string_of_int heap_size] in
  let result_optimized =
    run
      {opts with optimizations_enabled=true}
      program
      full_outfile_optimized
      run_no_vg
      [string_of_int heap_size] in
  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (errmsg) result_optimized
    ~cmp: (fun check result -> String.exists result check)

(** Tests that the file input/`input_file`.egg produces
    the given output *)
let tfile name input_file expected = name>::test_input_file input_file default_compile_options 10000 name expected
(** Tests that the file input/`input_file`.egg produces
    the given error message *)
let tefile name input_file errmsg = name>::test_err_input_file input_file default_compile_options 10000 name errmsg

let tgcfile name heap_size input_file expected = name>::test_input_file input_file default_compile_options heap_size name expected

let tgcefile name heap_size input_file errmsg = name>::test_err_input_file input_file default_compile_options heap_size name errmsg

let test_resolve_scope opts program_str outfile (expected : 'a aprogram) test_ctxt =
  let anf = compile_string_to_anf outfile opts program_str in
  let result = Grain.Pretty.string_of_aprogram (Grain.Resolve_scope.resolve_scope anf Grain.Compile.initial_load_env) in
  assert_equal (Grain.Pretty.string_of_aprogram expected) result

let test_final_anf opts program_str outfile (expected : 'a aprogram) test_ctxt =
  let final_anf = compile_string_to_final_anf outfile opts program_str in
  let result = Grain.Pretty.string_of_aprogram final_anf in
  assert_equal (Grain.Pretty.string_of_aprogram expected) result

let trs name (program : string) (expected : 'a aprogram) = name>::test_resolve_scope default_compile_options program name expected;;

let tfinalanf name ?opts:(opts=default_compile_options) (program : string) (expected : 'a aprogram) =
  name>::test_final_anf opts program name expected;;

let tsound name prog expected = name>::test_optimizations_sound prog default_compile_options 10000 name expected;;

let tesound name prog expected = name>::test_optimizations_sound_err prog default_compile_options 10000 name expected;;

let tfsound name filename expected = name>::test_file_optimizations_sound filename default_compile_options 10000 name expected;;

let tefsound name filename errmsg = name>::test_file_optimizations_sound_err filename default_compile_options 10000 name errmsg;;

let test_parse name input (expected : Grain_parsing.Parsetree.parsed_program) test_ctxt =
  let open Grain_parsing in
  let location_stripper = {Ast_mapper.default_mapper with location = (fun _ _ -> Location.dummy_loc)} in
  let strip_locs (({statements; body; _} as p) : Parsetree.parsed_program) =
    {p with
     statements=(List.map (location_stripper.toplevel location_stripper) statements);
     body=location_stripper.expr location_stripper body
    } in
  let parsed = strip_locs @@ parse_string name input in
  let untagged = strip_locs @@ parsed in
  assert_equal expected
    untagged ~printer:(fun p -> Sexplib.Sexp.to_string_hum @@ Grain_parsing.Parsetree.sexp_of_parsed_program p)

let tparse name input expected = name>::test_parse name input expected

let forty = "let x = 40; x"
let fals = "let x = false; x"
let tru = "let x = true; x"

(* Tests for functionality inherited from Cobra *)
let cobra_tests = [
  t "forty" forty "40";
  t "neg" "-1073741824" "-1073741824";
  t "fals" fals "false";
  t "tru" tru "true";
  t "complex1" "
    let x = 2, y = 3, z = if true { 4 } else { 5 };
    if true {
      print(y) - (z + x)
    } else {
      print(8)
    }
    "  "3\n-3";
  t "complex2" "print(2) + print(3)" "2\n3\n5";

  t "binop1" "2 + 2" "4";
  t "binop2" "2 - 2" "0";
  t "binop3" "2 - 4" "-2";
  t "binop4" "2 * 3" "6";

  t "and1" "true and true" "true";
  t "and2" "true and false" "false";
  t "and3" "false and true" "false";
  t "and4" "false and false" "false";

  t "or1" "true or true" "true";
  t "or2" "true or false" "true";
  t "or3" "false or true" "true";
  t "or4" "false or false" "false";

  t "comp1" "if 2 < 3 {true} else {false}" "true";
  te "comp1e" "if 2 < 3 {true} else {3}" "type";
  t "comp2" "if 2 <= 3 {true} else {false}" "true";
  te "comp2e" "if 2 <= 3 {true} else {3}" "type";
  t "comp3" "if 2 >= 3 {4} else {5}" "5";
  t "comp4" "if 2 > 3 {4} else {5}" "5";
  t "comp5" "if 2 < 3 {4} else {5}" "4";
  t "comp6" "if 2 == 3 {8} else {9}" "9";
  t "comp7" "if 2 == 2 {8} else {9}" "8";
  t "comp8" "if 2 <= 2 {10} else {11}" "10";
  t "comp9" "if 2 >= 2 {10} else {11}" "10";
  t "comp10" "let x = 2, y = 4; (y - 2) == x" "true";
  t "comp11" "true == 2" "false";
  t "comp12" "2 == false" "false";
  t "comp13" "true == true" "true";
  t "comp14" "true == false" "false";
  t "comp15" "false == true" "false";
  t "comp16" "false == false" "true";

  t "isbool1" "isbool(true)" "true";
  t "isbool2" "isbool(false)" "true";
  t "isbool3" "isbool(4)" "false";
  t "isbool4" "isbool(8)" "false";

  t "isnum1" "isnum(true)" "false";
  t "isnum2" "isnum(false)" "false";
  t "isnum3" "isnum(4)" "true";
  t "isnum4" "isnum(8)" "true";

  t "not1" "not(true)" "false";
  t "not2" "not(false)" "true";

  t "add1_1" "add1(2)" "3";
  t "add1_2" "add1(5)" "6";
  t "add1_3" "add1(-1)" "0";
  t "sub1_1" "sub1(2)" "1";
  t "sub1_2" "sub1(5)" "4";
  t "sub1_3" "sub1(0)" "-1";

  te "comp_bool1" "if 2 < true {3} else {4}" "type";
  te "comp_bool2" "if 2 > true {3} else {4}" "type";
  te "comp_bool3" "if true >= 4 {3} else {4}" "type";
  te "comp_bool4" "let x = true; if x < 4 {3} else {5}" "type";

  te "arith1" "2 + true" "type";
  te "arith2" "true + 4" "type";
  te "arith3" "false - 5" "type";
  te "arith4" "4 - true" "type";
  te "arith5" "let x = true; x * 4" "type";
  te "arith6" "let x = false; 4 * x" "type";

  te "if1" "if 2 {5} else {6}" "type";
  te "if2" "let y = 0; if y {5} else {6}" "type";
  te "if3" "if sub1(1) {2} else {5}" "type";

  (* te "generic1" "printStack(true)" "expected a number"; *)

  (* Non-compile-time overflows *)
  te "overflow1" "9999999 * 99999999" "overflow";
  te "overflow2" "-99999999 - 999999999" "overflow";
  te "overflow3" "99999999 + 999999999" "overflow";
  (* Compile-time overflow *)
  te "overflow4" "999999999999 + 9999999999999" "overflow";
  (* te "ps1" "printStack(-1)" "expected a nonnegative"; *)
]

(* Tests for functionality which is new to Diamondback *)
let diamondback_tests = [
  tfile "fib1" "fib" "55";
  tfile "fib2" "fib-better" "75025";
  tfile "indirect" "indirect-tail" "10";
  (* NOTE: This file also will test that we're doing tail calls
     and mutual recursion properly (should stack overflow otherwise) *)

  (* tfile "forward_decl" "forward-decl" "true"; *)
  (* This will test that we are doing tail calls for arbitrary-arity
     stack frame sizes correctly *)

  (* tfile "sinister_tail_call" "sinister-tail-call" "true"; *)
  (* tvgfile "sinister_tail_call2" "sinister-tail-call" "true"; *)
  tefile "fib_big" "too-much-fib" "overflow";

  t "func_no_args" "let foo = (() => {print(5)});\nfoo()" "5\n5";
  t "multi_bind" "let x = 2, y = x + 1; y" "3";
  te "unbound_fun" "2 + foo()" "unbound";
  te "unbound_id_simple" "5 - x" "unbound";
  te "unbound_id_let" "let x = x; 2 + 2" "unbound";
  te "shadow_simple" "let x = 12; let x = 15; x" "shadows";
  te "shadow_multi" "let x = 12, x = 14; x" "Variable x is bound several times";
  te "dup_func" "let rec foo = (() => {5});\nlet bar = (() => { 7 });\nlet rec foo = (() => {9});\nfoo()" "shadows";
  te "arity_1" "let foo = (() => {5});\nfoo(6)" "type";
  te "arity_2" "let foo = ((x) => {x + 5});\nfoo()" "type";
  te "arity_3" "let foo = ((x) => {x});\nfoo(1, 2, 3)" "type";
]

let mylist = "cons(1, cons(2, cons(3, false)))"

let egg_eater_tests = [
  t "print_tup" "print((1, 2))" "(1, 2)\n(1, 2)";
  t "big_tup" "(1, 2, 3, 4)" "(1, 2, 3, 4)";
  t "big_tup_access" "let (a, b, c, d) = (1, 2, 3, 4); c" "3";
  t "nested_tup_1" "let (a, b) = ((1, 2), (3, 4)); a" "(1, 2)";
  t "nested_tup_2" "let (a, b) = ((1, 2), (3, 4)); let (c, d) = b; d" "4";
  t "nested_tup_3" "let (x, y) = ((1, 2), (3, 4)); let (a, b) = y; a" "3";
  t "no_singleton_tup" "(1)" "1";
]

let egg_eater_stdlib_tests = [
  tlib "stdlib_cons" ("import lists; " ^ mylist) "(1, (2, (3, false)))";
  tlib "stdlib_sum_1" ("import lists; sum(" ^ mylist ^ ")") "6";
  tlib "stdlib_sum_2" "import lists; sum(false)" "0";
  tlib "stdlib_reverse" ("import lists; reverse(" ^ mylist ^ ")") "(3, (2, (1, false)))";
  tlib "stdlib_length" "import lists; length(cons(1, cons(2, cons(3, false))))" "3";
  tlib "stdlib_equal_1" "import lists; (1, 2) == (1, 2)" "false";
  tlib "stdlib_equal_2" "import lists; equal((1, 2), (1, 2))" "true";
  tlib "stdlib_equal_3" "import lists; equal((1, (2, (3, false))), (1, (2, (3, false))))" "true";
  tlib "stdlib_equal_4" "import lists; equal(1, 1)" "true";
  tlib "stdlib_equal_5" "import lists; equal(1, 2)" "false";
  tlib "stdlib_equal_6" "import lists; equal(true, true)" "true";
  tlib "stdlib_equal_7" "import lists; equal(true, false)" "false";
  tlib "stdlib_contains_1" "import lists; contains(true, cons(1, cons(2, cons(3, false))))" "false";
  tlib "stdlib_contains_2" "import lists; contains(false, cons(1, cons(2, cons(3, false))))" "false";
  tlib "stdlib_contains_3" "import lists; contains(3, cons(1, cons(2, cons(3, false))))" "true";
  telib "stdlib_err_1" "import lists; cons(1)" "cannot be called with 1 argument";
  telib "stdlib_err_2" "import lists; cons()" "cannot be called with 0 arguments";
  telib "stdlib_err_3" "import lists; cons(1, 2, 3)" "cannot be called with 3 arguments";
  telib "stdlib_sum_err" "import lists; sum(cons(true, false))" "This expression has type Bool but";
  telib "stdlib_length_err" "import lists; length(true)" "This expression has type Bool but";
  telib "stdlib_reverse_err" "import lists; reverse(1)" "This expression has type Number but";
  telib "tuple_index_large_1" "import lists; (1, 2, 3)[6]" "large";
  telib "tuple_index_large_2" "import lists; (1, 2, 3)[4]" "large";
  telib "tuple_index_small_1" "import lists; (1, 2, 3)[-1]" "small";
  telib "tuple_index_small_2" "import lists; (1, 2, 3)[-2]" "small";
  telib "tuple_index_type_1" "import lists; (1, 2)[false]" "number";
  telib "tuple_index_type_2" "import lists; ((1, 2), (3, 4))[(1, 2)]" "number";
  telib "tuple_access_1" "import lists; let x = false; x[6]" "tuple";
  telib "tuple_access_2" "import lists; let x = 2; x[6]" "tuple";
]

(* Note that our tail call tests above provide a good
   stress test of our lambda and letrec implementations
   (also see the files in input/, which the above suites run)*)
let fer_de_lance_tests = [
  t "lambda_1" "(x) => {x}" "<lambda>";
  t "app_1" "((x) => x)(1)" "1";
  t "letrec_1" "let rec x = ((n) => {if n > 3 {n} else {x(n + 2)}}),
                        y = ((n) => {x(n + 1)});
                 y(2)" "5";
  (* Check that recursion is order-independent *)
  t "letrec_2" "let rec y = ((n) => {x(n + 1)}),
                        x = ((n) => {if n > 3 {n} else {x(n + 2)}});
                 y(2)" "5";
  t "let_1" "let x = ((n) => {n + 1}),
                 y = x(3),
                 z = ((n) => {x(n) + y});
               z(5)" "10";
  te "let_norec_1" "let x = ((n) => {if n > 3 {n} else {x(n + 2)}}),
                        y = ((n) => {x(n + 1)});
                 y(2)" "not in scope";
  te "lambda_dup_args" "((x, y, x) => {5})" "duplicate";
  te "lambda_arity_1" "((x) => {6})()" "type";
  te "lambda_arity_2" "((x) => {5})(1, 2)" "type";
  te "letrec_nonstatic_const" "let rec x = 5; x" "not bound to a function";
  te "letrec_nonstatic_same" "let rec x = x; x" "Unbound value x.\n       Hint: You are probably missing the `rec' keyword on line 1.";
  te "letrec_nonstatic_other" "let rec x = ((z) => {z + 1}), y = x; y" "not bound to a function";
  te "nonfunction_1" "let x = 5; x(3)" "type";
  te "nontuple_1" "let x = ((y) => {y + 1}); x[1]" "type";
]

let fer_de_lance_stdlib_tests = [
  tlib "map_1" ("import lists; map(((x) => {x + 1}), " ^ mylist ^ ")")
    "(2, (3, (4, false)))";
  tlib "map_2" ("import lists; map(((x) => {x * 2}), " ^ mylist ^ ")")
    "(2, (4, (6, false)))";
  tlib "map_print" ("import lists; map(print, " ^ mylist ^ ")") "3\n2\n1\n(1, (2, (3, false)))";
  tlib "fold_left_1" ("import lists; fold_left(((acc, cur) => {acc - cur}), 0, " ^ mylist ^ ")")
    "-6";
  tlib "fold_right_1" ("import lists; fold_right(((cur, acc) => {cur - acc}), 0, " ^ mylist ^ ")")
    "2";
]

let pair_tests = [
  t "tup1" "let t = (4, (5, 6));
            {
              t[0] := 7;
              t
            }" "(7, (5, 6))";
  t "tup2" "let t = (4, (5, 6));
            {
              t[1] := 7;
              t
            }" "(4, 7)";
  t "tup3" "let t = (4, (5, 6));
            {
              t[1] := t;
              t
            }" "(4, <cyclic tuple 1>)";
  t "tup4" "let t = (4, 6);
            (t, t)"
           "((4, 6), (4, 6))"

]

let oom = [
  tgcerr "oomgc1" 7 "(1, (3, 4))" "Out of memory";
  tgc "oomgc2" 8 "(1, (3, 4))" "(1, (3, 4))";
  tgc "oomgc3" 4 "(3, 4)" "(3, 4)";
  tgcerr "oomgc4" 3 "(3, 4)" "Allocation";
]

let gc = [
  tgc "gc1" 10
      "let f = (() => (1, 2));
       begin
         f();
         f();
         f();
         f()
       end"
      "(1, 2)";
  (* Test that cyclic tuples are GC'd properly *)
  tgc "gc2" 10
    "let f = (() =>
      let x = (1, 2);
        x[1] := x);
      begin
        f();
        let x = (1, 2) in
          x
      end" "(1, 2)";
  tgcefile "fib_gc_err" 10 "fib-gc" "Out of memory";
  tgcfile "fib_gc" 16 "fib-gc" "832040";
  tgcfile "fib_gc_bigger" 64 "fib-gc" "832040";
  tgcfile "fib_gc_biggest" 512 "fib-gc" "832040";
  tgcfile "sinister_gc" 64 "sinister-tail-call-gc" "true";
  tgcfile "long_lists" 1024 "long_lists" "true";
]

let garter_extra_tests = [
  t "test_set_extra1" "(1, 2)[0] := 2" "2";
  tfile "counter" "counter" "0\n1\n2\n2";
  (*te "test_bad_import" "let x = (1, 2); import lists; x" "Includes must be at the beginning";*)
  te "test_missing_import" "import foo; 2" "not found";
  te "test_set_err1" "(1, 2)[-1] := 3" "small";
  te "test_set_err2" "(1, 2)[3] := 4" "large";
  te "test_set_err3" "(1, 2)[true] := 5" "number";
  te "test_set_err4" "let x = 2 in x[1] := 3" "tuple";
]

(* Note that optimizations are on by default, so all of the above tests
   check that it works correctly as well *)
let indigo_tests = [
  (* note on resolve-scope test: (tags are not checked) *)
  trs "trs1"
    "let f1 = ((x, y) => {x}),
         f2 = ((x, y) => {y}) in
       f1(1, 2)"
    (ALet("f1$1",
          CLambda(["x$2"; "y$3"],
                  ACExpr(CImmExpr(ImmId("x$2", 3))), 2),
          ALet("f2$4",
               CLambda(["x$5"; "y$6"],
                              ACExpr(CImmExpr(ImmId("y$6", 3))), 4),
             ACExpr(CApp(ImmId("f1$1", 3), [ImmNum(1, 3); ImmNum(2, 3)], 3)), 3),
          1));

  (* Primarily a constant-folding test, but DAE removes the let bindings as well *)
  tfinalanf "test_const_folding" "
    let x = 4 + 5;
    let y = x * 2;
    let z = y - x;
    let a = x + 7;
    let b = 14;
    a + b" (ACExpr(CImmExpr(ImmNum(30, ()))));

  tfinalanf "test_cse" "((x) => {let a = x + 1; let b = x + 1; a + b})"
    (ACExpr(CLambda(["x$1"], ALet("a$2", CPrim2(Plus, ImmId("x$1", ()), ImmNum(1, ()), ()),
                                  ACExpr(CPrim2(Plus, ImmId("a$2", ()), ImmId("a$2", ()), ())), ()), ())));

  tfinalanf "test_dae" "((x) => {let a = x + 1; let b = x + 1; x + 1})"
    (ACExpr(CLambda(["x$1"],
                    ACExpr(CPrim2(Plus, ImmId("x$1", ()), ImmNum(1, ()), ())), ())));

  (* All optimizations are needed to work completely on this input *)
  tfinalanf "test_optimizations_work_together" "
    let x = 5;
    let foo = ((y) => {y});
    let y = foo(3) + 5;
    foo(3) + x"
    (ALet("foo$2",
          CLambda(["y$3"], ACExpr(CImmExpr(ImmId("y$3", ()))), ()),
          ALet("app_12$4", CApp(ImmId("foo$2", ()), [ImmNum(3, ())], ()),
               ACExpr(CPrim2(Plus, ImmId("app_12$4", ()), ImmNum(5, ()), ())), ()), ()));

  tfsound "test_counter_sound" "counter" "0\n1\n2\n2";
  tefsound "fib_big" "too-much-fib" "overflow";
  te "test_dae_sound" "let x = 2 + false; 3" "type";
  te "test_const_fold_times_zero_sound" "let f = ((x) => {x * 0}); f(false)" "number";
  te "test_const_fold_or_sound" "let f = ((x) => {x or true}); f(1)" "bool";
  te "test_const_fold_and_sound" "let f = ((x) => {false and x}); f(1)" "bool";
  te "test_const_fold_plus_sound" "let f = ((x) => {0 + x}); f(true)" "number";
  te "test_const_fold_times_one_sound" "let f = ((x) => {x * 1}); f(true)" "number";

  te ~opts:{default_compile_options with sound_optimizations=false}
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
    "test_unsound_const_fold_times_one" "let f = ((x) => {x * 1}); f(true)" "true";
]

let string_tests =
  let open Grain_parsing in
  let open Ast_helper in
  let str s = Exp.constant (Const.string s) in
  [
  tparse "string_parse_dqs1" "\"foo\"" {statements=[]; body=str "foo"};
  tparse "string_parse_dqs2" "\"bar\\nbaz\"" {statements=[]; body=str "bar\nbaz"};
  tparse "string_parse_sqs1" "'foobar'" {statements=[]; body=str "foobar"};
  tparse "string_parse_sqs2" "'bar\\u41'" {statements=[]; body=str "barA"};
  tparse "string_parse_sqs3" "'bar\\x41'" {statements=[]; body=str "barA"};
  tparse "string_parse_sqs4" "'bar\\101'" {statements=[]; body=str "barA"};
  tparse "string_parse_emoji_escape" "\"\xF0\x9F\x98\x82\"" {statements=[]; body=str "😂"};
  tparse "string_parse_emoji_literal" "\"💯\"" {statements=[]; body=str "💯"};

  t "string1" "\"foo\"" "\"foo\"";
  t "string2" "\"💯\"" "\"💯\"";
  t "string3" "\"making my way downtown, walking fast\"" "\"making my way downtown, walking fast\"";
  te "string_err" "let x = \"hello\"; x + \", world\"" "type";
]

let suite =
  "suite">:::
  cobra_tests @
  diamondback_tests @
  egg_eater_tests @
  egg_eater_stdlib_tests @
  fer_de_lance_tests @
  fer_de_lance_stdlib_tests @
  pair_tests @ (*oom @ gc @*) garter_extra_tests @
  indigo_tests @ string_tests



let () =
  (** Override default stdlib location to use development version of stdlib *)
  Grain.Config.set_grain_root (BatFile.with_file_in "grain-root.txt" BatInnerIO.read_all);
  Grain_utils.Config.grain_root := Some(BatFile.with_file_in "grain-root.txt" BatInnerIO.read_all);
  run_test_tt_main suite
;;
