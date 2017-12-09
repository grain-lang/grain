open Grain.Compile
open Grain.Runner
open Printf
open OUnit2
open ExtLib
open Grain.Types
open Grain.Expr
open Grain.Pretty

let t ?opts:(opts=default_compile_options) name program expected = name>::test_run opts [] program name expected;;
let tlib name program expected = name>::test_run default_compile_options [] program name expected;;
let tgc name heap_size program expected = name>::test_run default_compile_options [string_of_int heap_size] program name expected;;
let terr name program expected = name>::test_err default_compile_options [] program name expected;;
let tgcerr name heap_size program expected = name>::test_err default_compile_options [string_of_int heap_size] program name expected;;

let te ?opts:(opts=default_compile_options) name program expected = name>::test_err default_compile_options ["1000"] program name expected;;
let telib name program expected = name>::test_err default_compile_options ["10000"] program name expected;;

let tfvs name program expected = name>::
  (fun _ ->
    let ast = parse_string name program in
    let anfed = anf (tag ast) in
    let vars = free_vars anfed in
    let c = Pervasives.compare in
    let str_list_print strs = "[" ^ (ExtString.String.join ", " strs) ^ "]" in
    assert_equal (List.sort ~cmp:c vars) (List.sort ~cmp:c expected) ~printer:str_list_print)
;;

let test_input_file filename include_stdlib heap_size name expected test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let input_channel = open_in input_filename in
  let full_outfile = "output/" ^ name in
  let program = parse_file filename input_channel in
  let result = run include_stdlib program full_outfile run_no_vg [string_of_int heap_size] in
  assert_equal (Right(expected ^ "\n")) result ~printer:either_printer

let test_err_input_file filename include_stdlib heap_size name errmsg test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let input_channel = open_in input_filename in
  let full_outfile = "output/" ^ name in
  let program = parse_file filename input_channel in
  let result = run include_stdlib program full_outfile run_no_vg [string_of_int heap_size] in
  assert_equal
    (Left(errmsg))
    result
    ~printer:either_printer
    ~cmp: (fun check result ->
      match check, result with
      | Left(expect_msg), Left(actual_message) ->
         String.exists actual_message expect_msg
      | _ -> false
    )

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
    result_unoptimized
    ~printer:either_printer;
  assert_equal (Right(expected ^ "\n")) result_optimized ~printer:either_printer

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
    result_unoptimized
    ~printer:either_printer;
  assert_equal (Left(errmsg)) result_optimized
    ~printer:either_printer
    ~cmp: (fun check result ->
      match check, result with
      | Left(expect_msg), Left(actual_message) ->
         String.exists actual_message expect_msg
      | _ -> false)

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
    result_unoptimized
    ~printer:either_printer;
  assert_equal (Right(expected ^ "\n")) result_optimized ~printer:either_printer

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
    result_unoptimized
    ~printer:either_printer;
  assert_equal (Left(errmsg)) result_optimized
    ~printer:either_printer
    ~cmp: (fun check result ->
      match check, result with
      | Left(expect_msg), Left(actual_message) ->
         String.exists actual_message expect_msg
      | _ -> false)

(** Tests that the file input/`input_file`.egg produces
    the given output *)
let tfile name input_file expected = name>::test_input_file input_file default_compile_options 10000 name expected
(** Tests that the file input/`input_file`.egg produces
    the given error message *)
let tefile name input_file errmsg = name>::test_err_input_file input_file default_compile_options 10000 name errmsg

let tgcfile name heap_size input_file expected = name>::test_input_file input_file default_compile_options heap_size name expected

let tgcefile name heap_size input_file errmsg = name>::test_err_input_file input_file default_compile_options heap_size name errmsg

let test_resolve_scope opts program_str outfile (expected : 'a aprogram) test_ctxt =
  let result = match (compile_string_to_anf outfile opts program_str) with
  | Left(errs) -> Left(ExtString.String.join "\n" (print_errors errs))
  | Right(anfed) -> Right(Grain.Pretty.string_of_aprogram (Grain.Resolve_scope.resolve_scope anfed Grain.Compile.initial_env)) in
  assert_equal (Right(Grain.Pretty.string_of_aprogram expected)) result ~printer:either_printer

let test_final_anf opts program_str outfile (expected : 'a aprogram) test_ctxt =
  let result = match (compile_string_to_final_anf outfile opts program_str) with
  | Left(errs) -> Left(ExtString.String.join "\n" (print_errors errs))
  | Right(final_anf) -> Right(Grain.Pretty.string_of_aprogram final_anf) in
  assert_equal (Right(Grain.Pretty.string_of_aprogram expected)) result ~printer:either_printer

let trs name (program : string) (expected : 'a aprogram) = name>::test_resolve_scope default_compile_options program name expected;;

let tfinalanf name ?opts:(opts=default_compile_options) (program : string) (expected : 'a aprogram) =
  name>::test_final_anf opts program name expected;;

let tsound name prog expected = name>::test_optimizations_sound prog default_compile_options 10000 name expected;;

let tesound name prog expected = name>::test_optimizations_sound_err prog default_compile_options 10000 name expected;;

let tfsound name filename expected = name>::test_file_optimizations_sound filename default_compile_options 10000 name expected;;

let tefsound name filename errmsg = name>::test_file_optimizations_sound_err filename default_compile_options 10000 name errmsg;;

let test_parse name input (expected : unit program) test_ctxt =
  let parsed = parse_string name input in
  let untagged = parsed in
  assert_equal (string_of_expr expected)
    (string_of_expr untagged) ~printer:identity

let tparse name input expected = name>::test_parse name input expected

let forty = "let x = 40 in x"
let fals = "let x = false in x"
let tru = "let x = true in x"

(* Tests for functionality inherited from Cobra *)
let cobra_tests = [
  t "forty" forty "40";
  t "neg" "-1073741824" "-1073741824";
  t "fals" fals "false";
  t "tru" tru "true";
  t "complex1" "
    let x = 2, y = 3, z = if true: 4 else: 5 in
      if true:
        print(y) - (z + x)
      else:
        print(8)
    "  "3\n-3";
  t "complex2" "print(2) + print(3)" "2\n3\n5";

  t "binop1" "2 + 2" "4";
  t "binop2" "2 - 2" "0";
  t "binop3" "2 - 4" "-2";
  t "binop4" "2 * 3" "6";

  t "and1" "true && true" "true";
  t "and2" "true && false" "false";
  t "and3" "false && true" "false";
  t "and4" "false && false" "false";

  t "or1" "true || true" "true";
  t "or2" "true || false" "true";
  t "or3" "false || true" "true";
  t "or4" "false || false" "false";

  t "comp1" "if 2 < 3: true else: 3" "true";
  t "comp2" "if 2 <= 3: true else: 3" "true";
  t "comp3" "if 2 >= 3: 4 else: 5" "5";
  t "comp4" "if 2 > 3: 4 else: 5" "5";
  t "comp5" "if 2 < 3: 4 else: 5" "4";
  t "comp6" "if 2 == 3: 8 else: 9" "9";
  t "comp7" "if 2 == 2: 8 else: 9" "8";
  t "comp8" "if 2 <= 2: 10 else: 11" "10";
  t "comp9" "if 2 >= 2: 10 else: 11" "10";
  t "comp10" "let x = 2, y = 4 in (y - 2) == x" "true";
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

  t "not1" "!(true)" "false";
  t "not2" "!(false)" "true";

  t "add1_1" "add1(2)" "3";
  t "add1_2" "add1(5)" "6";
  t "add1_3" "add1(-1)" "0";
  t "sub1_1" "sub1(2)" "1";
  t "sub1_2" "sub1(5)" "4";
  t "sub1_3" "sub1(0)" "-1";

  te "comp_bool1" "if 2 < true: 3 else: 4" "comparison expected a number";
  te "comp_bool2" "if 2 > true: 3 else: 4" "comparison expected a number";
  te "comp_bool3" "if true >= 4: 3 else: 4" "comparison expected a number";
  te "comp_bool4" "let x = true in if x < 4: 3 else: 5" "comparison expected a number";

  te "arith1" "2 + true" "arithmetic expected a number";
  te "arith2" "true + 4" "arithmetic expected a number";
  te "arith3" "false - 5" "arithmetic expected a number";
  te "arith4" "4 - true" "arithmetic expected a number";
  te "arith5" "let x = true in x * 4" "arithmetic expected a number";
  te "arith6" "let x = false in 4 * x" "arithmetic expected a number";

  te "if1" "if 2: 5 else: 6" "if expected a boolean";
  te "if2" "let y = 0 in if y: 5 else: 6" "if expected a boolean";
  te "if3" "if sub1(1): 2 else: 5" "if expected a boolean";

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

  t "func_no_args" "let foo = (lambda: print(5)) in\nfoo()" "5\n5";
  t "multi_bind" "let x = 2, y = x + 1 in y" "3";
  te "unbound_fun" "2 + foo()" "unbound";
  te "unbound_id_simple" "5 - x" "unbound";
  te "unbound_id_let" "let x = x in 2 + 2" "unbound";
  te "shadow_simple" "let x = 12 in let x = 15 in x" "shadows";
  te "shadow_multi" "let x = 12, x = 14 in x" "duplicate";
  te "dup_func" "let rec foo = (lambda: 5) in\nlet bar = (lambda: 7) in\nlet rec foo = (lambda: 9) in\nfoo()" "shadows";
  te "arity_1" "let foo = (lambda: 5) in\nfoo(6)" "arity";
  te "arity_2" "let foo = (lambda x: x + 5) in\nfoo()" "arity";
  te "arity_3" "let foo = (lambda x: x) in\nfoo(1, 2, 3)" "arity";
]

let mylist = "link(1, link(2, link(3, false)))"

let egg_eater_tests = [
  t "print_tup" "print((1, 2))" "(1, 2)\n(1, 2)";
  t "big_tup" "(1, 2, 3, 4)" "(1, 2, 3, 4)";
  t "big_tup_access" "(1, 2, 3, 4)[2]" "3";
  t "nested_tup_1" "((1, 2), (3, 4))[0]" "(1, 2)";
  t "nested_tup_2" "((1, 2), (3, 4))[1][1]" "4";
  t "nested_tup_3" "let x = ((1, 2), (3, 4)) in x[1][0]" "3";
  t "no_singleton_tup" "(1)" "1";
]

let egg_eater_stdlib_tests = [
  tlib "stdlib_link" ("include lists in " ^ mylist) "(1, (2, (3, false)))";
  tlib "stdlib_sum_1" ("include lists in sum(" ^ mylist ^ ")") "6";
  tlib "stdlib_sum_2" "include lists in sum(false)" "0";
  tlib "stdlib_reverse" ("include lists in reverse(" ^ mylist ^ ")") "(3, (2, (1, false)))";
  tlib "stdlib_length" "include lists in length(link(1, link(2, link(3, false))))" "3";
  tlib "stdlib_equal_1" "include lists in (1, 2) == (1, 2)" "false";
  tlib "stdlib_equal_2" "include lists in equal((1, 2), (1, 2))" "true";
  tlib "stdlib_equal_3" "include lists in equal((1, (2, (3, false))), (1, (2, (3, false))))" "true";
  tlib "stdlib_equal_4" "include lists in equal(1, 1)" "true";
  tlib "stdlib_equal_5" "include lists in equal(1, 2)" "false";
  tlib "stdlib_equal_6" "include lists in equal(true, true)" "true";
  tlib "stdlib_equal_7" "include lists in equal(true, false)" "false";
  tlib "stdlib_contains_1" "include lists in contains(true, link(1, link(2, link(3, false))))" "false";
  tlib "stdlib_contains_2" "include lists in contains(false, link(1, link(2, link(3, false))))" "false";
  tlib "stdlib_contains_3" "include lists in contains(3, link(1, link(2, link(3, false))))" "true";
  telib "stdlib_err_1" "include lists in link(1)" "arity";
  telib "stdlib_err_2" "include lists in link()" "arity";
  telib "stdlib_err_3" "include lists in link(1, 2, 3)" "arity";
  telib "stdlib_sum_err" "include lists in sum(link(true, false))" "number";
  telib "stdlib_length_err" "include lists in length(true)" "tuple";
  telib "stdlib_reverse_err" "include lists in reverse(1)" "tuple";
  telib "tuple_index_large_1" "include lists in (1, 2, 3)[6]" "large";
  telib "tuple_index_large_2" "include lists in (1, 2, 3)[4]" "large";
  telib "tuple_index_small_1" "include lists in (1, 2, 3)[-1]" "small";
  telib "tuple_index_small_2" "include lists in (1, 2, 3)[-2]" "small";
  telib "tuple_index_type_1" "include lists in (1, 2)[false]" "number";
  telib "tuple_index_type_2" "include lists in ((1, 2), (3, 4))[(1, 2)]" "number";
  telib "tuple_access_1" "include lists in let x = false in x[6]" "tuple";
  telib "tuple_access_2" "include lists in let x = 2 in x[6]" "tuple";
]

(* Note that our tail call tests above provide a good
   stress test of our lambda and letrec implementations
   (also see the files in input/, which the above suites run)*)
let fer_de_lance_tests = [
  t "lambda_1" "(lambda x: x)" "<lambda>";
  t "app_1" "(lambda x: x)(1)" "1";
  t "letrec_1" "let rec x = (lambda n: if n > 3: n else: x(n + 2)),
                        y = (lambda n: x(n + 1)) in
                 y(2)" "5";
  (* Check that recursion is order-independent *)
  t "letrec_2" "let rec y = (lambda n: x(n + 1)),
                        x = (lambda n: if n > 3: n else: x(n + 2)) in
                 y(2)" "5";
  t "let_1" "let x = (lambda n: n + 1),
                 y = x(3),
                 z = (lambda n: x(n) + y) in
               z(5)" "10";
  te "let_norec_1" "let x = (lambda n: if n > 3: n else: x(n + 2)),
                        y = (lambda n: x(n + 1)) in
                 y(2)" "not in scope";
  te "lambda_dup_args" "(lambda x, y, x: 5)" "duplicate";
  te "lambda_arity_1" "(lambda x: 6)()" "arity mismatch";
  te "lambda_arity_2" "(lambda x: 5)(1, 2)" "arity mismatch";
  te "letrec_nonstatic_const" "let rec x = 5 in x" "not bound to a function";
  te "letrec_nonstatic_same" "let rec x = x in x" "not bound to a function";
  te "letrec_nonstatic_other" "let rec x = (lambda z: z + 1), y = x in y" "not bound to a function";
  te "nonfunction_1" "let x = 5 in x(3)" "non-function";
  te "nontuple_1" "let x = (lambda y: y + 1) in x[1]" "expected tuple";
]

let fer_de_lance_stdlib_tests = [
  tlib "map_1" ("include lists in map((lambda x: x + 1), " ^ mylist ^ ")")
    "(2, (3, (4, false)))";
  tlib "map_2" ("include lists in map((lambda x: x * 2), " ^ mylist ^ ")")
    "(2, (4, (6, false)))";
  tlib "map_print" ("include lists in map(print, " ^ mylist ^ ")") "3\n2\n1\n(1, (2, (3, false)))";
  tlib "fold_left_1" ("include lists in fold_left((lambda acc, cur: acc - cur), 0, " ^ mylist ^ ")")
    "-6";
  tlib "fold_right_1" ("include lists in fold_right((lambda cur, acc: cur - acc), 0, " ^ mylist ^ ")")
    "2";
]

let pair_tests = [
  t "tup1" "let t = (4, (5, 6)) in
            begin
              t[0] := 7;
              t
            end" "(7, (5, 6))";
  t "tup2" "let t = (4, (5, 6)) in
            begin
              t[1] := 7;
              t
            end" "(4, 7)";
  t "tup3" "let t = (4, (5, 6)) in
            begin
              t[1] := t;
              t
            end" "(4, <cyclic tuple 1>)";
  t "tup4" "let t = (4, 6) in
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
      "let f = (lambda: (1, 2)) in
       begin
         f();
         f();
         f();
         f()
       end"
      "(1, 2)";
  (* Test that cyclic tuples are GC'd properly *)
  tgc "gc2" 10
    "let f = (lambda:
      let x = (1, 2) in
        x[1] := x) in
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
  te "test_bad_import" "let x = (1, 2) in include lists in x" "Includes must be at the beginning";
  te "test_missing_import" "include foo in 2" "not found";
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
    "let f1 = (lambda x, y: x),
         f2 = (lambda x, y: y) in
       f1(1)"
    (ALet("f1$1",
          CLambda(["x$2"; "y$3"],
                  ACExpr(CImmExpr(ImmId("x$2", 3))), 2),
          ALet("f2$4",
               CLambda(["x$5"; "y$6"],
                              ACExpr(CImmExpr(ImmId("y$6", 3))), 4),
             ACExpr(CApp(ImmId("f1$1", 3), [ImmNum(1, 3)], 3)), 3),
          1));

  (* Primarily a constant-folding test, but DAE removes the let bindings as well *)
  tfinalanf "test_const_folding" "
    let x = 4 + 5 in
    let y = x * 2 in
    let z = y - x in
    let a = x + 7 in
    let b = 14 in
    a + b" (ACExpr(CImmExpr(ImmNum(30, ()))));

  tfinalanf "test_cse" "(lambda x: let a = x + 1 in let b = x + 1 in a + b)"
    (ACExpr(CLambda(["x$1"], ALet("a$2", CPrim2(Plus, ImmId("x$1", ()), ImmNum(1, ()), ()),
                                  ACExpr(CPrim2(Plus, ImmId("a$2", ()), ImmId("a$2", ()), ())), ()), ())));

  tfinalanf "test_dae" "(lambda x: let a = x + 1 in let b = x + 1 in x + 1)"
    (ACExpr(CLambda(["x$1"],
                    ACExpr(CPrim2(Plus, ImmId("x$1", ()), ImmNum(1, ()), ())), ())));

  (* All optimizations are needed to work completely on this input *)
  tfinalanf "test_optimizations_work_together" "
    let x = 5 in
    let foo = (lambda y: y) in
    let y = foo(3) + 5 in
    foo(3) + x"
    (ALet("foo$2",
          CLambda(["y$3"], ACExpr(CImmExpr(ImmId("y$3", ()))), ()),
          ALet("app_12$4", CApp(ImmId("foo$2", ()), [ImmNum(3, ())], ()),
               ACExpr(CPrim2(Plus, ImmId("app_12$4", ()), ImmNum(5, ()), ())), ()), ()));

  tfsound "test_counter_sound" "counter" "0\n1\n2\n2";
  tefsound "fib_big" "too-much-fib" "overflow";
  te "test_dae_sound" "let x = 2 + false in 3" "number";
  te "test_const_fold_times_zero_sound" "let f = (lambda x: x * 0) in f(false)" "number";
  te "test_const_fold_or_sound" "let f = (lambda x: x || true) in f(1)" "bool";
  te "test_const_fold_and_sound" "let f = (lambda x: false && x) in f(1)" "bool";
  te "test_const_fold_plus_sound" "let f = (lambda x: 0 + x) in f(true)" "number";
  te "test_const_fold_times_one_sound" "let f = (lambda x: x * 1) in f(true)" "number";

  t ~opts:{default_compile_options with sound_optimizations=false}
    "test_unsound_dae" "let x = 2 + false in 3" "3";
  t ~opts:{default_compile_options with sound_optimizations=false}
    "test_unsound_const_fold_times_zero" "let f = (lambda x: x * 0) in f(false)" "0";
  t ~opts:{default_compile_options with sound_optimizations=false}
    "test_unsound_const_fold_or" "let f = (lambda x: x || true) in f(1)" "true";
  t ~opts:{default_compile_options with sound_optimizations=false}
    "test_unsound_const_fold_and" "let f = (lambda x: false && x) in f(1)" "false";
  t ~opts:{default_compile_options with sound_optimizations=false}
    "test_unsound_const_fold_plus" "let f = (lambda x: 0 + x) in f(true)" "true";
  t ~opts:{default_compile_options with sound_optimizations=false}
    "test_unsound_const_fold_times_one" "let f = (lambda x: x * 1) in f(true)" "true";
]

let string_tests = [
  tparse "string_parse_dqs1" "\"foo\"" (EString("foo", ()));
  tparse "string_parse_dqs2" "\"bar\\nbaz\"" (EString("bar\nbaz", ()));
  tparse "string_parse_sqs1" "'foobar'" (EString("foobar", ()));
  tparse "string_parse_sqs2" "'bar\\u41'" (EString("barA", ()));
  tparse "string_parse_sqs3" "'bar\\x41'" (EString("barA", ()));
  tparse "string_parse_sqs4" "'bar\\101'" (EString("barA", ()));
  tparse "string_parse_emoji_escape" "\"\xF0\x9F\x98\x82\"" (EString("😂", ()));
  tparse "string_parse_emoji_literal" "\"💯\"" (EString("💯", ()));

  t "string1" "\"foo\"" "\"foo\"";
  t "string2" "\"💯\"" "\"💯\"";
  t "string3" "\"making my way downtown, walking fast\"" "\"making my way downtown, walking fast\"";
  te "string_err" "let x = 'hello' in x + ', world'" "number";
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
  run_test_tt_main suite
;;

