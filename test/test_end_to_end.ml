open Grain.Compile
open Grain.Runner
open Grain_utils
open Printf
open OUnit2
open Batteries
open Grain_middle_end.Anftree
open Grain_middle_end.Anf_helper

let wrap_todo todo f x =
  match todo with
  | Some(msg) -> OUnit2.todo msg
  | None -> f x

let t ?todo name program expected = name>::(wrap_todo todo @@ test_run program name expected);;
let tlib ?todo name program expected = name>::(wrap_todo todo @@ test_run program name expected);;
let tgc ?todo name heap_size program expected = name>::(wrap_todo todo @@ test_run program name expected);;
let terr ?todo name program expected = name>::(wrap_todo todo @@ test_err program name expected);;
let tgcerr ?todo name heap_size program expected = name>::(wrap_todo todo @@ test_err program name expected);;

let te ?todo name program expected = name>::(wrap_todo todo @@ test_err program name expected);;
let telib ?todo name program expected = name>::(wrap_todo todo @@ test_err program name expected);;

(* let tfvs ?todo name program expected = name>::
  (fun _ ->
    begin match todo with
      | Some(msg) -> OUnit2.todo msg
      | None -> ()
    end;
    try
      let open Grain_typed in
      let ast = parse_string name program in
      let typed_tree = Grain_typed.Typemod.type_implementation ast in
      let anfed = anf typed_tree in
      let vars = free_vars (anfed.body) in
      let c = Pervasives.compare in
      let str_list_print strs = "[" ^ (ExtString.String.join ", " strs) ^ "]" in
      let id_list_print ids = str_list_print (List.map Ident.unique_name ids) in
      assert_equal (List.sort ~cmp:Ident.compare vars) (List.sort ~cmp:c expected) ~printer:id_list_print
    with x ->
      (*Grain_parsing.Location.report_exception Format.err_formatter x;*)
      raise x)
;; *)

let test_input_file filename name expected test_ctxt =
  try
    let input_filename = "input/" ^ filename ^ ".gr" in
    let outfile = "output/" ^ name in
    let compiled = compile_file ~hook:stop_after_compiled ~outfile input_filename in
    let result = run_output compiled in
    assert_equal ~printer:(fun x -> x) (expected ^ "\n") result
  with x ->
    (*Grain_parsing.Location.report_exception Format.err_formatter x;*)
    raise x

let test_err_input_file filename name errmsg test_ctxt =
  let result = try
      let input_filename = "input/" ^ filename ^ ".gr" in
      let outfile = "output/" ^ name in
      let compiled = compile_file ~hook:stop_after_compiled ~outfile input_filename in
      run_output compiled
    with x ->
      (*Grain_parsing.Location.report_exception Format.err_formatter x;*)
      Printexc.to_string x in
  assert_equal
    (errmsg)
    result
    ~cmp: (fun check result -> String.exists result check)

let test_optimizations_sound program_str name expected test_ctxt =
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in
  let compile_and_run outfile =
    run_output (compile_string ~hook:stop_after_compiled ~name ~outfile program_str) in
  let result_unoptimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := false;
      compile_and_run full_outfile_unoptimized) in
  let result_optimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := true;
      compile_and_run full_outfile_optimized) in
  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (expected ^ "\n") result_optimized

let test_optimizations_sound_err program_str name errmsg test_ctxt =
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in
  let compile_and_run outfile =
    run_output (compile_string ~hook:stop_after_compiled ~name ~outfile program_str) in
  let result_unoptimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := false;
      compile_and_run full_outfile_unoptimized) in
  let result_optimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := true;
      compile_and_run full_outfile_optimized) in
  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (errmsg) result_optimized
    ~cmp: (fun check result -> String.exists result check)

let test_file_optimizations_sound filename name expected test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in

  let compile_and_run outfile =
    run_output (compile_file ~hook:stop_after_compiled ~outfile input_filename) in
  let result_unoptimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := false;
      compile_and_run full_outfile_unoptimized) in
  let result_optimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := true;
      compile_and_run full_outfile_optimized) in

  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (expected ^ "\n") result_optimized

let test_file_optimizations_sound_err filename name errmsg test_ctxt =
  let input_filename = "input/" ^ filename ^ ".gr" in
  let full_outfile_unoptimized = "output/" ^ name ^ ".no-optimize" in
  let full_outfile_optimized = "output/" ^ name ^ "optimize" in
  let compile_and_run outfile =
    run_output (compile_file ~hook:stop_after_compiled ~outfile input_filename) in
  let result_unoptimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := false;
      compile_and_run full_outfile_unoptimized) in
  let result_optimized = Config.preserve_config (fun () ->
      Config.optimizations_enabled := true;
      compile_and_run full_outfile_optimized) in
  assert_equal
    result_optimized
    result_unoptimized;
  assert_equal (errmsg) result_optimized
    ~cmp: (fun check result -> String.exists result check)

(** Tests that the file input/`input_file`.egg produces
    the given output *)
let tfile ?todo name input_file expected = name>::(wrap_todo todo @@ test_input_file input_file name expected)
(** Tests that the file input/`input_file`.egg produces
    the given error message *)
let tefile ?todo name input_file errmsg = name>::(wrap_todo todo @@ test_err_input_file input_file name errmsg)

let tgcfile ?todo name heap_size input_file expected = name>::(wrap_todo todo @@ test_input_file input_file name expected)

let tgcefile ?todo name heap_size input_file errmsg = name>::(wrap_todo todo @@ test_err_input_file input_file name errmsg)

(*let test_resolve_scope opts program_str outfile (expected : 'a aprogram) test_ctxt =
  let anf = compile_string_to_anf outfile opts program_str in
  let result = Grain.Pretty.string_of_aprogram (Grain.Resolve_scope.resolve_scope anf Grain.Compile.initial_load_env) in
  assert_equal (Grain.Pretty.string_of_aprogram expected) result*)

let test_final_anf program_str outfile (expected : Grain_middle_end.Anftree.anf_expression) test_ctxt =
  let open Grain_middle_end in
  let final_anf = Anf_utils.clear_locations @@ compile_string_to_final_anf outfile program_str in
  let saved_disabled = !Grain_typed.Ident.disable_stamps in
  let result, expected = try begin
      Grain_typed.Ident.disable_stamps := true;
      let result = Sexplib.Sexp.to_string_hum @@ Anftree.sexp_of_anf_expression (final_anf.body) in
      let expected = (Sexplib.Sexp.to_string_hum @@ Anftree.sexp_of_anf_expression expected) in
      result, expected
    end with e ->
      Grain_typed.Ident.disable_stamps := saved_disabled;
      raise e
  in
  assert_equal ~printer:(fun x -> x) expected result

(*let trs ?todo name (program : string) (expected : 'a aprogram) = name>::(wrap_todo todo @@ test_resolve_scope default_compile_options program name expected);;*)

let tfinalanf name ?todo (program : string) (expected : Grain_middle_end.Anftree.anf_expression) =
  name>::(wrap_todo todo @@ test_final_anf program name expected);;

let tsound ?todo name prog expected = name>::(wrap_todo todo @@ test_optimizations_sound prog name expected);;

let tesound ?todo name prog expected = name>::(wrap_todo todo @@ test_optimizations_sound_err prog name expected);;

let tfsound ?todo name filename expected = name>::(wrap_todo todo @@ test_file_optimizations_sound filename name expected);;

let tefsound ?todo name filename errmsg = name>::(wrap_todo todo @@ test_file_optimizations_sound_err filename name errmsg);;

let test_parse ?todo name input (expected : Grain_parsing.Parsetree.parsed_program) test_ctxt =
  begin match todo with
  | Some(msg) -> OUnit2.todo msg
  | _ -> ()
  end;
  let open Grain_parsing in
  let location_stripper = {Ast_mapper.default_mapper with location = (fun _ _ -> Location.dummy_loc)} in
  let strip_locs ({statements; body; _} : Parsetree.parsed_program) =
    let open Parsetree in
    {
      statements=(List.map (location_stripper.toplevel location_stripper) statements);
      body=location_stripper.expr location_stripper body;
      prog_loc=Location.dummy_loc;
    } in
  let parsed = strip_locs @@ parse_string name input in
  let untagged = strip_locs @@ parsed in
  assert_equal expected
    untagged ~printer:(fun p -> Sexplib.Sexp.to_string_hum @@ Grain_parsing.Parsetree.sexp_of_parsed_program p)

let tparse ?todo name input expected = name>::(wrap_todo todo @@ test_parse name input expected)

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
  t "multi_bind" "let rec x = 2, y = x + 1; y" "3";
  te "unbound_fun" "2 + foo()" "unbound";
  te "unbound_id_simple" "5 - x" "unbound";
  te "unbound_id_let" "let x = x; 2 + 2" "unbound";
  te "shadow_multi" "let x = 12, x = 14; x" "Variable x is bound several times";
  t "dup_func" "let rec foo = (() => {5});\nlet bar = (() => { 7 });\nlet rec foo = (() => {9});\nfoo()" "9";
  te "arity_1" "let foo = (() => {5});\nfoo(6)" "type";
  te "arity_2" "let foo = ((x) => {x + 5});\nfoo()" "type";
  te "arity_3" "let foo = ((x) => {x});\nfoo(1, 2, 3)" "type";
]

let mylist = "Cons(1, Cons(2, Cons(3, Empty)))"

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
  tlib ~todo:"ADT printing NYI" "stdlib_cons" ("import lists; " ^ mylist) "Cons(1, Cons(2, Cons(3, Empty)))";
  tlib "stdlib_sum_1" ("import lists; sum(" ^ mylist ^ ")") "6";
  tlib "stdlib_sum_2" "import lists; sum(Empty)" "0";
  tlib ~todo:"ADT printing NYI"
    "stdlib_reverse" ("import lists; reverse(" ^ mylist ^ ")") "Cons(3, Cons(2, Cons(1, Empty)))";
  tlib "stdlib_length" "import lists; length(Cons(1, Cons(2, Cons(3, Empty))))" "3";
  tlib "stdlib_equal_1" "import lists; (1, 2) == (1, 2)" "false";
  tlib "stdlib_equal_2" "import pervasives; equal((1, 2), (1, 2))" "true";
  tlib "stdlib_equal_3" "import lists; equal(Cons(1, Cons(2, Cons(3, Empty))), Cons(1, Cons(2, Cons(3, Empty))))" "true";
  tlib "stdlib_equal_4" "import lists; equal(1, 1)" "true";
  tlib "stdlib_equal_5" "import lists; equal(1, 2)" "false";
  tlib "stdlib_equal_6" "import lists; equal(true, true)" "true";
  tlib "stdlib_equal_7" "import lists; equal(true, false)" "false";
  tlib "stdlib_contains_1" "import lists; contains(true, Cons(1, Cons(2, Cons(3, Empty))))" "false";
  tlib "stdlib_contains_2" "import lists; contains(false, Cons(1, Cons(2, Cons(3, Empty))))" "false";
  tlib "stdlib_contains_3" "import lists; contains(3, Cons(1, Cons(2, Cons(3, Empty))))" "true";
  telib "stdlib_err_1" "import lists; Cons(1)" "cannot be called with 1 argument";
  telib "stdlib_err_2" "import lists; Cons()" "cannot be called with 0 arguments";
  telib "stdlib_err_3" "import lists; Cons(1, 2, 3)" "cannot be called with 3 arguments";
  telib "stdlib_sum_err" "import lists; sum(Cons(true, false))" "This expression has type Bool but";
  telib "stdlib_length_err" "import lists; length(true)" "This expression has type Bool but";
  telib "stdlib_reverse_err" "import lists; reverse(1)" "This expression has type Number but";
]

(* Note that our tail call tests above provide a good
   stress test of our lambda and letrec implementations
   (also see the files in input/, which the above suites run)*)
let fer_de_lance_tests = [
  t "lambda_1" "(x) => {x}" "<lambda>";
  t "app_1" "((x) => {x})(1)" "1";
  t "letrec_1" "let rec x = ((n) => {if n > 3 {n} else {x(n + 2)}}),
                        y = ((n) => {x(n + 1)});
                 y(2)" "5";
  (* Check that recursion is order-independent *)
  t "letrec_2" "let rec y = ((n) => {x(n + 1)}),
                        x = ((n) => {if n > 3 {n} else {x(n + 2)}});
                 y(2)" "5";
  t "let_1" "let rec x = ((n) => {n + 1}),
                     y = x(3),
                     z = ((n) => {x(n) + y});
               z(5)" "10";
  te "let_norec_1" "let x = ((n) => {if n > 3 {n} else {x(n + 2)}}),
                        y = ((n) => {x(n + 1)});
                 y(2)" "Unbound value x.";
  te "lambda_dup_args" "((x, y, x) => {5})" "Variable x is bound several times";
  te "lambda_arity_1" "((x) => {6})()" "type";
  te "lambda_arity_2" "((x) => {5})(1, 2)" "type";
  t "letrec_nonstatic_const" "let rec x = 5; x" "5";
  te ~todo:"Recursive binding checking NYI" "letrec_nonstatic_same" "let rec x = x; x" "Unbound value x.\n       Hint: You are probably missing the `rec' keyword on line 1.";
  t "letrec_nonstatic_other" "let rec x = ((z) => {z + 1}), y = x; y(2)" "3";
  te "nonfunction_1" "let x = 5; x(3)" "type";
]

let fer_de_lance_stdlib_tests = [
  tlib ~todo:"ADT printing NYI" "map_1" ("import lists; map(((x) => {x + 1}), " ^ mylist ^ ")")
    "(2, (3, (4, false)))";
  tlib ~todo:"ADT printing NYI" "map_2" ("import lists; map(((x) => {x * 2}), " ^ mylist ^ ")")
    "(2, (4, (6, false)))";
  tlib ~todo:"ADT printing NYI" "map_print" ("import lists; map(print, " ^ mylist ^ ")") "3\n2\n1\n(1, (2, (3, empty)))";
  tlib "fold_left_1" ("import lists; fold_left(((acc, cur) => {acc - cur}), 0, " ^ mylist ^ ")")
    "-6";
  tlib "fold_right_1" ("import lists; fold_right(((cur, acc) => {cur - acc}), 0, " ^ mylist ^ ")")
    "2";
]

let box_tests = [
  t "box1" "let b = box(4);
            {
              unbox(b)
            }" "4";
  t "box2" "let b = box((4, (5, 6)));
            {
              unbox(b)
            }" "(4, (5, 6))";
  t "box3" "let b = box(box(4));
            {
              unbox(unbox(b))
            }" "4";
  t "box4" "let b = box(4);
            {
              b := 3;
              unbox(b)
            }" "3";
  t "box5" "let b = box(4);
            {
              b := unbox(b) - 1;
              unbox(b)
            }" "3";
]

let loop_tests = [
  t "loop1" "let b = box(3);
            {
              while (unbox(b) > 0) {
                b := unbox(b) - 1
              };
              unbox(b)
            }" "0";
  t "loop2" "let b = box(12);
             let count = box(0);
            {
              while (unbox(b) > 0) {
                b := unbox(b) - 1;
                count := unbox(count) + 1
              };
              unbox(count)
            }" "12";
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
  t "test_set_extra1" "box(1) := 2" "2";
  tfile "counter" "counter" "1\n2\n3\n3";
  (*te "test_bad_import" "let x = (1, 2); import lists; x" "Includes must be at the beginning";*)
  te "test_missing_import" "import foo; 2" "Unbound module";
  te "test_unbox_err" "unbox(5)" "Box";
]

(* Note that optimizations are on by default, so all of the above tests
   check that it works correctly as well *)
let indigo_tests = [
  (* note on resolve-scope test: (tags are not checked) *)
  t "trs1"
    "let f1 = ((x, y) => {x}),
         f2 = ((x, y) => {y});
       f1(1, 2)"
    (*(ALet("f1$1",
          CLambda(["x$2"; "y$3"],
                  ACExpr(CImmExpr(ImmId("x$2", 3))), 2),
          ALet("f2$4",
               CLambda(["x$5"; "y$6"],
                              ACExpr(CImmExpr(ImmId("y$6", 3))), 4),
             ACExpr(CApp(ImmId("f1$1", 3), [ImmNum(1, 3); ImmNum(2, 3)], 3)), 3),
          1))*) "1";

  (* Primarily a constant-propagation test, but DAE removes the let bindings as well *)
  tfinalanf "test_const_propagation" "{
    let x = 4;
    let y = x;
    x}"
    (AExp.comp (Comp.imm (Imm.const (Const_int 4))));

  (* Primarily a constant-propagation test, but DAE removes the let bindings as well *)
  tfinalanf "test_const_propagation2" "((x) => {
    let x = 4;
    let y = x;
    x})"
    (let open Grain_typed in
     let x = Ident.create "x" in
     AExp.comp (Comp.lambda [x]
                     (AExp.comp (Comp.imm (Imm.const (Const_int 4))))));

  (* Primarily a constant-propagation test, but DAE removes the let bindings as well *)
  tfinalanf "test_const_propagation_shadowing" "{
  let x = 5;
  let y = 12;
  let z = y;
  {
    let y = x;
    x
  };
  x + y}"
    (AExp.seq (Comp.imm (Imm.const (Const_int 5)))
      (AExp.comp (Comp.imm (Imm.const (Const_int 17)))));

  (* Primarily a constant-folding test, but DAE removes the let bindings as well *)
  tfinalanf "test_const_folding" "{
    let x = 4 + 5;
    let y = x * 2;
    let z = y - x;
    let a = x + 7;
    let b = 14;
    a + b}" (AExp.comp (Comp.imm (Imm.const (Const_int 30))));

  tfinalanf ~todo:"Optimizations not yet ported" "test_cse" "((x) => {let a = x + 1; let b = x + 1; a + b})"
    (let open Grain_typed in
     let x = Ident.create "x" in
     let a = Ident.create "a" in
     AExp.comp (Comp.lambda [x]
                  (AExp.let_ Nonrecursive [(a, Comp.prim2 Plus (Imm.id x) (Imm.const (Const_int 1)))]
                     (AExp.comp (Comp.prim2 Plus (Imm.id a) (Imm.id a))))));

  tfinalanf "test_dae" "((x) => {let a = x + 1; let b = x + 1; x + 1})"
    (let open Grain_typed in
     let x = Ident.create "x" in
     AExp.comp (Comp.lambda [x] @@ AExp.comp @@ Comp.prim2 Plus (Imm.id x) (Imm.const (Const_int 1))));

  tfinalanf "test_dae_lambda_unused" "((x) => {1})"
    (let open Grain_typed in
     let x = Ident.create "x" in
     AExp.comp (Comp.lambda [x]
     (AExp.comp (Comp.imm (Imm.const (Const_int 1))))));

  (* All optimizations are needed to work completely on this input *)
  tfinalanf ~todo:"CSE NYI" "test_optimizations_work_together" "
    let x = 5;
    let foo = ((y) => {y});
    let y = foo(3) + 5;
    foo(3) + x"
    (let open Grain_typed in
     let foo = Ident.create "foo" in
     let y = Ident.create "y" in
     let app = Ident.create "app" in
     AExp.let_ Nonrecursive [(foo, Comp.lambda [y] @@ AExp.comp @@ Comp.imm @@ Imm.id y)]
     @@ AExp.let_ Nonrecursive [(app, Comp.app (Imm.id foo) [(Imm.const (Const_int 3))])]
     @@ AExp.comp @@ Comp.prim2 Plus (Imm.id app) (Imm.const (Const_int 5)));

  tfsound ~todo:"mutable types NYI" "test_counter_sound" "counter" "0\n1\n2\n2";
  tefsound ~todo:"TCO NYI" "fib_big" "too-much-fib" "overflow";
  te "test_dae_sound" "let x = 2 + false; 3" "type";
  (*te "test_const_fold_times_zero_sound" "let f = ((x) => {x * 0}); f(false)" "number";
  te "test_const_fold_or_sound" "let f = ((x) => {x or true}); f(1)" "bool";
  te "test_const_fold_and_sound" "let f = ((x) => {false and x}); f(1)" "bool";
  te "test_const_fold_plus_sound" "let f = ((x) => {0 + x}); f(true)" "number";
  te "test_const_fold_times_one_sound" "let f = ((x) => {x * 1}); f(true)" "number";*)

  (*te ~opts:{default_compile_options with sound_optimizations=false}
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
    "test_unsound_const_fold_times_one" "let f = ((x) => {x * 1}); f(true)" "true";*)
]

let string_tests =
  let open Grain_parsing in
  let open Ast_helper in
  let str s = Exp.constant (Const.string s) in
  [
  tparse "string_parse_dqs1" "\"foo\"" {statements=[]; body=str "foo"; prog_loc=Location.dummy_loc};
  tparse "string_parse_dqs2" "\"bar\\nbaz\"" {statements=[]; body=str "bar\nbaz"; prog_loc=Location.dummy_loc};
  tparse "string_parse_sqs1" "'foobar'" {statements=[]; body=str "foobar"; prog_loc=Location.dummy_loc};
  tparse "string_parse_sqs2" "'bar\\u41'" {statements=[]; body=str "barA"; prog_loc=Location.dummy_loc};
  tparse "string_parse_sqs3" "'bar\\x41'" {statements=[]; body=str "barA"; prog_loc=Location.dummy_loc};
  tparse "string_parse_sqs4" "'bar\\101'" {statements=[]; body=str "barA"; prog_loc=Location.dummy_loc};
  tparse "string_parse_emoji_escape" "\"\xF0\x9F\x98\x82\"" {statements=[]; body=str "😂"; prog_loc=Location.dummy_loc};
  tparse "string_parse_emoji_literal" "\"💯\"" {statements=[]; body=str "💯"; prog_loc=Location.dummy_loc};

  t "string1" "\"foo\"" "\"foo\"";
  t "string2" "\"💯\"" "\"💯\"";
  t "string3" "\"making my way downtown, walking fast\"" "\"making my way downtown, walking fast\"";
  te "string_err" "let x = \"hello\"; x + \", world\"" "type";
]

let data_tests =
  [
    tfile "basicdata" "basicdata" "(false, true, true)";
  ]

let tests =
  "End to end">:::
  cobra_tests @
  diamondback_tests @
  egg_eater_tests @
  egg_eater_stdlib_tests @
  fer_de_lance_tests @
  fer_de_lance_stdlib_tests @
  box_tests @ loop_tests @(*oom @ gc @*) garter_extra_tests @
  indigo_tests @ string_tests @ data_tests




