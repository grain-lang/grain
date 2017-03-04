open Compile
open Runner
open Printf
open OUnit2
open ExtLib
open Types
open Expr
open Pretty

let t name program expected = name>::test_run [] program name expected;;
let tgc name heap_size program expected = name>::test_run [string_of_int heap_size] program name expected;;
let tvg name program expected = name>::test_run_valgrind [] program name expected;;
let tvgc name heap_size program expected = name>::test_run_valgrind [string_of_int heap_size] program name expected;;
let terr name program expected = name>::test_err [] program name expected;;
let tgcerr name heap_size program expected = name>::test_err [string_of_int heap_size] program name expected;;

let tfvs name program expected = name>::
  (fun _ ->
    let ast = parse_string name program in
    let anfed = anf (tag ast) in
    let vars = free_vars anfed in
    let c = Pervasives.compare in
    let str_list_print strs = "[" ^ (ExtString.String.join ", " strs) ^ "]" in
    assert_equal (List.sort ~cmp:c vars) (List.sort ~cmp:c expected) ~printer:str_list_print)
;;

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
  tvgc "oomgc3" 8 "(1, (3, 4))" "(1, (3, 4))";
  tgc "oomgc4" 4 "(3, 4)" "(3, 4)";
  tgcerr "oomgc5" 3 "(3, 4)" "Allocation";
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
]


let suite =
"suite">:::
 pair_tests @ oom @ gc



let () =
  run_test_tt_main suite
;;

