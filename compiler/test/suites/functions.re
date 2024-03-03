open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("functions", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);

  assertFileRun("fib1", "fib", "55\n");
  assertFileRun("fib2", "fib-better", "75025\n");
  assertFileRun("fib_big", "fib-bigint", "354224848179261915075\n");
  assertFileRun("indirect", "indirect-tail", "10\n");
  /* NOTE: This file also will test that we're doing tail calls
     and mutual recursion properly (should stack overflow otherwise) */
  /* Tests tail calls on only on one branch */
  assertFileRun("one_branch_tail_call", "oneBranchTail", "[2]\n");
  assertFileRun("forward_decl", "forward-decl", "true\n");
  /* This will test that we are doing tail calls for arbitrary-arity
     functions correctly */
  assertFileRun("sinister_tail_call", "sinister-tail-call", "true\n");
  assertRun("func_no_args", "let foo = (() => {print(5)});\nfoo()", "5\n");
  assertCompileError(
    "multi_bind",
    "let x = 2 and y = x + 1; y",
    "Unbound value x",
  );
  assertSnapshot("multi_bind2", "let x = 2 and y = 3; y");
  assertSnapshot("curried_func", "let add = a => b => a + b; add(2)(3)");
  assertCompileError("unbound_fun", "2 + foo()", "unbound");
  assertCompileError("unbound_id_simple", "5 - x", "unbound");
  assertCompileError("unbound_id_let", "let x = x; 2 + 2", "unbound");
  assertCompileError(
    "shadow_multi",
    "let x = 12 and x = 14; x",
    "Variable x is bound several times",
  );
  assertSnapshot(
    "dup_func",
    "let rec foo = (() => {5});\nlet bar = (() => { 7 });\nlet rec foo = (() => {9});\nfoo()",
  );
  assertCompileError("arity_1", "let foo = (() => {5});\nfoo(6)", "type");
  assertCompileError(
    "arity_2",
    "let foo = ((x) => {x + 5});\nfoo()",
    "missing an argument of type x: Number",
  );
  assertCompileError(
    "arity_3",
    "let foo = ((x) => {x});\nfoo(1, 2, 3)",
    "type",
  );
  assertSnapshot("shorthand_1", "let foo = (x) => x; foo(1)");
  assertSnapshot("shorthand_2", "let foo = (x) => x + 3; foo(1)");
  assertSnapshot("shorthand_3", "let foo = x => x; foo(1)");
  assertSnapshot("shorthand_4", "let foo = x => x + 3; foo(1)");
  // Trailing commas
  assertSnapshot(
    "fn_trailing_comma",
    "let testFn = (x, y,) => x + y; testFn(2, 3,)",
  );
  assertRun(
    "adt_trailing_comma",
    "enum Topping { Cheese, Pepperoni, Peppers, Pineapple, }
     enum Dough { WholeWheat, GlutenFree }
     enum Menu { Pizza(Topping,Dough,), Calzone(Topping,Dough,) }
     let item = Calzone(Peppers, WholeWheat,)
     print(item)
    ",
    "Calzone(Peppers, WholeWheat)\n",
  );
  assertSnapshot("lam_destructure_1", "((_) => 5)(\"foo\")");
  assertSnapshot("lam_destructure_2", "let foo = (_) => 5; foo(\"foo\")");
  assertSnapshot(
    "lam_destructure_3",
    "(((a, b, c)) => a + b + c)((1, 2, 3))",
  );
  assertSnapshot(
    "lam_destructure_4",
    "let foo = ((a, b, c)) => a + b + c; foo((1, 2, 3))",
  );
  assertSnapshot(
    "lam_destructure_5",
    "(((a, b, c), (x, y)) => a + b + c + x + y)((1, 2, 3), (4, 5))",
  );
  assertSnapshot(
    "lam_destructure_6",
    "let foo = ((a, b, c), (x, y)) => a + b + c + x + y; foo((1, 2, 3), (4, 5))",
  );
  assertSnapshot(
    "lam_destructure_7",
    "(((a, b, (c, d))) => a + b + c + d)((1, 2, (3, 4)))",
  );
  assertSnapshot(
    "lam_destructure_8",
    "let foo = ((a, b, (c, d))) => a + b + c + d; foo((1, 2, (3, 4)))",
  );
  assertRun("lambda_1", "print((x) => {x})", "<lambda>\n");
  assertSnapshot("app_1", "((x) => {x})(1)");
  assertRun(
    "letrec_1",
    "let rec x = ((n) => {if (n > 3) {n} else {x(n + 2)}})\n                        and y = ((n) => {x(n + 1)});\n                 print(y(2))",
    "5\n",
  );
  /* Check that recursion is order-independent */
  assertRun(
    "letrec_2",
    "let rec y = ((n) => {x(n + 1)})\n                        and x = ((n) => {if (n > 3) {n} else {x(n + 2)}});\n                 print(y(2))",
    "5\n",
  );
  assertRun(
    "let_1",
    "let rec x = ((n) => {n + 1})\n                     and y = (() => x(3))\n                     and z = ((n) => {x(n) + y()});\n               print(z(5))",
    "10\n",
  );
  assertCompileError(
    "let_norec_1",
    "let x = ((n) => {if (n > 3) {n} else {x(n + 2)}})\n                        and y = ((n) => {x(n + 1)});\n                 print(y(2))",
    "Unbound value x.",
  );
  assertCompileError(
    "lambda_dup_args",
    "((x, y, x) => {5})",
    "Variable x is bound several times",
  );
  assertCompileError(
    "lambda_arity_1",
    "((x) => {6})()",
    "missing an argument",
  );
  assertCompileError("lambda_arity_2", "((x) => {5})(1, 2)", "type");
  assertCompileError(
    "letrec_nonstatic_const",
    "let rec x = 5; x",
    "let rec may only be used with recursive function definitions",
  );
  assertCompileError(
    "letrec_nonstatic_same",
    "let x = x; x",
    "Unbound value x.\n       Hint: You are probably missing the `rec' keyword on line 1.",
  );
  assertCompileError(
    "letrec_nonstatic_other",
    "let rec x = ((z) => {z + 1}) and y = x; y(2)",
    "let rec may only be used with recursive function definitions",
  );
  assertCompileError("nonfunction_1", "let x = 5; x(3)", "type");
  assertSnapshot("lambda_pat_any", "let x = (_) => 5; x(\"foo\")");
  assertCompileError(
    "unknown_toplevel_attribute",
    "@unknown let x = () => 5",
    "Unknown top-level attribute",
  );
  assertCompileError(
    ~module_header="@unsafe module Test",
    "unknown_module_attribute",
    "",
    "Unknown module attribute",
  );
  assertSnapshot(
    "func_record_associativity1",
    "record Foo { f: () => Bool }; let foo = {f: () => false}; !foo.f()",
  );
  assertSnapshot(
    "func_record_associativity2",
    "record Foo { g: () => Bool }; record Bar { f: Foo }; let foo = {f: {g: () => false}}; !foo.f.g()",
  );

  assertSnapshot(
    "func_recursive_closure",
    {|let makeAdder = (n) => (x) => x + n
provide let truc = () => {
  let rec foo = (x) => {
    let baz = makeAdder(1);
    let bar = y => foo(0) + baz(1);
    if (x == 0) {
      0
    } else if (x == 1) {
      bar(1)
    } else {
      foo(x - 1)
    }
  }
  foo(5)
}
truc()|},
  );

  assertRun(
    "func_mutually_recursive_closure",
    {|
      let main = () => {
        let closureScope = "closureScope"
        let rec isEven = n => {
          print(closureScope)
          if (n <= 1) {
            n == 0
          } else {
            isOdd(n - 1)
          }
        }
        and isOdd = n => {
          if (n <= 1) {
            n == 1
          } else {
            isEven(n - 1)
          }
        }

        print(isOdd(3))
      }

    main()
  |},
    "closureScope\ntrue\n",
  );

  assertCompileError(
    "newline_before_arrow",
    {|
    let x = ()
      => 1
    |},
    "Expected an expression.",
  );

  assertSnapshot(
    "regression_1725",
    {|
    let foo = () => {
      let bar = return 5
      return 6
    }
    foo()
    |},
  );

  assertRun(
    "labeled_args1",
    {|
      let concat = (a, b) => a ++ b
      print(concat(a="1", b="2"))
    |},
    "12\n",
  );
  assertRun(
    "labeled_args2",
    {|
      let concat = (a, b) => a ++ b
      print(concat(a="1", "2"))
    |},
    "12\n",
  );
  assertRun(
    "labeled_args3",
    {|
      let concat = (a, b) => a ++ b
      print(concat("2", a="1"))
    |},
    "12\n",
  );
  assertRun(
    "labeled_args4",
    {|
      let concat = (a, b) => a ++ b
      print(concat("1", b="2"))
    |},
    "12\n",
  );
  assertRun(
    "labeled_args5",
    {|
      let concat = (a, b) => a ++ b
      print(concat(b="2", "1"))
    |},
    "12\n",
  );
  assertRun(
    "labeled_args6",
    {|
      let nothing = (a, b) => void
      nothing(b=print(1), print(2))
    |},
    "1\n2\n",
  );

  assertRun(
    "default_args1",
    {|
      let concat = (a="1", b) => a ++ b
      print(concat(a="3", "2"))
    |},
    "32\n",
  );
  assertRun(
    "default_args2",
    {|
      let concat = (a="1", b) => a ++ b
      print(concat(a="3", b="2"))
    |},
    "32\n",
  );
  assertRun(
    "default_args3",
    {|
      let concat = (a="1", b) => a ++ b
      print(concat(b="2", a="3"))
    |},
    "32\n",
  );
  assertRun(
    "default_args4",
    {|
      let concat = (a="1", b) => a ++ b
      print(concat("2"))
    |},
    "12\n",
  );
  assertRun(
    "default_args5",
    {|
      let concat = (a="1", b) => a ++ b
      print(concat(b="2"))
    |},
    "12\n",
  );
  assertRun(
    "default_args6",
    {|
      let concat = (a=b ++ b, b) => a ++ b
      print(concat(b="9"))
    |},
    "999\n",
  );

  assertRun(
    "labeled_args_typecheck1",
    {|
      let apply = (f: (arg1: Number) => Number) => f(arg1=5)
      print(apply(notarg1 => notarg1))
    |},
    "5\n",
  );

  assertCompileError(
    "labeled_args_err1",
    {|
      let concat = (a, b) => a ++ b
      print(concat(c="3"))
    |},
    "This argument cannot be supplied with label c",
  );
  assertCompileError(
    "labeled_args_err2",
    {|
      let concat = (a, b) => a ++ b
      print(concat("1", "2", c="3"))
    |},
    "This argument cannot be supplied with label c",
  );
  assertCompileError(
    "labeled_args_err3",
    {|
      let concat = (a="1", b) => a ++ b
      print(concat("1", "2"))
    |},
    "Did you mean to supply an argument with label a?",
  );
  assertCompileError(
    "labeled_args_err4",
    {|
      let apply = (f: (?arg1: Number) => Number) => f(arg1=5)
      print(apply(notarg1 => notarg1))
    |},
    "The expected function type contains the argument \\?arg1",
  );
  assertCompileError(
    "labeled_args_err5",
    {|
      let apply = (f: (?arg1: Number) => Number) => f(arg1=5)
      print(apply(notarg1 => notarg1))
    |},
    "which has a default value, but the matching argument does not.",
  );
});
