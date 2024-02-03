open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_utils;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

describe("pattern matching", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);
  let assertWarning = makeWarningRunner(test);
  let assertNoWarning = makeNoWarningRunner(test);

  /* Pattern matching on tuples */
  assertRun("tuple_match_1", "print(match ((1, 2)) { (a, _) => a })", "1\n");
  assertRun(
    "tuple_match_2",
    "print(match ((1, 2, 3)) { (a, b, c) => a + b + c })",
    "6\n",
  );
  assertSnapshot(
    "tuple_match_3",
    "match ((1, \"boop\", false)) { (a, b, c) => (a, b, c) }",
  );
  assertSnapshot(
    "tuple_match_deep",
    "match ((1, (4, 5), 3)) { (a, (d, e), c) => a + c + d + e }",
  );
  assertSnapshot(
    "tuple_match_deep2",
    "match ((1, (2, (3, (4, 5, (6, 7)))))) { (a, (b, (c, (d, e, (f, g))))) => a + b + c + d + e + f + g }",
  );
  assertSnapshot(
    "tuple_match_deep3",
    "match ((1, [])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
  );
  assertSnapshot(
    "tuple_match_deep4",
    "match ((1, [2])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
  );
  assertSnapshot(
    "tuple_match_deep5",
    "match ((1, [4, 5])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
  );
  assertSnapshot(
    "tuple_match_deep6",
    "match ((1, [4, 5, 6])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
  );
  assertSnapshot(
    "tuple_match_deep7",
    "match ((1, [4, 5, 6, 7])) { (a, []) => a, (a, [b]) => a + b, (a, [b, c]) => a + b + c, (a, [b, c, d]) => a + b + c + d, (_, [_, ..._]) => 999 }",
  );
  /* Pattern matching on records */
  assertSnapshot(
    "record_match_1",
    "record Rec {foo: Number, bar: String, baz: Bool}; match ({foo: 4, bar: \"boo\", baz: true}) { { foo, _ } => foo }",
  );
  assertSnapshot(
    "record_match_2",
    "record Rec {foo: Number, bar: String, baz: Bool}; match ({foo: 4, bar: \"boo\", baz: true}) { { bar, _ } => bar }",
  );
  assertSnapshot(
    "record_match_3",
    "record Rec {foo: Number, bar: Number, baz: Number}; match ({foo: 4, bar: 5, baz: 6}) { { foo, bar, _ } => foo + bar }",
  );
  assertSnapshot(
    "record_match_4",
    "record Rec {foo: Number, bar: Number, baz: Number}; match ({foo: 4, bar: 5, baz: 6}) { { foo, bar, baz } => foo + bar + baz}",
  );
  assertSnapshot(
    "record_match_deep",
    "record Rec {foo: Number}; record Rec2 {bar: Rec}; match ({bar: {foo: 4}}) { { bar: { foo } } => foo }",
  );
  assertCompileError(
    "record_match_deep_alias",
    "record Rec {foo: Number}; record Rec2 {bar: Rec}; match ({bar: {foo: 4}}) { { bar: { foo } } => bar }",
    "Unbound value bar",
  );
  /* Pattern matching on ADTs */
  assertSnapshot(
    "adt_match_1",
    "match ([]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
  );
  assertSnapshot(
    "adt_match_2",
    "match ([2]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
  );
  assertSnapshot(
    "adt_match_3",
    "match ([4, 5]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
  );
  assertSnapshot(
    "adt_match_4",
    "match ([4, 5, 6]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
  );
  assertSnapshot(
    "adt_match_5",
    "match ([4, 5, 6, 7]) { [] => 0, [b] => b, [b, c] => b + c, [b, c, d] => b + c + d, [_, ..._] => 999 }",
  );
  assertSnapshot(
    "adt_match_deep",
    "record Rec {foo: Number}; match ([{foo: 5}]) { [] => 999, [{foo}, ..._] => foo }",
  );
  // Guarded branches
  assertSnapshot(
    "guarded_match_1",
    "match ((1, 2, 3)) { (a, b, c) when a == 1 => 42, _ => 99 }",
  );
  assertSnapshot(
    "guarded_match_2",
    "match ((2, 2, 3)) { (a, b, c) when a == 1 => 42, _ => 99 }",
  );
  assertSnapshot(
    "guarded_match_3",
    "match ((2, 2, 3)) { (a, b, c) when (a == 2) && (c == 3) => 42, _ => 99 }",
  );
  assertSnapshot(
    "guarded_match_4",
    "match ((8, 2, 3)) { (a, b, c) when (a == 2) && (c == 3) => 42, _ => 99 }",
  );
  assertRun(
    "guarded_match_5",
    "enum ADT { Foo((String, Number)), Bar }
     let value = match (Foo((\"abcd\", 4))) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }
     print(value)",
    "42\n",
  );
  assertRun(
    "guarded_match_6",
    "enum ADT { Foo((String, Number)), Bar }
     let value = match (Foo((\"abcd\", 3))) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }
     print(value)",
    "89\n",
  );
  assertRun(
    "guarded_match_7",
    "enum ADT { Foo((String, Number)), Bar }
     let value = match (Foo((\"wxyz\", 4))) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }
     print(value)",
    "99\n",
  );
  assertRun(
    "guarded_match_8",
    "enum ADT { Foo((String, Number)), Bar }
     let value = match (Foo((\"wxyz\", 15))) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }
     print(value)",
    "89\n",
  );
  assertRun(
    "guarded_match_9",
    "enum ADT { Foo((String, Number)), Bar }
     let value = match (Bar) {
       Foo((s, n)) when (s == \"abcd\") && (n == 4) => 42,
       Foo((s, n)) when (s == \"wxyz\") && (n == 4) => 99,
       Foo(_) when false => 90,
       Foo(_) => 89,
       Bar => 79
     }
     print(value)",
    "79\n",
  );
  assertRun(
    "guarded_match_10",
    "let value = match ([1, 2, 3]) {
       [a, b, ...rest] when false => 42,
       [a, b, c, ...rest] when true => 24,
       _ => 79
     }
     print(value)",
    "24\n",
  );
  // Constant patterns
  assertSnapshot(
    "constant_match_1",
    "match (1/3) { 5 => false, 2/6 => true, _ => false }",
  );
  assertSnapshot(
    "constant_match_2",
    "match ((\"foo\", 5, false)) { (\"bar\", 5, false) => false, (\"foo\", _, true) => false, (\"foo\", _, false) => true, _ => false }",
  );
  assertSnapshot(
    "constant_match_3",
    "match (\"foo\") { \"foo\" when false => false, \"foo\" when true => true, _ => false }",
  );
  assertSnapshot(
    "constant_match_4",
    "match ((\"foo\", 5)) { (\"foo\", n) when n == 7 => false, (\"foo\", 9) when true => false, (\"foo\", n) when n == 5 => true, _ => false }",
  );
  // Constant low level wasm type patterns
  assertSnapshot(
    "low_level_constant_match_1",
    "@unsafe let _ = print(match (1n) { 0n => false, 1n => true, 2n => false, _ => false} )",
  );
  assertSnapshot(
    "low_level_constant_match_2",
    "@unsafe let _ = print(match (1N) { 0N => false, 1N => true, 2N => false, _ => false} )",
  );
  assertSnapshot(
    "low_level_constant_match_3",
    "@unsafe let _ = print(match (1.0w) { 0.0w => false, 1.0w => true, 2.0w => false, _ => false} )",
  );
  assertSnapshot(
    "low_level_constant_match_4",
    "@unsafe let _ = print(match (1.0W) { 0.0W => false, 1.0W => true, 2.0W => false, _ => false} )",
  );
  // Or patterns
  assertSnapshot("or_match_1", "match (true) { true | false => 3 }");
  assertSnapshot(
    "or_match_2",
    "match (Some(5)) { Some(3 | 4) => false, Some(5) | None => true, _ => false }",
  );
  assertSnapshot(
    "or_match_3",
    "match ([5]) { [a, _] | [_, a, _] | [a] => true, _ => false }",
  );
  assertSnapshot("or_match_4", {|match (true) { true
  | false => 3 }|});
  // Aliases
  assertSnapshot("alias_match_1", "match (true) { _ as p => p }");
  assertSnapshot("alias_match_2", "match (true) { a as b => a && b }");
  assertRun(
    "alias_match_3",
    "match (true) { true | false as p => print(p) }",
    "true\n",
  );
  assertSnapshot(
    "alias_match_4",
    "match (Some(5)) { Some(3 | 4 as a) => a, Some(_) | None => 5, _ => 6 }",
  );
  assertSnapshot(
    "alias_match_5",
    "match (Some(5)) { Some(3 | 4) as a => a, Some(5) | None as a => a, _ => None }",
  );
  assertFileRun("mixed_matching", "mixedPatternMatching", "true\n");
  assertWarning(
    "bool_exhaustiveness1",
    "match (true) {
       true => print(5),
     }",
    Warnings.PartialMatch("false"),
  );
  assertWarning(
    "bool_exhaustiveness2",
    "match (true) {
       false => print(5),
     }",
    Warnings.PartialMatch("true"),
  );
  assertWarning(
    "bool_exhaustiveness3",
    "match (true) {
       true => print(5),
       true => print(5),
     }",
    Warnings.PartialMatch("false"),
  );
  assertNoWarning(
    "bool_exhaustiveness4",
    "match (true) {
       false => print(5),
       true => print(5),
     }",
  );
  assertWarning(
    "bool_exhaustiveness5",
    "match (Some(true)) {
       Some(false) => print(5),
       None => print(5),
     }",
    Warnings.PartialMatch("Some(true)"),
  );
  assertNoWarning(
    "bool_exhaustiveness6",
    "match (Some(true)) {
       Some(false) => print(5),
       Some(true) => print(5),
       None => print(5),
     }",
  );
  assertWarning(
    "bool_exhaustiveness7",
    "match (true) {
       true when true => print(5),
       false => print(5),
     }",
    Warnings.PartialMatch(
      "true\n(However, some guarded clause may match this value.)",
    ),
  );
  assertWarning(
    "let_exhaustiveness",
    "let a = None\nlet Some(b) = a",
    Warnings.PartialMatch("None"),
  );
  assertCompileError(
    "newline_before_arrow",
    {|
      match (1) {
        a
          => a
      }
    |},
    "Expected `=>` followed by an expression or a branch guard—the keyword `when` followed by an expression",
  );
  assertCompileError(
    "newline_before_arrow_2",
    {|
      match (1) {
        a when a = 1
          => a
      }
    |},
    "Expected `=>` followed by an expression.",
  );

  // destructuring
  assertRun(
    "destructure_constant",
    {|
      let 1 | _ = 5
      print("ok")
    |},
    "ok\n",
  );
  assertRun(
    "destructure_singleton_adt",
    {|
      enum NumWrapper { NumWrapper(Number) }
      let NumWrapper(a) = NumWrapper(5)
      print(a)
    |},
    "5\n",
  );
  assertRun(
    "destructure_adt",
    {|
      enum Foo { A(Number), B(Number) }
      let A(val1) | B(val1) = A(5)
      let A(val2) | B(val2) = B(6)
      print(val1)
      print(val2)
    |},
    "5\n6\n",
  );
  assertRun(
    "destructure_tuple",
    {|
      let (a, b) = (3, 4)
      print(a + b)
    |},
    "7\n",
  );
  assertRun(
    "destructure_mut_tuple",
    {|
      let run = () => {
        let mut (a, b) = (1, 2)

        print(b)
      }

      run()
    |},
    "2\n",
  );
  assertRun(
    "destructure_mut_tuple_closure",
    {|
      let run = () => {
        let mut (a, b) = (1, 2)

        print(b)

        () => b
      }

      run()
    |},
    "2\n",
  );
  assertRun(
    "destructure_record",
    {|
      record Rec { a: Number, b: Number }
      let {a, b} = { a: 3, b: 4 }
      print(a + b)
    |},
    "7\n",
  );

  // inline record constructors
  assertRun(
    "inline_rec_pattern_1",
    {|
      enum E {
        Tup(String, Number),
        Rec { x: Number, y: String }
      }
      let a = Rec { x: 123, y: "abc" }
      match (a) {
        Rec { y, x } => print(x),
        _ => fail ""
      }
      match (a) {
        Rec { y, _ } | Tup(y, _) => print(y),
        _ => fail ""
      }
      match (a) {
        Rec { _ } => print("record"),
        _ => fail ""
      }
    |},
    "123\nabc\nrecord\n",
  );
  assertRun(
    "inline_rec_pattern_2",
    {|
      enum E {
        Rec { x: Number, y: String }
      }
      let a = Rec { x: 123, y: "abc" }
      let Rec { y, _ } = a
      print(y)
    |},
    "abc\n",
  );
  assertCompileError(
    "inline_rec_pattern_3",
    {|
      enum E {
        T(Number),
        R { x: Number }
      }
      match (T(1)) {
        T { _ } => print("success"),
        _ => fail ""
      }
    |},
    "T is a tuple constructor but a record constructor pattern was given.",
  );
  assertCompileError(
    "inline_rec_pattern_4",
    {|
      enum Rec {
        Rec { x: Number }
      }
      match (Rec { x: 1 }) {
        Rec(y) => print("success")
      }
    |},
    "Rec is a record constructor but a tuple constructor pattern was given.",
  );
});
