open TestFramework;
open Runner;

describe("pattern matching", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);
  let assertFileRun = makeFileRunner(test);

  /* Pattern matching on tuples */
  assertRun("tuple_match_1", "print(match ((1,)) { (a,) => a })", "1\n");
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
  assertFileRun("mixed_matching", "mixedPatternMatching", "true\n");
});
