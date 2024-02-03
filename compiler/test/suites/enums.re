open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("enums", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertCompileError = makeCompileErrorRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);

  assertFileRun("basicenum", "basicenum", "(false, true, true)\n");
  assertFileRun(
    "adtprint",
    "adtprint",
    "Foo\nBar\nBaz(\"baz\")\nQux(5, \"qux\", false)\nQuux\nFlip(\"flip\")\n",
  );
  assertRun("adtprint_nonexported", "enum Foo { Foo }; print(Foo)", "Foo\n");
  assertSnapshot(
    "adt_trailing",
    "enum Topping { Cheese(Bool,), Pepperoni }; Pepperoni",
  );
  // Taken from https://en.wikipedia.org/wiki/Recursive_data_type#Mutually_recursive_data_types
  let recursive_contents = {|
    enum rec Tree<a> {
      Empty,
      Node(a, Forest<a>)
    }
    and enum Forest<a> {
      Nil,
      Cons(Tree<a>, Forest<a>)
    }

    let forest = Cons(Node("tree 1", Cons(Node("tree 2", Nil), Nil)), Nil)
    print(forest);
  |};
  assertSnapshot("enum_recursive_data_definition", recursive_contents);
  assertRun(
    "enum_recursive_data_definition_prints",
    recursive_contents,
    "Cons(Node(\"tree 1\", Cons(Node(\"tree 2\", Nil), Nil)), Nil)\n",
  );
  // inline record variants
  assertRun(
    "enum_inline_record_1",
    {|
      enum Rec<a> {
        Rec{ x: a, y: a },
        Tup(Number, Number, Number)
      }
      let r = Rec{ x: 1, y: 2 }
      let x = 11
      let y = 12
      let r2 = Rec{ y, x }
      print(r)
      print(r2)
      print(r == Rec{ x: 1, y: 2 })
      print(r == Rec{ x: 2, y: 2 })
      print(Tup(1, 2, 3))
    |},
    "Rec{\n  x: 1,\n  y: 2\n}\nRec{\n  x: 11,\n  y: 12\n}\ntrue\nfalse\nTup(1, 2, 3)\n",
  );
  assertCompileError(
    "enum_inline_record_2",
    {|
      enum Rec<a> {
        Rec{ x: a, y: a }
      }
      let r = { x: 1, y: 2, }
    |},
    "Unbound record label x. However, this label exists on record constructor Rec, which is incompatible with this record type.",
  );
  assertCompileError(
    "enum_inline_record_3",
    {|
      enum Rec<a> {
        Rec{ x: a, y: a }
      }
      let r = Rec{ x: 1 }
    |},
    "Some record fields are undefined: y",
  );
  assertCompileError(
    "enum_inline_record_4",
    {|
      enum Rec {
        Rec{ mut x: Number }
      }
    |},
    "An inline record constructor cannot have mutable fields.",
  );
  assertRun(
    "enum_inline_record_5",
    {|
      enum Rec {
        Rec{ x: Number }
      }
      let x = 1
      let b = Rec{ x }
      print(b)
    |},
    "Rec{\n  x: 1\n}\n",
  );
  assertRun(
    "deeply_nested_enum",
    {|
      provide module Foo {
        provide module Bar {
          provide enum Baz { Baz }
        }
      }
      print(Foo.Bar.Baz)
    |},
    "Baz\n",
  );
});
