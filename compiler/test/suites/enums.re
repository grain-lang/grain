open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("enums", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertRun = makeRunner(test_or_skip);
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
    enum Tree<a> {
      Empty,
      Node(a, Forest<a>)
    },
    enum Forest<a> {
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
});
