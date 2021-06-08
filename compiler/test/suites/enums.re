open TestFramework;
open Runner;

describe("enums", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertRun = makeRunner(test);
  let assertFileRun = makeFileRunner(test);

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
});
