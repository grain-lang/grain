open TestFramework;
open Runner;

describe("exceptions", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertRun = makeRunner(test);
  let assertRunError = makeErrorRunner(test);

  assertRun("exception_1", "exception Foo; print(Foo)", "Foo\n");
  assertSnapshot("exception_2", "export exception Foo; Foo");
  assertRun(
    "exception_3",
    "export exception Foo(Bool, Number); print(Foo(false, 6))",
    "Foo(false, 6)\n",
  );
  assertSnapshot(
    "exception_4",
    "export exception Foo(Bool, Number); export exception Bar; Bar",
  );
  assertRunError(
    "throw_exception_1",
    "exception HorribleError; let _ = throw HorribleError",
    "HorribleError",
  );
  assertRunError(
    "throw_exception_2",
    "exception HorribleError(String, Bool, Number); let _ = throw HorribleError(\"oh no\", false, 1/3)",
    "HorribleError\\(\"oh no\", false, 1/3\\)",
  );
  assertRunError(
    ~check_exists=false,
    "throw_exception_3",
    "exception HorribleError(String, Bool, Number); let _ = throw HorribleError(\"oh no\", false, 1/3); print(\"shouldn't be printed\")",
    "shouldn't be printed",
  );
  assertRunError(
    "exception_register_1",
    {|import Exception from "exception"; exception HorribleError; Exception.registerPrinter(e => match (e) { HorribleError => Some("Spooky error"), _ => None }); let _ = throw HorribleError|},
    "Spooky error",
  );
  assertRunError(
    "exception_register_2",
    {|
    import Exception from "exception"
    exception HorribleError
    Exception.registerPrinter(e => match (e) { HorribleError => Some("Spooky error 1"), _ => None })
    Exception.registerPrinter(e => match (e) { HorribleError => Some("Spooky error 2"), _ => None })
    let _ = throw HorribleError
    |},
    "Spooky error 2",
  );
});
