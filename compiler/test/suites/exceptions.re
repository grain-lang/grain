open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("exceptions", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertRunError = makeErrorRunner(test_or_skip);

  assertRun("exception_1", "exception Foo; print(Foo)", "Foo\n");
  assertSnapshot("exception_2", "provide exception Foo; Foo");
  assertRun(
    "exception_3",
    "provide exception Foo(Bool, Number); print(Foo(false, 6))",
    "Foo(false, 6)\n",
  );
  assertSnapshot(
    "exception_4",
    "provide exception Foo(Bool, Number); provide exception Bar; Bar",
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
    {|from "exception" include Exception; exception HorribleError; Exception.registerPrinter(e => match (e) { HorribleError => Some("Spooky error"), _ => None }); let _ = throw HorribleError|},
    "Spooky error",
  );
  assertRunError(
    "exception_register_2",
    {|
    from "exception" include Exception
    exception HorribleError
    Exception.registerPrinter(e => match (e) { HorribleError => Some("Spooky error 1"), _ => None })
    Exception.registerPrinter(e => match (e) { HorribleError => Some("Spooky error 2"), _ => None })
    let _ = throw HorribleError
    |},
    "Spooky error 2",
  );
  assertRun(
    "record_exception_1",
    {|exception Foo { msg: String, bar: Number }; print(Foo{msg: "Oops", bar: 1})|},
    "Foo{\n  msg: \"Oops\",\n  bar: 1\n}\n",
  );
  assertRunError(
    "record_exception_2",
    {|from "exception" include Exception; exception Foo { msg: String, bar: Number }; Exception.registerPrinter(e => match (e) { Foo { msg, bar } => Some(msg ++ toString(bar)), _ => None }); let _ = throw Foo{msg: "Oops", bar: 1}|},
    "Oops1",
  );
});
