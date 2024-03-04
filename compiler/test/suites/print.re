open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("print", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertRun = makeRunner(test_or_skip);

  assertRun(
    ~config_fn=() => {Grain_utils.Config.elide_type_info := true},
    "elided_type_info_1",
    "enum Foo { Foo }; print(Foo)",
    "<enum value>\n",
  );
  assertRun(
    ~config_fn=() => {Grain_utils.Config.elide_type_info := true},
    "elided_type_info_2",
    "record Foo { foo: String }; print({ foo: \"foo\" })",
    "<record value>\n",
  );
  assertRun(
    "print_double_exception",
    "exception Foo; exception Bar; print(Foo); print(Bar)",
    "Foo\nBar\n",
  );
  assertRun(
    "print_nested_records",
    "record Foo { foo: Number }; record Bar { bar: Foo }; print({ bar: { foo: 1 } })",
    "{\n  bar: {\n    foo: 1\n  }\n}\n",
  );
  assertRun(
    "print_nested_records_multiple",
    "record Foo { foo: Number }; record Bar { bar: Foo }; print({ bar: { foo: 1 } }); print({ bar: { foo: 1 } }); print({ bar: { foo: 1 } })",
    "{\n  bar: {\n    foo: 1\n  }\n}\n{\n  bar: {\n    foo: 1\n  }\n}\n{\n  bar: {\n    foo: 1\n  }\n}\n",
  );
  assertRun(
    "print_issue892_1",
    "from \"list\" include List\nlet a = [1, 2]\nlet b = List.reverse(a)\nprint(a)\nprint(b)\n",
    "[1, 2]\n[2, 1]\n",
  );
  assertRun(
    "print_issue892_2",
    "let a = [1, 2]\nprint(a)\nprint(a)\nprint(a)\nprint(a)\n",
    "[1, 2]\n[1, 2]\n[1, 2]\n[1, 2]\n",
  );
  assertRun(
    "print_issue905_long_lists",
    "print([1, 2, 3, 4, 5, 6, 7, 8, 9])",
    "[1, 2, 3, 4, 5, 6, 7, 8, 9]\n",
  );
});
