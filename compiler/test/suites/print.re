open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("print", ({test}) => {
  let assertRun = makeRunner(test);

  assertRun(
    "elided_type_info_1",
    "/* grainc-flags --elide-type-info */ enum Foo { Foo }; print(Foo)",
    "<enum value>\n",
  );
  assertRun(
    "elided_type_info_2",
    "/* grainc-flags --elide-type-info */ record Foo { foo: String }; print({ foo: \"foo\" })",
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
});
