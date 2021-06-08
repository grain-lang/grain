open TestFramework;
open Runner;

describe("records", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);

  assertRun(
    "record_1",
    "record Rec {foo: Number}; print({foo: 4})",
    "{\n  foo: 4\n}\n",
  );
  assertRun(
    "record_2",
    "export record Rec {foo: Number}; print({foo: 4})",
    "{\n  foo: 4\n}\n",
  );
  assertRun(
    "record_multiple",
    "export record Rec {foo: Number, bar: String, baz: Bool}; print({foo: 4, bar: \"boo\", baz: true})",
    "{\n  foo: 4,\n  bar: \"boo\",\n  baz: true\n}\n",
  );
  assertSnapshot(
    "record_pun",
    "export record Rec {foo: Number}; let foo = 4; {foo}",
  );
  assertSnapshot(
    "record_pun_multiple",
    "export record Rec {foo: Number, bar: Bool}; let foo = 4; let bar = false; {foo, bar}",
  );
  assertSnapshot(
    "record_pun_mixed",
    "export record Rec {foo: Number, bar: Bool}; let foo = 4; {foo, bar: false}",
  );
  assertSnapshot(
    "record_pun_mixed_2",
    "export record Rec {foo: Number, bar: Bool}; let bar = false; {foo: 4, bar}",
  );
  assertCompileError("record_err_1", "{foo: 4}", "Unbound record label foo");
  assertCompileError(
    "record_err_2",
    "record Rec {foo: Number}; {foo: 4, bar: 4}",
    "Unbound record label bar",
  );
  assertRun(
    "record_get_1",
    "record Rec {foo: Number}; let bar = {foo: 4}; print(bar.foo)",
    "4\n",
  );
  assertSnapshot("record_get_2", "record Rec {foo: Number}; {foo: 4}.foo");
  assertSnapshot(
    "record_get_multiple",
    "record Rec {foo: Number, bar: Number}; let x = {foo: 4, bar: 9}; x.foo + x.bar",
  );
  assertSnapshot(
    "record_get_multilevel",
    "record Rec1 {foo: Number, bar: Number}; record Rec2 {baz: Rec1}; let x = {baz: {foo: 4, bar: 9}}; x.baz.bar",
  );
  assertCompileError(
    "record_get_err",
    "record Rec1 {foo: Number, bar: Number}; let x = {foo: 4, bar: 9}; x.baz",
    "The field baz does not belong to type Rec1",
  );
  /* mutable record fields */
  assertRun(
    "record_mut_1",
    "record Rec {foo: Number, mut bar: String, baz: Bool}; let a = {foo: 4, bar: \"boo\", baz: true}; a.bar = \"hoo\"; print(a.bar)",
    "hoo\n",
  );
  assertCompileError(
    "record_mut_2",
    "record Rec {foo: Number, mut bar: String, baz: Bool}; let a = {foo: 4, bar: \"boo\", baz: true}; a.foo = 5; a.foo",
    "The record field foo is not mutable",
  );
  /* record destructured assignment */
  assertSnapshot(
    "record_destruct_1",
    "record Rec {foo: Number, bar: String, baz: Bool}; let { foo, _ } = {foo: 4, bar: \"boo\", baz: true}; foo",
  );
  assertSnapshot(
    "record_destruct_2",
    "record Rec {foo: Number, bar: String, baz: Bool}; let { bar, _ } = {foo: 4, bar: \"boo\", baz: true}; bar",
  );
  assertSnapshot(
    "record_destruct_3",
    "record Rec {foo: Number, bar: Number, baz: Number}; let { foo, bar, _ } = {foo: 4, bar: 5, baz: 6}; foo + bar",
  );
  assertSnapshot(
    "record_destruct_4",
    "record Rec {foo: Number, bar: Number, baz: Number}; let { foo, bar, baz } = {foo: 4, bar: 5, baz: 6}; foo + bar + baz",
  );
  assertSnapshot(
    "record_destruct_deep",
    "record Rec {foo: Number}; record Rec2 {bar: Rec}; let { bar: { foo } } = {bar: {foo: 4}}; foo",
  );
  assertCompileError(
    "record_destruct_deep_alias",
    "record Rec {foo: Number}; record Rec2 {bar: Rec}; let { bar: { foo } } = {bar: {foo: 4}}; bar",
    "Unbound value bar",
  );
  // Record trailing commas
  assertSnapshot(
    "record_definition_trailing",
    "export record Rec {foo: Number,}; {foo: 4}",
  );
  assertSnapshot(
    "record_value_trailing",
    "export record Rec {foo: Number}; {foo: 4,}",
  );
  assertSnapshot(
    "record_both_trailing",
    "export record Rec {foo: Number,}; {foo: 4,}",
  );
  assertSnapshot(
    "record_multiple_fields_definition_trailing",
    "export record Rec {foo: Number, bar: String, baz: Bool,}; {foo: 4, bar: \"boo\", baz: true}",
  );
  assertSnapshot(
    "record_multiple_fields_value_trailing",
    "export record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true,}",
  );
  assertSnapshot(
    "record_multiple_fields_both_trailing",
    "export record Rec {foo: Number, bar: String, baz: Bool,}; {foo: 4, bar: \"boo\", baz: true,}",
  );
  assertSnapshot(
    "record_pun_trailing",
    "export record Rec {foo: Number}; let foo = 4; {foo,}",
  );
  assertSnapshot(
    "record_pun_multiple_trailing",
    "export record Rec {foo: Number, bar: Bool}; let foo = 4; let bar = false; {foo, bar,}",
  );
  assertSnapshot(
    "record_pun_mixed_trailing",
    "export record Rec {foo: Number, bar: Bool}; let foo = 4; {foo, bar: false,}",
  );
  assertSnapshot(
    "record_pun_mixed_2_trailing",
    "export record Rec {foo: Number, bar: Bool}; let bar = false; {foo: 4, bar,}",
  );
});
