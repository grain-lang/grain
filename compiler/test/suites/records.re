open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_utils;

let {describe} =
  describeConfig |> withCustomMatchers(customMatchers) |> build;

describe("records", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertWarning = makeWarningRunner(test);
  let assertNoWarning = makeNoWarningRunner(test);

  assertRun(
    "record_1",
    "record Rec {foo: Number}; print({foo: 4})",
    "{\n  foo: 4\n}\n",
  );
  assertRun(
    "record_2",
    "provide record Rec {foo: Number}; print({foo: 4})",
    "{\n  foo: 4\n}\n",
  );
  assertRun(
    "record_multiple",
    "provide record Rec {foo: Number, bar: String, baz: Bool}; print({foo: 4, bar: \"boo\", baz: true})",
    "{\n  foo: 4,\n  bar: \"boo\",\n  baz: true\n}\n",
  );
  assertSnapshot(
    "record_pun",
    "provide record Rec {foo: Number}; let foo = 4; {foo,}",
  );
  assertSnapshot(
    "record_pun_multiple",
    "provide record Rec {foo: Number, bar: Bool}; let foo = 4; let bar = false; {foo, bar}",
  );
  assertSnapshot(
    "record_pun_mixed",
    "provide record Rec {foo: Number, bar: Bool}; let foo = 4; {foo, bar: false}",
  );
  assertSnapshot(
    "record_pun_mixed_2",
    "provide record Rec {foo: Number, bar: Bool}; let bar = false; {foo: 4, bar}",
  );
  assertCompileError("record_err_1", "{foo: 4}", "Unbound record label foo");
  assertCompileError(
    "record_err_2",
    "record Rec {foo: Number}; {foo: 4, bar: 4}",
    "Unbound record label bar",
  );
  assertCompileError(
    "record_err_3",
    "let foo = \"\"; foo.charAt(0)",
    "Unbound record label charAt",
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
  assertRun(
    "record_mut_2",
    "record Rec {mut foo: Number, bar: String, baz: Bool}; let a = {foo: 4, bar: \"boo\", baz: true}; a.foo += 5; print(a.foo)",
    "9\n",
  );
  assertCompileError(
    "record_mut_3",
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
    "record_destruct_trailing",
    "record Rec {foo: Number, bar: Number, baz: Number}; let { foo, bar, baz, } = {foo: 4, bar: 5, baz: 6}; foo + bar + baz",
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
    "provide record Rec {foo: Number,}; {foo: 4}",
  );
  assertSnapshot(
    "record_value_trailing",
    "provide record Rec {foo: Number}; {foo: 4,}",
  );
  assertSnapshot(
    "record_both_trailing",
    "provide record Rec {foo: Number,}; {foo: 4,}",
  );
  assertSnapshot(
    "record_multiple_fields_definition_trailing",
    "provide record Rec {foo: Number, bar: String, baz: Bool,}; {foo: 4, bar: \"boo\", baz: true}",
  );
  assertSnapshot(
    "record_multiple_fields_value_trailing",
    "provide record Rec {foo: Number, bar: String, baz: Bool}; {foo: 4, bar: \"boo\", baz: true,}",
  );
  assertSnapshot(
    "record_multiple_fields_both_trailing",
    "provide record Rec {foo: Number, bar: String, baz: Bool,}; {foo: 4, bar: \"boo\", baz: true,}",
  );
  assertSnapshot(
    "record_pun_trailing",
    "provide record Rec {foo: Number}; let foo = 4; {foo,}",
  );
  assertSnapshot(
    "record_pun_multiple_trailing",
    "provide record Rec {foo: Number, bar: Bool}; let foo = 4; let bar = false; {foo, bar,}",
  );
  assertSnapshot(
    "record_pun_mixed_trailing",
    "provide record Rec {foo: Number, bar: Bool}; let foo = 4; {foo, bar: false,}",
  );
  assertSnapshot(
    "record_pun_mixed_2_trailing",
    "provide record Rec {foo: Number, bar: Bool}; let bar = false; {foo: 4, bar,}",
  );
  assertSnapshot(
    "record_recursive_data_definition",
    {|
      record rec Bar {
        mut foo: Option<Foo>
      }
      and record Foo {
        mut bar: Option<Bar>
      }

      let foo = {bar: None,}
      let bar = {foo: None,}

      foo.bar = Some(bar)
      bar.foo = Some(foo)
    |},
  );
  assertRun(
    "export_import_record_issue_665",
    {|
      from "data" include Data
      use Data.{ type Foo }
      provide enum Bar { Baz(Foo<Number>) }
      print(Baz({ bar: 1 }))
    |},
    "Baz({\n  bar: 1\n})\n",
  );
  // record spread
  assertRun(
    "record_spread_1",
    "record Rec {foo: Number, bar: Number, mut baz: Number}; let a = {foo: 1, bar: 2, baz: 3}; let b = {...a, bar: 3}; b.baz = 5; print(b); print(a)",
    "{\n  foo: 1,\n  bar: 3,\n  baz: 5\n}\n{\n  foo: 1,\n  bar: 2,\n  baz: 3\n}\n",
  );
  assertSnapshot(
    "record_spread_2",
    "record Rec {foo: Number, bar: Number}; let a = {foo: 1, bar: 2}; let b = {...a, bar: 3}",
  );
  assertCompileError(
    "record_spread_3",
    "record Rec {foo: Number, bar: Number}; let a = {foo: 1, bar: 2}; let b = {bar: 3, ...a}",
    "A record spread can only appear at the beginning of a record expression",
  );
  assertCompileError(
    "record_spread_4",
    "record Rec {foo: Number, bar: Number}; let a = {foo: 1, bar: 2}; let b = {...a, ...a}",
    "A record expression may only contain one record spread",
  );
  assertCompileError(
    "record_spread_5",
    "record Rec {foo: Number, bar: Number}; let a = {foo: 1, bar: 2}; let b = {...a}",
    "Expected a comma followed by one or more record field overrides to complete the record expression",
  );
  assertCompileError(
    "record_spread_6",
    "record Rec {foo: Number, bar: Number}; let a = {foo: 1, bar: 2}; let b = {...a,}",
    "Expected one or more record field overrides to complete the record expression",
  );
  assertCompileError(
    "record_spread_7",
    "record Rec {foo: Number, bar: Number}; record Rec2 {baz: Number}; let a = {foo: 1, bar: 2}; let b = {...a, baz: 3}",
    "The field baz does not belong to type Rec",
  );
  assertWarning(
    "record_spread_8",
    "record Rec {foo: Number, bar: Number}; let a = {foo: 1, bar: 2}; let b = {...a, foo: 2, bar: 3}",
    Warnings.UselessRecordSpread,
  );
  assertWarning(
    "disambiguation_1",
    {|
      record A { field: Number }
      record B { field: Number }
      x => x.field
    |},
    Warnings.AmbiguousName(["field"], ["B", "A"], false),
  );
  assertNoWarning(
    "disambiguation_2",
    {|
      record A { field: Number }
      record B { field: Number }
      (x: A) => x.field
    |},
  );
  assertNoWarning(
    "disambiguation_3",
    {|
      record A { field: Number }
      record B { field: Number }
      (x: B) => x.field
    |},
  );
  // well_formedness field omission warning
  assertWarning(
    "record_field_omit_1",
    "record Rec {foo: Number, bar: Number}; let a = {foo: 1, bar: 2}; match (a) { { foo } => void, _ => void }",
    Warnings.NonClosedRecordPattern("bar"),
  );
  assertWarning(
    "record_field_omit_2",
    "record Rec {foo: Number, bar: Number, bar2: Number}; let a = {foo: 1, bar: 2, bar2: 3}; match (a) { { foo } => void, _ => void }",
    Warnings.NonClosedRecordPattern("bar, bar2"),
  );
});
