open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("linking", ({test}) => {
  let assertRun = makeRunner(test);
  let assertRunError = makeErrorRunner(test);
  let assertWasiPolyfillRun = file =>
    makeRunner(
      ~config_fn=() => {Grain_utils.Config.wasi_polyfill := Some(file)},
      test,
    );

  assertRun("link_simple", {|print("Hello, world!")|}, "Hello, world!\n");
  assertRunError(
    "link_exception",
    {|exception BadError(Number, String); let _ = throw BadError(5, "foo")|},
    {|BadError\(5, "foo"\)|},
  );
  assertRun(
    "link_import",
    {|import List from "list"; print(List.map(n => n + 1, [1, 2, 3]))|},
    "[2, 3, 4]\n",
  );
  // --wasi-polyfill
  assertWasiPolyfillRun(
    "test/input/wasiPolyfill.gr",
    "wasi_polyfill",
    {|print("foo")|},
    "foo\nfoo\nfoo\n",
  );
  assertWasiPolyfillRun(
    "test/input/wasiPolyfillNoop.gr",
    "wasi_polyfill_noop",
    {|print("foo")|},
    "",
  );
});
