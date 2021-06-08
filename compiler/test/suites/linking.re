open TestFramework;
open Runner;

describe("linking", ({test}) => {
  let assertRun = makeRunner(test);
  let assertRunError = makeErrorRunner(test);

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
});
