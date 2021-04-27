open TestFramework;
open Runner;

describe("exports", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);

  assertCompileError(
    "export1",
    "import * from \"noExports\"; x",
    "Unbound value x",
  );
  assertCompileError(
    "export2",
    "import * from \"noExports\"; y",
    "Unbound value y",
  );
  assertCompileError(
    "export3",
    "import * from \"noExports\"; z",
    "Unbound value z",
  );
  assertSnapshot("export4", "import * from \"onlyXExported\"; x");
  assertCompileError(
    "export5",
    "import * from \"onlyXExported\"; y",
    "Unbound value y",
  );
  assertCompileError(
    "export6",
    "import * from \"onlyXExported\"; z",
    "Unbound value z",
  );
  assertSnapshot("export7", "import * from \"exportStar\"; x");
  assertSnapshot("export8", "import * from \"exportStar\"; x + y(4)");
  assertSnapshot("export9", "import * from \"exportStar\"; y(z)");
  assertCompileError(
    "export10",
    "import * from \"exportStar\"; y(secret)",
    "Unbound value secret",
  );
});
