open TestFramework;
open Runner;

describe("arrays", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);
  let assertRunError = makeErrorRunner(test);

  assertRun("array1", "print([> 1, 2, 3])", "[> 1, 2, 3]\n");
  assertRun("array2", "print([>])", "[> ]\n");
  assertSnapshot("array3", "[>\n1, 2, 3]");
  assertCompileError("array_error", "[> 1, false, 2]", "has type Bool but");
  assertSnapshot("array_access", "let x = [> 1, 2, 3]; x[0]");
  assertSnapshot("array_access2", "let x = [> 1, 2, 3]; x[1]");
  assertSnapshot("array_access3", "let x = [> 1, 2, 3]; x[2]");
  assertSnapshot("array_access4", "let x = [> 1, 2, 3]; x[-2]");
  assertSnapshot("array_access5", "let x = [> 1, 2, 3]; x[-3]");
  assertRunError(
    "array_access_err",
    "let x = [> 1, 2, 3]; x[3]",
    "Index out of bounds",
  );
  assertRunError(
    "array_access_err2",
    "let x = [> 1, 2, 3]; x[-4]",
    "Index out of bounds",
  );
  assertRunError(
    "array_access_err3",
    "let x = [> 1, 2, 3]; x[99]",
    "Index out of bounds",
  );
  assertRunError(
    "array_access_err4",
    "let x = [> 1, 2, 3]; x[-99]",
    "Index out of bounds",
  );
  assertCompileError(
    "array_access_err5",
    "let x = [> 1, 2, 3]; x[false]",
    "has type Bool but",
  );
  assertRun(
    "array_set",
    "let x = [> 1, 2, 3]; x[0] = 4; print(x)",
    "[> 4, 2, 3]\n",
  );
  assertRun(
    "array_set2",
    "let x = [> 1, 2, 3]; x[-2] = 4; print(x)",
    "[> 1, 4, 3]\n",
  );
  assertCompileError(
    "array_set_err",
    "let x = [> 1, 2, 3]; x[-2] = false",
    "has type Bool but",
  );
  assertRunError(
    "array_set_err2",
    "let x = [> 1, 2, 3]; x[-12] = 4",
    "Index out of bounds",
  );
  assertCompileError(
    "array_type",
    "let x = [> true, false, false]; x[1] + 3",
    "has type Bool but",
  );
  assertCompileError(
    "array_type2",
    "let x = [> true, false, false]; (x[1] = true) + 3",
    "has type Void but",
  );
  // trailing commas
  assertSnapshot("array1_trailing", "[> 1, 2, 3,]");
  assertSnapshot("array1_trailing_space", "[> 1, 2, 3, ]");
  assertCompileError(
    "invalid_empty_trailing",
    "[> ,]",
    "Error: Syntax error",
  );
});
