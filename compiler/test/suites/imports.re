open TestFramework;
open Runner;

describe("imports", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);
  let assertFileRun = makeFileRunner(test);

  /* import * tests */
  assertRun(
    "import_all",
    "import * from \"exportStar\"; {print(x); print(y(4)); print(z)}",
    "5\n4\nfoo\n",
  );
  assertRun(
    "import_all_except",
    "import * except {y} from \"exportStar\"; {print(x); print(z)}",
    "5\nfoo\n",
  );
  assertSnapshot(
    "import_all_except_multiple",
    "import * except {x, y} from \"exportStar\"; z",
  );
  assertSnapshot(
    "import_all_constructor",
    "import * from \"tlists\"; Cons(2, Empty)",
  );
  assertSnapshot(
    "import_all_except_constructor",
    "import * except {Cons} from \"tlists\"; Empty",
  );
  assertSnapshot(
    "import_all_except_multiple_constructor",
    "import * except {Cons, append} from \"tlists\"; sum(Empty)",
  );
  assertSnapshot(
    "import_with_export_multiple",
    "import * from \"sameExport\"; foo()",
  );
  /* import * errors */
  assertCompileError(
    "import_all_except_error",
    "import * except {y} from \"exportStar\"; {print(x); print(y); z}",
    "Unbound value y",
  );
  assertCompileError(
    "import_all_except_multiple_error",
    "import * except {x, y} from \"exportStar\"; {print(x); z}",
    "Unbound value x",
  );
  assertCompileError(
    "import_all_except_multiple_error2",
    "import * except {x, y} from \"exportStar\"; {print(x); print(y); z}",
    "Unbound value x",
  );
  assertCompileError(
    "import_all_except_error_constructor",
    "import * except {Cons} from \"tlists\"; Cons(2, Empty)",
    "Unbound value Cons",
  );
  assertCompileError(
    "import_all_except_multiple_error_constructor",
    "import * except {Cons, append} from \"tlists\"; append(Empty, Empty)",
    "Unbound value append",
  );
  assertCompileError(
    "import_all_except_multiple_error2_constructor",
    "import * except {Cons, append} from \"tlists\"; let x = Cons(2, Empty); append(x, Empty)",
    "Unbound value Cons",
  );
  /* import {} tests */
  assertSnapshot("import_some", "import {x} from \"exportStar\"; x");
  assertSnapshot(
    "import_some_multiple",
    "import {x, y} from \"exportStar\"; y(x)",
  );
  assertSnapshot(
    "import_some_constructor",
    "import {Cons, Empty} from \"tlists\"; Cons(5, Empty)",
  );
  assertSnapshot(
    "import_some_mixed",
    "import {Cons, Empty, sum} from \"tlists\"; sum(Cons(5, Empty))",
  );
  assertSnapshot("import_alias", "import {x as y} from \"exportStar\"; y");
  assertSnapshot(
    "import_alias_multiple",
    "import {x as y, y as x} from \"exportStar\"; x(y)",
  );
  assertSnapshot(
    "import_alias_constructor",
    "import {Empty as None, sum} from \"tlists\"; sum(None)",
  );
  assertSnapshot(
    "import_alias_multiple_constructor",
    "import {Cons as Add, Empty as None, sum} from \"tlists\"; sum(Add(1, None))",
  );
  /* import {} errors */
  assertCompileError(
    "import_some_error",
    "import {a} from \"exportStar\"; a",
    "Export \"a\" was not found in \"exportStar\"",
  );
  assertCompileError(
    "import_some_error2",
    "import {x, a} from \"exportStar\"; a",
    "Export \"a\" was not found in \"exportStar\"",
  );
  assertCompileError(
    "import_some_error3",
    "import {Foo} from \"exportStar\"; a",
    "Export \"Foo\" was not found in \"exportStar\"",
  );
  assertCompileError(
    "import_some_error3",
    "import {x, Foo} from \"exportStar\"; a",
    "Export \"Foo\" was not found in \"exportStar\"",
  );
  /* import module tests */
  assertSnapshot("import_module", "import Foo from \"exportStar\"; Foo.x");
  assertSnapshot(
    "import_module2",
    "import Foo from \"exportStar\"; Foo.y(Foo.x)",
  );
  /* import module errors */
  assertCompileError(
    "import_module_error",
    "import Foo from \"exportStar\"; Foo.foo",
    "Unbound value foo in module Foo",
  );
  /* import well-formedness errors */
  assertCompileError(
    "import_alias_illegal_renaming",
    "import {Cons as cons, Empty} from \"list\"; cons(3, Empty)",
    "Alias 'cons' should have proper casing",
  );
  assertCompileError(
    "import_alias_illegal_renaming2",
    "import {sum as Sum, Empty} from \"list\"; sum(Empty)",
    "Alias 'Sum' should have proper casing",
  );
  assertCompileError(
    "import_module_illegal_name",
    "import foo from \"list\";",
    "Module 'foo' should have an uppercase name",
  );
  assertCompileError(
    "import_module_not_external",
    "import Foo.Foo from \"list\";",
    "Module name 'Foo.Foo' should contain only one module.",
  );
  assertCompileError(
    "import_value_not_external",
    "import {foo as Foo.foo} from \"list\";",
    "Alias 'Foo.foo' should be at most one level deep",
  );
  /* import multiple modules tests */
  assertSnapshot(
    "import_muliple_modules",
    "import * from \"tlists\"; import * from \"exportStar\"; Cons(x, Empty)",
  );
  /* import same module tests */
  assertSnapshot(
    "import_same_module_unify",
    "import * from \"tlists\"; import TList from \"tlists\"; Cons(5, TList.Empty)",
  );
  assertSnapshot(
    "import_same_module_unify2",
    "import *, TList from \"tlists\"; Cons(5, TList.Empty)",
  );
  /* import filepath tests */
  assertSnapshot("import_relative_path", "import * from \"./exportStar\"; x");
  assertSnapshot(
    "import_relative_path2",
    "import * from \"../test-libs/exportStar\"; x",
  );
  assertSnapshot(
    "import_relative_path3",
    "import * from \"nested/nested\"; j",
  );
  assertCompileError(
    "import_missing_file",
    "import * from \"foo\"; 2",
    "Missing file for module foo",
  );
  /* Unbound module tests */
  assertCompileError(
    "test_unbound_module",
    "String.concat(\"hello \", \"world\")",
    "Unbound module String",
  );
  /* Misc import tests */
  assertCompileError(
    "test_bad_import",
    "{let x = (1, 2); import * from \"tlists\"; x}",
    "error",
  );
  assertFileRun("test_file_same_name", "list", "OK\n");
  assertSnapshot(
    "annotation_across_import",
    "import TList, { Empty } from \"tlists\"; let foo : TList.TList<String> = Empty; foo",
  );
});
