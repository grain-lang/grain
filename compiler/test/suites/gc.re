open Grain_tests.TestFramework;
open Grain_tests.Runner;

let makeGcProgram = (program, heap_size) => {
  Printf.sprintf(
    {|
    include "runtime/unsafe/wasmi32"
    include "runtime/malloc"
    include "runtime/unsafe/memory"

    @disableGC
    primitive heapBase = "@heap.base"

    @disableGC
    let leak = () => {
      from WasmI32 use { (+), (-) }
      // find current memory pointer, subtract space for two malloc headers + 1 GC header
      let offset = Memory.malloc(8n) - 24n
      // Calculate how much memory is left
      let availableMemory = offset - (Malloc._RESERVED_RUNTIME_SPACE + heapBase)
      // Calculate how much memory to leak
      let toLeak = availableMemory - %dn
      // Memory is not reclaimed due to no gc context
      // This will actually leak 16 extra bytes because of the headers
      Memory.malloc(toLeak - 16n);
      void
    }
    leak();
    %s
    |},
    heap_size,
    program,
  );
};

describe("garbage collection", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertRun = makeRunner(test_or_skip);
  let assertFileRun = makeFileRunner(test_or_skip);
  let assertMemoryLimitedFileRun = makeFileRunner(~num_pages=1, test_or_skip);
  let assertRunGC = (name, heapSize, prog, expected) =>
    makeRunner(
      ~num_pages=1,
      test_or_skip,
      name,
      makeGcProgram(prog, heapSize),
      expected,
    );
  let assertRunGCError = (name, heapSize, prog, expected) =>
    makeErrorRunner(
      test_or_skip,
      ~num_pages=1,
      name,
      makeGcProgram(prog, heapSize),
      expected,
    );

  // oom tests
  assertRunGCError(
    "oomgc1",
    48,
    "(1, (3, 4))",
    "Maximum memory size exceeded",
  );
  assertRunGC("oomgc2", 64, "(1, (3, 4))", "");
  assertRunGC("oomgc3", 32, "(3, 4)", "");

  // gc tests
  assertRunGC(
    "gc1",
    160,
    "let f = (() => (1, 2));\n       {\n         f();\n         f();\n         f();\n         f()\n       }",
    "",
  );
  /* https://github.com/grain-lang/grain/issues/774 */
  assertRunGC(
    "gc3",
    1024,
    "let foo = (s: String) => void\nlet printBool = (b: Bool) => foo(if (b) \"true\" else \"false\")\n\nlet b = true\nfor (let mut i=0; i<100000; i += 1) {\n  printBool(true)\n}",
    "",
  );
  assertRunGCError(
    "fib_gc_err",
    256,
    {|
    let fib = x => {
      let rec fib_help = (n, acc) => {
        let (cur, next) = acc
        if (n == 0) {
          cur
        } else {
          fib_help(n - 1, (next, cur + next))
        }
      }
      fib_help(x, (0, 1))
    }
    print(fib(30))
    |},
    "Maximum memory size exceeded",
  );
  assertRunGC(
    "fib_gc",
    512,
    {|
    let fib = x => {
      let rec fib_help = (n, acc) => {
        let (cur, next) = acc
        if (n == 0) {
          cur
        } else {
          fib_help(n - 1, (next, cur + next))
        }
      }
      fib_help(x, (0, 1))
    }
    print(fib(30))
    |},
    "832040\n",
  );
  assertRunGC(
    "loop_gc",
    256,
    {|
    for (let mut i = 0; i < 512; i += 1) {
      let string = "string"
      continue
      ignore(string)
    }
    print("OK")
    |},
    "OK\n",
  );
  assertRunGC(
    "long_lists",
    20000,
    {|
    include "list"
    from List use *

    let rec make_list = (x, n) => {
      let rec helper = (a, b, acc) => {
        if (a == 0) {
          acc
        } else {
          helper(a - 1, b, [b, ...acc])
        }
      }
      helper(n, x, [])
    },
    loop = n => {
      if (n == 0) {
        true
      } else {
        let lst = make_list(n, n)
        loop(n - 1)
      }
    }

    print(loop(25)) // <- eats up a lot of heap
    |},
    "true\n",
  );
  assertFileRun("malloc_tight", "mallocTight", "");
  assertFileRun("memory_grow1", "memoryGrow", "1000000000000\n");
  assertMemoryLimitedFileRun(
    "loop_memory_reclaim",
    "loopMemoryReclaim",
    "OK\n",
  );
  assertRun(
    "match_issue893_internal_equals",
    {|let f = (n: Number) => {
      match (n) {
        0 => {
          void
        },
        _ => {
          void
        },
      }
    }

    f(1)
    f(2)
    f(3)
    print("4")|},
    "4\n",
  );
});
