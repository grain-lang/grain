open Grain_tests.TestFramework;
open Grain_tests.Runner;

let makeGcProgram = (program, heap_size) => {
  Printf.sprintf(
    {|
    from "runtime/unsafe/wasmi32" include WasmI32
    from "runtime/malloc" include Malloc
    from "runtime/unsafe/memory" include Memory

    @unsafe
    let _ = {
      use WasmI32.{(*), (-), (==)}
      // Leak all available memory
      // The first call to malloc ensures it has been initialized
      Malloc.malloc(8n)
      Malloc.leakAll()
      // Next allocation will grow the memory by 1 page (64kib)
      // We'll manually leak all memory except what should be reserved for the test
      // Round reserved memory to nearest block size
      let reserved = %dn
      // If only one unit is requested, the allocator will include it in our next malloc,
      // so we request 2 instead
      let reserved = if (reserved == 1n) 2n else reserved
      // one page - 2 malloc headers - 1 gc header - extra morecore unit - reserved space
      let toLeak = 65536n - 16n - 8n - 64n - reserved * 64n
      Memory.malloc(toLeak)
    }

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
      ~max_pages=2,
      test_or_skip,
      name,
      makeGcProgram(prog, heapSize),
      expected,
    );
  let assertRunGCError = (name, heapSize, prog, expected) =>
    makeErrorRunner(
      test_or_skip,
      ~num_pages=1,
      ~max_pages=2,
      name,
      makeGcProgram(prog, heapSize),
      expected,
    );

  // oom tests
  // The allocator will use 2 units for the first allocation and then oom
  assertRunGCError(
    "oomgc1",
    2,
    "(1, (3, 4))",
    "Maximum memory size exceeded",
  );
  // This requires only 2 units, but if only two are requested they would be
  // used by the first allocation
  assertRunGC("oomgc2", 3, "(1, (3, 4))", "");
  assertRunGC("oomgc3", 1, "(3, 4)", "");

  // gc tests
  assertRunGC(
    "gc1",
    5,
    "let f = (() => (1, 2));\n       {\n         f();\n         f();\n         f();\n         f()\n       }",
    "",
  );
  /* https://github.com/grain-lang/grain/issues/774 */
  assertRunGC(
    "gc3",
    17,
    "let foo = (s: String) => void\nlet printBool = (b: Bool) => foo(if (b) \"true\" else \"false\")\n\nlet b = true\nfor (let mut i=0; i<100000; i += 1) {\n  printBool(true)\n}",
    "",
  );
  assertRunGCError(
    "fib_gc_err",
    5,
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
    9,
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
    5,
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
    350,
    {|
    from "list" include List
    use List.*

    let rec make_list = (x, n) => {
      let rec helper = (a, b, acc) => {
        if (a == 0) {
          acc
        } else {
          helper(a - 1, b, [b, ...acc])
        }
      }
      helper(n, x, [])
    }
    and loop = n => {
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
  assertRun(
    "no_tailcall_double_decref",
    {|
      let isNaN = x => x != x
      print(isNaN(NaN))
    |},
    "true\n",
  );
});
