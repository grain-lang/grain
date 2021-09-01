open Grain_tests.TestFramework;
open Grain_tests.Runner;

let makeGcProgram = (program, heap_size) => {
  Printf.sprintf(
    {|
    import WasmI32 from "runtime/unsafe/wasmi32"
    import Malloc from "runtime/malloc"
    import Memory from "runtime/unsafe/memory"
    @disableGC
    let leak = () => {
      // find current memory pointer, subtract space for two malloc headers + 1 GC header
      let offset = WasmI32.sub(Memory.malloc(8n), 24n)
      // Calculate how much memory is left
      let availableMemory = WasmI32.sub(offset, Malloc._RESERVED_RUNTIME_SPACE)
      // Calculate how much memory to leak
      let toLeak = WasmI32.sub(availableMemory, %dn)
      // Memory is not reclaimed due to no gc context
      // This will actually leak 16 extra bytes because of the headers
      Memory.malloc(WasmI32.sub(toLeak, 16n));
      void
    }
    leak();
    %s
    |},
    heap_size,
    program,
  );
};

let readWholeFile = filename => {
  let ch = open_in(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s;
};

describe("garbage collection", ({test}) => {
  let assertFileRun = makeFileRunner(test);
  let assertMemoryLimitedFileRun = makeFileRunner(~num_pages=1, test);
  let assertRunGC = (name, heapSize, prog) =>
    makeRunner(test, name, makeGcProgram(prog, heapSize), "");
  let assertRunGCError = (name, heapSize, prog, expected) =>
    makeErrorRunner(
      test,
      ~num_pages=1,
      name,
      makeGcProgram(prog, heapSize),
      expected,
    );
  let assertFileRunGC = (name, heapSize, file, expected) =>
    makeErrorRunner(
      test,
      ~num_pages=1,
      name,
      makeGcProgram(readWholeFile("test/input/" ++ file ++ ".gr"), heapSize),
      expected,
    );

  // oom tests
  assertRunGCError(
    "oomgc1",
    48,
    "(1, (3, 4))",
    "Maximum memory size exceeded",
  );
  assertRunGC("oomgc2", 64, "(1, (3, 4))");
  assertRunGC("oomgc3", 32, "(3, 4)");

  // gc tests
  assertRunGC(
    "gc1",
    160,
    "let f = (() => (1, 2));\n       {\n         f();\n         f();\n         f();\n         f()\n       }",
  );
  /* Test that cyclic tuples are GC'd properly */
  assertRunGC(
    "gc2",
    256,
    "enum Opt<x> { None, Some(x) };\n     let f = (() => {\n      let x = (box(None), 2);\n      let (fst, _) = x\n      fst := Some(x)\n      });\n      {\n        f();\n        let x = (1, 2);\n        x\n      }",
  );
  /* https://github.com/grain-lang/grain/issues/774 */
  assertRunGC(
    "gc3",
    1024,
    "let foo = (s: String) => void\nlet printBool = (b: Bool) => foo(if (b) \"true\" else \"false\")\n\nlet b = true\nfor (let mut i=0; i<100000; i += 1) {\n  printBool(true)\n}",
  );
  assertFileRunGC(
    "fib_gc_err",
    1024,
    "fib-gc",
    "Maximum memory size exceeded",
  );
  assertFileRunGC("fib_gc", 2048, "fib-gc", "832040");
  /* tgcfile "fib_gc_bigger" 3072 "fib-gc" "832040";
     tgcfile "fib_gc_biggest" 512 "fib-gc" "832040"; */
  /* I've manually tested this test, but see TODO for automated testing */
  /* tgcfile ~todo:"Need to figure out how to disable dead assignment elimination to make sure this test is actually checking what we want" "sinister_gc" 3072 "sinister-tail-call-gc" "true"; */
  assertFileRunGC("long_lists", 20000, "long_lists", "true");
  assertFileRun("malloc_tight", "mallocTight", "");
  assertFileRun("memory_grow1", "memoryGrow", "1000000000000\n");
  assertMemoryLimitedFileRun(
    "loop_memory_reclaim",
    "loopMemoryReclaim",
    "OK\n",
  );
});
