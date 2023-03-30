# The Grain Runtime

When we speak of the Grain runtime, we largely mean the memory allocator, garbage collector, and anything else that needs to exist before these faculties are available (those are the modules in the stdlib/runtime folder). These are essential to all Grain programs, and some care must be taken to compile them. For that reason, these modules are compiled with the `@runtimeMode` module attribute. In this mode, there is no access to Pervasives and all allocations happen in the runtime heap.

## The Runtime Heap

The runtime heap is the region of memory that is used by Grain programs in runtime mode, before a memory allocator is available. Allocations are done just by incrementing a bytes counter. At the current time, 0x800 bytes are reserved for the runtime.

Memory is laid out as follows:

- reserved space defined by the user via `--memory-base`, or 1024 bytes used by Binaryen optimizations otherwise
- runtime type information; more information on this can be found in [printing.md](./printing.md)
- the runtime heap
- the general, memory managed heap
