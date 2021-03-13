# The Grain Runtime

When we speak of the Grain runtime, we largely mean the memory allocator, garbage collector, and anything else that needs to exist before these faculties are available (those are the modules in the stdlib/runtime folder). These are essential to all Grain programs, and some care must be taken to compile them. For that reason, these modules are compiled with the `--compilation-mode=runtime` flag. In this mode, there is no access to Pervasives and all allocations happen in the runtime heap.

## The Runtime Heap

Currently, the Grain runtime heap spans half a WebAssembly page of memory. The low 1K of memory is reserved for Binaryen optimizations, the next few bytes are reserved for some static pointers (which we'll go over next), and the rest of half-page is space used for runtime allocations. It's important to note that this space is unmanaged. After all, we don't have a memory manager yet—we want to compile the memory manager. Allocations are done just by incrementing a bytes counter. This means that no space can be reclaimed—as such, runtime modules should do no dynamic allocations. Ideally, the only allocations that should occur are for the closures of top-level functions that are used by other modules.

## Static Runtime Pointers

There are a handful of static pointers that all modules have access to.

- `0x400`: The next position in the runtime heap to allocate from. When a runtime allocation is done, this value at this address is advanced by the allocation amount.
- `0x408`: The pointer to the linked list of runtime type information. More information on this can be found in [printing.md](./printing.md).
