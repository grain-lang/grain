# Memory Management in Grain

This guide documents Grain's memory management system. This document is aimed at those who are developing the code-generation pass of the compiler or writing low-level code inside of the Grain runtime; most users should not need to interact with the memory manager, and doing so risks corrupting the memory of your program (if the guidelines laid out here are not adhered to).

We ultimately aim to replace Grain's in-house memory management with the WebAssembly GC instructions once [the proposal](https://github.com/WebAssembly/gc/blob/master/proposals/gc/Overview.md) is implemented, but, in the meantime, Grain implements its own memory allocator and garbage collector.

## Memory Allocator
Grain uses a [memory allocator](https://github.com/grain-lang/grain/blob/main/stdlib/runtime/malloc.gr) derived from the `malloc`/`free` example given in Kernighan and Ritchie's ["The C Programming Language"](https://kremlin.cc/k&r.pdf) (K&R C), pages 185-188 (PDF page 199). This module exports the following values:

```grain
/**
 * The amount of space reserved for runtime code which needs to be executed
 * at boot before the malloc module is loaded
 */
export let _RESERVED_RUNTIME_SPACE: WasmI32

/**
 * Allocates the requested number of bytes, returning a pointer.
 *
 * @param nbytes: WasmI32 - The number of bytes to allocate
 * @return WasmI32 - The pointer to the allocated region (8-byte aligned), or -1 if the allocation failed.
 */
export let malloc: (nbytes: WasmI32) => WasmI32

/**
 * Frees the given allocated pointer.
 *
 * @param ap: WasmI32 - The pointer to free
 */
export let free = (ap: WasmI32) => Void

/**
 * Returns the current free list pointer
 * (used for debugging)
 *
 * @return WasmI32 - The free list pointer
 */
export let getFreePtr = () => WasmI32
```

These functions should be familiar to programmers who have used `malloc` and `free` in C (and C-like languages). For further reading, refer to this Wikipedia page: [C dynamic memory allocation](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation). The semantics of these functions align near-identically with those of C's corresponding functions.

Users should not call these functions directly in 99% of circumstances, and should instead use the `GC` module's wrappers (described below).

## Garbage Collector

Functional programming patterns often result in a large number of allocations, so it is imperative for developer experience that the language manages the creation and deletion of objects for users. Grain uses a reference-counting [garbage collector](https://github.com/grain-lang/grain/blob/main/stdlib/runtime/gc.gr) in order to determine when objects are no longer in active use by the program (and therefore able to have their memory freed).

At a technical level, this is represented in-memory in the following way, for an n-byte heap object:
```
 [ 32-bit counter ][ 32-bit padding ][ n-byte payload ]
 ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^~~~~~~~~~~~~~~~~~
 {start address}                    {pointer used by Grain runtime}
```
Here, `{start address}` is the value returned by `Malloc.malloc` (and passed to `Malloc.free` when the object is cleaned up), and the `{pointer used by Grain runtime}` is what everything outside of the `GC` wrapper uses as the pointer to the object. In other words, we "hide" the reference count in the 8 bytes prior to the pointer.

The interface provided by the `GC` module is similar (but not identical) to that which is provided by the `Malloc` module:
```grain
/**
 * Allocates the requested number of bytes, returning a pointer with a reference count of 1.
 *
 * @param size: WasmI32 - The number of bytes to allocate
 * @return WasmI32 - The pointer to the allocated region
 */
export let malloc = (size: WasmI32) => WasmI32

/**
 * Frees the given pointer. Using this pointer after it has been freed will result in undefined behavior.
 *
 * @param userPtr: WasmI32 - The pointer to free
 * @return Void
 */
export let free = (userPtr: WasmI32) => Void

/**
 * Increments the reference count of the given pointer.
 *
 * @param userPtr: WasmI32 - The pointer whose reference count should be incremented
 * @return WasmI32 - The given pointer
 */
export let incRef = (userPtr: WasmI32) => WasmI32

/**
 * Decrements the reference count of the given pointer if it is greater than zero
 *
 * @param userPtr: WasmI32 - The pointer whose reference count should be decremented
 * @return WasmI32 - The given pointer
 */
export let decRefIgnoreZeros = (userPtr: WasmI32) => WasmI32

/**
 * Decrements the reference count of the given pointer. An error is thrown if the
 * reference count is not greater than zero.
 *
 * @param userPtr: WasmI32 - The pointer whose reference count should be decremented
 * @return WasmI32 - The given pointer
 */
export let decRef = (userPtr: WasmI32) => WasmI32
```

When writing code in the runtime, `decRefIgnoreZeros` should never be used. It exists solely to support the cleanup step of compiled functions.

### Garbage Collection in Compiled Programs

In most Grain programs, the above functions do not need to be invoked by users. Instead, the compiler automatically inserts calls to them where required. When developing in `compcore.re`, it is important to understand the conventions used by the garbage collector.

**References from the local frame still count as references.** In other words, when a variable is stored inside of a Wasm local, its reference count should be incremented. There is one exception to this rule: so-called "swap slots". These are special locals that Grain uses to store temporary results within a single compiled `Mashtree` instruction (similar to how CPU registers are used in native compilers). For performance reasons, we do not increment the reference counter when storing values in these slots. This is safe due to the fact that these values do not escape beyond a single `Mashtree` instruction; while they may still be physically present in the local, they are not read by later pieces of the program.

For non-swap locals, this gives rise to the following pseudocode when storing a value in a local variable (this order is important to account for situations in which `val` is already in `local0`):
```
// We want to put `val` in `local0`
local0 := {
  tmp := incRef(val);
  decRef(local0);
  tmp
}
```
In practice, this is facilitated by `Tuple` instructions Binaryen, similar to this:
```
// We want to put `val` in `local0`
local0 := {
  (incRef(val), decRef(local0))[0]
}
```

**When a function returns a pointer, that pointer already has a reference count of at least one, which includes the reference from the local frame.** This means that, if the returned pointer does not escape the function, then a (net) single `decRef` should be invoked on that pointer in order to not leak memory.

When a function is exiting, it is imperative to clean up all references to values in the local frame while preserving the return value. We do this by doing the following:

- Calling `incRef` on the return value (which is in a local)
- Calling `decRefIgnoreZeros` on all locals

This should result in a net change of zero references on the return value, and a net change of -1 references on all other locals.

**References are caller-owned.** This has a few implications:
- If a function is called with an argument, it is the responsibility of whoever called that function to `decRef` that argument
- If a value is passed to a function, when it returns, it will have the same reference count as when the function was called (unless it was stored in an external location, in which case the reference count may be higher).

Moreover, the Grain compiler is designed to minimize the overall number of calls to `incRef` and `decRef` calls in its produced code. This means that, rather than the following (pseudocode):
```grain
let f = x => {
  incRef(x)
  foo(x)
  decRef(x)
  incRef(x)
  bar(x)
  decRef(x)
}
```
Grain's compiler will emit something more like this (again, note that `x` is an argument, so it is incumbent upon `f`'s caller to `decRef` it):
```grain
let f = x => {
  foo(x)
  bar(x)
}
```

## Disabling the Garbage Collector

There are times in which the garbage collector is not desired in Grain code. The typical example of this is the implementation of the core runtime (for example the garbage collector itself), which involves working with native WebAssembly values. Consequently, Grain provides two mechanisms for disabling the garbage collector, either on a per-function or a per-file basis.

First, it is possible to annotate functions with `@disableGC`. This will compile the annotated function with no garbage collection instructions. Second, it is possible to pass the `--compilation-mode=runtime` flag to the compiler (**NOTE** that this is not the only effect of this flag; see `runtime.md` for more details), which is typically done by placing this at the beginning of the file:
```grain
/* grainc-flags --compilation-mode=runtime */
```
When this flag is enabled, no functions in the file will be compiled with garbage collection instructions.

Naturally, this means that it is incumbent upon the programmer to make the appropriate calls to `GC.incRef` and `GC.decRef` in order to not leak memory.