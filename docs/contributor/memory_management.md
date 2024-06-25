# Memory Management in Grain

This guide documents Grain's memory management system. This document is aimed at those who are developing the code-generation pass of the compiler or writing low-level code inside of the Grain runtime; most users should not need to interact with the memory manager, and doing so risks corrupting the memory of your program if the guidelines laid out here are not adhered to.

We ultimately aim to replace Grain's bespoke memory management with the WebAssembly GC instructions once [the proposal](https://github.com/WebAssembly/gc/blob/master/proposals/gc/Overview.md) is implemented—in the meantime, Grain implements its own memory allocator and garbage collector.

## Memory Allocator

More documentation about Grain's [memory allocator](https://github.com/grain-lang/grain/blob/main/stdlib/runtime/malloc.gr) can be found in that module. It exports the following values:

```grain
/**
 * The amount of space reserved for runtime code which needs to be executed
 * at boot before the malloc module is loaded
 */
export let _RESERVED_RUNTIME_SPACE: WasmI32

/**
 * Allocates the requested number of bytes, returning a pointer.
 *
 * @param nbytes: The number of bytes to allocate
 * @returns The pointer to the allocated region (8-byte aligned), or -1 if the allocation failed.
 */
export let malloc: (nbytes: WasmI32) -> WasmI32

/**
 * Frees the given allocated pointer.
 *
 * @param ap: The pointer to free
 */
export let free = (ap: WasmI32) => Void

/**
 * Leaks all memory in all free lists; used for testing.
 */
export let leakAll = () => Void
```

These functions should be familiar to programmers who have used `malloc` and `free` in C (and C-like languages). For further reading, refer to this Wikipedia page: [C dynamic memory allocation](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation). The semantics of these functions align near-identically with those of C's corresponding functions.

These functions generally should not be called directly in 99% of circumstances, and the `GC` module's wrappers should be used instead (described below).

## Garbage Collector

Functional programming patterns often result in a large number of allocations, so it is imperative for developer experience that the language manages the creation and deletion of objects for users. Grain uses a reference-counting [garbage collector](https://github.com/grain-lang/grain/blob/main/stdlib/runtime/gc.gr) in order to determine when objects are no longer in active use by the program and therefore able to have their memory freed.

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
 * @param size: The number of bytes to allocate
 * @returns The pointer to the allocated region
 */
export let malloc = (size: WasmI32) -> WasmI32

/**
 * Frees the given pointer. Using this pointer after it has been freed will result in undefined behavior.
 *
 * @param userPtr: The pointer to free
 */
export let free = (userPtr: WasmI32) -> Void

/**
 * Increments the reference count of the given pointer.
 *
 * @param userPtr: The pointer whose reference count should be incremented
 * @returns The given pointer
 */
export let incRef = (userPtr: WasmI32) -> WasmI32

/**
 * Decrements the reference count of the given pointer. An error is thrown if the
 * reference count is not greater than zero.
 *
 * @param userPtr: The pointer whose reference count should be decremented
 * @returns The given pointer
 */
export let decRef = (userPtr: WasmI32) -> WasmI32
```

The reference count-managing functions are safe to use with non-pointers; if a non-pointer is passed
to them, it will be returned without any side effects. **However**, it should be noted that these functions are not safe to use with arbitrary (untagged) `WasmXX` (`WasmI32`/`WasmI64`/etc) values, which is why the use of such types is only permitted in `@disableGC` blocks—so that the compiler does not insert `incRef`/`decRef` calls.

### Garbage Collection in Compiled Programs

In normal Grain programs, the above functions do not need to be invoked by users. Instead, the compiler automatically inserts calls to them where required. When developing in `compcore.re`, it is important to understand the conventions used by the garbage collector.

**Functions contain references to themselves.** In order to support closures, Grain's compiler emits code which passes the current closure to each function as an extra argument. With respect to the calling convention, this extra argument is the same as normal arguments, so it follows the same `incRef`-before-function-call pattern as standard arguments and must be `decRef`ed before functions return, as described in the following section. This is particularly important to keep in mind when writing `@disableGC` code, as discussed in "Disabling the Garbage Collector" below.

**Reference counts are incremented upon access.** With some exceptions in generated code, reference counts are incremented whenever a value is accessed (loaded from storage slots such as arguments, locals, and globals). This streamlines the conventions in generated code, as function calls require no specific `incRef` invocation for arguments (see "References are callee-owned" subsection below). For clarity, a couple of notes:

- If a value is stored somewhere, no extra `incRef`/`decRef` calls are needed. There are rare exceptions to this in the compiler, specifically regarding "swap slots", which are local variables used as registers.
- If the computation no longer needs a live value (e.g. after a statement in the middle of a block), it should be `decRef`ed.
- No special `incRef` is needed for return values, as the access of those values performs the requisite `incRef`. It should be noted that in generated WebAssembly code, the function postamble looks as follows:

```
swap_slot <- return_value
for argument in arguments:
    decref(argument)
for local_variable in local_variables:
    decref(local_variable)
return load_swap(return_value) # see note below
```

Note that `load_swap` is one of the aforementioned exceptions which does not `incRef` the value upon access. Swap slots are used by the compiler to temporarily store intermediate values during computation, and they do not need to have their contents `incRef`ed; i.e. since they are only used for intermediate values, any references they hold are treated as weak and do not impact the reference count.

**References are callee-owned.** This has a couple of implications:

- If a function is called with an argument, it is the responsibility of that function to `decRef` that argument.
- If a value is passed to a function with a reference count of `n`, when it returns, it will have the a reference count of `n-1` (unless it was stored in an external location, in which case the reference count may be higher).

## Disabling the Garbage Collector

There are times in which the garbage collector is not desired in Grain code. The typical example of this is the implementation of the core runtime (the garbage collector and memory allocator). Generally, even for many parts of the runtime, the garbage collector should not be disabled as it is fairly easy to cause memory leaks or memory corruption. That said, Grain provides two mechanisms for disabling the garbage collector, either on a per-function or a per-file/program basis.

First, it is possible to annotate functions with `@disableGC`. This will compile the annotated function with no garbage collection instructions. It should be noted that this is not necessary to write low-level Grain code, and you can view the document on [@unsafe and low-level programming](./low_level_programming.md) to learn more. Second, the `--no-gc` compiler flag disables the garbage collector, performing allocations via `malloc`.

Additionally, the `@runtimeMode` attribute can be placed onto the top-level `module` statement of a file to compile a file in runtime mode, where GC doesn't exist (so no GC instructions will be emitted) and implicit allocations can't be reclaimed. See `runtime.md` for more details.

When this flag is enabled, no functions in the file will be compiled with garbage collection instructions.

Naturally, this means that it is incumbent upon the programmer to make the appropriate calls to `GC.incRef` and `GC.decRef` in order to not leak memory. For example, because references are callee-owned, it is required to increment the reference counts of all arguments _and_ the function itself (recall from the "Functions contain references to themselves" subsection that the function will receive the closure as a "hidden" argument) before calling GC-enabled functions. Some internal functions in the runtime and standard library may be GC-disabled, in which case this is not necessary. This is typically the case for functions which accept or return a `WasmXX` (`WasmI32`/`WasmI64`/etc) type value.

Here are some concrete examples.

When calling function from inside a GC-disabled function:

```grain
// Taken from string.gr
// @disableGC-safe wrapper
@disableGC
let wasmSafeLength = (s: String) => {
  Memory.incRef(WasmI32.fromGrain(length)) // <- incRef closure being invoked
  Memory.incRef(WasmI32.fromGrain(s)) // <- incRef argument before call
  length(s)
}
```

Before returning from a GC-disabled function which follows the Grain calling convention (i.e. one which is safe to call from GC-enabled code):

```grain
// Taken from sqrt() function in number.gr
@disableGC
export let rec sqrt = (x: Number) => {
  // ...
  let ret = ...
  Memory.decRef(WasmI32.fromGrain(x))   // <- decrement argument
  Memory.decRef(WasmI32.fromGrain(sqrt)) // <- decrement closure (note that this function needs to be recursive)
  ret
}
```
