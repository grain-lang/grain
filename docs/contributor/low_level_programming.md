# Low-level Programming in Grain

Grain is overwhelmingly a high-level programming language, but it provides a few tools for low-leveling programming meant for library authors and the development of Grain's runtime.

## Unsafe Types

Grain has four unsafe types, `WasmI32`, `WasmI64`, `WasmF32`, and `WasmF64`. Unlike all other Grain types which are always represented as an `i32`, these special types are represented by their corresponding WebAssembly types. This makes these types **incompatible** with regular Grain values, and they cannot be used with generic functions (like `print`) or appear on Grain's heap (like in a list, array, or tuple). While the typechecker will often prevent you from using them incorrectly, some uses may not be caught (#1153).

Libraries for working with these types can be found in `stdlib/runtime/unsafe` and map directly to WebAssembly instructions.

## `@unsafe`

To make it more visually obvious that something dangerous is happening, the `@unsafe` annotation is required for functions/statements using unsafe types. `@unsafe` doesn't alter how the function is compiled; it just disables the warnings.

### Gotchas

- Unsafe types cannot appear on the heap; the garbage collector will attempt to collect them. Convert them to native Grain `Int32`/`Int64`/`Float32`/`Float64` values first using functions in `runtime/unsafe/conv`.
- Function arguments/locals are decRefed (and potentially collected) at the end of the function. If a pointer to a value is obtained via `WasmI32.toGrain`, that reference should only be considered live up until the point that the function returns (unless it is explicitly incRefed).
- `WasmI32.toGrain` should only be used on freshly-produced values obtained from an `allocate` function from `runtime/dataStructures`. Doing something like `x => WasmI32.toGrain(WasmI32.fromGrain(x))` is _not_ safe and will cause memory corruption as the compiler does not consider `x` to be the same value as `WasmI32.fromGrain(x)`. An explicit `incRef` would be necessary to keep `x` alive in this case.
