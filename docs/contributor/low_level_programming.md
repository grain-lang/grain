# Low-level Programming in Grain

Grain is overwhelmingly a high-level programming language, but it provides a few tools for low-leveling programming meant for library authors and the development of Grain's runtime.

## Unsafe Types

Grain has four unsafe types, `WasmI32`, `WasmI64`, `WasmF32`, and `WasmF64`. Unlike all other Grain types which are always represented as an `wasmRef`, these special types are represented by their corresponding WebAssembly types. This makes these types **incompatible** with regular Grain values, and they cannot be used with generic functions (like `print`) or appear in Grain's heap values (like in a `list`, `array`, `tuple` or any other high level dataStructure). While the typechecker will often prevent you from using them incorrectly, some uses may not be caught (#1153).

Libraries for working with these types can be found in `stdlib/runtime/unsafe` and map directly to WebAssembly instructions. `runtime/debugPrint` can be used to print values of these types for debugging purposes as they cannot be printed with the regular `print` function due to their incompatibility with regular Grain values.

## `@unsafe`

To make it more visually obvious that something dangerous is happening, the `@unsafe` annotation is required for functions/statements using unsafe types. `@unsafe` doesn't alter how the function is compiled; it just disables the warnings.

### Gotchas

While the number of gotchas has been minimized compared to past versions of Grain, there are still a few things to keep in mind when working with unsafe types. As unsafe values are not a subtype of `WasmRef` they cannot be used in any context where a regular Grain value is expected. This means that, they cannot be stored in any high level data structures such as `list`, `array`, `tuple`, `records` or any other high level dataStructure, they cannot be passed to any generic functions (like `print`, `==`, `marshal` or `compare`) as they are incompatible with regular Grain values, and they cannot be captured by closures as closures are implemented as heap values and are also subject to the same limitations as other heap values. To work around these limitations, convert them to native Grain `Int32`/`Int64`/`Float32`/`Float64` values first using functions in `runtime/unsafe/conv`.