---
title: WasmRef
---

Utilities for working with WebAssembly reference types in Grain.

This modules provides unsafe functions and should be used with extreme caution;
incorrect usage will lead to runtime errors or undefined behavior.

## Values

Functions and constants included in the WasmRef module.

### WasmRef.**fromGrain**

```grain
fromGrain: (value: a) => WasmRef
```

Casts a Grain value to a WebAssembly reference type.

Parameters:

| param   | type | description       |
| ------- | ---- | ----------------- |
| `value` | `a`  | The value to cast |

Returns:

| type      | description                                           |
| --------- | ----------------------------------------------------- |
| `WasmRef` | A `WasmRef` corresponding to the provided Grain value |

### WasmRef.**toGrain**

```grain
toGrain: (value: WasmRef) => a
```

Casts a WebAssembly reference type to a Grain value.

NOTE:
This function does not tell the typechecker the exact type of the resulting Grain value,
it is the caller's responsibility to ensure that the provided `WasmRef` corresponds to a
valid Grain value, and the caller must ensure that the resulting Grain value is used in a
type-safe manner, as using an invalid reference will lead to runtime errors or undefined behavior.

Parameters:

| param   | type      | description           |
| ------- | --------- | --------------------- |
| `value` | `WasmRef` | The `WasmRef` to cast |

Returns:

| type | description                                           |
| ---- | ----------------------------------------------------- |
| `a`  | A Grain value corresponding to the provided `WasmRef` |

### WasmRef.**isRefI31**

```grain
isRefI31: (ref: WasmRef) => Bool
```

Checks if the provided `WasmRef` is a `ref i31`.

Parameters:

| param | type      | description              |
| ----- | --------- | ------------------------ |
| `ref` | `WasmRef` | The `WasmRef` to inspect |

Returns:

| type   | description                                               |
| ------ | --------------------------------------------------------- |
| `Bool` | `true` if the reference is a `ref i31`, `false` otherwise |

### WasmRef.**isGrainHeapValue**

```grain
isGrainHeapValue: (ref: WasmRef) => Bool
```

Checks if the provided `WasmRef` is a Grain heap value.

Parameters:

| param | type      | description              |
| ----- | --------- | ------------------------ |
| `ref` | `WasmRef` | The `WasmRef` to inspect |

Returns:

| type   | description                                                      |
| ------ | ---------------------------------------------------------------- |
| `Bool` | `true` if the reference is a Grain heap value, `false` otherwise |

### WasmRef.**i31GetS**

```grain
i31GetS: (ref: WasmRef) => WasmI32
```

Retrieves the signed 31-bit integer value from the provided `WasmRef`.

NOTE: This function will trap if the provided reference is not a `ref i31`.

Parameters:

| param | type      | description              |
| ----- | --------- | ------------------------ |
| `ref` | `WasmRef` | The `WasmRef` to inspect |

Returns:

| type      | description                     |
| --------- | ------------------------------- |
| `WasmI32` | The signed 31-bit integer value |

### WasmRef.**i31GetU**

```grain
i31GetU: (ref: WasmRef) => WasmI32
```

Retrieves the unsigned 31-bit integer value from the provided `WasmRef`.

NOTE: This function will trap if the provided reference is not a `ref i31`.

Parameters:

| param | type      | description              |
| ----- | --------- | ------------------------ |
| `ref` | `WasmRef` | The `WasmRef` to inspect |

Returns:

| type      | description                       |
| --------- | --------------------------------- |
| `WasmI32` | The unsigned 31-bit integer value |

### WasmRef.**i31Make**

```grain
i31Make: (value: WasmI32) => WasmRef
```

Creates an i31ref from a WasmI32 value.

Parameters:

| param   | type      | description                            |
| ------- | --------- | -------------------------------------- |
| `value` | `WasmI32` | The WasmI32 value to wrap as an i31ref |

Returns:

| type      | description                          |
| --------- | ------------------------------------ |
| `WasmRef` | The WasmRef containing the i31 value |

## WasmRef.WasmArrayRef

Utilities for working with WebAssembly array reference types in Grain.

### Types

Type declarations included in the WasmRef.WasmArrayRef module.

#### WasmRef.WasmArrayRef.**WasmArrayRef**

```grain
type WasmArrayRef
```

Represents a reference to an unsafe WebAssembly array.

### Values

Functions and constants included in the WasmRef.WasmArrayRef module.

#### WasmRef.WasmArrayRef.**makeAny**

```grain
makeAny: (length: WasmI32, initialValue: WasmRef) => WasmArrayRef
```

Constructs a new (array (mut (ref any))) with the specified length.
Where the elements of the array are initialized to provided references.

Parameters:

| param          | type      | description                                                      |
| -------------- | --------- | ---------------------------------------------------------------- |
| `length`       | `WasmI32` | The number of elements in the array                              |
| `initialValue` | `WasmRef` | The reference value to initialize each element of the array with |

Returns:

| type           | description                                     |
| -------------- | ----------------------------------------------- |
| `WasmArrayRef` | A new WebAssembly array of the specified length |

#### WasmRef.WasmArrayRef.**fromWasmRef**

```grain
fromWasmRef: (ref: WasmRef) => WasmArrayRef
```

Performs an unchecked cast from a `WasmRef` to a `WasmArrayRef`.
The caller must ensure that the provided reference is indeed a valid WebAssembly array reference,
as using an invalid reference will lead to runtime errors.

Parameters:

| param | type      | description              |
| ----- | --------- | ------------------------ |
| `ref` | `WasmRef` | The reference to convert |

Returns:

| type           | description                                              |
| -------------- | -------------------------------------------------------- |
| `WasmArrayRef` | A `WasmArrayRef` corresponding to the provided `WasmRef` |

#### WasmRef.WasmArrayRef.**length**

```grain
length: (array: WasmArrayRef) => WasmI32
```

Provides the length of the input `WasmArrayRef`.

Parameters:

| param   | type           | description                   |
| ------- | -------------- | ----------------------------- |
| `array` | `WasmArrayRef` | The `WasmArrayRef` to inspect |

Returns:

| type      | description                         |
| --------- | ----------------------------------- |
| `WasmI32` | The number of elements in the array |

#### WasmRef.WasmArrayRef.**getI8S**

```grain
getI8S: (array: WasmArrayRef, index: WasmI32) => WasmI32
```

Retrieves the signed 8-bit integer value at the specified index
from the given `WasmArrayRef`.

NOTE: This function will throw a runtime error if the provided index is out of bounds.
NOTE: This function will throw a runtime error if the array is not an array of i8 values.

Parameters:

| param   | type           | description                          |
| ------- | -------------- | ------------------------------------ |
| `array` | `WasmArrayRef` | The `WasmArrayRef` to access         |
| `index` | `WasmI32`      | The index of the element to retrieve |

Returns:

| type      | description                                           |
| --------- | ----------------------------------------------------- |
| `WasmI32` | The signed 8-bit integer value at the specified index |

#### WasmRef.WasmArrayRef.**getI8U**

```grain
getI8U: (array: WasmArrayRef, index: WasmI32) => WasmI32
```

Retrieves the unsigned 8-bit integer value at the specified index
from the given `WasmArrayRef`.

NOTE: This function will throw a runtime error if the provided index is out of bounds.
NOTE: This function will throw a runtime error if the array is not an array of i8 values.

Parameters:

| param   | type           | description                          |
| ------- | -------------- | ------------------------------------ |
| `array` | `WasmArrayRef` | The `WasmArrayRef` to access         |
| `index` | `WasmI32`      | The index of the element to retrieve |

Returns:

| type      | description                                             |
| --------- | ------------------------------------------------------- |
| `WasmI32` | The unsigned 8-bit integer value at the specified index |

#### WasmRef.WasmArrayRef.**setI8**

```grain
setI8: (array: WasmArrayRef, index: WasmI32, value: WasmI32) => Void
```

Sets the 8-bit integer value at the specified index in the given `WasmArrayRef`.

NOTE: This function will throw a runtime error if the provided index is out of bounds.
NOTE: This function will throw a runtime error if the array is not an array of i8 values.

Parameters:

| param   | type           | description                                           |
| ------- | -------------- | ----------------------------------------------------- |
| `array` | `WasmArrayRef` | The `WasmArrayRef` to modify                          |
| `index` | `WasmI32`      | The index of the element to set                       |
| `value` | `WasmI32`      | The 8-bit integer value to set at the specified index |

#### WasmRef.WasmArrayRef.**getI64**

```grain
getI64: (array: WasmArrayRef, index: WasmI32) => WasmI64
```

Retrieves the 64-bit integer value at the specified index

NOTE: This function will throw a runtime error if the provided index is out of bounds.
NOTE: This function will throw a runtime error if the array is not an array of i64 values.

Parameters:

| param   | type           | description                          |
| ------- | -------------- | ------------------------------------ |
| `array` | `WasmArrayRef` | The `WasmArrayRef` to access         |
| `index` | `WasmI32`      | The index of the element to retrieve |

Returns:

| type      | description                                     |
| --------- | ----------------------------------------------- |
| `WasmI64` | The 64-bit integer value at the specified index |

#### WasmRef.WasmArrayRef.**setI64**

```grain
setI64: (array: WasmArrayRef, index: WasmI32, value: WasmI64) => Void
```

Sets the 64-bit integer value at the specified index in the given `WasmArrayRef`.

NOTE: This function will throw a runtime error if the provided index is out of bounds.
NOTE: This function will throw a runtime error if the array is not an array of i64 values.

Parameters:

| param   | type           | description                                            |
| ------- | -------------- | ------------------------------------------------------ |
| `array` | `WasmArrayRef` | The `WasmArrayRef` to modify                           |
| `index` | `WasmI32`      | The index of the element to set                        |
| `value` | `WasmI64`      | The 64-bit integer value to set at the specified index |

#### WasmRef.WasmArrayRef.**getAny**

```grain
getAny: (array: WasmArrayRef, index: WasmI32) => WasmRef
```

Retrieves the reference value at the specified index from the given `WasmArrayRef` as a `WasmRef`.

NOTE: This function will throw a runtime error if the provided index is out of bounds.
NOTE: This function will throw a runtime error if the array is not an array of reference types.

Parameters:

| param   | type           | description                          |
| ------- | -------------- | ------------------------------------ |
| `array` | `WasmArrayRef` | The `WasmArrayRef` to access         |
| `index` | `WasmI32`      | The index of the element to retrieve |

Returns:

| type      | description                                               |
| --------- | --------------------------------------------------------- |
| `WasmRef` | The reference value at the specified index as a `WasmRef` |

#### WasmRef.WasmArrayRef.**setAny**

```grain
setAny: (array: WasmArrayRef, index: WasmI32, value: WasmRef) => Void
```

Sets the reference value at the specified index in the given `WasmArrayRef`.

NOTE: This function will throw a runtime error if the provided index is out of bounds.
NOTE: This function will throw a runtime error if the array is not an array of reference types.

Parameters:

| param   | type           | description                                                      |
| ------- | -------------- | ---------------------------------------------------------------- |
| `array` | `WasmArrayRef` | The `WasmArrayRef` to modify                                     |
| `index` | `WasmI32`      | The index of the element to set                                  |
| `value` | `WasmRef`      | The reference value to set at the specified index as a `WasmRef` |

#### WasmRef.WasmArrayRef.**copyI8**

```grain
copyI8:
  (dest: WasmArrayRef, destIndex: WasmI32, src: WasmArrayRef,
   srcIndex: WasmI32, length: WasmI32) => Void
```

Copies data from a i8 array to an i8 array.

Parameters:

| param       | type           | description                  |
| ----------- | -------------- | ---------------------------- |
| `dest`      | `WasmArrayRef` | The destination array        |
| `destIndex` | `WasmI32`      | The destination index        |
| `src`       | `WasmArrayRef` | The source array             |
| `srcIndex`  | `WasmI32`      | The source index             |
| `length`    | `WasmI32`      | The number of values to copy |

#### WasmRef.WasmArrayRef.**copyI64**

```grain
copyI64:
  (dest: WasmArrayRef, destIndex: WasmI32, src: WasmArrayRef,
   srcIndex: WasmI32, length: WasmI32) => Void
```

Copies data from an i64 array to an i64 array.

Parameters:

| param       | type           | description                  |
| ----------- | -------------- | ---------------------------- |
| `dest`      | `WasmArrayRef` | The destination array        |
| `destIndex` | `WasmI32`      | The destination index        |
| `src`       | `WasmArrayRef` | The source array             |
| `srcIndex`  | `WasmI32`      | The source index             |
| `length`    | `WasmI32`      | The number of values to copy |

#### WasmRef.WasmArrayRef.**copyAny**

```grain
copyAny:
  (dest: WasmArrayRef, destIndex: WasmI32, src: WasmArrayRef,
   srcIndex: WasmI32, length: WasmI32) => Void
```

Copies data from a (array (ref any)) to a (array mut (ref any)).

Parameters:

| param       | type           | description                  |
| ----------- | -------------- | ---------------------------- |
| `dest`      | `WasmArrayRef` | The destination array        |
| `destIndex` | `WasmI32`      | The destination index        |
| `src`       | `WasmArrayRef` | The source array             |
| `srcIndex`  | `WasmI32`      | The source index             |
| `length`    | `WasmI32`      | The number of values to copy |

#### WasmRef.WasmArrayRef.**fillI8**

```grain
fillI8:
  (array: WasmArrayRef, offset: WasmI32, value: WasmI32, length: WasmI32) =>
   Void
```

Fills the given (array (mut i8)) with the given 1-byte value. Values larger than 1 byte will be truncated.

Parameters:

| param    | type           | description                         |
| -------- | -------------- | ----------------------------------- |
| `array`  | `WasmArrayRef` | The destination array               |
| `offset` | `WasmI32`      | The offset in the destination array |
| `value`  | `WasmI32`      | The value to fill the array with    |
| `length` | `WasmI32`      | The length of the array to fill     |

#### WasmRef.WasmArrayRef.**fillI64**

```grain
fillI64:
  (array: WasmArrayRef, offset: WasmI32, value: WasmI64, length: WasmI32) =>
   Void
```

Fills the given (array (mut i64)) with the given value.

Parameters:

| param    | type           | description                         |
| -------- | -------------- | ----------------------------------- |
| `array`  | `WasmArrayRef` | The destination array               |
| `offset` | `WasmI32`      | The offset in the destination array |
| `value`  | `WasmI64`      | The value to fill the array with    |
| `length` | `WasmI32`      | The length of the array to fill     |

#### WasmRef.WasmArrayRef.**fillAny**

```grain
fillAny:
  (array: WasmArrayRef, offset: WasmI32, value: WasmArrayRef, length: WasmI32) =>
   Void
```

Fills the given (array (mut ref any)) with the given value.

Parameters:

| param    | type           | description                         |
| -------- | -------------- | ----------------------------------- |
| `array`  | `WasmArrayRef` | The destination array               |
| `offset` | `WasmI32`      | The offset in the destination array |
| `value`  | `WasmArrayRef` | The value to fill the array with    |
| `length` | `WasmI32`      | The length of the array to fill     |

#### WasmRef.WasmArrayRef.**compareI8**

```grain
compareI8:
  (first: WasmArrayRef, firstOffset: WasmI32, second: WasmArrayRef,
   secondOffset: WasmI32, length: WasmI32) => WasmI32
```

Compares two i8 arrays.

Parameters:

| param    | type           | description                |
| -------- | -------------- | -------------------------- |
| `first`  | `WasmArrayRef` | The first array            |
| `second` | `WasmArrayRef` | The second array           |
| `length` | `WasmI32`      | How many elements to check |

Returns:

| type      | description                                                                                                                                             |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `WasmI32` | `0` if the arrays are equal, a negative value if the array is less than the second, and a positive value if the first array is greater than the second. |

