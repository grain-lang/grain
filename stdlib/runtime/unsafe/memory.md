---
title: Memory
---

## Values

Functions and constants included in the Memory module.

### Memory.**copy**

```grain
copy: (dest: WasmI32, src: WasmI32, length: WasmI32) => Void
```

Copies the source memory region to the destination memory region. Regions may overlap.

Parameters:

| param    | type      | description                             |
| -------- | --------- | --------------------------------------- |
| `dest`   | `WasmI32` | The destination memory region           |
| `src`    | `WasmI32` | The source memory region                |
| `length` | `WasmI32` | The length of the memory region to copy |

### Memory.**fill**

```grain
fill: (dest: WasmI32, value: WasmI32, length: WasmI32) => Void
```

Fills the given memory region with the given 1-byte value. Values larger than 1 byte will be truncated.

Parameters:

| param    | type      | description                              |
| -------- | --------- | ---------------------------------------- |
| `dest`   | `WasmI32` | The destination memory region            |
| `value`  | `WasmI32` | The value to fill the memory region with |
| `length` | `WasmI32` | The length of the memory region to fill  |

### Memory.**fillArrayI8**

```grain
fillArrayI8:
  (dest: WasmRef, offset: WasmI32, value: WasmI32, length: WasmI32) => Void
```

Fills the given (array (mut i8)) with the given 1-byte value. Values larger than 1 byte will be truncated.

Parameters:

| param    | type      | description                         |
| -------- | --------- | ----------------------------------- |
| `dest`   | `WasmRef` | The destination array               |
| `offset` | `WasmI32` | The offset in the destination array |
| `value`  | `WasmI32` | The value to fill the array with    |
| `length` | `WasmI32` | The length of the array to fill     |

### Memory.**compare**

```grain
compare: (ptr1: WasmI32, ptr2: WasmI32, length: WasmI32) => WasmI32
```

### Memory.**compareArrayI8**

```grain
compareArrayI8:
  (first: WasmRef, firstOffset: WasmI32, second: WasmRef,
   secondOffset: WasmI32, length: WasmI32) => WasmI32
```

Compares two (array i8).

Parameters:

| param    | type      | description                |
| -------- | --------- | -------------------------- |
| `first`  | `WasmRef` | The first array            |
| `second` | `WasmRef` | The second array           |
| `length` | `WasmI32` | How many elements to check |

Returns:

| type      | description                                                                                                                                             |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `WasmI32` | `0` if the arrays are equal, a negative value if the array is less than the second, and a positive value if the first array is greater than the second. |

### Memory.**copyRefArrayToLinearMemory**

```grain
copyRefArrayToLinearMemory:
  (dest: WasmI32, src: WasmRef, length: WasmI32) => Void
```

Copies data from a (array i8) to linear memory.

Parameters:

| param    | type      | description                   |
| -------- | --------- | ----------------------------- |
| `dest`   | `WasmI32` | The destination memory region |
| `src`    | `WasmRef` | The source array              |
| `length` | `WasmI32` | The number of bytes to copy   |

### Memory.**copyLinearMemoryToRefArray**

```grain
copyLinearMemoryToRefArray:
  (dest: WasmRef, src: WasmI32, length: WasmI32) => Void
```

Copies data from linear memory to a (array mut i8).

Parameters:

| param    | type      | description                 |
| -------- | --------- | --------------------------- |
| `dest`   | `WasmRef` | The destination array       |
| `src`    | `WasmI32` | The source memory region    |
| `length` | `WasmI32` | The number of bytes to copy |

### Memory.**copyRefArrayI8**

```grain
copyRefArrayI8:
  (dest: WasmRef, destIndex: WasmI32, src: WasmRef, srcIndex: WasmI32,
   length: WasmI32) => Void
```

Copies data from a (array i8) to a (array mut i8).

Parameters:

| param       | type      | description                  |
| ----------- | --------- | ---------------------------- |
| `dest`      | `WasmRef` | The destination array        |
| `destIndex` | `WasmI32` | The destination index        |
| `src`       | `WasmRef` | The source array             |
| `srcIndex`  | `WasmI32` | The source index             |
| `length`    | `WasmI32` | The number of values to copy |

### Memory.**copyRefArrayI64**

```grain
copyRefArrayI64:
  (dest: WasmRef, destIndex: WasmI32, src: WasmRef, srcIndex: WasmI32,
   length: WasmI32) => Void
```

Copies data from a (array i64) to a (array mut i64).

Parameters:

| param       | type      | description                  |
| ----------- | --------- | ---------------------------- |
| `dest`      | `WasmRef` | The destination array        |
| `destIndex` | `WasmI32` | The destination index        |
| `src`       | `WasmRef` | The source array             |
| `srcIndex`  | `WasmI32` | The source index             |
| `length`    | `WasmI32` | The number of values to copy |

### Memory.**copyRefArrayAny**

```grain
copyRefArrayAny:
  (dest: WasmRef, destIndex: WasmI32, src: WasmRef, srcIndex: WasmI32,
   length: WasmI32) => Void
```

Copies data from a (array (ref any)) to a (array mut (ref any)).

Parameters:

| param       | type      | description                  |
| ----------- | --------- | ---------------------------- |
| `dest`      | `WasmRef` | The destination array        |
| `destIndex` | `WasmI32` | The destination index        |
| `src`       | `WasmRef` | The source array             |
| `srcIndex`  | `WasmI32` | The source index             |
| `length`    | `WasmI32` | The number of values to copy |

