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

### Memory.**compare**

```grain
compare: (ptr1: WasmI32, ptr2: WasmI32, length: WasmI32) => WasmI32
```

Compares two memory regions.

Parameters:

| param    | type      | description                                 |
| -------- | --------- | ------------------------------------------- |
| `ptr1`   | `WasmI32` | The first memory region                     |
| `ptr2`   | `WasmI32` | The second memory region                    |
| `length` | `WasmI32` | The length of the memory regions to compare |

Returns:

| type      | description                                                                                                                                                             |
| --------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `WasmI32` | `0` if the memory regions are equal, a negative value if the first region is less than the second, and a positive value if the first region is greater than the second. |

### Memory.**copyRefArrayToLinearMemory**

```grain
copyRefArrayToLinearMemory:
  (dest: WasmI32, src: WasmRef.WasmArrayRef.WasmArrayRef, length: WasmI32) =>
   Void
```

Copies data from a (array i8) to linear memory.

Parameters:

| param    | type                                | description                   |
| -------- | ----------------------------------- | ----------------------------- |
| `dest`   | `WasmI32`                           | The destination memory region |
| `src`    | `WasmRef.WasmArrayRef.WasmArrayRef` | The source array              |
| `length` | `WasmI32`                           | The number of bytes to copy   |

### Memory.**copyLinearMemoryToRefArray**

```grain
copyLinearMemoryToRefArray:
  (dest: WasmRef.WasmArrayRef.WasmArrayRef, src: WasmI32, length: WasmI32) =>
   Void
```

Copies data from linear memory to a (array mut i8).

Parameters:

| param    | type                                | description                 |
| -------- | ----------------------------------- | --------------------------- |
| `dest`   | `WasmRef.WasmArrayRef.WasmArrayRef` | The destination array       |
| `src`    | `WasmI32`                           | The source memory region    |
| `length` | `WasmI32`                           | The number of bytes to copy |

