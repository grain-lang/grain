---
title: Vector
---

A low level resizeable array implementation.

## Types

Type declarations included in the Vector module.

### Vector.**Vector**

```grain
type Vector<a>
```

A resizeable array.

## Values

Functions and constants included in the Vector module.

### Vector.**make**

```grain
make: () => Vector<a>
```

Constructs a new Vector.

### Vector.**length**

```grain
length: (vector: Vector<a>) => WasmI32
```

Gets the length of the Vector.

Parameters:

| param    | type        | description           |
| -------- | ----------- | --------------------- |
| `vector` | `Vector<a>` | The Vector to inspect |

Returns:

| type      | description              |
| --------- | ------------------------ |
| `WasmI32` | The length of the Vector |

### Vector.**push**

```grain
push: (vector: Vector<a>, value: a) => Void
```

Appends a value to the end of the Vec, resizing if necessary.

Parameters:

| param    | type        | description                       |
| -------- | ----------- | --------------------------------- |
| `vector` | `Vector<a>` | The Vector to append to           |
| `value`  | `a`         | The value to append to the Vector |

### Vector.**pop**

```grain
pop: (vector: Vector<a>) => a
```

Removes the last element from the vector.

This function does not do a bounds check, it is the callers responsibility
to ensure the vector is not empty before calling this function.

Parameters:

| param    | type        | description            |
| -------- | ----------- | ---------------------- |
| `vector` | `Vector<a>` | The Vector to pop from |

Returns:

| type | description                                |
| ---- | ------------------------------------------ |
| `a`  | The value that was removed from the vector |

### Vector.**findIndex**

```grain
findIndex: (vector: Vector<WasmRef>, value: WasmRef) => WasmI32
```

Finds the index of a reference in the vector, returning -1 if not found.

Parameters:

| param    | type              | description             |
| -------- | ----------------- | ----------------------- |
| `vector` | `Vector<WasmRef>` | The Vector to search    |
| `value`  | `WasmRef`         | The value to search for |

Returns:

| type      | description                                      |
| --------- | ------------------------------------------------ |
| `WasmI32` | The index of the vector if found, otherwise `-1` |

