### Malloc.**_RESERVED_RUNTIME_SPACE**

```grain
_RESERVED_RUNTIME_SPACE : WasmI32
```

### Malloc.**free**

```grain
free : WasmI32 -> Void
```

Frees the given allocated pointer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`ap`|`WasmI32`|The pointer to free|

### Malloc.**malloc**

```grain
malloc : WasmI32 -> WasmI32
```

Allocates the requested number of bytes, returning a pointer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`nbytes`|`WasmI32`|The number of bytes to allocate|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the allocated region (8-byte aligned) or -1 if the allocation failed|

### Malloc.**getFreePtr**

```grain
getFreePtr : () -> WasmI32
```

Returns the current free list pointer.
Used for debugging.

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The free list pointer|

