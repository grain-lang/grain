---
title: Malloc
---

## Values

Functions and constants included in the Malloc module.

### Malloc.**_RESERVED_RUNTIME_SPACE**

```grain
_RESERVED_RUNTIME_SPACE : WasmI32
```

### Malloc.**free**

```grain
free : (ap: WasmI32) => Void
```

Frees the given allocated pointer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`ap`|`WasmI32`|The pointer to free|

### Malloc.**malloc**

```grain
malloc : (nbytes: WasmI32) => WasmI32
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

### Malloc.**leakAll**

```grain
leakAll : () => Void
```

Leaks all memory in all free lists; used for testing.

