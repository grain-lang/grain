---
title: Memory
---

## Values

Functions and constants included in the Memory module.

### Memory.**malloc**

```grain
malloc: (size: WasmI32) => WasmI32
```

### Memory.**free**

```grain
free: (userPtr: WasmI32) => Void
```

### Memory.**incRef**

```grain
incRef: (userPtr: WasmI32) => WasmI32
```

### Memory.**decRef**

```grain
decRef: (userPtr: WasmI32) => WasmI32
```

### Memory.**copy**

```grain
copy: (dest: WasmI32, src: WasmI32, length: WasmI32) => Void
```

Copies the source memory region to the destination memory region. Regions may overlap.

Parameters:

|param|type|description|
|-----|----|-----------|
|`dest`|`WasmI32`|The destination memory region|
|`src`|`WasmI32`|The source memory region|
|`length`|`WasmI32`|The length of the memory region to copy|

### Memory.**fill**

```grain
fill: (dest: WasmI32, value: WasmI32, length: WasmI32) => Void
```

Fills the given memory region with the given 1-byte value. Values larger than 1 byte will be truncated.

Parameters:

|param|type|description|
|-----|----|-----------|
|`dest`|`WasmI32`|The destination memory region|
|`value`|`WasmI32`|The value to fill the memory region with|
|`length`|`WasmI32`|The length of the memory region to fill|

### Memory.**compare**

```grain
compare: (ptr1: WasmI32, ptr2: WasmI32, length: WasmI32) => WasmI32
```

