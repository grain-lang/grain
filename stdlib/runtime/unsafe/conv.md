---
title: Conv
---

## Values

Functions and constants included in the Conv module.

### Conv.**toInt32**

```grain
toInt32 : (n: WasmI32) => Int32
```

### Conv.**toUint32**

```grain
toUint32 : (n: WasmI32) => Uint32
```

### Conv.**fromInt32**

```grain
fromInt32 : (n: Int32) => WasmI32
```

### Conv.**fromUint32**

```grain
fromUint32 : (n: Uint32) => WasmI32
```

### Conv.**toInt64**

```grain
toInt64 : (n: WasmI64) => Int64
```

### Conv.**toUint64**

```grain
toUint64 : (n: WasmI64) => Uint64
```

### Conv.**fromInt64**

```grain
fromInt64 : (n: Int64) => WasmI64
```

### Conv.**fromUint64**

```grain
fromUint64 : (n: Uint64) => WasmI64
```

### Conv.**toFloat32**

```grain
toFloat32 : (n: WasmF32) => Float32
```

### Conv.**fromFloat32**

```grain
fromFloat32 : (n: Float32) => WasmF32
```

### Conv.**toFloat64**

```grain
toFloat64 : (n: WasmF64) => Float64
```

### Conv.**fromFloat64**

```grain
fromFloat64 : (n: Float64) => WasmF64
```

### Conv.**wasmI32ToNumber**

```grain
wasmI32ToNumber : (n: WasmI32) => Number
```

Converts a WasmI32 value to Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`n`|`WasmI32`|The WasmI32 to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The value converted to either a simple or a 32 bit heap allocated number.|

