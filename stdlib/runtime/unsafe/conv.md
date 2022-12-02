### Conv.**toInt32**

```grain
toInt32 : WasmI32 -> Int32
```

### Conv.**fromInt32**

```grain
fromInt32 : Int32 -> WasmI32
```

### Conv.**toInt64**

```grain
toInt64 : WasmI64 -> Int64
```

### Conv.**fromInt64**

```grain
fromInt64 : Int64 -> WasmI64
```

### Conv.**toFloat32**

```grain
toFloat32 : WasmF32 -> Float32
```

### Conv.**fromFloat32**

```grain
fromFloat32 : Float32 -> WasmF32
```

### Conv.**toFloat64**

```grain
toFloat64 : WasmF64 -> Float64
```

### Conv.**fromFloat64**

```grain
fromFloat64 : Float64 -> WasmF64
```

### Conv.**wasmI32ToNumber**

```grain
wasmI32ToNumber : WasmI32 -> Number
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

