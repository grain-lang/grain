---
title: DataStructures
---

## Values

Functions and constants included in the DataStructures module.

### DataStructures.**allocateArray**

```grain
allocateArray : WasmI32 => WasmI32
```

Allocates a new Grain array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`numElts`|`WasmI32`|The number of elements to be contained in this array|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the array|

### DataStructures.**allocateTuple**

```grain
allocateTuple : WasmI32 => WasmI32
```

Allocates a new Grain tuple.

Parameters:

|param|type|description|
|-----|----|-----------|
|`numElts`|`WasmI32`|The number of elements to be contained in this tuple|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the tuple|

### DataStructures.**allocateBytes**

```grain
allocateBytes : WasmI32 => WasmI32
```

Allocates a new Grain bytes.

Parameters:

|param|type|description|
|-----|----|-----------|
|`size`|`WasmI32`|The number of bytes to be contained in this buffer|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the bytes|

### DataStructures.**allocateString**

```grain
allocateString : WasmI32 => WasmI32
```

Allocates a new Grain string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`size`|`WasmI32`|The size (in bytes) of the string to allocate|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the string|

### DataStructures.**allocateInt32**

```grain
allocateInt32 : () => WasmI32
```

Allocates a new Int32.

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the empty Int32|

### DataStructures.**newInt32**

```grain
newInt32 : WasmI32 => WasmI32
```

Allocates a new Int32 with a prepopulated value

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`WasmI32`|The value to store|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the Int32|

### DataStructures.**allocateUint32**

```grain
allocateUint32 : () => WasmI32
```

Allocates a new Uint32.

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the empty Uint32|

### DataStructures.**newUint32**

```grain
newUint32 : WasmI32 => WasmI32
```

Allocates a new Uint32 with a prepopulated value

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`WasmI32`|The value to store|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the Uint32|

### DataStructures.**allocateInt64**

```grain
allocateInt64 : () => WasmI32
```

Allocates a new Int64.

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the empty Int64|

### DataStructures.**newInt64**

```grain
newInt64 : WasmI64 => WasmI32
```

Allocates a new Int64 with a prepopulated value

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`WasmI64`|The value to store|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the Int64|

### DataStructures.**allocateUint64**

```grain
allocateUint64 : () => WasmI32
```

Allocates a new Uint64.

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the empty Uint64|

### DataStructures.**newUint64**

```grain
newUint64 : WasmI64 => WasmI32
```

Allocates a new Uint64 with a prepopulated value

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`WasmI64`|The value to store|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the Uint64|

### DataStructures.**allocateFloat32**

```grain
allocateFloat32 : () => WasmI32
```

Allocates a new Float32.

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the empty Float32|

### DataStructures.**newFloat32**

```grain
newFloat32 : WasmF32 => WasmI32
```

Allocates a new Float32 with a prepopulated value

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`WasmF32`|The value to store|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|the pointer to the Float32|

### DataStructures.**allocateFloat64**

```grain
allocateFloat64 : () => WasmI32
```

Allocates a new Float64.

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the empty Float64|

### DataStructures.**newFloat64**

```grain
newFloat64 : WasmF64 => WasmI32
```

Allocates a new Float64 with a prepopulated value

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`WasmF64`|The value to store|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the Float64|

### DataStructures.**allocateRational**

```grain
allocateRational : () => WasmI32
```

Allocates a new Rational.

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the empty Rational|

### DataStructures.**newRational**

```grain
newRational : (WasmI32, WasmI32) => WasmI32
```

Allocates a new Rational with a prepopulated value

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`WasmI32`|The numerator value to store|
|`value`|`WasmI32`|The denominator value to store|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The pointer to the Rational|

### DataStructures.**loadAdtVariant**

```grain
loadAdtVariant : WasmI32 => WasmI32
```

Load the (tagged) variant of an ADT.

Parameters:

|param|type|description|
|-----|----|-----------|
|`ptr`|`WasmI32`|Untagged pointer to the ADT|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The (tagged) ADT variant id|

### DataStructures.**stringSize**

```grain
stringSize : WasmI32 => WasmI32
```

Load an untagged string's size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`ptr`|`WasmI32`|Untagged pointer to the string|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The string size (in bytes)|

### DataStructures.**bytesSize**

```grain
bytesSize : WasmI32 => WasmI32
```

Load an untagged Bytes' size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`ptr`|`WasmI32`|Untagged pointer to the Bytes|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The Bytes size (in bytes)|

### DataStructures.**tagSimpleNumber**

```grain
tagSimpleNumber : WasmI32 => Number
```

Tag a simple number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`WasmI32`|The number to tag|

Returns:

|type|description|
|----|-----------|
|`Number`|The tagged number|

### DataStructures.**untagSimpleNumber**

```grain
untagSimpleNumber : Number => WasmI32
```

Untag a simple number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`Number`|The number to untag|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The untagged number|

### DataStructures.**tagChar**

```grain
tagChar : WasmI32 => Char
```

Tag a char.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`WasmI32`|The usv to tag|

Returns:

|type|description|
|----|-----------|
|`Char`|The tagged char|

### DataStructures.**untagChar**

```grain
untagChar : Char => WasmI32
```

Untag a char.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`Char`|The char to untag|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The untagged usv|

### DataStructures.**tagInt8**

```grain
tagInt8 : WasmI32 => Int8
```

Tag an int8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`WasmI32`|The int8 to tag|

Returns:

|type|description|
|----|-----------|
|`Int8`|The tagged int8|

### DataStructures.**untagInt8**

```grain
untagInt8 : Int8 => WasmI32
```

Untag an int8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`Int8`|The int8 to untag|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The untagged int8|

### DataStructures.**tagInt16**

```grain
tagInt16 : WasmI32 => Int16
```

Tag an int16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`WasmI32`|The int16 to tag|

Returns:

|type|description|
|----|-----------|
|`Int16`|The tagged int16|

### DataStructures.**untagInt16**

```grain
untagInt16 : Int16 => WasmI32
```

Untag an int16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`Int16`|The int16 to untag|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The untagged int16|

### DataStructures.**tagUint8**

```grain
tagUint8 : WasmI32 => Uint8
```

Tag a uint8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`WasmI32`|The uint8 to tag|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The tagged uint8|

### DataStructures.**untagUint8**

```grain
untagUint8 : Uint8 => WasmI32
```

Untag a uint8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`Uint8`|The uint8 to untag|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The untagged uint8|

### DataStructures.**tagUint16**

```grain
tagUint16 : WasmI32 => Uint16
```

Tag a uint16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`WasmI32`|The uint16 to tag|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The tagged uint16|

### DataStructures.**untagUint16**

```grain
untagUint16 : Uint16 => WasmI32
```

Untag a uint16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`Uint16`|The uint16 to untag|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The untagged uint16|

