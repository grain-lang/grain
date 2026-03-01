---
title: DataStructures
---

## Values

Functions and constants included in the DataStructures module.

### DataStructures.**allocateArray**

```grain
allocateArray: (size: WasmI32, initial: WasmRef) => WasmRef
```

Allocates a new Grain array.

Parameters:

| param     | type      | description                                          |
| --------- | --------- | ---------------------------------------------------- |
| `size`    | `WasmI32` | The number of elements to be contained in this array |
| `initial` | `WasmRef` | The initial value to fill the array with             |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the array |

### DataStructures.**allocateTuple**

```grain
allocateTuple: (size: WasmI32) => WasmRef
```

Allocates a new Grain tuple.

Parameters:

| param  | type      | description                                          |
| ------ | --------- | ---------------------------------------------------- |
| `size` | `WasmI32` | The number of elements to be contained in this tuple |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the tuple |

### DataStructures.**allocateBytes**

```grain
allocateBytes: (size: WasmI32) => WasmRef
```

Allocates a new Grain bytes.

Parameters:

| param  | type      | description                                        |
| ------ | --------- | -------------------------------------------------- |
| `size` | `WasmI32` | The number of bytes to be contained in this buffer |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the bytes |

### DataStructures.**getBytesArrayRef**

```grain
getBytesArrayRef: (ref: WasmRef) => WasmRef
```

Gets the array backing a Grain Bytes.

Parameters:

| param | type      | description   |
| ----- | --------- | ------------- |
| `ref` | `WasmRef` | The Bytes ref |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the array |

### DataStructures.**getBytesSize**

```grain
getBytesSize: (ref: WasmRef) => WasmI32
```

Gets the size of the array backing a Grain Bytes.

Parameters:

| param | type      | description |
| ----- | --------- | ----------- |
| `ref` | `WasmRef` | The Bytes   |

Returns:

| type      | description           |
| --------- | --------------------- |
| `WasmI32` | The size of the Bytes |

### DataStructures.**allocateString**

```grain
allocateString: (size: WasmI32) => WasmRef
```

Allocates a new Grain string.

Parameters:

| param  | type      | description                                   |
| ------ | --------- | --------------------------------------------- |
| `size` | `WasmI32` | The size (in bytes) of the string to allocate |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The reference to the string |

### DataStructures.**getStringArrayRef**

```grain
getStringArrayRef: (ref: WasmRef) => WasmRef
```

Gets the array backing a Grain string.

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The string ref |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the array |

### DataStructures.**getStringSize**

```grain
getStringSize: (ref: WasmRef) => WasmI32
```

Gets the size of the array backing a Grain string.

Parameters:

| param | type      | description |
| ----- | --------- | ----------- |
| `ref` | `WasmRef` | The string  |

Returns:

| type      | description            |
| --------- | ---------------------- |
| `WasmI32` | The size of the string |

### DataStructures.**getCompoundValueArrayRef**

```grain
getCompoundValueArrayRef: (ref: WasmRef) => WasmRef
```

Gets the array backing a compound Grain value.

Parameters:

| param | type      | description   |
| ----- | --------- | ------------- |
| `ref` | `WasmRef` | The value ref |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the array |

### DataStructures.**loadCycleMarker**

```grain
loadCycleMarker: (ref: WasmRef) => WasmI32
```

Gets a compound Grain value's cycle marker.

Parameters:

| param | type      | description   |
| ----- | --------- | ------------- |
| `ref` | `WasmRef` | The value ref |

Returns:

| type      | description      |
| --------- | ---------------- |
| `WasmI32` | The cycle marker |

### DataStructures.**storeCycleMarker**

```grain
storeCycleMarker: (ref: WasmRef, value: WasmI32) => Void
```

Sets a compound Grain value's cycle marker.

Parameters:

| param   | type      | description             |
| ------- | --------- | ----------------------- |
| `ref`   | `WasmRef` | The value ref           |
| `value` | `WasmI32` | The cycle marker to set |

### DataStructures.**loadRecordTypeHash**

```grain
loadRecordTypeHash: (ref: WasmRef) => WasmRef
```

Gets a record's type hash.

Parameters:

| param | type      | description   |
| ----- | --------- | ------------- |
| `ref` | `WasmRef` | The value ref |

Returns:

| type      | description     |
| --------- | --------------- |
| `WasmRef` | : The type hash |

### DataStructures.**loadVariantTypeHash**

```grain
loadVariantTypeHash: (ref: WasmRef) => WasmRef
```

Gets a variant's type hash.

Parameters:

| param | type      | description   |
| ----- | --------- | ------------- |
| `ref` | `WasmRef` | The value ref |

Returns:

| type      | description     |
| --------- | --------------- |
| `WasmRef` | : The type hash |

### DataStructures.**loadRecordTypeId**

```grain
loadRecordTypeId: (ref: WasmRef) => WasmRef
```

Gets a record's type id.

Parameters:

| param | type      | description   |
| ----- | --------- | ------------- |
| `ref` | `WasmRef` | The value ref |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmRef` | : The type id |

### DataStructures.**loadVariantTypeId**

```grain
loadVariantTypeId: (ref: WasmRef) => WasmRef
```

Gets a variant's type id.

Parameters:

| param | type      | description   |
| ----- | --------- | ------------- |
| `ref` | `WasmRef` | The value ref |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmRef` | : The type id |

### DataStructures.**newInt32**

```grain
newInt32: (int: WasmI32) => WasmRef
```

Allocates a new Int32 with a prepopulated value

Parameters:

| param | type      | description        |
| ----- | --------- | ------------------ |
| `int` | `WasmI32` | The value to store |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the Int32 |

### DataStructures.**newUint32**

```grain
newUint32: (int: WasmI32) => WasmRef
```

Allocates a new Uint32 with a prepopulated value

Parameters:

| param | type      | description        |
| ----- | --------- | ------------------ |
| `int` | `WasmI32` | The value to store |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The reference to the Uint32 |

### DataStructures.**newInt64**

```grain
newInt64: (int: WasmI64) => WasmRef
```

Allocates a new Int64 with a prepopulated value

Parameters:

| param | type      | description        |
| ----- | --------- | ------------------ |
| `int` | `WasmI64` | The value to store |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the Int64 |

### DataStructures.**newUint64**

```grain
newUint64: (int: WasmI64) => WasmRef
```

Allocates a new Uint64 with a prepopulated value

Parameters:

| param | type      | description        |
| ----- | --------- | ------------------ |
| `int` | `WasmI64` | The value to store |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The reference to the Uint64 |

### DataStructures.**newFloat32**

```grain
newFloat32: (float: WasmF32) => WasmRef
```

Allocates a new Float32 with a prepopulated value

Parameters:

| param   | type      | description        |
| ------- | --------- | ------------------ |
| `float` | `WasmF32` | The value to store |

Returns:

| type      | description                  |
| --------- | ---------------------------- |
| `WasmRef` | the reference to the Float32 |

### DataStructures.**newFloat64**

```grain
newFloat64: (float: WasmF64) => WasmRef
```

Allocates a new Float64 with a prepopulated value

Parameters:

| param   | type      | description        |
| ------- | --------- | ------------------ |
| `float` | `WasmF64` | The value to store |

Returns:

| type      | description                  |
| --------- | ---------------------------- |
| `WasmRef` | The reference to the Float64 |

### DataStructures.**newRational**

```grain
newRational: (numerator: WasmRef, denominator: WasmRef) => WasmRef
```

Allocates a new Rational with a prepopulated value

Parameters:

| param         | type      | description                    |
| ------------- | --------- | ------------------------------ |
| `numerator`   | `WasmRef` | The numerator value to store   |
| `denominator` | `WasmRef` | The denominator value to store |

Returns:

| type      | description                   |
| --------- | ----------------------------- |
| `WasmRef` | The reference to the Rational |

### DataStructures.**allocateBigInt**

```grain
allocateBigInt: (size: WasmI32) => WasmRef
```

Allocates a new BigInt with the specified number of limbs

Parameters:

| param  | type      | description         |
| ------ | --------- | ------------------- |
| `size` | `WasmI32` | The number of limbs |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The reference to the BigInt |

### DataStructures.**getBigIntSize**

```grain
getBigIntSize: (ref: WasmRef) => WasmI32
```

Gets number of limbs of a BigInt.

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The BigInt ref |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmI32` | The number of BigInt limbs |

### DataStructures.**getBigIntFlags**

```grain
getBigIntFlags: (ref: WasmRef) => WasmI32
```

Gets the BigInt flags.

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The BigInt ref |

Returns:

| type      | description      |
| --------- | ---------------- |
| `WasmI32` | The BigInt flags |

### DataStructures.**setBigIntFlags**

```grain
setBigIntFlags: (ref: WasmRef, flags: WasmI32) => Void
```

Sets the BigInt flags.

Parameters:

| param   | type      | description             |
| ------- | --------- | ----------------------- |
| `ref`   | `WasmRef` | The BigInt ref          |
| `flags` | `WasmI32` | The BigInt flags to set |

### DataStructures.**getBigIntArrayRef**

```grain
getBigIntArrayRef: (ref: WasmRef) => WasmRef
```

Gets the array backing a BigInt.

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The BigInt ref |

Returns:

| type      | description                       |
| --------- | --------------------------------- |
| `WasmRef` | The reference to the BigInt limbs |

### DataStructures.**getNumberTag**

```grain
getNumberTag: (ref: WasmRef) => WasmI32
```

Gets the tag of a Number.

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description          |
| --------- | -------------------- |
| `WasmI32` | The boxed Number tag |

### DataStructures.**getInt32Value**

```grain
getInt32Value: (ref: WasmRef) => WasmI32
```

Gets the value of a boxed Int32

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmI32` | The inner i32 |

### DataStructures.**getUint32Value**

```grain
getUint32Value: (ref: WasmRef) => WasmI32
```

Gets the value of a boxed Uint32

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmI32` | The inner i32 |

### DataStructures.**getFloat32Value**

```grain
getFloat32Value: (ref: WasmRef) => WasmF32
```

Gets the value of a boxed Float32

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmF32` | The inner f32 |

### DataStructures.**getInt64Value**

```grain
getInt64Value: (ref: WasmRef) => WasmI64
```

Gets the value of a boxed Int64

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmI64` | The inner i64 |

### DataStructures.**getUint64Value**

```grain
getUint64Value: (ref: WasmRef) => WasmI64
```

Gets the value of a boxed Uint64

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmI64` | The inner i64 |

### DataStructures.**getFloat64Value**

```grain
getFloat64Value: (ref: WasmRef) => WasmF64
```

Gets the value of a boxed Float64

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmF64` | The inner f64 |

### DataStructures.**getRationalNumerator**

```grain
getRationalNumerator: (ref: WasmRef) => WasmRef
```

Gets the numerator of a boxed Rational

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description            |
| --------- | ---------------------- |
| `WasmRef` | The rational numerator |

### DataStructures.**getRationalDenominator**

```grain
getRationalDenominator: (ref: WasmRef) => WasmRef
```

Gets the denominator of a boxed Rational

Parameters:

| param | type      | description    |
| ----- | --------- | -------------- |
| `ref` | `WasmRef` | The Number ref |

Returns:

| type      | description              |
| --------- | ------------------------ |
| `WasmRef` | The rational denominator |

### DataStructures.**loadAdtVariant**

```grain
loadAdtVariant: (ref: WasmRef) => WasmRef
```

Load the (tagged) variant of an ADT.

Parameters:

| param | type      | description              |
| ----- | --------- | ------------------------ |
| `ref` | `WasmRef` | The reference to the ADT |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The (tagged) ADT variant id |

### DataStructures.**loadValueTag**

```grain
loadValueTag: (ref: WasmRef) => WasmI32
```

Load Grain heap value's tag.

Parameters:

| param | type      | description                |
| ----- | --------- | -------------------------- |
| `ref` | `WasmRef` | The reference to the value |

Returns:

| type      | description   |
| --------- | ------------- |
| `WasmI32` | The value tag |

### DataStructures.**stringSize**

```grain
stringSize: (ref: WasmRef) => WasmI32
```

Load an untagged string's size.

Parameters:

| param | type      | description                 |
| ----- | --------- | --------------------------- |
| `ref` | `WasmRef` | The reference to the string |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmI32` | The string size (in bytes) |

### DataStructures.**bytesSize**

```grain
bytesSize: (ref: WasmRef) => WasmI32
```

Load an untagged Bytes' size.

Parameters:

| param | type      | description                |
| ----- | --------- | -------------------------- |
| `ref` | `WasmRef` | The reference to the Bytes |

Returns:

| type      | description               |
| --------- | ------------------------- |
| `WasmI32` | The Bytes size (in bytes) |

### DataStructures.**tagSimpleNumber**

```grain
tagSimpleNumber: (num: WasmI32) => Number
```

Tag a simple number.

Parameters:

| param | type      | description       |
| ----- | --------- | ----------------- |
| `num` | `WasmI32` | The number to tag |

Returns:

| type     | description       |
| -------- | ----------------- |
| `Number` | The tagged number |

### DataStructures.**untagSimpleNumber**

```grain
untagSimpleNumber: (num: Number) => WasmI32
```

Untag a simple number.

Parameters:

| param | type     | description         |
| ----- | -------- | ------------------- |
| `num` | `Number` | The number to untag |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmI32` | The untagged number |

### DataStructures.**tagChar**

```grain
tagChar: (char: WasmI32) => Char
```

Tag a char.

Parameters:

| param  | type      | description    |
| ------ | --------- | -------------- |
| `char` | `WasmI32` | The usv to tag |

Returns:

| type   | description     |
| ------ | --------------- |
| `Char` | The tagged char |

### DataStructures.**untagChar**

```grain
untagChar: (char: Char) => WasmI32
```

Untag a char.

Parameters:

| param  | type   | description       |
| ------ | ------ | ----------------- |
| `char` | `Char` | The char to untag |

Returns:

| type      | description      |
| --------- | ---------------- |
| `WasmI32` | The untagged usv |

### DataStructures.**tagInt8**

```grain
tagInt8: (int: WasmI32) => Int8
```

Tag an int8.

Parameters:

| param | type      | description     |
| ----- | --------- | --------------- |
| `int` | `WasmI32` | The int8 to tag |

Returns:

| type   | description     |
| ------ | --------------- |
| `Int8` | The tagged int8 |

### DataStructures.**untagInt8**

```grain
untagInt8: (int: Int8) => WasmI32
```

Untag an int8.

Parameters:

| param | type   | description       |
| ----- | ------ | ----------------- |
| `int` | `Int8` | The int8 to untag |

Returns:

| type      | description       |
| --------- | ----------------- |
| `WasmI32` | The untagged int8 |

### DataStructures.**tagInt16**

```grain
tagInt16: (int: WasmI32) => Int16
```

Tag an int16.

Parameters:

| param | type      | description      |
| ----- | --------- | ---------------- |
| `int` | `WasmI32` | The int16 to tag |

Returns:

| type    | description      |
| ------- | ---------------- |
| `Int16` | The tagged int16 |

### DataStructures.**untagInt16**

```grain
untagInt16: (int: Int16) => WasmI32
```

Untag an int16.

Parameters:

| param | type    | description        |
| ----- | ------- | ------------------ |
| `int` | `Int16` | The int16 to untag |

Returns:

| type      | description        |
| --------- | ------------------ |
| `WasmI32` | The untagged int16 |

### DataStructures.**tagUint8**

```grain
tagUint8: (int: WasmI32) => Uint8
```

Tag a uint8.

Parameters:

| param | type      | description      |
| ----- | --------- | ---------------- |
| `int` | `WasmI32` | The uint8 to tag |

Returns:

| type    | description      |
| ------- | ---------------- |
| `Uint8` | The tagged uint8 |

### DataStructures.**untagUint8**

```grain
untagUint8: (int: Uint8) => WasmI32
```

Untag a uint8.

Parameters:

| param | type    | description        |
| ----- | ------- | ------------------ |
| `int` | `Uint8` | The uint8 to untag |

Returns:

| type      | description        |
| --------- | ------------------ |
| `WasmI32` | The untagged uint8 |

### DataStructures.**tagUint16**

```grain
tagUint16: (int: WasmI32) => Uint16
```

Tag a uint16.

Parameters:

| param | type      | description       |
| ----- | --------- | ----------------- |
| `int` | `WasmI32` | The uint16 to tag |

Returns:

| type     | description       |
| -------- | ----------------- |
| `Uint16` | The tagged uint16 |

### DataStructures.**untagUint16**

```grain
untagUint16: (int: Uint16) => WasmI32
```

Untag a uint16.

Parameters:

| param | type     | description         |
| ----- | -------- | ------------------- |
| `int` | `Uint16` | The uint16 to untag |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmI32` | The untagged uint16 |

