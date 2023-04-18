---
title: Bytes
---

Utilities for working with byte sequences.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
include "bytes"
```

## Values

Functions and constants included in the Bytes module.

### Bytes.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
make : (size: Number) -> Bytes
```

Creates a new byte sequence of the input size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`size`|`Number`|The number of bytes to store|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The new byte sequence|

### Bytes.**empty**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
empty : Bytes
```

An empty byte sequence.

### Bytes.**fromString**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
fromString : (string: String) -> Bytes
```

Creates a new byte sequence from the input string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to copy into a byte sequence|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The new byte sequence|

### Bytes.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
toString : (bytes: Bytes) -> String
```

Creates a new string from the input bytes.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The source byte sequence|

Returns:

|type|description|
|----|-----------|
|`String`|The string representation of the bytes|

### Bytes.**length**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
length : (bytes: Bytes) -> Number
```

Returns the length of a byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The byte sequence to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of bytes|

### Bytes.**copy**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
copy : (b: Bytes) -> Bytes
```

Creates a new byte sequence that contains the same bytes as the input byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The byte sequence to copy|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The new byte sequence|

### Bytes.**slice**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
slice : (start: Number, length: Number, bytes: Bytes) -> Bytes
```

Returns a copy of a subset of the input byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The start index|
|`length`|`Number`|The number of bytes to include after the starting index|
|`bytes`|`Bytes`|The byte sequence to copy from|

Returns:

|type|description|
|----|-----------|
|`Bytes`|A byte sequence with of the copied bytes|

Throws:

`InvalidArgument(String)`

* When `start + length` is greater than the bytes size

### Bytes.**resize**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
resize : (left: Number, right: Number, bytes: Bytes) -> Bytes
```

Returns a copy of a byte sequence with bytes added or removed from the beginning and/or end.

A positive number represents bytes to add, while a negative number represents bytes to remove.

Parameters:

|param|type|description|
|-----|----|-----------|
|`left`|`Number`|The number of uninitialized bytes to prepend|
|`right`|`Number`|The number of uninitialized bytes to append|
|`bytes`|`Bytes`|The byte sequence get a subset of bytes from|

Returns:

|type|description|
|----|-----------|
|`Bytes`|A resized byte sequence|

Throws:

`InvalidArgument(String)`

* When the new size is negative

### Bytes.**move**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
move :
  (srcIndex: Number, dstIndex: Number, length: Number, src: Bytes, dst: Bytes) ->
   Void
```

Copies a range of bytes from a source byte sequence to a given location
in a destination byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`srcIndex`|`Number`|The starting index to copy bytes from|
|`dstIndex`|`Number`|The starting index to copy bytes into|
|`length`|`Number`|The amount of bytes to copy from the source buffer|
|`src`|`Bytes`|The source byte sequence|
|`dst`|`Bytes`|The destination byte sequence|

Throws:

`InvalidArgument(String)`

* When `srcIndex + length` is greater than the `src` bytes size
* When the `dstIndex + length` is greater than the `dst` bytes size

### Bytes.**concat**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
concat : (bytes1: Bytes, bytes2: Bytes) -> Bytes
```

Creates a new byte sequence that contains the bytes of both byte sequences.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes1`|`Bytes`|The beginning byte sequence|
|`bytes2`|`Bytes`|The ending byte sequence|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The new byte sequence|

### Bytes.**fill**

<details>
<summary>Added in <code>0.3.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>next</code></td><td>`value` argument type changed to `Uint8`</td></tr>
</tbody>
</table>
</details>

```grain
fill : (value: Uint8, bytes: Bytes) -> Void
```

Replaces all bytes in a byte sequnce with the new value provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint8`|The value replacing each byte|
|`bytes`|`Bytes`|The byte sequence to update|

### Bytes.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
clear : (bytes: Bytes) -> Void
```

Replaces all bytes in a byte sequence with zeroes.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The byte sequence to clear|

### Bytes.**getInt8**

<details>
<summary>Added in <code>next</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.2</code></td><td>Originally called `getInt8S`, returning an `Int32`</td></tr>
</tbody>
</table>
</details>

```grain
getInt8 : (index: Number, bytes: Bytes) -> Int8
```

Gets a signed 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Int8`|A signed 8-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 1` is greater than the bytes size

### Bytes.**setInt8**

<details>
<summary>Added in <code>0.3.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>next</code></td><td>`value` argument type changed to `Int8`</td></tr>
</tbody>
</table>
</details>

```grain
setInt8 : (index: Number, value: Int8, bytes: Bytes) -> Void
```

Sets a signed 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int8`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 1` is greater than the bytes size

### Bytes.**getUint8**

<details>
<summary>Added in <code>next</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.2</code></td><td>Originally called `getInt8U`, returning an `Int32`</td></tr>
</tbody>
</table>
</details>

```grain
getUint8 : (index: Number, bytes: Bytes) -> Uint8
```

Gets an unsigned 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Uint8`|An unsigned 8-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 1` is greater than the bytes size

### Bytes.**setUint8**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
setUint8 : (index: Number, value: Uint8, bytes: Bytes) -> Void
```

Sets an unsigned 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Uint8`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 1` is greater than the bytes size

### Bytes.**getInt16**

<details>
<summary>Added in <code>next</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.2</code></td><td>Originally called `getInt16S`, returning an `Int32`</td></tr>
</tbody>
</table>
</details>

```grain
getInt16 : (index: Number, bytes: Bytes) -> Int16
```

Gets a signed 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Int16`|A signed 16-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 2` is greater than the bytes size

### Bytes.**setInt16**

<details>
<summary>Added in <code>0.3.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>next</code></td><td>`value` argument type changed to `Int16`</td></tr>
</tbody>
</table>
</details>

```grain
setInt16 : (index: Number, value: Int16, bytes: Bytes) -> Void
```

Sets a signed 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int16`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 2` is greater than the bytes size

### Bytes.**getUint16**

<details>
<summary>Added in <code>next</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.2</code></td><td>Originally called `getInt16U`, returning an `Int32`</td></tr>
</tbody>
</table>
</details>

```grain
getUint16 : (index: Number, bytes: Bytes) -> Uint16
```

Gets an unsigned 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Uint16`|An unsigned 16-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 2` is greater than the bytes size

### Bytes.**setUint16**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
setUint16 : (index: Number, value: Uint16, bytes: Bytes) -> Void
```

Sets an unsigned 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Uint16`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 2` is greater than the bytes size

### Bytes.**getInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getInt32 : (index: Number, bytes: Bytes) -> Int32
```

Gets a signed 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Int32`|A signed 32-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 4` is greater than the bytes size

### Bytes.**setInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
setInt32 : (index: Number, value: Int32, bytes: Bytes) -> Void
```

Sets a signed 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int32`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 4` is greater than the bytes size

### Bytes.**getUint32**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
getUint32 : (index: Number, bytes: Bytes) -> Uint32
```

Gets an unsigned 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Uint32`|An unsigned 32-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 4` is greater than the bytes size

### Bytes.**setUint32**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
setUint32 : (index: Number, value: Uint32, bytes: Bytes) -> Void
```

Sets an unsigned 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Uint32`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 4` is greater than the bytes size

### Bytes.**getFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getFloat32 : (index: Number, bytes: Bytes) -> Float32
```

Gets a 32-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Float32`|A 32-bit float that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 4` is greater than the bytes size

### Bytes.**setFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
setFloat32 : (index: Number, value: Float32, bytes: Bytes) -> Void
```

Sets a 32-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Float32`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 4` is greater than the bytes size

### Bytes.**getInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getInt64 : (index: Number, bytes: Bytes) -> Int64
```

Gets a signed 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Int64`|A signed 64-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 8` is greater than the bytes size

### Bytes.**setInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
setInt64 : (index: Number, value: Int64, bytes: Bytes) -> Void
```

Sets a signed 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int64`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 8` is greater than the bytes size

### Bytes.**getUint64**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
getUint64 : (index: Number, bytes: Bytes) -> Uint64
```

Gets an unsigned 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Uint64`|An unsigned 64-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 8` is greater than the bytes size

### Bytes.**setUint64**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
setUint64 : (index: Number, value: Uint64, bytes: Bytes) -> Void
```

Sets an unsigned 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Uint64`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 8` is greater than the bytes size

### Bytes.**getFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getFloat64 : (index: Number, bytes: Bytes) -> Float64
```

Gets a 64-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`bytes`|`Bytes`|The byte sequence to access|

Returns:

|type|description|
|----|-----------|
|`Float64`|A 64-bit float that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 8` is greater than the bytes size

### Bytes.**setFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
setFloat64 : (index: Number, value: Float64, bytes: Bytes) -> Void
```

Sets a 64-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Float64`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 8` is greater than the bytes size

