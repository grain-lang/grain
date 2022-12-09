---
title: Bytes
---

Utilities for working with byte sequences.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
import Bytes from "bytes"
```

## Values

Functions for working with the Bytes data type.

### Bytes.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
make : Number -> Bytes
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
fromString : String -> Bytes
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
toString : Bytes -> String
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
length : Bytes -> Number
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
copy : Bytes -> Bytes
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
slice : (Number, Number, Bytes) -> Bytes
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
resize : (Number, Number, Bytes) -> Bytes
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
move : (Number, Number, Number, Bytes, Bytes) -> Void
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
concat : (Bytes, Bytes) -> Bytes
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

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
fill : (Int32, Bytes) -> Void
```

Replaces all bytes in a byte sequnce with the new value provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value replacing each byte|
|`bytes`|`Bytes`|The byte sequence to update|

### Bytes.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
clear : Bytes -> Void
```

Replaces all bytes in a byte sequence with zeroes.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The byte sequence to clear|

## Binary operations on integers

Functions for encoding and decoding integers stored in a byte sequence.

### Bytes.**getInt8S**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getInt8S : (Number, Bytes) -> Int32
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
|`Int32`|A 32-bit integer representing a signed 8-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 1` is greater than the bytes size

### Bytes.**getInt8U**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getInt8U : (Number, Bytes) -> Int32
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
|`Int32`|A 32-bit integer representing an unsigned 8-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 1` is greater than the bytes size

### Bytes.**setInt8**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
setInt8 : (Number, Int32, Bytes) -> Void
```

Sets a signed 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int32`|The value to set|
|`bytes`|`Bytes`|The byte sequence to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 1` is greater than the bytes size

### Bytes.**getInt16S**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getInt16S : (Number, Bytes) -> Int32
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
|`Int32`|A 32-bit integer representing a signed 16-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 2` is greater than the bytes size

### Bytes.**getInt16U**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getInt16U : (Number, Bytes) -> Int32
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
|`Int32`|A 32-bit integer representing an unsigned 16-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index + 2` is greater than the bytes size

### Bytes.**setInt16**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
setInt16 : (Number, Int32, Bytes) -> Void
```

Sets a signed 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int32`|The value to set|
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
getInt32 : (Number, Bytes) -> Int32
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
setInt32 : (Number, Int32, Bytes) -> Void
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

### Bytes.**getFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getFloat32 : (Number, Bytes) -> Float32
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
setFloat32 : (Number, Float32, Bytes) -> Void
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
getInt64 : (Number, Bytes) -> Int64
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
setInt64 : (Number, Int64, Bytes) -> Void
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

### Bytes.**getFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
getFloat64 : (Number, Bytes) -> Float64
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
setFloat64 : (Number, Float64, Bytes) -> Void
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

