---
title: Buffer
---

Utilities for working with buffers.

Buffers are data structures that automatically expand as more data is appended. They are useful for storing and operating on an unknown number of bytes. All set or append operations mutate the buffer.

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
import Buffer from "buffer"
```

## Values

Functions for working with the Buffer data type.

### Buffer.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
make : Number -> Buffer
```

Creates a fresh buffer, initially empty.

The `initialSize` parameter is the initial size of the internal byte sequence that holds the buffer contents.
That byte sequence is automatically reallocated when more than `initialSize` bytes are stored in the buffer, but shrinks back to `initialSize` characters when reset is called.

Parameters:

|param|type|description|
|-----|----|-----------|
|`initialSize`|`Number`|The initial size of the buffer|

Returns:

|type|description|
|----|-----------|
|`Buffer`|The new buffer|

Throws:

`InvalidArgument(String)`

* When the `initialSize` is a negative number

### Buffer.**length**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
length : Buffer -> Number
```

Gets the number of bytes currently contained in a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Number`|The length of the buffer in bytes|

### Buffer.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
clear : Buffer -> Void
```

Clears data in the buffer and sets its length to zero.

This operation does not resize the underlying byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`buffer`|`Buffer`|The buffer to clear|

### Buffer.**reset**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
reset : Buffer -> Void
```

Empty a buffer and deallocate the internal byte sequence holding the buffer contents.

This operation resizes the underlying byte sequence to the initial size of the buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`buffer`|`Buffer`|The buffer to reset|

### Buffer.**truncate**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
truncate : (Number, Buffer) -> Void
```

Shortens a buffer to the given length.

This operation does not resize the underlying byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`length`|`Number`|The number of bytes to truncate the buffer to|
|`buffer`|`Buffer`|The buffer to truncate|

Throws:

`IndexOutOfBounds`

* When the `length` is negative
* When the `length` is greater than the buffer size

### Buffer.**toBytes**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
toBytes : Buffer -> Bytes
```

Returns a copy of the current contents of the buffer as a byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`buffer`|`Buffer`|The buffer to copy into a byte sequence|

Returns:

|type|description|
|----|-----------|
|`Bytes`|A byte sequence made from copied buffer data|

### Buffer.**toBytesSlice**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
toBytesSlice : (Number, Number, Buffer) -> Bytes
```

Returns a slice of the current contents of the buffer as a byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The start index|
|`length`|`Number`|The number of bytes to include after the starting index|
|`buffer`|`Buffer`|The buffer to copy from|

Returns:

|type|description|
|----|-----------|
|`Bytes`|A byte sequence with bytes copied from the buffer|

Throws:

`IndexOutOfBounds`

* When `start` is negative
* When `start` is greater than or equal to the buffer size
* When `start + length` is greater than the buffer size

### Buffer.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
toString : Buffer -> String
```

Returns a copy of the current contents of the buffer as a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`buffer`|`Buffer`|The buffer to stringify|

Returns:

|type|description|
|----|-----------|
|`String`|A string made with data copied from the buffer|

### Buffer.**toStringSlice**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
toStringSlice : (Number, Number, Buffer) -> String
```

Returns a copy of a subset of the current contents of the buffer as a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The start index|
|`length`|`Number`|The number of bytes to include after the starting index|
|`buffer`|`Buffer`|The buffer to copy from|

Returns:

|type|description|
|----|-----------|
|`String`|A string made with a subset of data copied from the buffer|

### Buffer.**addBytes**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addBytes : (Bytes, Buffer) -> Void
```

Appends a byte sequence to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The byte sequence to append|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**addString**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addString : (String, Buffer) -> Void
```

Appends the bytes of a string to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to append|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**addChar**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addChar : (Char, Buffer) -> Void
```

Appends the bytes of a char to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to append to the buffer|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**addStringSlice**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Now takes the end offset instead of length</td></tr>
</tbody>
</table>
</details>

```grain
addStringSlice : (Number, Number, String, Buffer) -> Void
```

Appends the bytes of a subset of a string to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The char offset into the string|
|`end`|`Number`|The end offset into the string|
|`string`|`String`|The string to append|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**addBytesSlice**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addBytesSlice : (Number, Number, Bytes, Buffer) -> Void
```

Appends the bytes of a subset of a byte sequence to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The byte offset into the byte sequence|
|`length`|`Number`|The number of bytes to append|
|`bytes`|`Bytes`|The byte sequence to append|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When the `start` is negative
* When the `start` is greater than or equal to the `bytes` size
* When the `length` is negative
* When the `length` is greater than the `bytes` length minus `start`

### Buffer.**addBuffer**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addBuffer : (Buffer, Buffer) -> Void
```

Appends the bytes of a source buffer to destination buffer.

The source buffer is not mutated by this operation. The destination buffer, however, is mutated.

Parameters:

|param|type|description|
|-----|----|-----------|
|`srcBuffer`|`Buffer`|The buffer to append|
|`dstBuffer`|`Buffer`|The buffer to mutate|

### Buffer.**addBufferSlice**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addBufferSlice : (Number, Number, Buffer, Buffer) -> Void
```

Appends the bytes of a part of a buffer to the end of the buffer

The source buffer is not mutated by this operation. The destination buffer, however, is mutated.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The byte offset into the buffer|
|`length`|`Number`|The number of bytes to append|
|`srcBuffer`|`Buffer`|The buffer to append|
|`dstBuffer`|`Buffer`|The buffer to mutate|

## Binary operations on integers

Functions for encoding and decoding integers stored in a buffer.

### Buffer.**getInt8S**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getInt8S : (Number, Buffer) -> Int32
```

Gets a signed 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Int32`|A 32-bit integer representing a signed 8-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 1` is greater than the buffer size

### Buffer.**getInt8U**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getInt8U : (Number, Buffer) -> Int32
```

Gets an unsigned 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Int32`|A 32-bit integer representing an unsigned 8-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 1` is greater than the buffer size

### Buffer.**setInt8**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setInt8 : (Number, Int32, Buffer) -> Void
```

Sets a signed 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int32`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 1` is greater than the buffer size

### Buffer.**addInt8**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addInt8 : (Int32, Buffer) -> Void
```

Appends a signed 8-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**getInt16S**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getInt16S : (Number, Buffer) -> Int32
```

Gets a signed 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Int32`|A 32-bit integer representing a signed 16-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 2` is greater than the buffer size

### Buffer.**getInt16U**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getInt16U : (Number, Buffer) -> Int32
```

Gets an unsigned 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Int32`|A 32-bit integer representing an unsigned 16-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 2` is greater than the buffer size

### Buffer.**setInt16**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setInt16 : (Number, Int32, Buffer) -> Void
```

Sets a signed 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int32`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 2` is greater than the buffer size

### Buffer.**addInt16**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addInt16 : (Int32, Buffer) -> Void
```

Appends a signed 16-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**getInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getInt32 : (Number, Buffer) -> Int32
```

Gets a signed 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Int32`|A signed 32-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 4` is greater than the buffer size

### Buffer.**setInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setInt32 : (Number, Int32, Buffer) -> Void
```

Sets a signed 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int32`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 4` is greater than the buffer size

### Buffer.**addInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addInt32 : (Int32, Buffer) -> Void
```

Appends a signed 32-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**getFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getFloat32 : (Number, Buffer) -> Float32
```

Gets a 32-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Float32`|A 32-bit float that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 4` is greater than the buffer size

### Buffer.**setFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setFloat32 : (Number, Float32, Buffer) -> Void
```

Sets a 32-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Float32`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 4` is greater than the buffer size

### Buffer.**addFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addFloat32 : (Float32, Buffer) -> Void
```

Appends a 32-bit float to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Float32`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**getInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getInt64 : (Number, Buffer) -> Int64
```

Gets a signed 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Int64`|A signed 64-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 8` is greater than the buffer size

### Buffer.**setInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setInt64 : (Number, Int64, Buffer) -> Void
```

Sets a signed 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int64`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 8` is greater than the buffer size

### Buffer.**addInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addInt64 : (Int64, Buffer) -> Void
```

Appends a signed 64-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

### Buffer.**getFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getFloat64 : (Number, Buffer) -> Float64
```

Gets a 64-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Float64`|A 64-bit float that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 8` is greater than the buffer size

### Buffer.**setFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setFloat64 : (Number, Float64, Buffer) -> Void
```

Sets a 64-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Float64`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 8` is greater than the buffer size

### Buffer.**addFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addFloat64 : (Float64, Buffer) -> Void
```

Appends a 64-bit float to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Float64`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

