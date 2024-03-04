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
from "buffer" include Buffer
```

```grain
Buffer.make(64)
```

## Types

Type declarations included in the Buffer module.

### Buffer.**Buffer**

```grain
type Buffer
```

## Values

Functions and constants included in the Buffer module.

### Buffer.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
make : (initialSize: Number) => Buffer
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

Examples:

```grain
Buffer.make(0)
```

```grain
Buffer.make(64)
```

### Buffer.**length**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
length : (buffer: Buffer) => Number
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

Examples:

```grain
Buffer.length(Buffer.make(32)) == 0
```

```grain
let buf = Buffer.make(32)
Buffer.addInt32(1l, buf)
assert Buffer.length(buf) == 4
```

### Buffer.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
clear : (buffer: Buffer) => Void
```

Clears data in the buffer and sets its length to zero.

This operation does not resize the underlying byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`buffer`|`Buffer`|The buffer to clear|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addInt32(1l, buf)
assert Buffer.length(buf) == 4
Buffer.clear(buf)
assert Buffer.length(buf) == 0
```

### Buffer.**reset**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
reset : (buffer: Buffer) => Void
```

Empty a buffer and deallocate the internal byte sequence holding the buffer contents.

This operation resizes the underlying byte sequence to the initial size of the buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`buffer`|`Buffer`|The buffer to reset|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addInt32(1l, buf)
assert Buffer.length(buf) == 4
Buffer.reset(buf)
assert Buffer.length(buf) == 0
```

### Buffer.**truncate**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
truncate : (length: Number, buffer: Buffer) => Void
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

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addInt32(1l, buf)
assert Buffer.length(buf) == 4
Buffer.truncate(1, buf)
assert Buffer.length(buf) == 1
```

### Buffer.**toBytes**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
toBytes : (buffer: Buffer) => Bytes
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

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addString("test", buf)
assert Buffer.toBytes(buf) == b"test"
```

### Buffer.**toBytesSlice**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
toBytesSlice : (start: Number, length: Number, buffer: Buffer) => Bytes
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

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addString("HelloWorld", buf)
assert Buffer.toBytesSlice(0, 5, buf) == b"Hello"
```

### Buffer.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
toString : (buffer: Buffer) => String
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

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addString("HelloWorld", buf)
assert Buffer.toString(buf) == "HelloWorld"
```

### Buffer.**toStringSlice**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
toStringSlice : (start: Number, length: Number, buffer: Buffer) => String
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

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addString("HelloWorld", buf)
assert Buffer.toStringSlice(0, 5, buf) == "Hello"
```

### Buffer.**addBytes**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addBytes : (bytes: Bytes, buffer: Buffer) => Void
```

Appends a byte sequence to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The byte sequence to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addBytes(b"test", buf)
assert Buffer.toBytes(buf) == b"test"
```

### Buffer.**addString**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addString : (string: String, buffer: Buffer) => Void
```

Appends the bytes of a string to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addString("Hello", buf)
assert Buffer.toString(buf) == "Hello"
```

### Buffer.**addChar**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addChar : (char: Char, buffer: Buffer) => Void
```

Appends the bytes of a character to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to append to the buffer|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addChar('H', buf)
assert Buffer.toString(buf) == "H"
```

### Buffer.**addCharFromCodePoint**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
addCharFromCodePoint : (codePoint: Number, buffer: Buffer) => Void
```

Appends a character represented by a code point to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`codePoint`|`Number`|The code point to append to the buffer|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addCharFromCodePoint(72, buf)
assert Buffer.toString(buf) == "H"
```

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
addStringSlice :
  (start: Number, end: Number, string: String, buffer: Buffer) => Void
```

Appends the bytes of a subset of a string to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The char offset into the string|
|`end`|`Number`|The end offset into the string|
|`string`|`String`|The string to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addStringSlice(0, 5, "HelloWorld", buf)
assert Buffer.toString(buf) == "Hello"
```

### Buffer.**addBytesSlice**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addBytesSlice :
  (start: Number, length: Number, bytes: Bytes, buffer: Buffer) => Void
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

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addBytesSlice(0, 5, b"HelloWorld", buf)
assert Buffer.toString(buf) == "Hello"
```

### Buffer.**addBuffer**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addBuffer : (srcBuffer: Buffer, dstBuffer: Buffer) => Void
```

Appends the bytes of a source buffer to destination buffer.

The source buffer is not mutated by this operation. The destination buffer, however, is mutated.

Parameters:

|param|type|description|
|-----|----|-----------|
|`srcBuffer`|`Buffer`|The buffer to append|
|`dstBuffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf1 = Buffer.make(0)
Buffer.addString("Hello", buf1)
let buf2 = Buffer.make(0)
Buffer.addString("World", buf2)
Buffer.addBuffer(buf2, buf1)
assert Buffer.toString(buf1) == "HelloWorld"
```

### Buffer.**addBufferSlice**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addBufferSlice :
  (start: Number, length: Number, srcBuffer: Buffer, dstBuffer: Buffer) =>
   Void
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

Examples:

```grain
let buf1 = Buffer.make(0)
Buffer.addString("Hello", buf1)
let buf2 = Buffer.make(0)
Buffer.addString("HiWorld", buf2)
Buffer.addBufferSlice(2, 5, buf2, buf1)
assert Buffer.toString(buf1) == "HelloWorld"
```

### Buffer.**getInt8**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally called `getInt8S`, returning an `Int32`</td></tr>
</tbody>
</table>
</details>

```grain
getInt8 : (index: Number, buffer: Buffer) => Int8
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
|`Int8`|A signed 8-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 1` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addInt8(1s, buf)
assert Buffer.getInt8(0, buf) == 1s
```

### Buffer.**setInt8**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>`value` argument type changed to `Int8`</td></tr>
</tbody>
</table>
</details>

```grain
setInt8 : (index: Number, value: Int8, buffer: Buffer) => Void
```

Sets a signed 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int8`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 1` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setInt8(0, 3s, buf)
assert Buffer.getInt8(0, buf) == 3s
```

### Buffer.**addInt8**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>`value` argument type changed to `Int8`</td></tr>
</tbody>
</table>
</details>

```grain
addInt8 : (value: Int8, buffer: Buffer) => Void
```

Appends a signed 8-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int8`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addInt8(2s, buf)
assert Buffer.getInt8(0, buf) == 2s
```

### Buffer.**getUint8**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally called `getInt8U`, returning an `Int32`</td></tr>
</tbody>
</table>
</details>

```grain
getUint8 : (index: Number, buffer: Buffer) => Uint8
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
|`Uint8`|An unsigned 8-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 1` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addUint8(3us, buf)
assert Buffer.getUint8(0, buf) == 3us
```

### Buffer.**setUint8**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
setUint8 : (index: Number, value: Uint8, buffer: Buffer) => Void
```

Sets an unsigned 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Uint8`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 1` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setUint8(4us, buf)
assert Buffer.getUint8(0, buf) == 4us
```

### Buffer.**addUint8**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
addUint8 : (value: Uint8, buffer: Buffer) => Void
```

Appends an unsigned 8-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint8`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addUint8(0us, buf)
assert Buffer.getUint8(0, buf) == 0us
```

### Buffer.**getInt16**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally called `getInt16S`, returning an `Int32`</td></tr>
</tbody>
</table>
</details>

```grain
getInt16 : (index: Number, buffer: Buffer) => Int16
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
|`Int16`|A signed 16-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 2` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addInt16(1S, buf)
assert Buffer.getInt16(0, buf) == 1S
```

### Buffer.**setInt16**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>`value` argument type changed to `Int16`</td></tr>
</tbody>
</table>
</details>

```grain
setInt16 : (index: Number, value: Int16, buffer: Buffer) => Void
```

Sets a signed 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Int16`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 2` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setInt16(5, 1S, buf)
assert Buffer.getInt16(5, buf) == 1S
```

### Buffer.**addInt16**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>`value` argument type changed to `Int16`</td></tr>
</tbody>
</table>
</details>

```grain
addInt16 : (value: Int16, buffer: Buffer) => Void
```

Appends a signed 16-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int16`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addInt16(2S, buf)
assert Buffer.getInt16(0, buf) == 2S
```

### Buffer.**getUint16**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally called `getInt16U`, returning an `Int32`</td></tr>
</tbody>
</table>
</details>

```grain
getUint16 : (index: Number, buffer: Buffer) => Uint16
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
|`Uint16`|An unsigned 16-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 2` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addUint16(1uS, buf)
assert Buffer.getUint16(0, buf) == 1uS
```

### Buffer.**setUint16**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
setUint16 : (index: Number, value: Uint16, buffer: Buffer) => Void
```

Sets an unsigned 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Uint16`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 2` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setUint16(0, 1uS, buf)
assert Buffer.getUint16(0, buf) == 1uS
```

### Buffer.**addUint16**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
addUint16 : (value: Uint16, buffer: Buffer) => Void
```

Appends an unsigned 16-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint16`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addUint16(0, 2uS, buf)
assert Buffer.getUint16(0, buf) == 2uS
```

### Buffer.**getInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getInt32 : (index: Number, buffer: Buffer) => Int32
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

Examples:

```grain
let buf = Buffer.make(0)
Buffer.addInt32(1l, buf)
assert Buffer.getInt32(0, buf) == 1l
```

### Buffer.**setInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setInt32 : (index: Number, value: Int32, buffer: Buffer) => Void
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

Examples:

```grain
let buf = Buffer.make(64)
Buffer.addString("Hello World", buf)
Buffer.setInt32(3, 1l, buf)
assert Buffer.getInt32(3, buf) == 1l
```

### Buffer.**addInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addInt32 : (value: Int32, buffer: Buffer) => Void
```

Appends a signed 32-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(64)
Buffer.addInt32(1l, buf)
assert Buffer.getInt32(0, buf) == 1l
```

### Buffer.**getUint32**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
getUint32 : (index: Number, buffer: Buffer) => Uint32
```

Gets an unsigned 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Uint32`|An unsigned 32-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 4` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addUint32(1ul, buf)
assert Buffer.getUint32(0, buf) == 1ul
```

### Buffer.**setUint32**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
setUint32 : (index: Number, value: Uint32, buffer: Buffer) => Void
```

Sets an unsigned 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Uint32`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 4` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setUint32(0, 1ul, buf)
assert Buffer.getUint32(0, buf) == 1ul
```

### Buffer.**addUint32**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
addUint32 : (value: Uint32, buffer: Buffer) => Void
```

Appends an unsigned 32-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addUint32(1ul, buf)
assert Buffer.getUint32(0, buf) == 1ul
```

### Buffer.**getFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getFloat32 : (index: Number, buffer: Buffer) => Float32
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

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addFloat32(1.0f, buf)
assert Buffer.getFloat32(0, buf) == 1.0f
```

### Buffer.**setFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setFloat32 : (index: Number, value: Float32, buffer: Buffer) => Void
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

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setFloat32(0, 1.0f, buf)
assert Buffer.getFloat32(0, buf) == 1.0f
```

### Buffer.**addFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addFloat32 : (value: Float32, buffer: Buffer) => Void
```

Appends a 32-bit float to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Float32`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addFloat32(1.0f, buf)
assert Buffer.getFloat32(0, buf) == 1.0f
```

### Buffer.**getInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getInt64 : (index: Number, buffer: Buffer) => Int64
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

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addInt64(1L, buf)
assert Buffer.getInt64(0, buf) == 1L
```

### Buffer.**setInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setInt64 : (index: Number, value: Int64, buffer: Buffer) => Void
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

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setInt64(0, 1L, buf)
assert Buffer.getInt64(0, buf) == 1L
```

### Buffer.**addInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addInt64 : (value: Int64, buffer: Buffer) => Void
```

Appends a signed 64-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addInt64(1L, buf)
assert Buffer.getInt64(0, buf) == 1L
```

### Buffer.**getUint64**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
getUint64 : (index: Number, buffer: Buffer) => Uint64
```

Gets an unsigned 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to access|
|`buffer`|`Buffer`|The buffer to access|

Returns:

|type|description|
|----|-----------|
|`Uint64`|An unsigned 64-bit integer that starts at the given index|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 8` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addUint64(1uL, buf)
assert Buffer.getUint64(0, buf) == 1uL
```

### Buffer.**setUint64**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
setUint64 : (index: Number, value: Uint64, buffer: Buffer) => Void
```

Sets an unsigned 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The byte index to update|
|`value`|`Uint64`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Throws:

`IndexOutOfBounds`

* When `index` is negative
* When `index` is greater than or equal to the buffer size
* When `index + 8` is greater than the buffer size

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setUint64(0, 1uL, buf)
assert Buffer.getUint64(0, buf) == 1uL
```

### Buffer.**addUint64**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
addUint64 : (value: Uint64, buffer: Buffer) => Void
```

Appends an unsigned 64-bit integer to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to set|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addUint64(1uL, buf)
assert Buffer.getUint64(0, buf) == 1uL
```

### Buffer.**getFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
getFloat64 : (index: Number, buffer: Buffer) => Float64
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

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addFloat64(1.0F, buf)
assert Buffer.getFloat64(0, buf) == 1.0F
```

### Buffer.**setFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
setFloat64 : (index: Number, value: Float64, buffer: Buffer) => Void
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

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addString("Hello World", buf)
Buffer.setFloat64(0, 1.0F, buf)
assert Buffer.getFloat64(0, buf) == 1.0F
```

### Buffer.**addFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
addFloat64 : (value: Float64, buffer: Buffer) => Void
```

Appends a 64-bit float to a buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Float64`|The value to append|
|`buffer`|`Buffer`|The buffer to mutate|

Examples:

```grain
let buf = Buffer.make(32)
Buffer.addFloat64(1.0F, buf)
assert Buffer.getFloat64(0, buf) == 1.0F
```

