### Bytes.**setInt8**

```grain
setInt8 : (Number, Int32, Bytes) -> Void
```

Sets a signed 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`v`|`Int32`|Int32 - The value to set|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Void`|Void|

### Bytes.**getInt8S**

```grain
getInt8S : (Number, Bytes) -> Int32
```

Gets a signed 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Int32`|Int32|

### Bytes.**getInt8U**

```grain
getInt8U : (Number, Bytes) -> Int32
```

Gets an unsigned 8-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Int32`|Int32|

### Bytes.**setInt16**

```grain
setInt16 : (Number, Int32, Bytes) -> Void
```

Sets a signed 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`v`|`Int32`|Int32 - The value to set|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Void`|Void|

### Bytes.**getInt16S**

```grain
getInt16S : (Number, Bytes) -> Int32
```

Gets a signed 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Int32`|Int32|

### Bytes.**getInt16U**

```grain
getInt16U : (Number, Bytes) -> Int32
```

Gets an unsigned 16-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Int32`|Int32|

### Bytes.**setInt32**

```grain
setInt32 : (Number, Int32, Bytes) -> Void
```

Sets a signed 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`v`|`Int32`|Int32 - The value to set|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Void`|Void|

### Bytes.**getInt32**

```grain
getInt32 : (Number, Bytes) -> Int32
```

Gets a signed 32-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Int32`|Int32|

### Bytes.**setFloat32**

```grain
setFloat32 : (Number, Float32, Bytes) -> Void
```

Sets a 32-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`v`|`Float32`|Float32 - The value to set|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Void`|Void|

### Bytes.**getFloat32**

```grain
getFloat32 : (Number, Bytes) -> Float32
```

Gets a 32-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Float32`|Float32|

### Bytes.**setInt64**

```grain
setInt64 : (Number, Int64, Bytes) -> Void
```

Sets a signed 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`v`|`Int64`|Int64 - The value to set|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Void`|Void|

### Bytes.**getInt64**

```grain
getInt64 : (Number, Bytes) -> Int64
```

Gets a signed 64-bit integer starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Int64`|Int64|

### Bytes.**setFloat64**

```grain
setFloat64 : (Number, Float64, Bytes) -> Void
```

Sets a 64-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`v`|`Float64`|Float64 - The value to set|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Void`|Void|

### Bytes.**getFloat64**

```grain
getFloat64 : (Number, Bytes) -> Float64
```

Gets a 64-bit float starting at the given byte index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The byte index|
|`b`|`Bytes`|Bytes - The byte sequence|

Returns:

|type|description|
|----|-----------|
|`Float64`|Float64|

### Bytes.**copy**

```grain
copy : Bytes -> Bytes
```

Return a new byte sequence that contains the same bytes as the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`b`|`Bytes`|Bytes - The byte sequence to copy|

Returns:

|type|description|
|----|-----------|
|`Bytes`|Bytes|

### Bytes.**slice**

```grain
slice : (Number, Number, Bytes) -> Bytes
```

Returns a new byte sequence containing a subset of the original byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`i`|`Number`|Number - The start position to copy from|
|`len`|`Number`|Number - The number of bytes to copy|
|`b`|`Bytes`|Bytes - The byte sequence get a subset of bytes from|

Returns:

|type|description|
|----|-----------|
|`Bytes`|Bytes|

### Bytes.**resize**

```grain
resize : (Number, Number, Bytes) -> Bytes
```

Add or remove bytes from the start and/or end of a byte sequence.
A positive number represents bytes to add, while a negative number represents bytes to remove.

Parameters:

|param|type|description|
|-----|----|-----------|
|`left`|`Number`|Number - The number of uninitialized bytes to prepend|
|`right`|`Number`|Number - The number of uninitialized bytes to append|
|`b`|`Bytes`|Bytes - The byte sequence get a subset of bytes from|

Returns:

|type|description|
|----|-----------|
|`Bytes`|Bytes|

### Bytes.**move**

```grain
move : (Number, Number, Number, Bytes, Bytes) -> Void
```

Copies a range of bytes from a source buffer to a given location in a destination buffer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`srcPos`|`Number`|Number - The starting byte index to copy bytes from|
|`dstPos`|`Number`|Number - The starting byte index to copy bytes into|
|`len`|`Number`|Number - The amount of bytes to copy from the source buffer|
|`src`|`Bytes`|Bytes - The source buffer|
|`dst`|`Bytes`|Bytes - The destination buffer|

Returns:

|type|description|
|----|-----------|
|`Void`|Void|

### Bytes.**length**

```grain
length : Bytes -> Number
```

Get the byte length of a byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`b`|`Bytes`|Bytes - The byte sequence to check|

Returns:

|type|description|
|----|-----------|
|`Number`|Number|

### Bytes.**concat**

```grain
concat : (Bytes, Bytes) -> Bytes
```

Creates a new byte sequence that contains the bytes of both buffers a and b.

Parameters:

|param|type|description|
|-----|----|-----------|
|`a`|`Bytes`|Bytes - The buffer to be copied first|
|`b`|`Bytes`|Bytes - The buffer to be copied last|

Returns:

|type|description|
|----|-----------|
|`Bytes`|Bytes|

### Bytes.**toString**

```grain
toString : Bytes -> String
```

Creates a new String from a byte sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`b`|`Bytes`|Bytes - The source buffer|

Returns:

|type|description|
|----|-----------|
|`String`|String|

### Bytes.**fromString**

```grain
fromString : String -> Bytes
```

Creates a new byte sequence from a String.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str`|`String`|String - The String to copy into a byte sequence|

Returns:

|type|description|
|----|-----------|
|`Bytes`|Bytes|

### Bytes.**fill**

```grain
fill : (Int32, Bytes) -> Void
```

Fills a byte sequence with a given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`v`|`Int32`|Int32 - The value to fill the byte sequence with|
|`b`|`Bytes`|Bytes - The byte sequence to fill|

### Bytes.**make**

```grain
make : Number -> Bytes
```

Make a new byte sequence of n-bytes size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`n`|`Number`|Number - The number of bytes to store|

Returns:

|type|description|
|----|-----------|
|`Bytes`|Bytes|

### Bytes.**empty**

```grain
empty : Bytes
```

An empty byte sequence

