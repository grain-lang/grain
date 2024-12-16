---
title: Utf8
---

The `Utf8` module provides functions for working with UTF-8 encoded strings.

## Values

Functions and constants included in the Utf8 module.

### Utf8.**getUtf8ByteCount**

```grain
getUtf8ByteCount : (byte: WasmI32) => WasmI32
```

Returns the number of bytes in the given UTF-8 character, given the first byte.

Parameters:

|param|type|description|
|-----|----|-----------|
|`byte`|`WasmI32`|The first byte of the UTF-8 character|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The number of bytes in the UTF-8 character|

### Utf8.**getCodePointByteCount**

```grain
getCodePointByteCount : (usv: WasmI32) => WasmI32
```

Returns the number of bytes required to encode the given USV as UTF-8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`usv`|`WasmI32`|The Unicode scalar value|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The number of bytes required to encode the given USV as UTF-8|

### Utf8.**getCodePoint**

```grain
getCodePoint : (ptr: WasmI32) => WasmI32
```

Returns the Unicode code point of the character at the given pointer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`ptr`|`WasmI32`|The pointer to the character in memory|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The Unicode code point of the character at the given pointer|

Throws:

`MalformedUnicode`

* if the character is not a valid UTF-8 sequence

### Utf8.**writeUtf8CodePoint**

```grain
writeUtf8CodePoint : (ptr: WasmI32, codePoint: WasmI32) => WasmI32
```

Writes the given Unicode code point to the given pointer as a UTF-8 character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`ptr`|`WasmI32`|The pointer to write the UTF-8 character to|
|`codePoint`|`WasmI32`|The Unicode code point to write|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The number of bytes written|

