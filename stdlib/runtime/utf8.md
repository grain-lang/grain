---
title: Utf8
---

The `Utf8` module provides functions for working with UTF-8 encoded strings.

## Values

Functions and constants included in the Utf8 module.

### Utf8.**isLeadingByte**

```grain
isLeadingByte: (byte: WasmI32) => Bool
```

Determines if the given byte is a leading byte in a UTF-8 sequence.

Parameters:

| param  | type      | description       |
| ------ | --------- | ----------------- |
| `byte` | `WasmI32` | The byte to check |

Returns:

| type   | description                                             |
| ------ | ------------------------------------------------------- |
| `Bool` | `true` if the byte is a leading byte, `false` otherwise |

### Utf8.**utf8ByteCount**

```grain
utf8ByteCount: (byte: WasmI32) => WasmI32
```

Returns the total number of bytes for a UTF-8 code point given the first byte.

Parameters:

| param  | type      | description                            |
| ------ | --------- | -------------------------------------- |
| `byte` | `WasmI32` | The first byte of the UTF-8 code point |

Returns:

| type      | description                                 |
| --------- | ------------------------------------------- |
| `WasmI32` | The number of bytes in the UTF-8 code point |

### Utf8.**usvEncodeLength**

```grain
usvEncodeLength: (usv: WasmI32) => WasmI32
```

Returns the number of bytes required to encode the given USV as UTF-8.

Parameters:

| param | type      | description              |
| ----- | --------- | ------------------------ |
| `usv` | `WasmI32` | The Unicode scalar value |

Returns:

| type      | description                                                   |
| --------- | ------------------------------------------------------------- |
| `WasmI32` | The number of bytes required to encode the given USV as UTF-8 |

### Utf8.**getCodePoint**

```grain
getCodePoint: (arrRef: WasmRef, offset: WasmI32) => WasmI32
```

Returns the Unicode code point from the encoded value at the given offset.

Parameters:

| param    | type      | description                                  |
| -------- | --------- | -------------------------------------------- |
| `arrRef` | `WasmRef` | The reference to the encoded value in memory |

Returns:

| type      | description                                                     |
| --------- | --------------------------------------------------------------- |
| `WasmI32` | The Unicode code point of the encoded value at the given offset |

Throws:

`MalformedUnicode`

* if the encoded value is not a valid UTF-8 sequence

### Utf8.**writeUtf8CodePoint**

```grain
writeUtf8CodePoint:
  (arrRef: WasmRef, offset: WasmI32, codePoint: WasmI32) => WasmI32
```

Writes the given Unicode code point to the array at the given offset as encoded UTF-8.

Parameters:

| param       | type      | description                                     |
| ----------- | --------- | ----------------------------------------------- |
| `arrRef`    | `WasmRef` | The (ref array) to write the UTF-8 character to |
| `offset`    | `WasmI32` | The offset in the array                         |
| `codePoint` | `WasmI32` | The Unicode code point to write                 |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmI32` | The number of bytes written |

