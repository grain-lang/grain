---
title: MiniBuffer
---

A low level micro buffer implementation.

## Types

Type declarations included in the MiniBuffer module.

### MiniBuffer.**MiniBuffer**

```grain
type MiniBuffer
```

A tiny string buffer.

## Values

Functions and constants included in the MiniBuffer module.

### MiniBuffer.**make**

```grain
make: (size: WasmI32) => MiniBuffer
```

Constructs a new MiniBuffer with the given initial size.

Parameters:

| param  | type      | description                              |
| ------ | --------- | ---------------------------------------- |
| `size` | `WasmI32` | The initial size of the buffer in bytes. |

Returns:

| type         | description                                                |
| ------------ | ---------------------------------------------------------- |
| `MiniBuffer` | A new MiniBuffer instance with the specified initial size. |

### MiniBuffer.**addString**

```grain
addString: (str: String, buffer: MiniBuffer) => Void
```

Appends the given string to the MiniBuffer, automatically growing the buffer if necessary.

Parameters:

| param    | type         | description                                          |
| -------- | ------------ | ---------------------------------------------------- |
| `str`    | `String`     | The string to append to the buffer.                  |
| `buffer` | `MiniBuffer` | The MiniBuffer to which the string will be appended. |

### MiniBuffer.**toString**

```grain
toString: (buffer: MiniBuffer) => String
```

Converts the contents of the MiniBuffer to a String.

Parameters:

| param    | type         | description                                                  |
| -------- | ------------ | ------------------------------------------------------------ |
| `buffer` | `MiniBuffer` | The MiniBuffer whose contents will be converted to a String. |

Returns:

| type     | description                                |
| -------- | ------------------------------------------ |
| `String` | The content of the MiniBuffer as a String. |

