---
title: TypeMetaData
---

Unsafe utilities for extracting runtime type information from grain values.

Note:
  This module is unsafe and should be used with caution.
  The grain team offers no guarantees on breaking changes or
  end user support.

## Values

Functions and constants included in the TypeMetaData module.

### TypeMetaData.**getTupleData**

```grain
getTupleData: (ref: WasmRef) => Array<WasmRef>
```

Provides the tuples contents.

Parameters:

| param | type      | description                                         |
| ----- | --------- | --------------------------------------------------- |
| `ref` | `WasmRef` | A reference to the tuple value to extract data from |

Returns:

| type             | description                                              |
| ---------------- | -------------------------------------------------------- |
| `Array<WasmRef>` | An array of references representing the tuple's elements |

### TypeMetaData.**getRecordMetaData**

```grain
getRecordMetaData: (ref: WasmRef) => Array<String>
```

Provides the metadata for a record type.

Parameters:

| param | type      | description                                                |
| ----- | --------- | ---------------------------------------------------------- |
| `ref` | `WasmRef` | The reference to the record value to extract metadata from |

Returns:

| type            | description                                                            |
| --------------- | ---------------------------------------------------------------------- |
| `Array<String>` | The names of the fields in the record, or an empty array if none found |

### TypeMetaData.**getVariantMetaData**

```grain
getVariantMetaData: (ref: WasmRef) => (String, Array<String>)
```

Provides the metadata for a variant type.

Parameters:

| param | type      | description                                                 |
| ----- | --------- | ----------------------------------------------------------- |
| `ref` | `WasmRef` | The reference to the variant value to extract metadata from |

Returns:

| type                      | description                        |
| ------------------------- | ---------------------------------- |
| `(String, Array<String>)` | The name and fields of the variant |

### TypeMetaData.**getRecordData**

```grain
getRecordData: (ref: WasmRef) => Array<(String, WasmRef)>
```

Provides the records field data along with field names if available.

If the names are not available, the field names will be returned as "<unknown field>".

Parameters:

| param | type      | description                                                  |
| ----- | --------- | ------------------------------------------------------------ |
| `ref` | `WasmRef` | The reference to the record value to extract field data from |

Returns:

| type                       | description                                                       |
| -------------------------- | ----------------------------------------------------------------- |
| `Array<(String, WasmRef)>` | An associated array of field names and their corresponding values |

### TypeMetaData.**isListVariant**

```grain
isListVariant: (ref: WasmRef) => Bool
```

Checks if the given ADT value is a List variant.

Parameters:

| param | type      | description             |
| ----- | --------- | ----------------------- |
| `ref` | `WasmRef` | The ADT value to check. |

Returns:

| type   | description                                                   |
| ------ | ------------------------------------------------------------- |
| `Bool` | `true` if the ADT value is a List variant, `false` otherwise. |

