---
title: Marshal
---

Utilities for serializing and deserializing Grain data.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
from "marshal" include Marshal
```

```grain
Marshal.marshal(1)
```

```grain
Marshal.marshal("Hello World")
```

```grain
Marshal.unmarshal(b"\x01\x01\x00\x00\x00"): Result<Number, String> == Ok(0)
```

## Values

Functions and constants included in the Marshal module.

### Marshal.**marshal**

<details>
<summary>Added in <code>0.5.3</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>next</code></td><td>Closures are no longer supported</td></tr>
</tbody>
</table>
</details>

```grain
marshal: (value: a) => Bytes
```

Serialize a value into a byte-based representation suitable for transmission
across a network or disk storage. The byte-based representation can be
deserialized at a later time to restore the value.

Parameters:

| param   | type | description            |
| ------- | ---- | ---------------------- |
| `value` | `a`  | The value to serialize |

Returns:

| type    | description                              |
| ------- | ---------------------------------------- |
| `Bytes` | A byte-based representation of the value |

Throws:

`Failure(String)`

* When trying to marshal a closure

Examples:

```grain
Marshal.marshal(1) == Marshal.marshal(1)
```

```grain
Marshal.marshal("🌾") == Marshal.marshal("🌾")
```

### Marshal.**unmarshal**

<details>
<summary>Added in <code>0.5.3</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>next</code></td><td>Closures are no longer supported</td></tr>
</tbody>
</table>
</details>

```grain
unmarshal: (bytes: Bytes) => Result<a, String>
```

Deserialize the byte-based representation of a value back into an in-memory
value. This operation is not type-safe, and it is recommended that a type
annotation is used to declare the type of the unmarshaled value. While
attempts to unmarshal bad data will fail, this operation is still generally
unsafe and great care should be taken to ensure that the data being
unmarshaled corresponds to the expected type.

Parameters:

| param   | type    | description             |
| ------- | ------- | ----------------------- |
| `bytes` | `Bytes` | The data to deserialize |

Returns:

| type                | description        |
| ------------------- | ------------------ |
| `Result<a, String>` | An in-memory value |

Examples:

```grain
Marshal.unmarshal(Marshal.marshal('🌾')) == Ok('🌾')
```

