---
title: Marshal
---

Utilities for serializing and deserializing Grain data.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
import Marshal from "marshal"
```

## Values

Functions for marshaling and unmarshaling data.

### Marshal.**marshal**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
marshal : a -> Bytes
```

Serialize a value into a byte-based representation suitable for transmission
across a network or disk storage. The byte-based representation can be
deserialized at a later time to restore the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value to serialize|

Returns:

|type|description|
|----|-----------|
|`Bytes`|A byte-based representation of the value|

### Marshal.**unmarshal**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
unmarshal : Bytes -> Result<a, String>
```

Deserialize the byte-based representation of a value back into an in-memory
value. This operation is not type-safe, and it is recommended that a type
annotation is used to declare the type of the unmarshaled value. While
attempts to unmarshal bad data will fail, this operation is still generally
unsafe and great care should be taken to ensure that the data being
unmarshaled corresponds to the expected type.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The data to deserialize|

Returns:

|type|description|
|----|-----------|
|`Result<a, String>`|An in-memory value|

