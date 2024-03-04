---
title: Random
---

System access to random values.

```grain
from "wasi/random" include Random
```

## Values

Functions and constants included in the Random module.

### Random.**randomUint32**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `randomInt32`</td></tr>
</tbody>
</table>
</details>

```grain
randomUint32 : () => Result<Uint32, Exception>
```

Produce a random 32-bit integer. This function can be slow, so it's best to seed a generator if lots of random data is needed.

Returns:

|type|description|
|----|-----------|
|`Result<Uint32, Exception>`|`Ok(num)` of a random Uint32 if successful or `Err(exception)` otherwise|

### Random.**randomUint64**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `randomInt64`</td></tr>
</tbody>
</table>
</details>

```grain
randomUint64 : () => Result<Uint64, Exception>
```

Produce a random 64-bit integer. This function can be slow, so it's best to seed a generator if lots of random data is needed.

Returns:

|type|description|
|----|-----------|
|`Result<Uint64, Exception>`|`Ok(num)` of a random Uint64 if successful or `Err(exception)` otherwise|

### Random.**random**

```grain
random : () => Result<Number, Exception>
```

Produce a random number. This function can be slow, so it's best to seed a generator if lots of random data is needed.

Returns:

|type|description|
|----|-----------|
|`Result<Number, Exception>`|`Ok(num)` of a random number if successful or `Err(exception)` otherwise|

