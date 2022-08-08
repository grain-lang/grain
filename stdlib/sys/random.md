---
title: Random
---

System access to random values.

```grain
import Random from "sys/random"
```

## Values

Functions and constants included in the Random module.

### Random.**randomInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
randomInt32 : () -> Result<Int32, Exception>
```

Produce a random 32-bit integer. This function can be slow, so it's best to seed a generator if lots of random data is needed.

Returns:

|type|description|
|----|-----------|
|`Result<Int32, Exception>`|`Ok(num)` of a random Int32 if successful or `Err(exception)` otherwise|

### Random.**randomInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
randomInt64 : () -> Result<Int64, Exception>
```

Produce a random 64-bit integer. This function can be slow, so it's best to seed a generator if lots of random data is needed.

Returns:

|type|description|
|----|-----------|
|`Result<Int64, Exception>`|`Ok(num)` of a random Int64 if successful or `Err(exception)` otherwise|

### Random.**random**

```grain
random : () -> Result<Number, Exception>
```

Produce a random number. This function can be slow, so it's best to seed a generator if lots of random data is needed.

Returns:

|type|description|
|----|-----------|
|`Result<Number, Exception>`|`Ok(num)` of a random number if successful or `Err(exception)` otherwise|

