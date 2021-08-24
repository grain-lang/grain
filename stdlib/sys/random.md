---
title: Sys/Random
---

System access to random values.

```grain
import Random from "sys/random"
```

## Values

Functions and constants included in the Sys/Random module.

### Random.**random**

```grain
random : () -> Result<Number, Exception>
```

Produce a random number. This function can be slow, so it's best to seed a generator if lots of random data is needed.

Returns:

|type|description|
|----|-----------|
|`Result<Number, Exception>`|`Ok(num)` of a random number if successful or `Err(exception)` otherwise|

