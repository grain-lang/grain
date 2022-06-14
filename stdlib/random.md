---
title: Random
---

Pseudo-random number generation.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
import Random from "random"
```

## Types

Type declarations included in the Random module.

### Random.**Random**

```grain
type Random
```

## Values

Functions for working with pseudo-random number generators.

### Random.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
make : Int64 -> Random
```

Creates a new pseudo-random number generator with the given seed.

Parameters:

|param|type|description|
|-----|----|-----------|
|`seed`|`Int64`|The seed for the pseudo-random number generator|

Returns:

|type|description|
|----|-----------|
|`Random`|The pseudo-random number generator|

### Random.**makeUnseeded**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
makeUnseeded : () -> Result<Random, Exception>
```

Creates a new pseudo-random number generator with a random seed.

Returns:

|type|description|
|----|-----------|
|`Result<Random, Exception>`|`Ok(generator)` of a pseudo-random number generator if successful or `Err(exception)` otherwise|

### Random.**nextInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
nextInt32 : Random -> Int32
```

Generates a random 32-bit integer from the given pseudo-random number generator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`random`|`Random`|The pseudo-random number generator to use|

Returns:

|type|description|
|----|-----------|
|`Int32`|The randomly generated number|

### Random.**nextInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
nextInt64 : Random -> Int64
```

Generates a random 64-bit integer from the given pseudo-random number generator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`random`|`Random`|The pseudo-random number generator to use|

Returns:

|type|description|
|----|-----------|
|`Int64`|The randomly generated number|

### Random.**nextInt32InRange**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
nextInt32InRange : (Random, Int32, Int32) -> Int32
```

Generates a random 32-bit integer from the given pseudo-random number generator
from a uniform distribution in the given range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`random`|`Random`|The pseudo-random number generator to use|
|`low`|`Int32`|The lower bound of the range (inclusive)|
|`high`|`Int32`|The upper bound of the range (exclusive)|

Returns:

|type|description|
|----|-----------|
|`Int32`|The randomly generated number|

### Random.**nextInt64InRange**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
nextInt64InRange : (Random, Int64, Int64) -> Int64
```

Generates a random 64-bit integer from the given pseudo-random number generator
from a uniform distribution in the given range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`random`|`Random`|The pseudo-random number generator to use|
|`low`|`Int64`|The lower bound of the range (inclusive)|
|`high`|`Int64`|The upper bound of the range (exclusive)|

Returns:

|type|description|
|----|-----------|
|`Int64`|The randomly generated number|

