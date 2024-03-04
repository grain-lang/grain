---
title: Random
---

Pseudo-random number generation.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
from "random" include Random
```

## Types

Type declarations included in the Random module.

### Random.**Random**

```grain
type Random
```

## Values

Functions and constants included in the Random module.

### Random.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
make : (seed: Uint64) => Random
```

Creates a new pseudo-random number generator with the given seed.

Parameters:

|param|type|description|
|-----|----|-----------|
|`seed`|`Uint64`|The seed for the pseudo-random number generator|

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
makeUnseeded : () => Result<Random, Exception>
```

Creates a new pseudo-random number generator with a random seed.

Returns:

|type|description|
|----|-----------|
|`Result<Random, Exception>`|`Ok(generator)` of a pseudo-random number generator if successful or `Err(exception)` otherwise|

### Random.**nextUint32**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `nextInt32`</td></tr>
</tbody>
</table>
</details>

```grain
nextUint32 : (random: Random) => Uint32
```

Generates a random 32-bit integer from the given pseudo-random number generator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`random`|`Random`|The pseudo-random number generator to use|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The randomly generated number|

### Random.**nextUint64**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `nextInt64`</td></tr>
</tbody>
</table>
</details>

```grain
nextUint64 : (random: Random) => Uint64
```

Generates a random 64-bit integer from the given pseudo-random number generator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`random`|`Random`|The pseudo-random number generator to use|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The randomly generated number|

### Random.**nextUint32InRange**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `nextInt32InRange`</td></tr>
</tbody>
</table>
</details>

```grain
nextUint32InRange : (random: Random, low: Uint32, high: Uint32) => Uint32
```

Generates a random 32-bit integer from the given pseudo-random number generator
from a uniform distribution in the given range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`random`|`Random`|The pseudo-random number generator to use|
|`low`|`Uint32`|The lower bound of the range (inclusive)|
|`high`|`Uint32`|The upper bound of the range (exclusive)|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The randomly generated number|

### Random.**nextUint64InRange**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `nextInt64InRange`</td></tr>
</tbody>
</table>
</details>

```grain
nextUint64InRange : (random: Random, low: Uint64, high: Uint64) => Uint64
```

Generates a random 64-bit integer from the given pseudo-random number generator
from a uniform distribution in the given range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`random`|`Random`|The pseudo-random number generator to use|
|`low`|`Uint64`|The lower bound of the range (inclusive)|
|`high`|`Uint64`|The upper bound of the range (exclusive)|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The randomly generated number|

