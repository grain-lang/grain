---
title: Hash
---

Utilities for hashing any value.

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>next</code></td><td>Added SeededHash submodule</td></tr>
</tbody>
</table>
</details>

```grain
include "hash"
```

## Values

Functions and constants included in the Hash module.

### Hash.**hash**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
hash : (anything: a) => Number
```

A generic hash function that produces an integer from any value. If `a == b` then `Hash.hash(a) == Hash.hash(b)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`anything`|`a`|The value to hash|

Returns:

|type|description|
|----|-----------|
|`Number`|A hash for the given value|

Examples:

```grain
Hash.hash(a) == Hash.hash(a)
```

## Hash.SeededHash

Utilities for performing hashes with a specific seed.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
from Hash use { module SeededHash }
```

```grain
Hash.SeededHash.make(1)
```

### Types

Type declarations included in the Hash.SeededHash module.

#### Hash.SeededHash.**HashingInstance**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
type HashingInstance
```

Represents a seeded hashing instance.

### Values

Functions and constants included in the Hash.SeededHash module.

#### Hash.SeededHash.**make**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
make : (seed: Number) => HashingInstance
```

Creates a new hashing instance with the given seed.

Parameters:

|param|type|description|
|-----|----|-----------|
|`seed`|`Number`|The seed for the new hashing instance|

Returns:

|type|description|
|----|-----------|
|`HashingInstance`|A new hashing instance|

Examples:

```grain
Hash.SeedHash.make(1) == Hash.SeedHash.make(1)
```

```grain
Hash.SeedHash.make(10) == Hash.SeedHash.make(10)
```

#### Hash.SeededHash.**hash**

```grain
hash : (hashingInstance: HashingInstance, anything: a) => Number
```

