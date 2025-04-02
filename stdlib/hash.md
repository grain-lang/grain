---
title: Hash
---

Utilities for hashing any value.

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
from "hash" include Hash
```

```grain
let hashInstance = Hash.make()
assert Hash.hash(hashInstance, "Hello World") == Hash.hash(hashInstance, "Hello World")
```

```grain
let hashInstance = Hash.makeSeeded(10)
assert Hash.hash(hashInstance, "Hello World") == Hash.hash(hashInstance, "Hello World")
```

## Types

Type declarations included in the Hash module.

### Hash.**HashInstance**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
type HashInstance
```

Represents a particular hashing instance.

## Values

Functions and constants included in the Hash module.

### Hash.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
make : () => HashInstance
```

Produces a generic hash instance using a random seed value.

Returns:

|type|description|
|----|-----------|
|`HashInstance`|A hashing instance that can be consumed during hashing|

Throws:

`Failure(String)`

* If WASI random_get fails

Examples:

```grain
let hashInstance = Hash.make()
assert Hash.hash(hashInstance," Hello World") == Hash.hash(hashInstance, "Hello World)
```

### Hash.**makeSeeded**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
makeSeeded : (seed: Number) => HashInstance
```

Produces a hashInstance using the given seed.

Parameters:

|param|type|description|
|-----|----|-----------|
|`seed`|`Number`|The seed to use while hashing|

Returns:

|type|description|
|----|-----------|
|`HashInstance`|A hashing instance that can be consumed during hashing|

Examples:

```grain
let hashInstance = Hash.makeSeeded(1)
assert Hash.hash(hashInstance," Hello World") == Hash.hash(hashInstance, "Hello World)
```

```grain
let hashInstance1 = Hash.makeSeeded(1)
let hashInstance2 = Hash.makeSeeded(2)
assert Hash.hash(hashInstance1," Hello World") != Hash.hash(hashInstance2, "Hello World)
```

### Hash.**hash**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.7.0</code></td><td>Added `hashInstance` parameter instead of using a global seed</td></tr>
</tbody>
</table>
</details>

```grain
hash : (hashInstance: HashInstance, anything: a) => Number
```

A generic hash function that produces an integer from any value given a hashing instance. If `a == b` then `Hash.hash(h, a) == Hash.hash(h, b)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`hashInstance`|`HashInstance`|The hashing instance to use as a seed|
|`anything`|`a`|The value to hash|

Returns:

|type|description|
|----|-----------|
|`Number`|A hash for the given value|

Examples:

```grain
let hashInstance = Hash.makeSeeded(1)
assert Hash.hash(hashInstance," Hello World") == Hash.hash(hashInstance, "Hello World)
```

