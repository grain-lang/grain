---
title: Hash
---

Utilities for hashing any value.

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
import Hash from "hash"
```

## Values

Functions for hashing.

### Hash.**hash**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
hash : a -> Number
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

