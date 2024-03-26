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
Hash.hash(1)
```

```grain
Hash.hash("Hello World")
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

Throws:

`Failure(String)`

* If WASI random_get fails

Examples:

```grain
assert Hash.hash(1) == Hash.hash(1)
```

```grain
assert Hash.hash("Hello World") == Hash.hash("Hello World")
```

