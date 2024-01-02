---
title: Hash
---

Utilities for hashing any value.

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
include "hash"
```

## Values

Functions and constants included in the Hash module.

### Hash.**hash**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>next</code></td><td>Added ability to set the seed</td></tr>
</tbody>
</table>
</details>

```grain
hash : (anything: a, ?seed: Number) => Number
```

A generic hash function that produces an integer from any value. If `a == b` then `Hash.hash(a) == Hash.hash(b)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`anything`|`a`|The value to hash|
|`seed`|`Option<Number>`|The seed to use|

Returns:

|type|description|
|----|-----------|
|`Number`|A hash for the given value|

Examples:

```grain
Hash.hash(a) == Hash.hash(a)
```

```grain
Hash.hash(a, seed=1) == -1015171190
```

