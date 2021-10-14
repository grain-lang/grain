---
title: Hash
---

Utilities for hashing any values.

```grain
import Hash from "hash"
```

## Values

Functions for hashing.

### Hash.**hash**

```grain
hash : a -> Number
```

A generic hash function that turns any value into an integer. If `a == b` then `Hash.hash(a) == Hash.hash(b)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`anything`|`a`|The value to hash|

Returns:

|type|description|
|----|-----------|
|`Number`|A hash for the given value|

