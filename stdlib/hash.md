---
title: Hash
---

Utilities for hashing.

```grain
import Hash from "hash"
```

## Values

Functions for hashing.

### Hash.**hash**

```grain
hash : a -> Number
```

Generic hashing.
Takes any value and produces an integer. If `a == b` then `Hash.hash(a) == Hash.hash(b)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`a`|`a`|The value to hash|

Returns:

|type|description|
|----|-----------|
|`Number`|The generated hash|

