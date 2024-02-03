---
title: Equal
---

## Values

Functions and constants included in the Equal module.

### Equal.**equal**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
equal : (value1: a, value2: a) => Bool
```

Check that two values are equal. This checks for structural equality,
so it also works for comparing things like tuples and lists.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value1`|`a`|The first operand|
|`value2`|`a`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the values are structurally equal or `false` otherwise|

