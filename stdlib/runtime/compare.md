---
title: Compare
---

## Values

Functions and constants included in the Compare module.

### Compare.**compare**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
compare : (num1: a, num2: a) => Number
```

Compares the first argument to the second argument and produces an integer result.
Provides a consistent ordering over all types and is suitable for sorting and other kinds of ordering.
`compare` treats `NaN` differently than the other comparison operators in that it considers `NaN` equal to itself and smaller than any other number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`a`|The first operand|
|`num2`|`a`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|A negative integer if the first operand is less than the second operand, `0` if they are equal, or a positive integer otherwise|

