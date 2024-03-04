---
title: Uint64
---

Utilities for working with the Uint64 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
from "uint64" include Uint64
```

## Values

Functions and constants included in the Uint64 module.

### Uint64.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Uint64
```

Converts a Number to a Uint64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The Number represented as a Uint64|

### Uint64.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : (value: Uint64) => Number
```

Converts a Uint64 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Uint64 represented as a Number|

### Uint64.**fromInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromInt64 : (number: Int64) => Uint64
```

Converts an Int64 to a Uint64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Int64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The Int64 represented as a Uint64|

### Uint64.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Uint64) => Uint64
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The incremented value|

### Uint64.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Uint64) => Uint64
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The decremented value|

### Uint64.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(+) : (x: Uint64, y: Uint64) => Uint64
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first operand|
|`y`|`Uint64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The sum of the two operands|

### Uint64.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(-) : (x: Uint64, y: Uint64) => Uint64
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first operand|
|`y`|`Uint64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The difference of the two operands|

### Uint64.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(*) : (x: Uint64, y: Uint64) => Uint64
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first operand|
|`y`|`Uint64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The product of the two operands|

### Uint64.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(/) : (x: Uint64, y: Uint64) => Uint64
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first operand|
|`y`|`Uint64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The quotient of its operands|

### Uint64.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rem : (x: Uint64, y: Uint64) => Uint64
```

Computes the remainder of the division of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first operand|
|`y`|`Uint64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The remainder of its operands|

### Uint64.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rotl : (value: Uint64, amount: Uint64) => Uint64
```

Rotates the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to rotate|
|`amount`|`Uint64`|The number of bits to rotate by|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The rotated value|

### Uint64.**rotr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rotr : (value: Uint64, amount: Uint64) => Uint64
```

Rotates the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to rotate|
|`amount`|`Uint64`|The number of bits to rotate by|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The rotated value|

### Uint64.**(<<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<<) : (value: Uint64, amount: Uint64) => Uint64
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to shift|
|`amount`|`Uint64`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The shifted value|

### Uint64.**(>>>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>>>) : (value: Uint64, amount: Uint64) => Uint64
```

Shifts the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to shift|
|`amount`|`Uint64`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The shifted value|

### Uint64.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(==) : (x: Uint64, y: Uint64) => Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first value|
|`y`|`Uint64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

### Uint64.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(!=) : (x: Uint64, y: Uint64) => Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first value|
|`y`|`Uint64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

### Uint64.**eqz**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
eqz : (value: Uint64) => Bool
```

Checks if the given value is equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to zero or `false` otherwise|

### Uint64.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<) : (x: Uint64, y: Uint64) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first value|
|`y`|`Uint64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

### Uint64.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>) : (x: Uint64, y: Uint64) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first value|
|`y`|`Uint64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

### Uint64.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<=) : (x: Uint64, y: Uint64) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first value|
|`y`|`Uint64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

### Uint64.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>=) : (x: Uint64, y: Uint64) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first value|
|`y`|`Uint64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

### Uint64.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Uint64) => Uint64
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The given value|

Returns:

|type|description|
|----|-----------|
|`Uint64`|Containing the inverted bits of the given value|

### Uint64.**(&)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(&) : (x: Uint64, y: Uint64) => Uint64
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first operand|
|`y`|`Uint64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint64`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

### Uint64.**(|)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(|) : (x: Uint64, y: Uint64) => Uint64
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first operand|
|`y`|`Uint64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint64`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

### Uint64.**(^)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(^) : (x: Uint64, y: Uint64) => Uint64
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint64`|The first operand|
|`y`|`Uint64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint64`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

### Uint64.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
clz : (value: Uint64) => Uint64
```

Counts the number of leading zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The amount of leading zeros|

### Uint64.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
ctz : (value: Uint64) => Uint64
```

Counts the number of trailing zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The amount of trailing zeros|

### Uint64.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
popcnt : (value: Uint64) => Uint64
```

Counts the number of bits set to `1` in the value, also known as a population count.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Uint64`|The amount of 1-bits in its operand|

