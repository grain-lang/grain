---
title: Uint32
---

Utilities for working with the Uint32 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
from "uint32" include Uint32
```

## Values

Functions and constants included in the Uint32 module.

### Uint32.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Uint32
```

Converts a Number to a Uint32.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The Number represented as a Uint32|

### Uint32.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : (value: Uint32) => Number
```

Converts a Uint32 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Uint32 represented as a Number|

### Uint32.**fromInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromInt32 : (number: Int32) => Uint32
```

Converts an Int32 to a Uint32.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Int32`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The Int32 represented as a Uint32|

### Uint32.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Uint32) => Uint32
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The incremented value|

### Uint32.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Uint32) => Uint32
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The decremented value|

### Uint32.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(+) : (x: Uint32, y: Uint32) => Uint32
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first operand|
|`y`|`Uint32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The sum of the two operands|

### Uint32.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(-) : (x: Uint32, y: Uint32) => Uint32
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first operand|
|`y`|`Uint32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The difference of the two operands|

### Uint32.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(*) : (x: Uint32, y: Uint32) => Uint32
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first operand|
|`y`|`Uint32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The product of the two operands|

### Uint32.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(/) : (x: Uint32, y: Uint32) => Uint32
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first operand|
|`y`|`Uint32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The quotient of its operands|

### Uint32.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rem : (x: Uint32, y: Uint32) => Uint32
```

Computes the remainder of the division of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first operand|
|`y`|`Uint32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The remainder of its operands|

### Uint32.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rotl : (value: Uint32, amount: Uint32) => Uint32
```

Rotates the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to rotate|
|`amount`|`Uint32`|The number of bits to rotate by|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The rotated value|

### Uint32.**rotr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rotr : (value: Uint32, amount: Uint32) => Uint32
```

Rotates the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to rotate|
|`amount`|`Uint32`|The number of bits to rotate by|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The rotated value|

### Uint32.**(<<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<<) : (value: Uint32, amount: Uint32) => Uint32
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to shift|
|`amount`|`Uint32`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The shifted value|

### Uint32.**(>>>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>>>) : (value: Uint32, amount: Uint32) => Uint32
```

Shifts the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to shift|
|`amount`|`Uint32`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The shifted value|

### Uint32.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(==) : (x: Uint32, y: Uint32) => Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first value|
|`y`|`Uint32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

### Uint32.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(!=) : (x: Uint32, y: Uint32) => Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first value|
|`y`|`Uint32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

### Uint32.**eqz**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
eqz : (value: Uint32) => Bool
```

Checks if the given value is equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to zero or `false` otherwise|

### Uint32.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<) : (x: Uint32, y: Uint32) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first value|
|`y`|`Uint32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

### Uint32.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>) : (x: Uint32, y: Uint32) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first value|
|`y`|`Uint32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

### Uint32.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<=) : (x: Uint32, y: Uint32) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first value|
|`y`|`Uint32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

### Uint32.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>=) : (x: Uint32, y: Uint32) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first value|
|`y`|`Uint32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

### Uint32.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Uint32) => Uint32
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The given value|

Returns:

|type|description|
|----|-----------|
|`Uint32`|Containing the inverted bits of the given value|

### Uint32.**(&)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(&) : (x: Uint32, y: Uint32) => Uint32
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first operand|
|`y`|`Uint32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint32`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

### Uint32.**(|)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(|) : (x: Uint32, y: Uint32) => Uint32
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first operand|
|`y`|`Uint32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint32`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

### Uint32.**(^)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(^) : (x: Uint32, y: Uint32) => Uint32
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint32`|The first operand|
|`y`|`Uint32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint32`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

### Uint32.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
clz : (value: Uint32) => Uint32
```

Counts the number of leading zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The amount of leading zeros|

### Uint32.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
ctz : (value: Uint32) => Uint32
```

Counts the number of trailing zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The amount of trailing zeros|

### Uint32.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
popcnt : (value: Uint32) => Uint32
```

Counts the number of bits set to `1` in the value, also known as a population count.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Uint32`|The amount of 1-bits in its operand|

