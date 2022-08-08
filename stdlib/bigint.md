---
title: BigInt
---

Utilities for working with the BigInt type.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
import BigInt from "bigint"
```

## Conversions

Functions for converting between Numbers and the BigInt type.

### BigInt.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : Number -> BigInt
```

Converts a Number to a BigInt.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The Number represented as a BigInt|

### BigInt.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : BigInt -> Number
```

Converts a BigInt to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The BigInt represented as a Number|

## Operations

Mathematical operations for BigInt values.

### BigInt.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
incr : BigInt -> BigInt
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The incremented value|

### BigInt.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
decr : BigInt -> BigInt
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The decremented value|

### BigInt.**neg**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
neg : BigInt -> BigInt
```

Negates the given operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The negated operand|

### BigInt.**abs**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
abs : BigInt -> BigInt
```

Returns the absolute value of the given operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The operand's absolute value|

### BigInt.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
add : (BigInt, BigInt) -> BigInt
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The sum of the two operands|

### BigInt.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
sub : (BigInt, BigInt) -> BigInt
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The difference of the two operands|

### BigInt.**mul**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
mul : (BigInt, BigInt) -> BigInt
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The product of the two operands|

### BigInt.**div**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
div : (BigInt, BigInt) -> BigInt
```

Computes the quotient of its operands using signed (truncated) division
(in which the quotient is always rounded towards zero).

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The quotient of its operands|

### BigInt.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
rem : (BigInt, BigInt) -> BigInt
```

Computes the remainder of the division of its operands using signed (truncated) division
(in which the quotient is always rounded towards zero).

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The remainder of its operands|

### BigInt.**quotRem**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
quotRem : (BigInt, BigInt) -> (BigInt, BigInt)
```

Computes the quotient and remainder of its operands using signed (truncated) division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`(BigInt, BigInt)`|The quotient and remainder of its operands|

### BigInt.**gcd**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
gcd : (BigInt, BigInt) -> BigInt
```

Computes the greatest common divisior of the two operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The greatest common divisor of its operands|

## Bitwise operations

Functions for operating on bits of BigInt values.

### BigInt.**shl**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
shl : (BigInt, Int32) -> BigInt
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to shift|
|`places`|`Int32`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The shifted value|

### BigInt.**shr**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
shr : (BigInt, Int32) -> BigInt
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to shift|
|`places`|`Int32`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The shifted value|

## Comparisons

Functions for comparing BigInt values.

### BigInt.**eqz**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
eqz : BigInt -> Bool
```

Checks if the given value is equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to zero or `false` otherwise|

### BigInt.**eq**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
eq : (BigInt, BigInt) -> Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

### BigInt.**ne**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
ne : (BigInt, BigInt) -> Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

### BigInt.**lt**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
lt : (BigInt, BigInt) -> Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

### BigInt.**lte**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
lte : (BigInt, BigInt) -> Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

### BigInt.**gt**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
gt : (BigInt, BigInt) -> Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

### BigInt.**gte**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
gte : (BigInt, BigInt) -> Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

## Bitwise logic

Boolean operations on the bits of BigInt values.

### BigInt.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
lnot : BigInt -> BigInt
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The given value|

Returns:

|type|description|
|----|-----------|
|`BigInt`|Containing the inverted bits of the given value|

### BigInt.**land**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
land : (BigInt, BigInt) -> BigInt
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

### BigInt.**lor**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
lor : (BigInt, BigInt) -> BigInt
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

### BigInt.**lxor**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
lxor : (BigInt, BigInt) -> BigInt
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

### BigInt.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
clz : BigInt -> Int32
```

Counts the number of leading zero bits in the value.
Will return the maximum integer for negative numbers.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int32`|The amount of leading zeros|

### BigInt.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
ctz : BigInt -> Int64
```

Counts the number of trailing zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int64`|The amount of trailing zeros|

### BigInt.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
popcnt : BigInt -> Option<Int64>
```

Counts the number of bits set to `1` in the value, also known as a population count.
Will return the `None` if given a negative integer

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<Int64>`|The amount of 1-bits in its operand|

## Other

Other functions on BigInts.

### BigInt.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
toString : BigInt -> String
```

Converts the given operand to a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The operand|

Returns:

|type|description|
|----|-----------|
|`String`|The operand, as a string|

