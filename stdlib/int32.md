---
title: Int32
---

Utilities for working with the Int32 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
include "int32"
```

## Values

Functions and constants included in the Int32 module.

### Int32.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (x: Number) -> Int32
```

Converts a Number to an Int32.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int32`|The Number represented as an Int32|

### Int32.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : (x: Int32) -> Number
```

Converts an Int32 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int32 represented as a Number|

### Int32.**fromUint32**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromUint32 : (x: Uint32) -> Int32
```

Converts a Uint32 to an Int32.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Uint32`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int32`|The Uint32 represented as an Int32|

### Int32.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Int32) -> Int32
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Int32`|The incremented value|

### Int32.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Int32) -> Int32
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Int32`|The decremented value|

### Int32.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
add : (x: Int32, y: Int32) -> Int32
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The sum of the two operands|

### Int32.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
sub : (x: Int32, y: Int32) -> Int32
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The difference of the two operands|

### Int32.**mul**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mul : (x: Int32, y: Int32) -> Int32
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The product of the two operands|

### Int32.**div**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
div : (x: Int32, y: Int32) -> Int32
```

Computes the quotient of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The quotient of its operands|

### Int32.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
rem : (x: Int32, y: Int32) -> Int32
```

Computes the remainder of the division of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The remainder of its operands|

### Int32.**mod**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mod : (x: Int32, y: Int32) -> Int32
```

Computes the remainder of the division of the first operand by the second.
The result will have the sign of the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The modulus of its operands|

Throws:

`ModuloByZero`

* When `y` is zero

### Int32.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotl : (value: Int32, amount: Int32) -> Int32
```

Rotates the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to rotate|
|`amount`|`Int32`|The number of bits to rotate by|

Returns:

|type|description|
|----|-----------|
|`Int32`|The rotated value|

### Int32.**rotr**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotr : (value: Int32, amount: Int32) -> Int32
```

Rotates the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to rotate|
|`amount`|`Int32`|The number of bits to rotate by|

Returns:

|type|description|
|----|-----------|
|`Int32`|The rotated value|

### Int32.**shl**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
shl : (value: Int32, amount: Int32) -> Int32
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to shift|
|`amount`|`Int32`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Int32`|The shifted value|

### Int32.**shr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
shr : (value: Int32, amount: Int32) -> Int32
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to shift|
|`amount`|`Int32`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Int32`|The shifted value|

### Int32.**eq**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
eq : (x: Int32, y: Int32) -> Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first value|
|`y`|`Int32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

### Int32.**ne**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
ne : (x: Int32, y: Int32) -> Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first value|
|`y`|`Int32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

### Int32.**eqz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
eqz : (value: Int32) -> Bool
```

Checks if the given value is equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to zero or `false` otherwise|

### Int32.**lt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lt : (x: Int32, y: Int32) -> Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first value|
|`y`|`Int32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

### Int32.**gt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
gt : (x: Int32, y: Int32) -> Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first value|
|`y`|`Int32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

### Int32.**lte**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lte : (x: Int32, y: Int32) -> Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first value|
|`y`|`Int32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

### Int32.**gte**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
gte : (x: Int32, y: Int32) -> Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first value|
|`y`|`Int32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

### Int32.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Int32) -> Int32
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The given value|

Returns:

|type|description|
|----|-----------|
|`Int32`|Containing the inverted bits of the given value|

### Int32.**land**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
land : (x: Int32, y: Int32) -> Int32
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

### Int32.**lor**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lor : (x: Int32, y: Int32) -> Int32
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

### Int32.**lxor**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lxor : (x: Int32, y: Int32) -> Int32
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

### Int32.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
clz : (value: Int32) -> Int32
```

Counts the number of leading zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int32`|The amount of leading zeros|

### Int32.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
ctz : (value: Int32) -> Int32
```

Counts the number of trailing zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int32`|The amount of trailing zeros|

### Int32.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
popcnt : (value: Int32) -> Int32
```

Counts the number of bits set to `1` in the value, also known as a population count.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int32`|The amount of 1-bits in its operand|

