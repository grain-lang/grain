---
title: Int32
---

Utilities for working with the Int32 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
import Int32 from "int32"
```

## Conversions

Functions for converting between Numbers and the Int32 type.

### Int32.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : Number -> Int32
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
toNumber : Int32 -> Number
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

## Operations

Mathematical operations for Int32 values.

### Int32.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
incr : Int32 -> Int32
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
decr : Int32 -> Int32
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
add : (Int32, Int32) -> Int32
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
sub : (Int32, Int32) -> Int32
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
mul : (Int32, Int32) -> Int32
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
div : (Int32, Int32) -> Int32
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

### Int32.**divU**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
divU : (Int32, Int32) -> Int32
```

Computes the quotient of its operands using unsigned division.

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
rem : (Int32, Int32) -> Int32
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

### Int32.**remU**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
remU : (Int32, Int32) -> Int32
```

Computes the remainder of the division of its operands using unsigned division.

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
mod : (Int32, Int32) -> Int32
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

## Bitwise operations

Functions for operating on bits of Int32 values.

### Int32.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotl : (Int32, Int32) -> Int32
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
rotr : (Int32, Int32) -> Int32
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
shl : (Int32, Int32) -> Int32
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
shr : (Int32, Int32) -> Int32
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

### Int32.**shrU**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
shrU : (Int32, Int32) -> Int32
```

Shifts the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to shift|
|`amount`|`Int32`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Int32`|The shifted value|

## Comparisons

Functions for comparing Int32 values.

### Int32.**eq**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
eq : (Int32, Int32) -> Bool
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
ne : (Int32, Int32) -> Bool
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
eqz : Int32 -> Bool
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
lt : (Int32, Int32) -> Bool
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

### Int32.**ltU**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
ltU : (Int32, Int32) -> Bool
```

Checks if the first unsigned value is less than the second unsigned value.

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
gt : (Int32, Int32) -> Bool
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

### Int32.**gtU**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
gtU : (Int32, Int32) -> Bool
```

Checks if the first unsigned value is greater than the second unsigned value.

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
lte : (Int32, Int32) -> Bool
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

### Int32.**lteU**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
lteU : (Int32, Int32) -> Bool
```

Checks if the first unsigned value is less than or equal to the second unsigned value.

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
gte : (Int32, Int32) -> Bool
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

### Int32.**gteU**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
gteU : (Int32, Int32) -> Bool
```

Checks if the first unsigned value is greater than or equal to the second unsigned value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first value|
|`y`|`Int32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

## Bitwise logic

Boolean operations on the bits of Int32 values.

### Int32.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lnot : Int32 -> Int32
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
land : (Int32, Int32) -> Int32
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
lor : (Int32, Int32) -> Int32
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
lxor : (Int32, Int32) -> Int32
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
clz : Int32 -> Int32
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
ctz : Int32 -> Int32
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
popcnt : Int32 -> Int32
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

