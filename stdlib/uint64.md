---
title: Uint65
---

Unsigned utilities for working with the Int64 type.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
import Uint64 from "uint64"
```

## Conversions

Functions for converting between Numbers and the Int64 type.

### Uint64.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromNumber : Number -> Int64
```

Converts a Number to an Int64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int64`|The Number represented as an Int64|

### Uint64.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toNumber : Int64 -> Number
```

Converts an Int64 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int64 represented as a Number|

## Operations

Mathematical operations for Int64 values.

### Uint64.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
incr : Int64 -> Int64
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Int64`|The incremented value|

### Uint64.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
decr : Int64 -> Int64
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Int64`|The decremented value|

### Uint64.**add**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
add : (Int64, Int64) -> Int64
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The sum of the two operands|

### Uint64.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
sub : (Int64, Int64) -> Int64
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The difference of the two operands|

### Uint64.**mul**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
mul : (Int64, Int64) -> Int64
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The product of the two operands|

### Uint64.**div**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
div : (Int64, Int64) -> Int64
```

Computes the quotient of its operands using unsigned division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The quotient of its operands|

### Uint64.**divS**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
divS : (Int64, Int64) -> Int64
```

Computes the quotient of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The quotient of its operands|

### Uint64.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
rem : (Int64, Int64) -> Int64
```

Computes the remainder of the division of its operands using unsigned division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The remainder of its operands|

### Uint64.**remS**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
remS : (Int64, Int64) -> Int64
```

Computes the remainder of the division of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The remainder of its operands|

### Uint64.**mod**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
mod : (Int64, Int64) -> Int64
```

Computes the remainder of the division of the first operand by the second.
The result will have the sign of the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The modulus of its operands|

## Bitwise operations

Functions for operating on bits of Int64 values.

### Uint64.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
rotl : (Int64, Int64) -> Int64
```

Rotates the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to rotate|
|`amount`|`Int64`|The number of bits to rotate by|

Returns:

|type|description|
|----|-----------|
|`Int64`|The rotated value|

### Uint64.**rotr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
rotr : (Int64, Int64) -> Int64
```

Rotates the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to rotate|
|`amount`|`Int64`|The number of bits to rotate by|

Returns:

|type|description|
|----|-----------|
|`Int64`|The rotated value|

### Uint64.**shl**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
shl : (Int64, Int64) -> Int64
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to shift|
|`amount`|`Int64`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Int64`|The shifted value|

### Uint64.**shr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
shr : (Int64, Int64) -> Int64
```

Shifts the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to shift|
|`amount`|`Int64`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Int64`|The shifted value|

### Uint64.**shrS**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
shrS : (Int64, Int64) -> Int64
```

Shifts the bits of the value right by the given number of bits (signed).

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to shift|
|`amount`|`Int64`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Int64`|The shifted value|

## Comparisons

Functions for comparing Int64 values.

### Uint64.**eq**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
eq : (Int64, Int64) -> Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

### Uint64.**ne**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
ne : (Int64, Int64) -> Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

### Uint64.**eqz**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
eqz : Int64 -> Bool
```

Checks if the given value is equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to zero or `false` otherwise|

### Uint64.**lt**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lt : (Int64, Int64) -> Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

### Uint64.**ltS**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
ltS : (Int64, Int64) -> Bool
```

Checks if the first signed value is less than the second signed value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

### Uint64.**gt**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
gt : (Int64, Int64) -> Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

### Uint64.**gtS**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
gtS : (Int64, Int64) -> Bool
```

Checks if the first signed value is greater than the second signed value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

### Uint64.**lte**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lte : (Int64, Int64) -> Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

### Uint64.**lteS**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lteS : (Int64, Int64) -> Bool
```

Checks if the first signed value is less than or equal to the second signed value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

### Uint64.**gte**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
gte : (Int64, Int64) -> Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

### Uint64.**gteS**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
gteS : (Int64, Int64) -> Bool
```

Checks if the first signed value is greater than or equal to the second signed value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first value|
|`y`|`Int64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

## Bitwise logic

Boolean operations on the bits of Int64 values.

### Uint64.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lnot : Int64 -> Int64
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The given value|

Returns:

|type|description|
|----|-----------|
|`Int64`|Containing the inverted bits of the given value|

### Uint64.**land**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
land : (Int64, Int64) -> Int64
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

### Uint64.**lor**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lor : (Int64, Int64) -> Int64
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

### Uint64.**lxor**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lxor : (Int64, Int64) -> Int64
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

### Uint64.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
clz : Int64 -> Int64
```

Counts the number of leading zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int64`|The amount of leading zeros|

### Uint64.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
ctz : Int64 -> Int64
```

Counts the number of trailing zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int64`|The amount of trailing zeros|

### Uint64.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
popcnt : Int64 -> Int64
```

Counts the number of bits set to `1` in the value, also known as a population count.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int64`|The amount of 1-bits in its operand|

