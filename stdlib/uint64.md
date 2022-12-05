---
title: Uint64
---

Utilities for working with the Uint64 type.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
include "uint64"
```

## Conversions

Functions for converting between Numbers and the Uint64 type.

### Uint64.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromNumber : Number -> Uint64
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toNumber : Uint64 -> Number
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

## Operations

Mathematical operations for Uint64 values.

### Uint64.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
incr : Uint64 -> Uint64
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
decr : Uint64 -> Uint64
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

### Uint64.**add**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
add : (Uint64, Uint64) -> Uint64
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

### Uint64.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
sub : (Uint64, Uint64) -> Uint64
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

### Uint64.**mul**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
mul : (Uint64, Uint64) -> Uint64
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

### Uint64.**div**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
div : (Uint64, Uint64) -> Uint64
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
rem : (Uint64, Uint64) -> Uint64
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

## Bitwise operations

Functions for operating on bits of Uint64 values.

### Uint64.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
rotl : (Uint64, Uint64) -> Uint64
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
rotr : (Uint64, Uint64) -> Uint64
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

### Uint64.**shl**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
shl : (Uint64, Uint64) -> Uint64
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

### Uint64.**shr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
shr : (Uint64, Uint64) -> Uint64
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

## Comparisons

Functions for comparing Uint64 values.

### Uint64.**eq**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
eq : (Uint64, Uint64) -> Bool
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

### Uint64.**ne**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
ne : (Uint64, Uint64) -> Bool
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
eqz : Uint64 -> Bool
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

### Uint64.**lt**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lt : (Uint64, Uint64) -> Bool
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

### Uint64.**gt**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
gt : (Uint64, Uint64) -> Bool
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

### Uint64.**lte**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lte : (Uint64, Uint64) -> Bool
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

### Uint64.**gte**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
gte : (Uint64, Uint64) -> Bool
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

## Bitwise logic

Boolean operations on the bits of Uint64 values.

### Uint64.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lnot : Uint64 -> Uint64
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

### Uint64.**land**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
land : (Uint64, Uint64) -> Uint64
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

### Uint64.**lor**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lor : (Uint64, Uint64) -> Uint64
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

### Uint64.**lxor**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lxor : (Uint64, Uint64) -> Uint64
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
clz : Uint64 -> Uint64
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
ctz : Uint64 -> Uint64
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
popcnt : Uint64 -> Uint64
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

