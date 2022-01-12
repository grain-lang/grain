---
title: Int64
---

Utilities for working with the Int64 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
import Int64 from "int64"
```

## Conversions

Functions for converting between Numbers and the Int64 type.

### Int64.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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

### Int64.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : Int64 -> Number
```

Converts an Int64 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`float`|`Int64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int64 represented as a Number|

## Operations

Mathematical operations for Int64 values.

### Int64.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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
|`Int64`|The value incremented by one|

### Int64.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
decr : Int64 -> Int64
```

Decremented the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Int64`|The value decremented by one|

### Int64.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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

### Int64.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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

### Int64.**mul**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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

### Int64.**div**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
div : (Int64, Int64) -> Int64
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
|`Int64`|The quotient of its operands using signed division|

### Int64.**divU**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
divU : (Int64, Int64) -> Int64
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
|`Int64`|The quotient of its operands using unsigned division|

### Int64.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
rem : (Int64, Int64) -> Int64
```

Computes the remainder of the signed division of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The remainder of the signed division of its operands|

### Int64.**remU**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
remU : (Int64, Int64) -> Int64
```

Computes the remainder of the unsigned division of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The remainder of the unsigned division of its operands.|

### Int64.**mod**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
( mod ) : (Int64, Int64) -> Int64
```

Computer the modulo of a division of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The modulo of a division of its operands|

### Int64.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotl : (Int64, Int64) -> Int64
```

Rotates the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to rotate|
|`shiftCount`|`Int64`|The shiftCount|

Returns:

|type|description|
|----|-----------|
|`Int64`|The value rotated to the left by the shiftCount|

### Int64.**rotr**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotr : (Int64, Int64) -> Int64
```

Rotates the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to rotate|
|`shiftCount`|`Int64`|The shiftCount|

Returns:

|type|description|
|----|-----------|
|`Int64`|The value rotated to the right by the shiftCount|

### Int64.**shl**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
shl : (Int64, Int64) -> Int64
```

Shifts the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to shift|
|`shiftCount`|`Int64`|The shiftCount|

Returns:

|type|description|
|----|-----------|
|`Int64`|The value shifted to the left by the shiftCount|

### Int64.**shr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
shr : (Int64, Int64) -> Int64
```

Shifts the value right by the given signed number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to shift|
|`shiftCount`|`Int64`|The signed shiftCount|

Returns:

|type|description|
|----|-----------|
|`Int64`|The value shifted to the right by the signed shiftCount|

### Int64.**shrU**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
shrU : (Int64, Int64) -> Int64
```

Shifts the value right by the given unsigned number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to shift|
|`shiftCount`|`Int64`|The unsigned shiftCount|

Returns:

|type|description|
|----|-----------|
|`Int64`|The value shifted to the right by the unsigned shiftCount|

## Comparisons

Functions for comparing Int64 values.

### Int64.**eq**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
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

### Int64.**ne**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
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

### Int64.**eqz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
eqz : Int64 -> Bool
```

Checks if the given value equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to zero or `false` otherwise|

### Int64.**lt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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

### Int64.**gt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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

### Int64.**lte**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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

### Int64.**gte**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
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

## Logical

Int64 Logical Functions.

### Int64.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lnot : Int64 -> Int64
```

Computes the bitwise logical "not" of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The given value|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical "not" of the given value.|

### Int64.**land**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
( land ) : (Int64, Int64) -> Int64
```

Computes the bitwise logical "and" on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical "and" of the given operands.|

### Int64.**lor**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
( lor ) : (Int64, Int64) -> Int64
```

Computes the bitwise logical "or" on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical "or" of the given operands.|

## Inspections

Functions for inspecting Int64 values.

### Int64.**lxor**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
( lxor ) : (Int64, Int64) -> Int64
```

Computes the bitwise logical "xor" on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical "xor" of the given operands.|

### Int64.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
clz : Int64 -> Int64
```

Counts the number of leading zeros in value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int64`|Values count of leading zeros|

### Int64.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
ctz : Int64 -> Int64
```

Counts the number of trailing zeros in value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int64`|Values count of trailing zeros|

### Int64.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
popcnt : Int64 -> Int64
```

The number of 1-bits in value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int64`|The number of 1-bits in its operand|

