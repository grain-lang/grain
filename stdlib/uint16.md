---
title: Uint16
---

Utilities for working with the Uint16 type.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
include "uint16"
```

## Conversions

Functions for converting between Numbers and the Uint16 type.

### Uint16.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromNumber : Number -> Uint16
```

Converts a Number to an Uint16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The Number represented as an Uint16|

### Uint16.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toNumber : Uint16 -> Number
```

Converts an Uint16 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint16`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Uint16 represented as a Number|

### Uint16.**fromInt16**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromInt16 : Int16 -> Uint16
```

Converts an Int16 to a Uint16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Int16`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The Int16 represented as a Uint16|

## Operations

Mathematical operations for Uint16 values.

### Uint16.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
incr : Uint16 -> Uint16
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint16`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The incremented value|

### Uint16.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
decr : Uint16 -> Uint16
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint16`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The decremented value|

### Uint16.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(+) : (Uint16, Uint16) -> Uint16
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first operand|
|`y`|`Uint16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The sum of the two operands|

### Uint16.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(-) : (Uint16, Uint16) -> Uint16
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first operand|
|`y`|`Uint16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The difference of the two operands|

### Uint16.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(*) : (Uint16, Uint16) -> Uint16
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first operand|
|`y`|`Uint16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The product of the two operands|

### Uint16.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(/) : (Uint16, Uint16) -> Uint16
```

Computes the quotient of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first operand|
|`y`|`Uint16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The quotient of its operands|

### Uint16.**(%)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(%) : (Uint16, Uint16) -> Uint16
```

Computes the remainder of the division of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first operand|
|`y`|`Uint16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The remainder of its operands|

## Bitwise operations

Functions for operating on bits of Uint16 values.

### Uint16.**(<<)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<<) : (Uint16, Uint16) -> Uint16
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint16`|The value to shift|
|`amount`|`Uint16`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The shifted value|

### Uint16.**(>>)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>>) : (Uint16, Uint16) -> Uint16
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint16`|The value to shift|
|`amount`|`Uint16`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The shifted value|

## Comparisons

Functions for comparing Uint16 values.

### Uint16.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(==) : (Uint16, Uint16) -> Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first value|
|`y`|`Uint16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

### Uint16.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(!=) : (Uint16, Uint16) -> Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first value|
|`y`|`Uint16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

### Uint16.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<) : (Uint16, Uint16) -> Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first value|
|`y`|`Uint16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

### Uint16.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>) : (Uint16, Uint16) -> Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first value|
|`y`|`Uint16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

### Uint16.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<=) : (Uint16, Uint16) -> Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first value|
|`y`|`Uint16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

### Uint16.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>=) : (Uint16, Uint16) -> Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first value|
|`y`|`Uint16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

## Bitwise logic

Boolean operations on the bits of Uint16 values.

### Uint16.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lnot : Uint16 -> Uint16
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint16`|The given value|

Returns:

|type|description|
|----|-----------|
|`Uint16`|Containing the inverted bits of the given value|

### Uint16.**(&)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(&) : (Uint16, Uint16) -> Uint16
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first operand|
|`y`|`Uint16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint16`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

### Uint16.**(|)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(|) : (Uint16, Uint16) -> Uint16
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first operand|
|`y`|`Uint16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint16`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

### Uint16.**(^)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(^) : (Uint16, Uint16) -> Uint16
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint16`|The first operand|
|`y`|`Uint16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint16`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

