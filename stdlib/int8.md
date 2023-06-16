---
title: Int8
---

Utilities for working with the Int8 type.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
include "int8"
```

## Values

Functions and constants included in the Int8 module.

### Int8.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Int8
```

Converts a Number to an Int8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int8`|The Number represented as an Int8|

### Int8.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toNumber : (value: Int8) => Number
```

Converts an Int8 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int8`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int8 represented as a Number|

### Int8.**fromUint8**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromUint8 : (x: Uint8) => Int8
```

Converts a Uint8 to an Int8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Uint8`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int8`|The Uint8 represented as an Int8|

### Int8.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Int8) => Int8
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int8`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Int8`|The incremented value|

### Int8.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Int8) => Int8
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int8`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Int8`|The decremented value|

### Int8.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(+) : (x: Int8, y: Int8) => Int8
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|The sum of the two operands|

### Int8.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(-) : (x: Int8, y: Int8) => Int8
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|The difference of the two operands|

### Int8.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(*) : (x: Int8, y: Int8) => Int8
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|The product of the two operands|

### Int8.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(/) : (x: Int8, y: Int8) => Int8
```

Computes the quotient of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|The quotient of its operands|

### Int8.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
rem : (x: Int8, y: Int8) => Int8
```

Computes the remainder of the division of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|The remainder of its operands|

### Int8.**(%)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(%) : (x: Int8, y: Int8) => Int8
```

Computes the remainder of the division of the first operand by the second.
The result will have the sign of the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|The modulus of its operands|

Throws:

`ModuloByZero`

* When `y` is zero

### Int8.**(<<)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<<) : (value: Int8, amount: Int8) => Int8
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int8`|The value to shift|
|`amount`|`Int8`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Int8`|The shifted value|

### Int8.**(>>)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>>) : (value: Int8, amount: Int8) => Int8
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int8`|The value to shift|
|`amount`|`Int8`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Int8`|The shifted value|

### Int8.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(==) : (x: Int8, y: Int8) => Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first value|
|`y`|`Int8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

### Int8.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(!=) : (x: Int8, y: Int8) => Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first value|
|`y`|`Int8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

### Int8.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<) : (x: Int8, y: Int8) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first value|
|`y`|`Int8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

### Int8.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>) : (x: Int8, y: Int8) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first value|
|`y`|`Int8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

### Int8.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<=) : (x: Int8, y: Int8) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first value|
|`y`|`Int8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

### Int8.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>=) : (x: Int8, y: Int8) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first value|
|`y`|`Int8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

### Int8.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Int8) => Int8
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int8`|The given value|

Returns:

|type|description|
|----|-----------|
|`Int8`|Containing the inverted bits of the given value|

### Int8.**(&)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(&) : (x: Int8, y: Int8) => Int8
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

### Int8.**(|)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(|) : (x: Int8, y: Int8) => Int8
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

### Int8.**(^)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(^) : (x: Int8, y: Int8) => Int8
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int8`|The first operand|
|`y`|`Int8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int8`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

