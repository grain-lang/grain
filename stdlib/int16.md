---
title: Int16
---

Utilities for working with the Int16 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
from "int16" include Int16
```

```grain
1S
```

```grain
-1S
```

## Values

Functions and constants included in the Int16 module.

### Int16.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Int16
```

Converts a Number to an Int16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int16`|The Number represented as an Int16|

### Int16.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : (value: Int16) => Number
```

Converts an Int16 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int16`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int16 represented as a Number|

### Int16.**fromUint16**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromUint16 : (number: Uint16) => Int16
```

Converts a Uint16 to an Int16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Uint16`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int16`|The Uint16 represented as an Int16|

Examples:

```grain
Int16.fromUint16(1uS) == 1S
```

### Int16.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Int16) => Int16
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int16`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Int16`|The incremented value|

Examples:

```grain
Int16.incr(1S) == 2S
```

```grain
Int16.incr(-2S) == -1S
```

### Int16.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Int16) => Int16
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int16`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Int16`|The decremented value|

Examples:

```grain
Int16.decr(2S) == 1S
```

```grain
Int16.decr(0S) == -1S
```

### Int16.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(+) : (x: Int16, y: Int16) => Int16
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|The sum of the two operands|

Examples:

```grain
use Int16.{ (+) }
assert 1S + 1S == 2S
```

### Int16.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(-) : (x: Int16, y: Int16) => Int16
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|The difference of the two operands|

Examples:

```grain
use Int16.{ (-) }
assert 2S - 1S == 1S
```

### Int16.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(*) : (x: Int16, y: Int16) => Int16
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|The product of the two operands|

Examples:

```grain
use Int16.{ (*) }
assert 2S * 2S == 4S
```

### Int16.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(/) : (x: Int16, y: Int16) => Int16
```

Computes the quotient of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|The quotient of its operands|

Examples:

```grain
use Int16.{ (/) }
assert 8S / 2S == 4S
```

### Int16.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rem : (x: Int16, y: Int16) => Int16
```

Computes the remainder of the division of its operands using signed division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|The remainder of its operands|

Examples:

```grain
Int16.rem(8S, 3S) == 2S
```

### Int16.**(%)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(%) : (x: Int16, y: Int16) => Int16
```

Computes the remainder of the division of the first operand by the second.
The result will have the sign of the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|The modulus of its operands|

Throws:

`ModuloByZero`

* When `y` is zero

Examples:

```grain
use Int16.{ (%) }
assert -5S % 3S == 1S
```

### Int16.**(<<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<<) : (value: Int16, amount: Int16) => Int16
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int16`|The value to shift|
|`amount`|`Int16`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Int16`|The shifted value|

Examples:

```grain
use Int16.{ (<<) }
assert (5S << 1S) == 10S
```

### Int16.**(>>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>>) : (value: Int16, amount: Int16) => Int16
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int16`|The value to shift|
|`amount`|`Int16`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Int16`|The shifted value|

Examples:

```grain
use Int16.{ (>>) }
assert (5S >> 1S) == 2S
```

### Int16.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(==) : (x: Int16, y: Int16) => Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first value|
|`y`|`Int16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

Examples:

```grain
use Int16.{ (==) }
assert 1S == 1S
```

### Int16.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(!=) : (x: Int16, y: Int16) => Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first value|
|`y`|`Int16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

Examples:

```grain
use Int16.{ (!=) }
assert 1S != 2S
```

### Int16.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<) : (x: Int16, y: Int16) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first value|
|`y`|`Int16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

Examples:

```grain
use Int16.{ (<) }
assert 1S < 2S
```

### Int16.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>) : (x: Int16, y: Int16) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first value|
|`y`|`Int16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

Examples:

```grain
use Int16.{ (>) }
assert 2S > 1S
```

### Int16.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<=) : (x: Int16, y: Int16) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first value|
|`y`|`Int16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

Examples:

```grain
use Int16.{ (<=) }
assert 1S <= 2S
```

```grain
use Int16.{ (<=) }
assert 1S <= 1S
```

### Int16.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>=) : (x: Int16, y: Int16) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first value|
|`y`|`Int16`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

Examples:

```grain
use Int16.{ (>=) }
assert 2S >= 1S
```

```grain
use Int16.{ (>=) }
assert 1S >= 1S
```

### Int16.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Int16) => Int16
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int16`|The given value|

Returns:

|type|description|
|----|-----------|
|`Int16`|Containing the inverted bits of the given value|

Examples:

```grain
Int16.lnot(-5S) == 4S
```

### Int16.**(&)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(&) : (x: Int16, y: Int16) => Int16
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

Examples:

```grain
use Int16.{ (&) }
assert (3S & 4S) == 0S
```

### Int16.**(|)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(|) : (x: Int16, y: Int16) => Int16
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

Examples:

```grain
use Int16.{ (|) }
assert (3S | 4S) == 7S
```

### Int16.**(^)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(^) : (x: Int16, y: Int16) => Int16
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int16`|The first operand|
|`y`|`Int16`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int16`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

Examples:

```grain
use Int16.{ (^) }
assert (3S ^ 5S) == 6S
```

