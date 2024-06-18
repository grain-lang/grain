---
title: Int32
---

Utilities for working with the Int32 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
from "int32" include Int32
```

```grain
1l
```

```grain
-1l
```

## Values

Functions and constants included in the Int32 module.

### Int32.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Int32
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
toNumber : (value: Int32) => Number
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
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromUint32 : (number: Uint32) => Int32
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

Examples:

```grain
Int32.fromUint32(1ul) == 1l
```

### Int32.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Int32) => Int32
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

Examples:

```grain
Int32.incr(1l) == 2l
```

```grain
Int32.incr(-2l) == -1l
```

### Int32.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Int32) => Int32
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

Examples:

```grain
Int32.decr(2l) == 1l
```

```grain
Int32.decr(0l) == -1l
```

### Int32.**(+)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `add`</td></tr>
</tbody>
</table>
</details>

```grain
(+) : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
use Int32.{ (+) }
assert 1l + 1l == 2l
```

### Int32.**(-)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `sub`</td></tr>
</tbody>
</table>
</details>

```grain
(-) : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
use Int32.{ (-) }
assert 2l - 1l == 1l
```

### Int32.**(*)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `mul`</td></tr>
</tbody>
</table>
</details>

```grain
(*) : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
use Int32.{ (*) }
assert 2l * 2l == 4l
```

### Int32.**(/)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `div`</td></tr>
</tbody>
</table>
</details>

```grain
(/) : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
use Int32.{ (/) }
assert 8l / 2l == 4l
```

### Int32.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
rem : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
Int32.rem(8l, 3l) == 2l
```

### Int32.**(%)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `mod`</td></tr>
</tbody>
</table>
</details>

```grain
(%) : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
use Int32.{ (%) }
assert -5l % 3l == 1l
```

### Int32.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotl : (value: Int32, amount: Int32) => Int32
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

Examples:

```grain
Int32.rotl(1l, 1l) == 2l
```

```grain
Int32.rotl(1l, 2l) == 4l
```

### Int32.**rotr**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotr : (value: Int32, amount: Int32) => Int32
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

Examples:

```grain
Int32.rotr(2l, 1l) == 1l
```

```grain
Int32.rotr(4l, 2l) == 1l
```

### Int32.**(<<)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `shl`</td></tr>
</tbody>
</table>
</details>

```grain
(<<) : (value: Int32, amount: Int32) => Int32
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

Examples:

```grain
use Int32.{ (<<) }
assert (5l << 1l) == 10l
```

### Int32.**(>>)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `shr`</td></tr>
</tbody>
</table>
</details>

```grain
(>>) : (value: Int32, amount: Int32) => Int32
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

Examples:

```grain
use Int32.{ (>>) }
assert (5l >> 1l) == 2l
```

### Int32.**(==)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally named `eq`</td></tr>
</tbody>
</table>
</details>

```grain
(==) : (x: Int32, y: Int32) => Bool
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

Examples:

```grain
use Int32.{ (==) }
assert 1l == 1l
```

### Int32.**(!=)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally named `ne`</td></tr>
</tbody>
</table>
</details>

```grain
(!=) : (x: Int32, y: Int32) => Bool
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

Examples:

```grain
use Int32.{ (!=) }
assert 1l != 2l
```

### Int32.**eqz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
eqz : (value: Int32) => Bool
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

Examples:

```grain
Int32.eqz(0l) == true
```

```grain
Int32.eqz(1l) == false
```

### Int32.**(<)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `lt`</td></tr>
</tbody>
</table>
</details>

```grain
(<) : (x: Int32, y: Int32) => Bool
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

Examples:

```grain
use Int32.{ (<) }
assert 1l < 2l
```

### Int32.**(>)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `gt`</td></tr>
</tbody>
</table>
</details>

```grain
(>) : (x: Int32, y: Int32) => Bool
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

Examples:

```grain
use Int32.{ (>) }
assert 2l > 1l
```

### Int32.**(<=)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `lte`</td></tr>
</tbody>
</table>
</details>

```grain
(<=) : (x: Int32, y: Int32) => Bool
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

Examples:

```grain
use Int32.{ (<=) }
assert 1l <= 2l
```

```grain
use Int32.{ (<=) }
assert 1l <= 1l
```

### Int32.**(>=)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `gte`</td></tr>
</tbody>
</table>
</details>

```grain
(>=) : (x: Int32, y: Int32) => Bool
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

Examples:

```grain
use Int32.{ (>=) }
assert 2l >= 1l
```

```grain
use Int32.{ (>=) }
assert 1l >= 1l
```

### Int32.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Int32) => Int32
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

Examples:

```grain
Int32.lnot(-5l) == 4l
```

### Int32.**(&)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `land`</td></tr>
</tbody>
</table>
</details>

```grain
(&) : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
use Int32.{ (&) }
assert (3l & 4l) == 0l
```

### Int32.**(|)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `lor`</td></tr>
</tbody>
</table>
</details>

```grain
(|) : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
use Int32.{ (|) }
assert (3l | 4l) == 7l
```

### Int32.**(^)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `lxor`</td></tr>
</tbody>
</table>
</details>

```grain
(^) : (x: Int32, y: Int32) => Int32
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

Examples:

```grain
use Int32.{ (^) }
assert (3l ^ 5l) == 6l
```

### Int32.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
clz : (value: Int32) => Int32
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

Examples:

```grain
Int32.clz(1l) == 31l
```

```grain
Int32.clz(4l) == 29l
```

### Int32.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
ctz : (value: Int32) => Int32
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

Examples:

```grain
Int32.ctz(1l) == 0l
```

```grain
Int32.ctz(4l) == 2l
```

### Int32.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
popcnt : (value: Int32) => Int32
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

Examples:

```grain
Int32.popcnt(1l) == 1l
```

```grain
Int32.popcnt(3l) == 2l
```

### Int32.**(\*\*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(**) : (base: Int32, power: Int32) => Int32
```

Computes the exponentiation of the given base and power.

Parameters:

|param|type|description|
|-----|----|-----------|
|`base`|`Int32`|The base number|
|`power`|`Int32`|The exponent number|

Returns:

|type|description|
|----|-----------|
|`Int32`|The base raised to the given power|

Examples:

```grain
from Int32 use { (**) }
assert 2l ** 3l == 8l
```

