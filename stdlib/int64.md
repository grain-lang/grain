---
title: Int64
---

Utilities for working with the Int64 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
from "int64" include Int64
```

## Values

Functions and constants included in the Int64 module.

### Int64.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Int64
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
toNumber : (value: Int64) => Number
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

### Int64.**fromUint64**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromUint64 : (number: Uint64) => Int64
```

Converts a Uint64 to an Int64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Uint64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int64`|The Uint64 represented as an Int64|

### Int64.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Int64) => Int64
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

### Int64.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Int64) => Int64
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

### Int64.**(+)**

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
(+) : (x: Int64, y: Int64) => Int64
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

### Int64.**(-)**

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
(-) : (x: Int64, y: Int64) => Int64
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

### Int64.**(*)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `*`</td></tr>
</tbody>
</table>
</details>

```grain
(*) : (x: Int64, y: Int64) => Int64
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

### Int64.**(/)**

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
(/) : (x: Int64, y: Int64) => Int64
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

### Int64.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
rem : (x: Int64, y: Int64) => Int64
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

### Int64.**(%)**

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
(%) : (x: Int64, y: Int64) => Int64
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

Throws:

`ModuloByZero`

* When `y` is zero

### Int64.**rotl**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotl : (value: Int64, amount: Int64) => Int64
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

### Int64.**rotr**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
rotr : (value: Int64, amount: Int64) => Int64
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

### Int64.**(<<)**

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
(<<) : (value: Int64, amount: Int64) => Int64
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

### Int64.**(>>)**

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
(>>) : (value: Int64, amount: Int64) => Int64
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to shift|
|`amount`|`Int64`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Int64`|The shifted value|

### Int64.**(==)**

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
(==) : (x: Int64, y: Int64) => Bool
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

### Int64.**(!=)**

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
(!=) : (x: Int64, y: Int64) => Bool
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
eqz : (value: Int64) => Bool
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

### Int64.**(<)**

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
(<) : (x: Int64, y: Int64) => Bool
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

### Int64.**(>)**

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
(>) : (x: Int64, y: Int64) => Bool
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

### Int64.**(<=)**

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
(<=) : (x: Int64, y: Int64) => Bool
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

### Int64.**(>=)**

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
(>=) : (x: Int64, y: Int64) => Bool
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

### Int64.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Int64) => Int64
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

### Int64.**(&)**

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
(&) : (x: Int64, y: Int64) => Int64
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

### Int64.**(|)**

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
(|) : (x: Int64, y: Int64) => Int64
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

### Int64.**(^)**

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
(^) : (x: Int64, y: Int64) => Int64
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

### Int64.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
clz : (value: Int64) => Int64
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

### Int64.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
ctz : (value: Int64) => Int64
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

### Int64.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
popcnt : (value: Int64) => Int64
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

### Int64.**(\*\*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(**) : (base: Int64, power: Int64) => Int64
```

Computes the exponentiation of the given base and power.

Parameters:

|param|type|description|
|-----|----|-----------|
|`base`|`Int64`|The base number|
|`power`|`Int64`|The exponent number|

Returns:

|type|description|
|----|-----------|
|`Int64`|The base raised to the given power|

