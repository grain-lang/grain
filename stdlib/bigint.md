---
title: BigInt
---

Utilities for working with the BigInt type.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
from "bigint" include Bigint
```

```grain
9223372036854775809t
```

```grain
0t
```

```grain
-9223372036854775809t
```

## Values

Functions and constants included in the BigInt module.

### BigInt.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => BigInt
```

Converts a Number to a BigInt.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The Number represented as a BigInt|

### BigInt.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : (num: BigInt) => Number
```

Converts a BigInt to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The BigInt represented as a Number|

### BigInt.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
incr : (num: BigInt) => BigInt
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The incremented value|

Examples:

```grain
BigInt.incr(1t) == 2t
```

```grain
BigInt.incr(-2t) == -1t
```

### BigInt.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
decr : (num: BigInt) => BigInt
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The decremented value|

Examples:

```grain
BigInt.decr(2t) == 1t
```

```grain
BigInt.decr(-2t) == -3t
```

### BigInt.**neg**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
neg : (num: BigInt) => BigInt
```

Negates the given operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The negated operand|

Examples:

```grain
BigInt.neg(1t) == -1t
```

```grain
BigInt.neg(-1t) == 1t
```

### BigInt.**abs**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
abs : (num: BigInt) => BigInt
```

Returns the absolute value of the given operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The operand's absolute value|

Examples:

```grain
BigInt.abs(1t) == 1t
```

```grain
BigInt.abs(-1t) == 1t
```

### BigInt.**(+)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `add`</td></tr>
</tbody>
</table>
</details>

```grain
(+) : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The sum of the two operands|

Examples:

```grain
use BigInt.{ (+) }
assert 1t + 1t == 2t
```

### BigInt.**(-)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `sub`</td></tr>
</tbody>
</table>
</details>

```grain
(-) : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The difference of the two operands|

Examples:

```grain
use BigInt.{ (-) }
assert 3t - 1t == 2t
```

### BigInt.**(*)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `mul`</td></tr>
</tbody>
</table>
</details>

```grain
(*) : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The product of the two operands|

Examples:

```grain
use BigInt.{ (*) }
assert 3t * 3t == 9t
```

### BigInt.**(/)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `div`</td></tr>
</tbody>
</table>
</details>

```grain
(/) : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the quotient of its operands using signed (truncated) division
(in which the quotient is always rounded towards zero).

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The quotient of its operands|

Examples:

```grain
use BigInt.{ (/) }
assert 9t / 3t == 3t
```

### BigInt.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
rem : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the remainder of the division of its operands using signed (truncated) division
(in which the quotient is always rounded towards zero).

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The remainder of its operands|

Examples:

```grain
BigInt.rem(3t, 2t) == 1t
```

### BigInt.**quotRem**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
quotRem : (num1: BigInt, num2: BigInt) => (BigInt, BigInt)
```

Computes the quotient and remainder of its operands using signed (truncated) division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`(BigInt, BigInt)`|The quotient and remainder of its operands|

Examples:

```grain
BigInt.quotRem(7t, 2t) == (3t, 1t))
```

### BigInt.**gcd**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
gcd : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the greatest common divisior of the two operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The greatest common divisor of its operands|

Examples:

```grain
BigInt.gcd(36t, 24t) == 12t
```

### BigInt.**(<<)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `shl`</td></tr>
</tbody>
</table>
</details>

```grain
(<<) : (num: BigInt, places: Int32) => BigInt
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to shift|
|`places`|`Int32`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The shifted value|

Examples:

```grain
use BigInt.{ (<<) }
assert (10t << 2l) == 40t
```

### BigInt.**(>>)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `shr`</td></tr>
</tbody>
</table>
</details>

```grain
(>>) : (num: BigInt, places: Int32) => BigInt
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to shift|
|`places`|`Int32`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The shifted value|

Examples:

```grain
use BigInt.{ (>>) }
assert (9999t >> 2l) == 2499t
```

### BigInt.**eqz**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
eqz : (num: BigInt) => Bool
```

Checks if the given value is equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to zero or `false` otherwise|

Examples:

```grain
assert BigInt.eqz(0t) == true
```

```grain
assert BigInt.eqz(1t) == false
```

### BigInt.**(==)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `eq`</td></tr>
</tbody>
</table>
</details>

```grain
(==) : (num1: BigInt, num2: BigInt) => Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

Examples:

```grain
use BigInt.{ (==) }
assert 1t == 1t
```

```grain
use BigInt.{ (==) }
assert -10t == -10t
```

### BigInt.**(!=)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `ne`</td></tr>
</tbody>
</table>
</details>

```grain
(!=) : (num1: BigInt, num2: BigInt) => Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

Examples:

```grain
use BigInt.{ (!=) }
assert 1t != 2t
```

```grain
use BigInt.{ (!=) }
assert -10t != -20t
```

### BigInt.**(<)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `lt`</td></tr>
</tbody>
</table>
</details>

```grain
(<) : (num1: BigInt, num2: BigInt) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

Examples:

```grain
use BigInt.{ (<) }
assert 1t < 2t
```

```grain
use BigInt.{ (<) }
assert -10t < 0t
```

### BigInt.**(<=)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `lte`</td></tr>
</tbody>
</table>
</details>

```grain
(<=) : (num1: BigInt, num2: BigInt) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

Examples:

```grain
use BigInt.{ (<=) }
assert 1t <= 1t
```

```grain
use BigInt.{ (<=) }
assert -10t <= 0t
```

```grain
use BigInt.{ (<=) }
assert 2t <= 3t
```

### BigInt.**(>)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `gt`</td></tr>
</tbody>
</table>
</details>

```grain
(>) : (num1: BigInt, num2: BigInt) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

Examples:

```grain
use BigInt.{ (>) }
assert 2t > 1t
```

```grain
use BigInt.{ (>) }
assert 0t > -10t
```

### BigInt.**(>=)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `gte`</td></tr>
</tbody>
</table>
</details>

```grain
(>=) : (num1: BigInt, num2: BigInt) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first value|
|`num2`|`BigInt`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

Examples:

```grain
use BigInt.{ (>=) }
assert 1t >= 1t
```

```grain
use BigInt.{ (>=) }
assert 0t >= -10t
```

```grain
use BigInt.{ (>=) }
assert 3t >= 2t
```

### BigInt.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (num: BigInt) => BigInt
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The given value|

Returns:

|type|description|
|----|-----------|
|`BigInt`|Containing the inverted bits of the given value|

Examples:

```grain
BigInt.lnot(91234t) == -91235t
```

### BigInt.**(&)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `land`</td></tr>
</tbody>
</table>
</details>

```grain
(&) : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

Examples:

```grain
use BigInt.{ (&) }
assert (4t & 3t) == 0t
```

### BigInt.**(|)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `lor`</td></tr>
</tbody>
</table>
</details>

```grain
(|) : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

Examples:

```grain
use BigInt.{ (|) }
assert (5t | 3t) == 7t
```

### BigInt.**(^)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Originally named `lxor`</td></tr>
</tbody>
</table>
</details>

```grain
(^) : (num1: BigInt, num2: BigInt) => BigInt
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`BigInt`|The first operand|
|`num2`|`BigInt`|The second operand|

Returns:

|type|description|
|----|-----------|
|`BigInt`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

Examples:

```grain
use BigInt.{ (^) }
assert (5t ^ 3t) == 6t
```

### BigInt.**clz**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
clz : (num: BigInt) => Int32
```

Counts the number of leading zero bits in the value.
Will return the maximum integer for negative numbers.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int32`|The amount of leading zeros|

Examples:

```grain
BigInt.clz(5t) == 2147483647t
```

### BigInt.**ctz**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
ctz : (num: BigInt) => Int64
```

Counts the number of trailing zero bits in the value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Int64`|The amount of trailing zeros|

Examples:

```grain
BigInt.ctz(14t) == 1t
```

### BigInt.**popcnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
popcnt : (num: BigInt) => Option<Int64>
```

Counts the number of bits set to `1` in the value, also known as a population count.
Will return the `None` if given a negative integer

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<Int64>`|The amount of 1-bits in its operand|

Examples:

```grain
BigInt.popcnt(14t) == 1t
```

### BigInt.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
toString : (num: BigInt) => String
```

Converts the given operand to a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The operand|

Returns:

|type|description|
|----|-----------|
|`String`|The operand, as a string|

Examples:

```grain
BigInt.toString(1t) == "1"
```

```grain
BigInt.toString(-1t) == "-1"
```

