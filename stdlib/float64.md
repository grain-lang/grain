---
title: Float64
---

Utilities for working with the Float64 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
from "float64" include Float64
```

```grain
5.0d
```

```grain
-5.0d
```

```grain
Infinityd
```

```grain
NaNd
```

## Values

Functions and constants included in the Float64 module.

### Float64.**infinity**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
infinity: Float64
```

Infinity represented as a Float64 value.
This is an alternative to the `Infinityd` literal.

### Float64.**nan**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
nan: Float64
```

NaN (Not a Number) represented as a Float64 value.
This is an alternative to the `NaNd` literal.

### Float64.**pi**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
pi: Float64
```

Pi represented as a Float64 value.

### Float64.**tau**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
tau: Float64
```

Tau represented as a Float64 value.

### Float64.**e**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
e: Float64
```

Euler's number represented as a Float64 value.

### Float64.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber: (number: Number) => Float64
```

Converts a Number to a Float64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Float64`|The Number represented as a Float64|

### Float64.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toNumber: (float: Float64) => Number
```

Converts a Float64 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`float`|`Float64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Float64 represented as a Number|

### Float64.**reinterpretInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
reinterpretInt64: (value: Int64) => Float64
```

Interprets an Int64 as a Float64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Float64`|The Int64 interpreted as an Float64|

Examples:

```grain
assert Float64.reinterpretInt64(4607182418800017408L) == 1.0d
```

```grain
assert Float64.reinterpretInt64(-4616189618054758400L) == -1.0d
```

### Float64.**reinterpretUint64**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
reinterpretUint64: (value: Uint64) => Float64
```

Interprets an Uint64 as a Float64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Float64`|The Uint64 interpreted as an Float64|

Examples:

```grain
assert Float64.reinterpretUint64(4607182418800017408uL) == 1.0d
```

```grain
assert Float64.reinterpretUint64(13830554455654793216uL) == -1.0d
```

### Float64.**(+)**

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
(+): (x: Float64, y: Float64) => Float64
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The sum of the two operands|

Examples:

```grain
use Float64.{ (+) }
assert 1.0d + 1.0d == 2.0d
```

### Float64.**(-)**

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
(-): (x: Float64, y: Float64) => Float64
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The difference of the two operands|

Examples:

```grain
use Float64.{ (-) }
assert 5.0d - 4.0d == 1.0d
```

### Float64.**(*)**

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
(*): (x: Float64, y: Float64) => Float64
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The product of the two operands|

Examples:

```grain
use Float64.{ (*) }
assert -5.0d * 4.0d == -20.0d
```

### Float64.**(/)**

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
(/): (x: Float64, y: Float64) => Float64
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The quotient of the two operands|

Examples:

```grain
use Float64.{ (/) }
assert 25.0d / 4.0d == 6.25d
```

### Float64.**(\*\*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
(**): (base: Float64, power: Float64) => Float64
```

Computes the exponentiation of the given base and power.

Parameters:

|param|type|description|
|-----|----|-----------|
|`base`|`Float64`|The base float|
|`power`|`Float64`|The exponent float|

Returns:

|type|description|
|----|-----------|
|`Float64`|The base raised to the given power|

Examples:

```grain
use Float64.{ (**) }
assert 2.0d ** 2.0d == 4.0d
```

### Float64.**(<)**

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
(<): (x: Float64, y: Float64) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first value|
|`y`|`Float64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

Examples:

```grain
use Float64.{ (<) }
assert -5.0d < 5.0d
```

### Float64.**(>)**

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
(>): (x: Float64, y: Float64) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first value|
|`y`|`Float64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

Examples:

```grain
use Float64.{ (>) }
assert 6.0d > 5.0d
```

### Float64.**(<=)**

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
(<=): (x: Float64, y: Float64) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first value|
|`y`|`Float64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

Examples:

```grain
use Float64.{ (<=) }
assert 1.0d <= 2.0d
```

```grain
use Float64.{ (<=) }
assert 2.0d <= 2.0d
```

### Float64.**(>=)**

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
(>=): (x: Float64, y: Float64) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first value|
|`y`|`Float64`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

Examples:

```grain
use Float64.{ (>=) }
assert 5.0d >= 2.0d
```

```grain
use Float64.{ (>=) }
assert -1.0d >= -1.0d
```

### Float64.**isFinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isFinite: (x: Float64) => Bool
```

Checks if a float is finite.
All values are finite exept for NaN, infinity or negative infinity.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is finite or `false` otherwise|

Examples:

```grain
Float64.isFinite(0.5d)
```

```grain
Float64.isFinite(1.0d)
```

```grain
Float64.isFinite(Infinityd) == false
```

```grain
Float64.isFinite(-Infinityd) == false
```

```grain
Float64.isFinite(NaNd) == false
```

### Float64.**isNaN**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.5</code></summary>
No other changes yet.
</details>

```grain
isNaN: (x: Float64) => Bool
```

Checks if the value is a float NaN value (Not A Number).

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The value to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is NaN, otherwise `false`|

Examples:

```grain
Float64.isNaN(NaNd)
```

```grain
Float64.isNaN(Infinityd) == false
```

```grain
Float64.isNaN(-Infinityd) == false
```

```grain
Float64.isNaN(0.5d) == false
```

```grain
Float64.isNaN(1.0d) == false
```

### Float64.**isInfinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.5</code></summary>
No other changes yet.
</details>

```grain
isInfinite: (x: Float64) => Bool
```

Checks if a float is infinite, that is either of positive or negative infinity.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The value to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is infinite or `false` otherwise|

Examples:

```grain
Float64.isInfinite(Infinityd)
```

```grain
Float64.isInfinite(-Infinityd)
```

```grain
Float64.isInfinite(NaNd) == false
```

```grain
Float64.isInfinite(0.5d) == false
```

```grain
Float64.isInfinite(1.0d) == false
```

### Float64.**min**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
min: (x: Float64, y: Float64) => Float64
```

Returns the smaller of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The smaller of the two operands|

Examples:

```grain
Float64.min(5.0d, 2.0d) == 2.0d
```

### Float64.**max**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
max: (x: Float64, y: Float64) => Float64
```

Returns the larger of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The larger of the two operands|

Examples:

```grain
Float64.max(5.0d, 2.0d) == 5.0d
```

### Float64.**abs**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.5</code></summary>
No other changes yet.
</details>

```grain
abs: (x: Float64) => Float64
```

Returns the absolute value. That is, it returns `x` if `x` is positive or zero and the negation of `x` if `x` is negative.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The absolute value of the operand|

Examples:

```grain
Float64.abs(-1.0d) == 1.0d
```

```grain
Float64.abs(5.0d) == 5.0d
```

### Float64.**neg**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.5</code></summary>
No other changes yet.
</details>

```grain
neg: (x: Float64) => Float64
```

Returns the negation of its operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The negated operand|

Examples:

```grain
Float64.neg(-1.0d) == 1.0d
```

```grain
Float64.neg(1.0d) == -1.0d
```

### Float64.**ceil**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
ceil: (x: Float64) => Float64
```

Rounds its operand up to the next largest whole value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The operand to ceil|

Returns:

|type|description|
|----|-----------|
|`Float64`|The next largest whole value of the operand|

Examples:

```grain
Float64.ceil(5.5d) == 6.0d
```

```grain
Float64.ceil(-5.5d) == -5.0d
```

### Float64.**floor**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
floor: (x: Float64) => Float64
```

Rounds its operand down to the largest whole value less than the operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The operand to floor|

Returns:

|type|description|
|----|-----------|
|`Float64`|The previous whole value of the operand|

Examples:

```grain
Float64.floor(5.5d) == 5.0d
```

```grain
Float64.floor(-5.5d) == -6.0d
```

### Float64.**trunc**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
trunc: (x: Float64) => Float64
```

Returns the whole value part of its operand, removing any fractional value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The operand to truncate|

Returns:

|type|description|
|----|-----------|
|`Float64`|The whole value part of the operand|

Examples:

```grain
Float64.trunc(5.5d) == 5.0d
```

### Float64.**round**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
round: (x: Float64) => Float64
```

Returns its operand rounded to its nearest integer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The operand to round|

Returns:

|type|description|
|----|-----------|
|`Float64`|The nearest integer to the operand|

Examples:

```grain
Float64.round(5.5d) == 6.0d
```

```grain
Float64.round(5.4d) == 5.0d
```

```grain
Float64.round(-5.5d) == -6.0d
```

```grain
Float64.round(-5.4d) == -5.0d
```

### Float64.**sqrt**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
sqrt: (x: Float64) => Float64
```

Computes the square root of its operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The operand to square root|

Returns:

|type|description|
|----|-----------|
|`Float64`|The square root of the operand|

Examples:

```grain
Float64.sqrt(25.0d) == 5.0d
```

### Float64.**copySign**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
copySign: (x: Float64, y: Float64) => Float64
```

Copys the sign of the second operand to the first operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The operand to be modify|
|`y`|`Float64`|The operand to copy the sign from|

Returns:

|type|description|
|----|-----------|
|`Float64`|The first operand with the sign of the second operand|

Examples:

```grain
Float64.copySign(2.0d, 1.0d) == 2.0d
```

```grain
Float64.copySign(3.0d, -1.0d) == -3.0d
```

```grain
Float64.copySign(-5.0d, 1.0d) == 5.0d
```

### Float64.**isClose**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isClose:
  (a: Float64, b: Float64, ?relativeTolerance: Float64,
   ?absoluteTolerance: Float64) => Bool
```

Determines whether two values are considered close to each other using a relative and absolute tolerance.

Parameters:

|param|type|description|
|-----|----|-----------|
|`a`|`Float64`|The first value|
|`b`|`Float64`|The second value|
|`?relativeTolerance`|`Float64`|The maximum tolerance to use relative to the larger absolute value `a` or `b`|
|`?absoluteTolerance`|`Float64`|The absolute tolerance to use, regardless of the values of `a` or `b`|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the values are considered close to each other or `false` otherwise|

Examples:

```grain
Float64.isClose(1.233d, 1.233d)
```

```grain
Float64.isClose(1.233d, 1.233000001d)
```

```grain
Float64.isClose(8.005d, 8.450d, absoluteTolerance=0.5d)
```

```grain
Float64.isClose(4.0d, 4.1d, relativeTolerance=0.025d)
```

```grain
Float64.isClose(1.233d, 1.24d) == false
```

```grain
Float64.isClose(1.233d, 1.4566d) == false
```

```grain
Float64.isClose(8.005d, 8.450d, absoluteTolerance=0.4d) == false
```

```grain
Float64.isClose(4.0d, 4.1d, relativeTolerance=0.024d) == false
```

### Float64.**sin**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
sin: (radians: Float64) => Float64
```

Computes the sine of a float (in radians).

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Float64`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Float64`|The computed sine|

Examples:

```grain
Float64.sin(0.0d) == 0.0d
```

### Float64.**cos**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
cos: (radians: Float64) => Float64
```

Computes the cosine of a float (in radians).

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Float64`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Float64`|The computed cosine|

Examples:

```grain
Float64.cos(0.0d) == 1.0d
```

### Float64.**tan**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
tan: (radians: Float64) => Float64
```

Computes the tangent of a number (in radians).

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Float64`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Float64`|The computed tangent|

Examples:

```grain
Float64.tan(0.0d) == 0.0d
```

