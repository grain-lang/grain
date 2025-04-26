---
title: Float32
---

Utilities for working with the Float32 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
from "float32" include Float32
```

```grain
4.0f
```

```grain
-4.0f
```

```grain
Infinityf
```

```grain
NaNf
```

## Values

Functions and constants included in the Float32 module.

### Float32.**infinity**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
infinity: Float32
```

Infinity represented as a Float32 value.
This is an alternative to the `Infinityf` literal.

### Float32.**nan**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
nan: Float32
```

NaN (Not a Number) represented as a Float32 value.
This is an alternative to the `NaNf` literal.

### Float32.**pi**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
pi: Float32
```

Pi represented as a Float32 value.

### Float32.**tau**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
tau: Float32
```

Tau represented as a Float32 value.

### Float32.**e**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
e: Float32
```

Euler's number represented as a Float32 value.

### Float32.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber: (number: Number) => Float32
```

Converts a Number to a Float32.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Float32`|The Number represented as a Float32|

### Float32.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toNumber: (float: Float32) => Number
```

Converts a Float32 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`float`|`Float32`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Float32 represented as a Number|

### Float32.**reinterpretInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
reinterpretInt32: (value: Int32) => Float32
```

Interprets an Int32 as a Float32.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Float32`|The Int32 interpreted as an Float32|

Examples:

```grain
assert Float32.reinterpretInt32(1065353216l) == 1.0f
```

```grain
assert Float32.reinterpretInt32(-1082130432l) == -1.0f
```

### Float32.**reinterpretUint32**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
reinterpretUint32: (value: Uint32) => Float32
```

Interprets an Uint32 as a Float32.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint32`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Float32`|The Uint32 interpreted as an Float32|

Examples:

```grain
assert Float32.reinterpretUint32(1065353216ul) == 1.0f
```

```grain
assert Float32.reinterpretUint32(3212836864ul) == -1.0f
```

### Float32.**(+)**

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
(+): (x: Float32, y: Float32) => Float32
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand|
|`y`|`Float32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float32`|The sum of the two operands|

Examples:

```grain
use Float32.{ (+) }
assert 1.0f + 1.0f == 2.0f
```

### Float32.**(-)**

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
(-): (x: Float32, y: Float32) => Float32
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand|
|`y`|`Float32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float32`|The difference of the two operands|

Examples:

```grain
use Float32.{ (-) }
assert 1.0f - 1.0f == 0.0f
```

### Float32.**(*)**

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
(*): (x: Float32, y: Float32) => Float32
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand|
|`y`|`Float32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float32`|The product of the two operands|

Examples:

```grain
use Float32.{ (*) }
assert 2.0f * 2.0f == 4.0f
```

### Float32.**(/)**

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
(/): (x: Float32, y: Float32) => Float32
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand|
|`y`|`Float32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float32`|The quotient of the two operands|

Examples:

```grain
use Float32.{ (/) }
assert 10.0f / 4.0f == 2.5f
```

### Float32.**(\*\*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
(**): (base: Float32, power: Float32) => Float32
```

Computes the exponentiation of the given base and power.

Parameters:

|param|type|description|
|-----|----|-----------|
|`base`|`Float32`|The base float|
|`power`|`Float32`|The exponent float|

Returns:

|type|description|
|----|-----------|
|`Float32`|The base raised to the given power|

Examples:

```grain
use Float64.{ (**) }
assert 2.0f ** 2.0f == 4.0f
```

### Float32.**(<)**

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
(<): (x: Float32, y: Float32) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first value|
|`y`|`Float32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

Examples:

```grain
use Float32.{ (<) }
assert 1.0f < 2.0f
```

### Float32.**(>)**

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
(>): (x: Float32, y: Float32) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first value|
|`y`|`Float32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

Examples:

```grain
use Float32.{ (>) }
assert 2.0f > 1.0f
```

### Float32.**(<=)**

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
(<=): (x: Float32, y: Float32) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first value|
|`y`|`Float32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

Examples:

```grain
use Float32.{ (<=) }
assert -1.0f <= 1.0f
```

```grain
use Float32.{ (<=) }
assert -2.0f <= -2.0f
```

### Float32.**(>=)**

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
(>=): (x: Float32, y: Float32) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first value|
|`y`|`Float32`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

Examples:

```grain
use Float32.{ (>=) }
assert 4.0f >= 1.0f
```

```grain
use Float32.{ (>=) }
assert 3.0f >= 3.0f
```

### Float32.**isFinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isFinite: (x: Float32) => Bool
```

Checks if a float is finite.
All values are finite exept for NaN, infinity or negative infinity.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is finite or `false` otherwise|

Examples:

```grain
Float32.isFinite(0.5f)
```

```grain
Float32.isFinite(1.0f)
```

```grain
Float32.isFinite(Infinityf) == false
```

```grain
Float32.isFinite(-Infinityf) == false
```

```grain
Float32.isFinite(NaNf) == false
```

### Float32.**isNaN**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.5</code></summary>
No other changes yet.
</details>

```grain
isNaN: (x: Float32) => Bool
```

Checks if the value is a float NaN value (Not A Number).

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The value to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is NaN, otherwise `false`|

Examples:

```grain
Float32.isNaN(NaNf)
```

```grain
Float32.isNaN(Infinityf) == false
```

```grain
Float32.isNaN(-Infinityf) == false
```

```grain
Float32.isNaN(0.5f) == false
```

```grain
Float32.isNaN(1.0f) == false
```

### Float32.**isInfinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.5</code></summary>
No other changes yet.
</details>

```grain
isInfinite: (x: Float32) => Bool
```

Checks if a float is infinite, that is either of positive or negative infinity.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The value to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is infinite or `false` otherwise|

Examples:

```grain
Float32.isInfinite(Infinityf)
```

```grain
Float32.isInfinite(-Infinityf)
```

```grain
Float32.isInfinite(NaNf) == false
```

```grain
Float32.isInfinite(0.5f) == false
```

```grain
Float32.isInfinite(1.0f) == false
```

### Float32.**min**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
min: (x: Float32, y: Float32) => Float32
```

Returns the smaller of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand|
|`y`|`Float32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float32`|The smaller of the two operands|

Examples:

```grain
Float32.min(5.0f, 2.0f) == 2.0f
```

### Float32.**max**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
max: (x: Float32, y: Float32) => Float32
```

Returns the larger of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand|
|`y`|`Float32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float32`|The larger of the two operands|

Examples:

```grain
Float32.max(5.0f, 2.0f) == 5.0f
```

### Float32.**abs**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.5</code></summary>
No other changes yet.
</details>

```grain
abs: (x: Float32) => Float32
```

Returns the absolute value. That is, it returns `x` if `x` is positive or zero and the negation of `x` if `x` is negative.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The operand|

Returns:

|type|description|
|----|-----------|
|`Float32`|The absolute value of the operand|

Examples:

```grain
Float32.abs(-1.0f) == 1.0f
```

```grain
Float32.abs(5.0f) == 5.0f
```

### Float32.**neg**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.5</code></summary>
No other changes yet.
</details>

```grain
neg: (x: Float32) => Float32
```

Returns the negation of its operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The operand|

Returns:

|type|description|
|----|-----------|
|`Float32`|The negated operand|

Examples:

```grain
Float32.neg(-1.0f) == 1.0f
```

```grain
Float32.neg(1.0f) == -1.0f
```

### Float32.**ceil**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
ceil: (x: Float32) => Float32
```

Rounds its operand up to the next largest whole value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The operand to ceil|

Returns:

|type|description|
|----|-----------|
|`Float32`|The next largest whole value of the operand|

Examples:

```grain
Float32.ceil(5.5f) == 6.0f
```

```grain
Float32.ceil(-5.5f) == -5.0f
```

### Float32.**floor**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
floor: (x: Float32) => Float32
```

Rounds its operand down to the largest whole value less than the operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The operand to floor|

Returns:

|type|description|
|----|-----------|
|`Float32`|The previous whole value of the operand|

Examples:

```grain
Float32.floor(5.5f) == 5.0f
```

```grain
Float32.floor(-5.5f) == -6.0f
```

### Float32.**trunc**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
trunc: (x: Float32) => Float32
```

Returns the whole value part of its operand, removing any fractional value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The operand to truncate|

Returns:

|type|description|
|----|-----------|
|`Float32`|The whole value part of the operand|

Examples:

```grain
Float32.trunc(5.5f) == 5.0f
```

### Float32.**round**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
round: (x: Float32) => Float32
```

Returns its operand rounded to its nearest integer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The operand to round|

Returns:

|type|description|
|----|-----------|
|`Float32`|The nearest integer to the operand|

Examples:

```grain
Float32.round(5.5f) == 6.0f
```

```grain
Float32.round(5.4f) == 5.0f
```

```grain
Float32.round(-5.5f) == -6.0f
```

```grain
Float32.round(-5.4f) == -5.0f
```

### Float32.**sqrt**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
sqrt: (x: Float32) => Float32
```

Computes the square root of its operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The operand to square root|

Returns:

|type|description|
|----|-----------|
|`Float32`|The square root of the operand|

Examples:

```grain
Float32.sqrt(25.0f) == 5.0f
```

### Float32.**copySign**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
copySign: (x: Float32, y: Float32) => Float32
```

Copys the sign of the second operand to the first operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The operand to modify|
|`y`|`Float32`|The operand to copy the sign from|

Returns:

|type|description|
|----|-----------|
|`Float32`|The first operand with the sign of the second operand|

Examples:

```grain
Float32.copySign(2.0f, 1.0f) == 2.0f
```

```grain
Float32.copySign(3.0f, -1.0f) == -3.0f
```

```grain
Float32.copySign(-5.0f, 1.0f) == 5.0f
```

### Float32.**isClose**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isClose:
  (a: Float32, b: Float32, ?relativeTolerance: Float32,
   ?absoluteTolerance: Float32) => Bool
```

Determines whether two values are considered close to each other using a relative and absolute tolerance.

Parameters:

|param|type|description|
|-----|----|-----------|
|`a`|`Float32`|The first value|
|`b`|`Float32`|The second value|
|`?relativeTolerance`|`Float32`|The maximum tolerance to use relative to the larger absolute value `a` or `b`|
|`?absoluteTolerance`|`Float32`|The absolute tolerance to use, regardless of the values of `a` or `b`|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the values are considered close to each other or `false` otherwise|

Examples:

```grain
Float32.isClose(1.233f, 1.233f)
```

```grain
Float32.isClose(1.233f, 1.233000001f)
```

```grain
Float32.isClose(8.005f, 8.450f, absoluteTolerance=0.5f)
```

```grain
Float32.isClose(4.0f, 4.1f, relativeTolerance=0.025f)
```

```grain
Float32.isClose(1.233f, 1.24f) == false
```

```grain
Float32.isClose(1.233f, 1.4566f) == false
```

```grain
Float32.isClose(8.005f, 8.450f, absoluteTolerance=0.4f) == false
```

```grain
Float32.isClose(4.0f, 4.1f, relativeTolerance=0.024f) == false
```

### Float32.**sin**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
sin: (radians: Float32) => Float32
```

Computes the sine of a float (in radians).

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Float32`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Float32`|The computed sine|

Examples:

```grain
Float32.sin(0.0f) == 0.0f
```

### Float32.**cos**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
cos: (radians: Float32) => Float32
```

Computes the cosine of a float (in radians).

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Float32`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Float32`|The computed cosine|

Examples:

```grain
Float32.cos(0.0f) == 1.0f
```

### Float32.**tan**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
tan: (radians: Float32) => Float32
```

Computes the tangent of a number (in radians).

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Float32`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Float32`|The computed tangent|

Examples:

```grain
Float32.tan(0.0f) == 0.0f
```

