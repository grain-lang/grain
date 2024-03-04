---
title: Number
---

Utilities for working with numbers.

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
from "number" include Number
```

```grain
1
```

```grain
-1
```

```grain
0.5
```

```grain
1/2
```

```grain
Infinity
```

```grain
NaN
```

## Types

Type declarations included in the Number module.

### Number.**ParseIntError**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
enum ParseIntError {
  ParseIntEmptyString,
  ParseIntInvalidDigit,
  ParseIntInvalidRadix,
}
```

Represents an error that occurred trying to parse an integer.

Variants:

```grain
ParseIntEmptyString
```

Represents an error caused by trying to parse an empty string.

```grain
ParseIntInvalidDigit
```

Represents an error caused by trying to parse a string with an invalid character.

```grain
ParseIntInvalidRadix
```

Represents an error caused by trying to parse with an invalid radix.

## Values

Functions and constants included in the Number module.

### Number.**pi**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
pi : Number
```

Pi represented as a Number value.

### Number.**tau**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
tau : Number
```

Tau represented as a Number value.

### Number.**e**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
e : Number
```

Euler's number represented as a Number value.

### Number.**(+)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally named `add`</td></tr>
</tbody>
</table>
</details>

```grain
(+) : (num1: Number, num2: Number) => Number
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The sum of the two operands|

Examples:

```grain
from Number use { (+) }
assert 1 + 2 == 3
```

### Number.**(-)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally named `sub`</td></tr>
</tbody>
</table>
</details>

```grain
(-) : (num1: Number, num2: Number) => Number
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The difference of the two operands|

Examples:

```grain
from Number use { (-) }
assert 5 - 2 == 3
```

### Number.**(*)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally named `mul`</td></tr>
</tbody>
</table>
</details>

```grain
(*) : (num1: Number, num2: Number) => Number
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The product of the two operands|

Examples:

```grain
from Number use { (*) }
assert 5 * 4 == 20
```

### Number.**(/)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.4.0</code></td><td>Originally named `div`</td></tr>
</tbody>
</table>
</details>

```grain
(/) : (num1: Number, num2: Number) => Number
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The dividend|
|`num2`|`Number`|The divisor|

Returns:

|type|description|
|----|-----------|
|`Number`|The quotient of the two operands|

Examples:

```grain
from Number use { (/) }
assert 10 / 2.5 == 4
```

### Number.**(\*\*)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally named `pow`</td></tr>
</tbody>
</table>
</details>

```grain
(**) : (base: Number, power: Number) => Number
```

Computes the exponentiation of the given base and power.

Parameters:

|param|type|description|
|-----|----|-----------|
|`base`|`Number`|The base number|
|`power`|`Number`|The exponent number|

Returns:

|type|description|
|----|-----------|
|`Number`|The base raised to the given power|

Examples:

```grain
from Number use { (**) }
assert 10 ** 2 == 100
```

### Number.**exp**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
exp : (power: Number) => Number
```

Computes the exponentiation of Euler's number to the given power.

Parameters:

|param|type|description|
|-----|----|-----------|
|`power`|`Number`|The exponent number|

Returns:

|type|description|
|----|-----------|
|`Number`|The `Number.e` value raised to the given power|

Examples:

```grain
Number.exp(1) == Number.e
```

```grain
Number.exp(10) == 22026.465794806703
```

### Number.**sqrt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
sqrt : (x: Number) => Number
```

Computes the square root of its operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to square root|

Returns:

|type|description|
|----|-----------|
|`Number`|The square root of the operand|

Examples:

```grain
Number.sqrt(25) == 5
```

### Number.**sign**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
sign : (x: Number) => Number
```

Determine the positivity or negativity of a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|`-1` if the number is negative, `1` if positive, or `0` otherwise; signedness of `-0.0` is preserved|

Examples:

```grain
Number.sign(-10000) == -1
```

```grain
Number.sign(222222) == 1
```

```grain
Number.sign(0) == 0
```

### Number.**min**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Handle NaN properly</td></tr>
</tbody>
</table>
</details>

```grain
min : (x: Number, y: Number) => Number
```

Returns the smaller of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The first operand|
|`y`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The smaller of the two operands|

Examples:

```grain
Number.min(5, 2) == 2
```

### Number.**max**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Handle NaN properly</td></tr>
</tbody>
</table>
</details>

```grain
max : (x: Number, y: Number) => Number
```

Returns the larger of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The first operand|
|`y`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The larger of the two operands|

Examples:

```grain
Number.max(5, 2) == 5
```

### Number.**ceil**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Handle NaN and Infinity properly</td></tr>
</tbody>
</table>
</details>

```grain
ceil : (x: Number) => Number
```

Rounds its operand up to the next largest integer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to round|

Returns:

|type|description|
|----|-----------|
|`Number`|The next largest integer of the operand|

Examples:

```grain
Number.ceil(5.5) == 6
```

```grain
Number.ceil(-5.5) == -5
```

### Number.**floor**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Handle NaN and Infinity properly</td></tr>
</tbody>
</table>
</details>

```grain
floor : (x: Number) => Number
```

Rounds its operand down to the largest integer less than the operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to round|

Returns:

|type|description|
|----|-----------|
|`Number`|The previous integer of the operand|

Examples:

```grain
Number.floor(5.5) == 5
```

```grain
Number.floor(-5.5) == -6
```

### Number.**trunc**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Handle NaN and Infinity properly</td></tr>
</tbody>
</table>
</details>

```grain
trunc : (x: Number) => Number
```

Returns the integer part of its operand, removing any fractional value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to truncate|

Returns:

|type|description|
|----|-----------|
|`Number`|The integer part of the operand|

Examples:

```grain
Number.trunc(5.5) == 5
```

### Number.**round**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Handle NaN and Infinity properly</td></tr>
</tbody>
</table>
</details>

```grain
round : (x: Number) => Number
```

Returns its operand rounded to its nearest integer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to round|

Returns:

|type|description|
|----|-----------|
|`Number`|The nearest integer to the operand|

Examples:

```grain
Number.round(5.5) == 6
```

```grain
Number.round(5.4) == 5
```

```grain
Number.round(-5.5) == -6
```

```grain
Number.round(-5.4) == -5
```

### Number.**abs**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
abs : (x: Number) => Number
```

Returns the absolute value of a number. That is, it returns `x` if `x` is positive or zero and the negation of `x` if `x` is negative.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The absolute value of the operand|

Examples:

```grain
Number.abs(-1) == 1
```

```grain
Number.abs(5) == 5
```

### Number.**neg**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
neg : (x: Number) => Number
```

Returns the negation of its operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to negate|

Returns:

|type|description|
|----|-----------|
|`Number`|The negated operand|

Examples:

```grain
Number.neg(-1) == 1
```

```grain
Number.neg(1) == -1
```

### Number.**isFloat**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
isFloat : (x: Number) => Bool
```

Checks if a number is a floating point value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is a floating point number or `false` otherwise|

Examples:

```grain
Number.isFloat(0.5)
```

```grain
Number.isFloat(1.0)
```

```grain
Number.isFloat(Infinity)
```

```grain
Number.isFloat(NaN)
```

```grain
Number.isFloat(1/2) == false
```

```grain
Number.isFloat(1) == false
```

### Number.**isInteger**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
isInteger : (x: Number) => Bool
```

Checks if a number is an integer.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is an integer or `false` otherwise|

Examples:

```grain
Number.isInteger(1)
```

```grain
Number.isInteger(0.5) == false
```

```grain
Number.isInteger(1.0) == false
```

```grain
Number.isInteger(1/2) == false
```

```grain
Number.isInteger(Infinity) == false
```

```grain
Number.isInteger(NaN) == false
```

### Number.**isRational**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
isRational : (x: Number) => Bool
```

Checks if a number is a non-integer rational value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is a non-integer rational number or `false` otherwise|

Examples:

```grain
Number.isRational(1/2)
```

```grain
Number.isRational(0.5) == false
```

```grain
Number.isRational(1.0) == false
```

```grain
Number.isRational(1) == false
```

```grain
Number.isRational(Infinity) == false
```

```grain
Number.isRational(NaN) == false
```

### Number.**isFinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
isFinite : (x: Number) => Bool
```

Checks if a number is finite.
All values are finite exept for floating point NaN, infinity or negative infinity.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is finite or `false` otherwise|

Examples:

```grain
Number.isFinite(1/2)
```

```grain
Number.isFinite(0.5)
```

```grain
Number.isFinite(1.0)
```

```grain
Number.isFinite(1)
```

```grain
Number.isFinite(Infinity) == false
```

```grain
Number.isFinite(-Infinity) == false
```

```grain
Number.isFinite(NaN) == false
```

### Number.**isNaN**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
isNaN : (x: Number) => Bool
```

Checks if a number is the float NaN value (Not A Number).

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is NaN, otherwise `false`|

Examples:

```grain
Number.isNaN(NaN)
```

```grain
Number.isNaN(Infinity) == false
```

```grain
Number.isNaN(-Infinity) == false
```

```grain
Number.isNaN(1/2) == false
```

```grain
Number.isNaN(0.5) == false
```

```grain
Number.isNaN(1.0) == false
```

```grain
Number.isNaN(1) == false
```

### Number.**isInfinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
isInfinite : (x: Number) => Bool
```

Checks if a number is infinite, that is either of floating point positive or negative infinity.
Note that this function is not the exact opposite of isFinite(Number) in that it doesn't return true for NaN.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value is infinite or `false` otherwise|

Examples:

```grain
Number.isInfinite(Infinity)
```

```grain
Number.isInfinite(-Infinity)
```

```grain
Number.isInfinite(NaN) == false
```

```grain
Number.isInfinite(1/2) == false
```

```grain
Number.isInfinite(0.5) == false
```

```grain
Number.isInfinite(1.0) == false
```

```grain
Number.isInfinite(1) == false
```

### Number.**isClose**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
isClose :
  (a: Number, b: Number, ?relativeTolerance: Number,
   ?absoluteTolerance: Number) => Bool
```

Determines whether two values are considered close to each other using a relative and absolute tolerance.

Parameters:

|param|type|description|
|-----|----|-----------|
|`a`|`Number`|The first value|
|`b`|`Number`|The second value|
|`?relativeTolerance`|`Number`|The maximum tolerance to use relative to the larger absolute value `a` or `b`|
|`?absoluteTolerance`|`Number`|The absolute tolerance to use, regardless of the values of `a` or `b`|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the values are considered close to each other or `false` otherwise|

Examples:

```grain
Number.isClose(1.233, 1.233)
```

```grain
Number.isClose(1.233, 1.233000001)
```

```grain
Number.isClose(8.005, 8.450, absoluteTolerance=0.5)
```

```grain
Number.isClose(4, 4.1, relativeTolerance=0.025)
```

```grain
Number.isClose(1.233, 1.24) == false
```

```grain
Number.isClose(1.233, 1.4566) == false
```

```grain
Number.isClose(8.005, 8.450, absoluteTolerance=0.4) == false
```

```grain
Number.isClose(4, 4.1, relativeTolerance=0.024) == false
```

### Number.**parseInt**

<details>
<summary>Added in <code>0.4.5</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Switched from a string-based error message to a structured error enum</td></tr>
</tbody>
</table>
</details>

```grain
parseInt :
  (string: String, radix: Number) => Result<Number, Atoi.ParseIntError>
```

Parses a string representation of an integer into a `Number` using the
specified radix (also known as a number system "base").

If the string has a radix prefix (i.e. "0x"/"0X", "0o"/"0O", or "0b"/"0B"
for radixes 16, 8, or 2 respectively), the supplied radix is ignored in
favor of the prefix. Underscores that appear in the numeric portion of the
input are ignored.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to parse|
|`radix`|`Number`|The number system base to use when parsing the input string|

Returns:

|type|description|
|----|-----------|
|`Result<Number, Atoi.ParseIntError>`|`Ok(value)` containing the parsed number on a successful parse or `Err(err)` containing a variant of `ParseIntError`|

Examples:

```grain
Number.parseInt("1", radix=10) == Ok(1)
```

```grain
Number.parseInt("-1", radix=10) == Ok(-1)
```

```grain
Number.parseInt("0xf0", radix=16) == Ok(0x0f0)
```

### Number.**parseFloat**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
parseFloat : (string: String) => Result<Number, String>
```

Parses a string representation of a float into a `Number`. Underscores that appear
in numeric portions of the input are ignored.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to parse|

Returns:

|type|description|
|----|-----------|
|`Result<Number, String>`|`Ok(value)` containing the parsed number on a successful parse or `Err(msg)` containing an error message string otherwise|

Examples:

```grain
Number.parseFloat("1") == Ok(1.0)
```

```grain
Number.parseFloat("-1") == Ok(-1.0)
```

```grain
Number.parseFloat("-1.5") == Ok(-1.5)
```

### Number.**parse**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
parse : (input: String) => Result<Number, Atoi.ParseIntError>
```

Parses a string representation of an integer, float, or rational into a `Number`.
Underscores that appear in the numeric portion of the input are ignored.

Parameters:

|param|type|description|
|-----|----|-----------|
|`input`|`String`|The string to parse|

Returns:

|type|description|
|----|-----------|
|`Result<Number, Atoi.ParseIntError>`|`Ok(value)` containing the parsed number on a successful parse or `Err(msg)` containing an error message string otherwise|

Examples:

```grain
Number.parse("1") == Ok(1)
```

```grain
Number.parse("-1") == Ok(-1)
```

```grain
Number.parse("0xf0") == Ok(0x0f0)
```

```grain
Number.parse("-1.5") == Ok(-1.5)
```

### Number.**asin**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
asin : (angle: Number) => Number
```

Computes the inverse sine of the given angle.

Parameters:

|param|type|description|
|-----|----|-----------|
|`angle`|`Number`|A number between -1 and 1, representing the angle's sine value|

Returns:

|type|description|
|----|-----------|
|`Number`|The inverse sine (angle in radians between `-pi/2` and `pi/2`) of the given `angle` or `NaN` if the given `angle` is not between`-1` and `1`|

Examples:

```grain
Number.asin(0) == 0
```

```grain
Number.asin(1) == 1.5707963267948966
```

### Number.**acos**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
acos : (angle: Number) => Number
```

Computes the inverse cosine of the given angle.

Parameters:

|param|type|description|
|-----|----|-----------|
|`angle`|`Number`|A number between -1 and 1, representing the angle's cosine value|

Returns:

|type|description|
|----|-----------|
|`Number`|The inverse cosine (angle in radians between `-pi/2` and `pi/2`) of the given `angle` or `NaN` if the given `angle` is not between`-1` and `1`|

Examples:

```grain
Number.acos(1) == 0
```

```grain
Number.acos(0) == 1.5707963267948966
```

### Number.**atan**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
atan : (angle: Number) => Number
```

Computes the inverse tangent of the given angle.

Parameters:

|param|type|description|
|-----|----|-----------|
|`angle`|`Number`|A number between -1 and 1, representing the angle's tangent value|

Returns:

|type|description|
|----|-----------|
|`Number`|The inverse tangent (angle in radians between `-pi/2` and `pi/2`) of the given `angle` or `NaN` if the given `angle` is not between`-1` and `1`|

Examples:

```grain
Number.atan(0) == 0
```

```grain
Number.atan(1) == 0.7853981633974483
```

### Number.**atan2**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
atan2 : (y: Number, x: Number) => Number
```

Computes the angle between the positive x-axis and the ray from the origin to the point (x, y).

Parameters:

|param|type|description|
|-----|----|-----------|
|`y`|`Number`|The given y coordinate|
|`x`|`Number`|The given x coordinate|

Returns:

|type|description|
|----|-----------|
|`Number`|The angle in radians between the positive x-axis and the point (x, y)|

Examples:

```grain
Number.atan2(0, 1) == Number.pi
```

### Number.**toRadians**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
toRadians : (degrees: Number) => Number
```

Converts degrees to radians.

Parameters:

|param|type|description|
|-----|----|-----------|
|`degrees`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The value in radians|

Examples:

```grain
Number.toRadians(180) == Number.pi
```

### Number.**toDegrees**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
toDegrees : (radians: Number) => Number
```

Converts radians to degrees.

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The value in degrees|

Examples:

```grain
Number.toRadians(Number.pi) == 180
```

### Number.**clamp**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
clamp : (range: Range<Number>, input: Number) => Number
```

Constrains a number within the given inclusive range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`range`|`Range<Number>`|The inclusive range to clamp within|
|`input`|`Number`|The number to clamp|

Returns:

|type|description|
|----|-----------|
|`Number`|The constrained number|

### Number.**linearInterpolate**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
linearInterpolate : (range: Range<Number>, weight: Number) => Number
```

Maps a weight between 0 and 1 within the given inclusive range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`range`|`Range<Number>`|The inclusive range to interpolate within|
|`weight`|`Number`|The weight to interpolate|

Returns:

|type|description|
|----|-----------|
|`Number`|The blended value|

Throws:

`InvalidArgument(String)`

* When `weight` is not between 0 and 1
* When `range` is not finite
* When `range` includes NaN

### Number.**linearMap**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
linearMap :
  (inputRange: Range<Number>, outputRange: Range<Number>, current: Number) =>
   Number
```

Scales a number from one inclusive range to another inclusive range.
If the number is outside the input range, it will be clamped.

Parameters:

|param|type|description|
|-----|----|-----------|
|`inputRange`|`Range<Number>`|The inclusive range you are mapping from|
|`outputRange`|`Range<Number>`|The inclusive range you are mapping to|
|`current`|`Number`|The number to map|

Returns:

|type|description|
|----|-----------|
|`Number`|The mapped number|

Throws:

`InvalidArgument(String)`

* When `inputRange` is not finite
* When `inputRange` includes NaN
* When `outputRange` is not finite
* When `outputRange` includes NaN

