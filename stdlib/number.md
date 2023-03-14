---
title: Number
---

Utilities for working with numbers.

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
include "number"
```

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
<summary>Added in <code>next</code></summary>
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
(+) : (x: Number, y: Number) -> Number
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The first operand|
|`y`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The sum of the two operands|

### Number.**(-)**

<details>
<summary>Added in <code>next</code></summary>
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
(-) : (x: Number, y: Number) -> Number
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The first operand|
|`y`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The difference of the two operands|

### Number.**(*)**

<details>
<summary>Added in <code>next</code></summary>
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
(*) : (x: Number, y: Number) -> Number
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The first operand|
|`y`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The product of the two operands|

### Number.**(/)**

<details>
<summary>Added in <code>next</code></summary>
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
(/) : (x: Number, y: Number) -> Number
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The dividend|
|`y`|`Number`|The divisor|

Returns:

|type|description|
|----|-----------|
|`Number`|The quotient of the two operands|

### Number.**(\*\*)**

<details>
<summary>Added in <code>next</code></summary>
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
(**) : (base: Number, power: Number) -> Number
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

### Number.**exp**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
exp : (power: Number) -> Number
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

### Number.**sqrt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
sqrt : (x: Number) -> Number
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

### Number.**sign**

```grain
sign : (x: Number) -> Number
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
min : (x: Number, y: Number) -> Number
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
max : (x: Number, y: Number) -> Number
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
ceil : (x: Number) -> Number
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
floor : (x: Number) -> Number
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
trunc : (x: Number) -> Number
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
round : (x: Number) -> Number
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

### Number.**abs**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
abs : (x: Number) -> Number
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

### Number.**neg**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
neg : (x: Number) -> Number
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

### Number.**isFloat**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
isFloat : (x: Number) -> Bool
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

### Number.**isInteger**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
isInteger : (x: Number) -> Bool
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

### Number.**isRational**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
isRational : (x: Number) -> Bool
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

### Number.**isFinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
isFinite : (x: Number) -> Bool
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

### Number.**isNaN**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
isNaN : (x: Number) -> Bool
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

### Number.**isInfinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
isInfinite : (x: Number) -> Bool
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

### Number.**parseInt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.5</code></summary>
No other changes yet.
</details>

```grain
parseInt : (string: String, radix: Number) -> Result<Number, String>
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
|`input`|`String`|The string to parse|
|`radix`|`Number`|The number system base to use when parsing the input string|

Returns:

|type|description|
|----|-----------|
|`Result<Number, String>`|`Ok(value)` containing the parsed number on a successful parse or `Err(msg)` containing an error message string otherwise|

### Number.**parseFloat**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
parseFloat : (string: String) -> Result<Number, String>
```

Parses a string representation of a float into a `Number`. Underscores that appear
in numeric portions of the input are ignored.

Parameters:

|param|type|description|
|-----|----|-----------|
|`input`|`String`|The string to parse|

Returns:

|type|description|
|----|-----------|
|`Result<Number, String>`|`Ok(value)` containing the parsed number on a successful parse or `Err(msg)` containing an error message string otherwise|

### Number.**parse**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
parse : (input: String) -> Result<Number, String>
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
|`Result<Number, String>`|`Ok(value)` containing the parsed number on a successful parse or `Err(msg)` containing an error message string otherwise|

### Number.**sin**

<details>
<summary>Added in <code>0.5.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Handle NaN and Infinity</td></tr>
</tbody>
</table>
</details>

```grain
sin : (radians: Number) -> Number
```

Computes the sine of a number (in radians) using Chebyshev polynomials.

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Number`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Number`|The computed sine|

### Number.**cos**

<details>
<summary>Added in <code>0.5.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Handle NaN and Infinity</td></tr>
</tbody>
</table>
</details>

```grain
cos : (radians: Number) -> Number
```

Computes the cosine of a number (in radians) using Chebyshev polynomials.

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Number`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Number`|The computed cosine|

### Number.**tan**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
tan : (radians: Number) -> Number
```

Computes the tangent of a number (in radians) using Chebyshev polynomials.

Parameters:

|param|type|description|
|-----|----|-----------|
|`radians`|`Number`|The input in radians|

Returns:

|type|description|
|----|-----------|
|`Number`|The computed tangent|

### Number.**gamma**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
gamma : (z: Number) -> Number
```

Computes the gamma function of a value using Lanczos approximation.

Parameters:

|param|type|description|
|-----|----|-----------|
|`z`|`Number`|The value to interpolate|

Returns:

|type|description|
|----|-----------|
|`Number`|The gamma of the given value|

Throws:

`InvalidArgument(String)`

* When `z` is zero

### Number.**factorial**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
factorial : (n: Number) -> Number
```

Computes the product of consecutive integers for an integer input and computes the gamma function for non-integer inputs.

Parameters:

|param|type|description|
|-----|----|-----------|
|`n`|`Number`|The value to factorialize|

Returns:

|type|description|
|----|-----------|
|`Number`|The factorial of the given value|

Throws:

`InvalidArgument(String)`

* When `n` is negative

### Number.**toRadians**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
toRadians : (degrees: Number) -> Number
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

### Number.**toDegrees**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
toDegrees : (radians: Number) -> Number
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

