---
title: Number
---

Utilities for working with numbers.

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
import Number from "number"
```

## Constants

Number constant values.

### Number.**pi**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
pi : Number
```

Pi represented as a Number value.

### Number.**tau**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
tau : Number
```

Tau represented as a Number value.

### Number.**e**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
e : Number
```

Euler's number represented as a Number value.

## Operations

Functions for operating on values of the Number type.

### Number.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
add : (Number, Number) -> Number
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

### Number.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
sub : (Number, Number) -> Number
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

### Number.**mul**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
mul : (Number, Number) -> Number
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

### Number.**div**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
div : (Number, Number) -> Number
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

### Number.**sqrt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
sqrt : Number -> Number
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
sign : Number -> Number
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

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
min : (Number, Number) -> Number
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

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
max : (Number, Number) -> Number
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

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
ceil : Number -> Number
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

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
floor : Number -> Number
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

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
trunc : Number -> Number
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

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
round : Number -> Number
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
abs : Number -> Number
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
neg : Number -> Number
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

### Number.**isFinite**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
isFinite : Number -> Bool
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
isNaN : Number -> Bool
```

Checks if a number contains the NaN value (Not A Number).
Only boxed floating point numbers can contain NaN.

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
isInfinite : Number -> Bool
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
parseInt : (String, Number) -> Result<Number, String>
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

