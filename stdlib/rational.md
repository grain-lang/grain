---
title: Rational
---

Utilities for working with the Rational type.

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
from "rational" include Rational
```

```grain
1/2r
```

```grain
3/4r
```

## Values

Functions and constants included in the Rational module.

### Rational.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Rational
```

Converts a Number to a Rational.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Rational`|The Number represented as a Rational|

### Rational.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : (rational: Rational) => Number
```

Converts a Rational to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rational`|`Rational`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Rational represented as a Number|

### Rational.**numerator**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
numerator : (x: Rational) => Number
```

Finds the numerator of the rational number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The rational number to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The numerator of the rational number|

### Rational.**denominator**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
denominator : (x: Rational) => Number
```

Finds the denominator of the rational number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The rational number to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The denominator of the rational number|

### Rational.**toIntegerRatio**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toIntegerRatio : (x: Rational) => (Number, Number)
```

Gets the numerator and denominator of the rational.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The rational to split|

Returns:

|type|description|
|----|-----------|
|`(Number, Number)`|The numerator and denominator of the rational|

Examples:

```grain
Rational.toIntegerRatio(1/2r) == (1, 2)
```

```grain
Rational.toIntegerRatio(2/8r) == (1, 4)
```

### Rational.**fromIntegerRatio**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromIntegerRatio : (numerator: Number, denominator: Number) => Rational
```

Creates a rational from a numerator and denominator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`numerator`|`Number`|The numerator|
|`denominator`|`Number`|The denominator|

Returns:

|type|description|
|----|-----------|
|`Rational`|The reduced rational|

Throws:

`InvalidArgument(String)`

* If the numerator is not an integer
* If the denominator is not an integer

Examples:

```grain
Rational.fromIntegerRatio(1, 2) == 1/2r
```

```grain
Rational.fromIntegerRatio(2, 8) == 1/4r
```

### Rational.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(+) : (x: Rational, y: Rational) => Rational
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first operand|
|`y`|`Rational`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Rational`|The sum of the two operands|

Examples:

```grain
use Rational.{ (+) }
assert 1/2r + 1/4r == 3/4r
```

### Rational.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(-) : (x: Rational, y: Rational) => Rational
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first operand|
|`y`|`Rational`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Rational`|The difference of the two operands|

Examples:

```grain
use Rational.{ (-) }
assert 1/2r - 1/4r == 1/4r
```

### Rational.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(*) : (x: Rational, y: Rational) => Rational
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first operand|
|`y`|`Rational`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Rational`|The product of the two operands|

Examples:

```grain
use Rational.{ (*) }
assert 1/2r * 1/4r == 1/8r
```

### Rational.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(/) : (x: Rational, y: Rational) => Rational
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first operand|
|`y`|`Rational`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Rational`|The quotient of the two operands|

Examples:

```grain
use Rational.{ (/) }
assert 1/2r / 1/4r == 2/1r
```

### Rational.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(==) : (x: Rational, y: Rational) => Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first value|
|`y`|`Rational`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

Examples:

```grain
use Rational.{ (==) }
assert 1/2r == 1/2r
```

### Rational.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(!=) : (x: Rational, y: Rational) => Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first value|
|`y`|`Rational`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

Examples:

```grain
use Rational.{ (!=) }
assert 1/2r != 1/4r
```

### Rational.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<) : (x: Rational, y: Rational) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first value|
|`y`|`Rational`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

Examples:

```grain
use Rational.{ (<) }
assert 1/4r < 1/2r
```

### Rational.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>) : (x: Rational, y: Rational) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first value|
|`y`|`Rational`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

Examples:

```grain
use Rational.{ (>) }
assert 1/2r > 1/4r
```

### Rational.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<=) : (x: Rational, y: Rational) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first value|
|`y`|`Rational`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

Examples:

```grain
use Rational.{ (<=) }
assert 1/4r <= 1/2r
```

```grain
use Rational.{ (<=) }
assert 1/2r <= 1/2r
```

### Rational.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>=) : (x: Rational, y: Rational) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Rational`|The first value|
|`y`|`Rational`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

Examples:

```grain
use Rational.{ (>=) }
assert 1/2r >= 1/4r
```

```grain
use Rational.{ (>=) }
assert 1/2r >= 1/2r
```

