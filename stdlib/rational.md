---
title: Rational
---

Utilities for working with the Rational type.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
include "rational"
```

## Conversions

Functions for converting between Numbers and the Rational type.

### Rational.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromNumber : Number -> Rational
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
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toNumber : Rational -> Number
```

Converts a Rational to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`float`|`Rational`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Rational represented as a Number|

## Operations

Mathematical operations for Rational values.

### Rational.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(+) : (Rational, Rational) -> Rational
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

### Rational.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(-) : (Rational, Rational) -> Rational
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

### Rational.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(*) : (Rational, Rational) -> Rational
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

### Rational.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(/) : (Rational, Rational) -> Rational
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

## Comparisons

Functions for comparing Rational values.

### Rational.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(==) : (Rational, Rational) -> Bool
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

### Rational.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(!=) : (Rational, Rational) -> Bool
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

### Rational.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<) : (Rational, Rational) -> Bool
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

### Rational.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>) : (Rational, Rational) -> Bool
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

### Rational.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<=) : (Rational, Rational) -> Bool
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

### Rational.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>=) : (Rational, Rational) -> Bool
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

