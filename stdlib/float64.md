---
title: Float64
---

Utilities for working with the Float64 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
import Float64 from "float64"
```

## Constants

Float64 constant values.

### Float64.**infinity**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
infinity : Float64
```

Infinity represented as a Float64 value.

### Float64.**nan**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
nan : Float64
```

NaN (Not a Number) represented as a Float64 value.

### Float64.**pi**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
pi : Float64
```

Pi represented as a Float64 value.

### Float64.**tau**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
tau : Float64
```

Tau represented as a Float64 value.

### Float64.**e**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
e : Float64
```

Euler's number represented as a Float64 value.

## Conversions

Functions for converting between Numbers and the Float64 type.

### Float64.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : Number -> Float64
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
toNumber : Float64 -> Number
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

## Operations

Mathematical operations for Float64 values.

### Float64.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
add : (Float64, Float64) -> Float64
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

### Float64.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
sub : (Float64, Float64) -> Float64
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

### Float64.**mul**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mul : (Float64, Float64) -> Float64
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

### Float64.**div**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
div : (Float64, Float64) -> Float64
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

## Comparisons

Functions for comparing Float64 values.

### Float64.**lt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lt : (Float64, Float64) -> Bool
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

### Float64.**gt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
gt : (Float64, Float64) -> Bool
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

### Float64.**lte**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lte : (Float64, Float64) -> Bool
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

### Float64.**gte**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
gte : (Float64, Float64) -> Bool
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

