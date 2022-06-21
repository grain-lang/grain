---
title: Float32
---

Utilities for working with the Float32 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
import Float32 from "float32"
```

## Constants

Float32 constant values.

### Float32.**infinity**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
infinity : Float32
```

Infinity represented as a Float32 value.

### Float32.**nan**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
nan : Float32
```

NaN (Not a Number) represented as a Float32 value.

### Float32.**pi**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
pi : Float32
```

Pi represented as a Float32 value.

### Float32.**tau**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
tau : Float32
```

Tau represented as a Float32 value.

### Float32.**e**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.2</code></summary>
No other changes yet.
</details>

```grain
e : Float32
```

Euler's number represented as a Float32 value.

## Conversions

Functions for converting between Numbers and the Float32 type.

### Float32.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : Number -> Float32
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
toNumber : Float32 -> Number
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

## Operations

Mathematical operations for Float32 values.

### Float32.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
add : (Float32, Float32) -> Float32
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

### Float32.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
sub : (Float32, Float32) -> Float32
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

### Float32.**mul**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mul : (Float32, Float32) -> Float32
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

### Float32.**div**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
div : (Float32, Float32) -> Float32
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

## Comparisons

Functions for comparing Float32 values.

### Float32.**lt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lt : (Float32, Float32) -> Bool
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

### Float32.**gt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
gt : (Float32, Float32) -> Bool
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

### Float32.**lte**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lte : (Float32, Float32) -> Bool
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

### Float32.**gte**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
gte : (Float32, Float32) -> Bool
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

