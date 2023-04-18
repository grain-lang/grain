---
title: Float64
---

Utilities for working with the Float64 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
include "float64"
```

## Values

Functions and constants included in the Float64 module.

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

### Float64.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (x: Number) -> Float64
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
toNumber : (x: Float64) -> Number
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

### Float64.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
add : (x: Float64, y: Float64) -> Float64
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
sub : (x: Float64, y: Float64) -> Float64
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
mul : (x: Float64, y: Float64) -> Float64
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
div : (x: Float64, y: Float64) -> Float64
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

### Float64.**lt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lt : (x: Float64, y: Float64) -> Bool
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
gt : (x: Float64, y: Float64) -> Bool
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
lte : (x: Float64, y: Float64) -> Bool
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
gte : (x: Float64, y: Float64) -> Bool
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

