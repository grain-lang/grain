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
infinity : Float32
```

Infinity represented as a Float32 value.
This is an alternative to the `Infinityf` literal.

### Float32.**nan**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
nan : Float32
```

NaN (Not a Number) represented as a Float32 value.
This is an alternative to the `NaNf` literal.

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

### Float32.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Float32
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
toNumber : (float: Float32) => Number
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
(+) : (x: Float32, y: Float32) => Float32
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
(-) : (x: Float32, y: Float32) => Float32
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
(*) : (x: Float32, y: Float32) => Float32
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
(/) : (x: Float32, y: Float32) => Float32
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
(<) : (x: Float32, y: Float32) => Bool
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
(>) : (x: Float32, y: Float32) => Bool
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
(<=) : (x: Float32, y: Float32) => Bool
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
(>=) : (x: Float32, y: Float32) => Bool
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

