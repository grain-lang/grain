---
title: Uint8
---

Utilities for working with the Uint8 type.

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
from "uint8" include Uint8
```

```grain
1us
```

```grain
10us
```

## Values

Functions and constants included in the Uint8 module.

### Uint8.**fromNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromNumber : (number: Number) => Uint8
```

Converts a Number to a Uint8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The Number represented as a Uint8|

### Uint8.**toNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toNumber : (value: Uint8) => Number
```

Converts a Uint8 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint8`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Uint8 represented as a Number|

### Uint8.**fromInt8**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromInt8 : (number: Int8) => Uint8
```

Converts an Int8 to a Uint8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Int8`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The Int8 represented as a Uint8|

Examples:

```grain
Uint8.fromInt8(1s) == 1us
```

```grain
Uint8.fromInt8(-1s) == 255us
```

### Uint8.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Uint8) => Uint8
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint8`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The incremented value|

Examples:

```grain
Uint8.incr(1us) == 2us
```

### Uint8.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Uint8) => Uint8
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint8`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The decremented value|

Examples:

```grain
Uint8.decr(1us) == 0us
```

```grain
Uint8.decr(0us) == 255us
```

### Uint8.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(+) : (x: Uint8, y: Uint8) => Uint8
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first operand|
|`y`|`Uint8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The sum of the two operands|

Examples:

```grain
use Uint8.{ (+) }
assert 1us + 1us == 2us
```

### Uint8.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(-) : (x: Uint8, y: Uint8) => Uint8
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first operand|
|`y`|`Uint8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The difference of the two operands|

Examples:

```grain
use Uint8.{ (-) }
assert 2us - 1us == 1us
```

### Uint8.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(*) : (x: Uint8, y: Uint8) => Uint8
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first operand|
|`y`|`Uint8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The product of the two operands|

Examples:

```grain
use Uint8.{ (*) }
assert 2us * 2us == 4us
```

### Uint8.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(/) : (x: Uint8, y: Uint8) => Uint8
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first operand|
|`y`|`Uint8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The quotient of its operands|

Examples:

```grain
use Uint8.{ (/) }
assert 5us / 2us == 2us
```

### Uint8.**rem**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rem : (x: Uint8, y: Uint8) => Uint8
```

Computes the remainder of the division of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first operand|
|`y`|`Uint8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The remainder of its operands|

Examples:

```grain
Uint8.rem(5us, 2us) == 1us
```

### Uint8.**(<<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<<) : (value: Uint8, amount: Uint8) => Uint8
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint8`|The value to shift|
|`amount`|`Uint8`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The shifted value|

Examples:

```grain
use Uint8.{ (<<) }
assert (5us << 1us) == 10us
```

### Uint8.**(>>>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>>>) : (value: Uint8, amount: Uint8) => Uint8
```

Shifts the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint8`|The value to shift|
|`amount`|`Uint8`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Uint8`|The shifted value|

Examples:

```grain
use Uint8.{ (>>>) }
assert (5us >>> 1us) == 2us
```

### Uint8.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(==) : (x: Uint8, y: Uint8) => Bool
```

Checks if the first value is equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first value|
|`y`|`Uint8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is equal to the second value or `false` otherwise|

Examples:

```grain
use Uint8.{ (==) }
assert 1us == 1us
```

### Uint8.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(!=) : (x: Uint8, y: Uint8) => Bool
```

Checks if the first value is not equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first value|
|`y`|`Uint8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is not equal to the second value or `false` otherwise|

Examples:

```grain
use Uint8.{ (!=) }
assert 1us != 3us
```

### Uint8.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<) : (x: Uint8, y: Uint8) => Bool
```

Checks if the first value is less than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first value|
|`y`|`Uint8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than the second value or `false` otherwise|

Examples:

```grain
use Uint8.{ (<) }
assert 1us < 5us
```

### Uint8.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>) : (x: Uint8, y: Uint8) => Bool
```

Checks if the first value is greater than the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first value|
|`y`|`Uint8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than the second value or `false` otherwise|

Examples:

```grain
use Uint8.{ (>) }
assert 4us > 2us
```

### Uint8.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<=) : (x: Uint8, y: Uint8) => Bool
```

Checks if the first value is less than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first value|
|`y`|`Uint8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is less than or equal to the second value or `false` otherwise|

Examples:

```grain
use Uint8.{ (<=) }
assert 1us <= 2us
```

```grain
use Uint8.{ (<=) }
assert 1us <= 1us
```

### Uint8.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>=) : (x: Uint8, y: Uint8) => Bool
```

Checks if the first value is greater than or equal to the second value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first value|
|`y`|`Uint8`|The second value|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first value is greater than or equal to the second value or `false` otherwise|

Examples:

```grain
use Uint8.{ (>=) }
assert 3us >= 2us
```

```grain
use Uint8.{ (>=) }
assert 1us >= 1us
```

### Uint8.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Uint8) => Uint8
```

Computes the bitwise NOT of the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint8`|The given value|

Returns:

|type|description|
|----|-----------|
|`Uint8`|Containing the inverted bits of the given value|

Examples:

```grain
Uint8.lnot(5us) == 250us
```

### Uint8.**(&)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(&) : (x: Uint8, y: Uint8) => Uint8
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first operand|
|`y`|`Uint8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint8`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

Examples:

```grain
use Uint8.{ (&) }
assert (3us & 4us) == 0us
```

### Uint8.**(|)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(|) : (x: Uint8, y: Uint8) => Uint8
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first operand|
|`y`|`Uint8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint8`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

Examples:

```grain
use Uint8.{ (|) }
assert (3us | 4us) == 7us
```

### Uint8.**(^)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(^) : (x: Uint8, y: Uint8) => Uint8
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Uint8`|The first operand|
|`y`|`Uint8`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Uint8`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

Examples:

```grain
use Uint8.{ (^) }
assert (3us ^ 5us) == 6us
```

