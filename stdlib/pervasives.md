---
title: Pervasives
---

This module is automatically imported into every Grain program. You can think of it as the global environment. Although it is automatically imported, it can still be imported manually.

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
from "pervasives" include Pervasives
```

## Values

Functions and constants included in the Pervasives module.

### Pervasives.**(!)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(!): (bool: Bool) => Bool
```

Computes the logical NOT (`!`) of the given operand.
Inverts the given Boolean value.

Parameters:

| param  | type   | description |
| ------ | ------ | ----------- |
| `bool` | `Bool` | The operand |

Returns:

| type   | description        |
| ------ | ------------------ |
| `Bool` | The inverted value |

Examples:

```grain
!true // false
```

```grain
!false // true
```

### Pervasives.**(&&)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(&&): (left: Bool, right: Bool) => Bool
```

Computes the logical AND (`&&`) of the given operands.

If the first operand is `false`, returns `false` without evaluating the second operand.
If the first operand is `true`, returns the value of the second operand.

Parameters:

| param   | type   | description        |
| ------- | ------ | ------------------ |
| `left`  | `Bool` | The first operand  |
| `right` | `Bool` | The second operand |

Returns:

| type   | description                                                                     |
| ------ | ------------------------------------------------------------------------------- |
| `Bool` | The first operand if it is `false` or the value of the second operand otherwise |

### Pervasives.**(||)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(||): (left: Bool, right: Bool) => Bool
```

Computes the logical OR `||` of the given operands.

If the first operand is `true`, returns `true` without evaluating the second operand.
If the first operand is `false`, returns the value of the second operand.

Parameters:

| param   | type   | description        |
| ------- | ------ | ------------------ |
| `left`  | `Bool` | The first operand  |
| `right` | `Bool` | The second operand |

Returns:

| type   | description                                                                    |
| ------ | ------------------------------------------------------------------------------ |
| `Bool` | The first operand if it is `true` or the value of the second operand otherwise |

### Pervasives.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(==): (value1: a, value2: a) => Bool
```

Check that two values are equal. This checks for structural equality,
so it also works for comparing things like tuples and lists.

Parameters:

| param    | type | description        |
| -------- | ---- | ------------------ |
| `value1` | `a`  | The first operand  |
| `value2` | `a`  | The second operand |

Returns:

| type   | description                                                      |
| ------ | ---------------------------------------------------------------- |
| `Bool` | `true` if the values are structurally equal or `false` otherwise |

### Pervasives.**(!=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
(!=): (value1: a, value2: a) => Bool
```

Check that two values are **not** equal. This checks for structural equality,
so it also works for comparing things like tuples and lists.

Parameters:

| param    | type | description        |
| -------- | ---- | ------------------ |
| `value1` | `a`  | The first operand  |
| `value2` | `a`  | The second operand |

Returns:

| type   | description                                                      |
| ------ | ---------------------------------------------------------------- |
| `Bool` | `false` if the values are structurally equal or `true` otherwise |

### Pervasives.**is**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
is: (left: a, right: a) => Bool
```

Checks that two values are physically equal.
Use this operator if you don’t need or want structural equality.

Parameters:

| param   | type | description        |
| ------- | ---- | ------------------ |
| `left`  | `a`  | The first operand  |
| `right` | `a`  | The second operand |

Returns:

| type   | description                                                    |
| ------ | -------------------------------------------------------------- |
| `Bool` | `true` if the values are physically equal or `false` otherwise |

### Pervasives.**isnt**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
isnt: (value1: a, value2: a) => Bool
```

Checks that two values are **not** physically equal.
Use this operator if you don’t need or want structural equality.

Parameters:

| param    | type | description        |
| -------- | ---- | ------------------ |
| `value1` | `a`  | The first operand  |
| `value2` | `a`  | The second operand |

Returns:

| type   | description                                                    |
| ------ | -------------------------------------------------------------- |
| `Bool` | `false` if the values are physically equal or `true` otherwise |

### Pervasives.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(<): (num1: Number, num2: Number) => Bool
```

Checks if the first operand is less than the second operand.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type   | description                                                                      |
| ------ | -------------------------------------------------------------------------------- |
| `Bool` | `true` if the first operand is less than the second operand or `false` otherwise |

### Pervasives.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(>): (num1: Number, num2: Number) => Bool
```

Checks if the first operand is greater than the second operand.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type   | description                                                                         |
| ------ | ----------------------------------------------------------------------------------- |
| `Bool` | `true` if the first operand is greater than the second operand or `false` otherwise |

### Pervasives.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(<=): (num1: Number, num2: Number) => Bool
```

Checks if the first operand is less than or equal to the second operand.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type   | description                                                                                  |
| ------ | -------------------------------------------------------------------------------------------- |
| `Bool` | `true` if the first operand is less than or equal to the second operand or `false` otherwise |

### Pervasives.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(>=): (num1: Number, num2: Number) => Bool
```

Checks if the first operand is greater than or equal to the second operand.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type   | description                                                                                     |
| ------ | ----------------------------------------------------------------------------------------------- |
| `Bool` | `true` if the first operand is greater than or equal to the second operand or `false` otherwise |

### Pervasives.**compare**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
compare: (num1: a, num2: a) => Number
```

Compares the first argument to the second argument and produces an integer result.
Provides a consistent ordering over all types and is suitable for sorting and other kinds of ordering.
`compare` treats `NaN` differently than the other comparison operators in that it considers `NaN` equal to itself and smaller than any other number.

Parameters:

| param  | type | description        |
| ------ | ---- | ------------------ |
| `num1` | `a`  | The first operand  |
| `num2` | `a`  | The second operand |

Returns:

| type     | description                                                                                                                     |
| -------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `Number` | A negative integer if the first operand is less than the second operand, `0` if they are equal, or a positive integer otherwise |

### Pervasives.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(+): (num1: Number, num2: Number) => Number
```

Computes the sum of its operands.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type     | description                 |
| -------- | --------------------------- |
| `Number` | The sum of the two operands |

### Pervasives.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(-): (num1: Number, num2: Number) => Number
```

Computes the difference of its operands.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type     | description                        |
| -------- | ---------------------------------- |
| `Number` | The difference of the two operands |

### Pervasives.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(*): (num1: Number, num2: Number) => Number
```

Computes the product of its operands.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type     | description                     |
| -------- | ------------------------------- |
| `Number` | The product of the two operands |

### Pervasives.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(/): (num1: Number, num2: Number) => Number
```

Computes the quotient of its operands.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type     | description                      |
| -------- | -------------------------------- |
| `Number` | The quotient of the two operands |

### Pervasives.**(%)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(%): (num1: Number, num2: Number) => Number
```

Computes the remainder of the division of the first operand by the second.
The result will have the sign of the second operand.

Parameters:

| param  | type     | description        |
| ------ | -------- | ------------------ |
| `num1` | `Number` | The first operand  |
| `num2` | `Number` | The second operand |

Returns:

| type     | description                 |
| -------- | --------------------------- |
| `Number` | The modulus of its operands |

### Pervasives.**(\*\*)**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally existed in Number module</td></tr>
</tbody>
</table>
</details>

```grain
(**): (base: Number, power: Number) => Number
```

Computes the exponentiation of the given base and power.

Parameters:

| param   | type     | description         |
| ------- | -------- | ------------------- |
| `base`  | `Number` | The base number     |
| `power` | `Number` | The exponent number |

Returns:

| type     | description                        |
| -------- | ---------------------------------- |
| `Number` | The base raised to the given power |

### Pervasives.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
incr: (value: Number) => Number
```

Increments the value by one.

Parameters:

| param   | type     | description            |
| ------- | -------- | ---------------------- |
| `value` | `Number` | The value to increment |

Returns:

| type     | description           |
| -------- | --------------------- |
| `Number` | The incremented value |

### Pervasives.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
decr: (value: Number) => Number
```

Decrements the value by one.

Parameters:

| param   | type     | description            |
| ------- | -------- | ---------------------- |
| `value` | `Number` | The value to decrement |

Returns:

| type     | description           |
| -------- | --------------------- |
| `Number` | The decremented value |

### Pervasives.**(++)**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
(++): (str1: String, str2: String) => String
```

Concatenate two strings.

Parameters:

| param  | type     | description          |
| ------ | -------- | -------------------- |
| `str1` | `String` | The beginning string |
| `str2` | `String` | The ending string    |

Returns:

| type     | description         |
| -------- | ------------------- |
| `String` | The combined string |

Examples:

```grain
"Foo" ++ "Bar" == "FooBar"
```

### Pervasives.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lnot: (value: Number) => Number
```

Computes the bitwise NOT of the operand.

Parameters:

| param   | type     | description |
| ------- | -------- | ----------- |
| `value` | `Number` | The operand |

Returns:

| type     | description                                 |
| -------- | ------------------------------------------- |
| `Number` | Containing the inverted bits of the operand |

### Pervasives.**(&)**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `land`</td></tr>
<tr><td><code>0.3.0</code></td><td>Renamed to `&`</td></tr>
</tbody>
</table>
</details>

```grain
(&): (value1: Number, value2: Number) => Number
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

| param    | type     | description        |
| -------- | -------- | ------------------ |
| `value1` | `Number` | The first operand  |
| `value2` | `Number` | The second operand |

Returns:

| type     | description                                                                                     |
| -------- | ----------------------------------------------------------------------------------------------- |
| `Number` | Containing a `1` in each bit position for which the corresponding bits of both operands are `1` |

### Pervasives.**(|)**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `lor`</td></tr>
<tr><td><code>0.3.0</code></td><td>Renamed to `|`</td></tr>
</tbody>
</table>
</details>

```grain
(|): (value1: Number, value2: Number) => Number
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

| param    | type     | description        |
| -------- | -------- | ------------------ |
| `value1` | `Number` | The first operand  |
| `value2` | `Number` | The second operand |

Returns:

| type     | description                                                                                               |
| -------- | --------------------------------------------------------------------------------------------------------- |
| `Number` | Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1` |

### Pervasives.**(^)**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>The `^` operator was originally an alias of `unbox`</td></tr>
<tr><td><code>0.2.0</code></td><td>Originally named `lxor`</td></tr>
<tr><td><code>0.3.0</code></td><td>Renamed to `^`</td></tr>
</tbody>
</table>
</details>

```grain
(^): (value1: Number, value2: Number) => Number
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

| param    | type     | description        |
| -------- | -------- | ------------------ |
| `value1` | `Number` | The first operand  |
| `value2` | `Number` | The second operand |

Returns:

| type     | description                                                                                                    |
| -------- | -------------------------------------------------------------------------------------------------------------- |
| `Number` | Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1` |

### Pervasives.**(<<)**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `lsl`</td></tr>
<tr><td><code>0.3.0</code></td><td>Renamed to `<<`</td></tr>
</tbody>
</table>
</details>

```grain
(<<): (value: Number, amount: Number) => Number
```

Shifts the bits of the value left by the given number of bits.

Parameters:

| param    | type     | description                    |
| -------- | -------- | ------------------------------ |
| `value`  | `Number` | The value to shift             |
| `amount` | `Number` | The number of bits to shift by |

Returns:

| type     | description       |
| -------- | ----------------- |
| `Number` | The shifted value |

### Pervasives.**(>>>)**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `lsr`</td></tr>
<tr><td><code>0.3.0</code></td><td>Renamed to `>>>`</td></tr>
</tbody>
</table>
</details>

```grain
(>>>): (value: Number, amount: Number) => Number
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

| param    | type     | description            |
| -------- | -------- | ---------------------- |
| `value`  | `Number` | The value to shift     |
| `amount` | `Number` | The amount to shift by |

Returns:

| type     | description       |
| -------- | ----------------- |
| `Number` | The shifted value |

### Pervasives.**(>>)**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `asr`</td></tr>
<tr><td><code>0.3.0</code></td><td>Renamed to `>>`</td></tr>
</tbody>
</table>
</details>

```grain
(>>): (value: Number, amount: Number) => Number
```

Shifts the bits of the value right by the given number of bits.

Parameters:

| param    | type     | description            |
| -------- | -------- | ---------------------- |
| `value`  | `Number` | The value to shift     |
| `amount` | `Number` | The amount to shift by |

Returns:

| type     | description       |
| -------- | ----------------- |
| `Number` | The shifted value |

### Pervasives.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
toString: (value: a) => String
```

Converts the given operand to a string.
Provides a better representation of data types if those types are provided from the module.

Parameters:

| param   | type | description |
| ------- | ---- | ----------- |
| `value` | `a`  | The operand |

Returns:

| type     | description              |
| -------- | ------------------------ |
| `String` | The operand, as a string |

### Pervasives.**print**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Added support for custom suffixes</td></tr>
</tbody>
</table>
</details>

```grain
print: (value: a, ?suffix: String) => Void
```

Prints the given operand to the console. Works for any type. Internally, calls `toString`
on the operand, so a better representation of data type will be printed if those types
are provided from the module.

Parameters:

| param     | type     | description                            |
| --------- | -------- | -------------------------------------- |
| `value`   | `a`      | The operand                            |
| `?suffix` | `String` | The string to print after the argument |

### Pervasives.**ignore**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
ignore: (value: a) => Void
```

Accepts any value and always returns `void`.

Parameters:

| param   | type | description         |
| ------- | ---- | ------------------- |
| `value` | `a`  | The value to ignore |

### Pervasives.**assert**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
assert: (condition: Bool) => Void
```

Assert that the given Boolean condition is `true`.

Parameters:

| param       | type   | description             |
| ----------- | ------ | ----------------------- |
| `condition` | `Bool` | The condition to assert |

Throws:

`AssertionError`

* When the `condition` is false

Examples:

```grain
assert 3 > 2
```

```grain
assert true
```

### Pervasives.**throw**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
throw: (exn: Exception) => a
```

Throw an exception. Currently, exceptions cannot be caught and will crash your program.

Parameters:

| param | type        | description                |
| ----- | ----------- | -------------------------- |
| `exn` | `Exception` | The exception to be thrown |

Returns:

| type | description                                                     |
| ---- | --------------------------------------------------------------- |
| `a`  | Anything and nothing—your program won't continue past a throw |

### Pervasives.**fail**

```grain
fail: (message: String) => a
```

Unconditionally throw a `Failure` exception with a message.
Currently, Exceptions cannot be caught and will crash your program.

Parameters:

| param     | type     | description                |
| --------- | -------- | -------------------------- |
| `message` | `String` | The reason for the failure |

Returns:

| type | description                                                               |
| ---- | ------------------------------------------------------------------------- |
| `a`  | Anything and nothing—your program won't continue past a fail expression |

### Pervasives.**identity**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
identity: (value: a) => a
```

Provides the operand untouched.

Parameters:

| param   | type | description         |
| ------- | ---- | ------------------- |
| `value` | `a`  | The value to return |

Returns:

| type | description         |
| ---- | ------------------- |
| `a`  | The value untouched |

### Pervasives.**box**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
box: (value: a) => Box<a>
```

Creates a box containing the given initial value.
Values inside a box can be swapped out with the `:=` operator.
Generally, `let mut` expressions are preferable to using a Box.

Parameters:

| param   | type | description                      |
| ------- | ---- | -------------------------------- |
| `value` | `a`  | The initial value inside the box |

Returns:

| type     | description                          |
| -------- | ------------------------------------ |
| `Box<a>` | The box containing the initial value |

### Pervasives.**unbox**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
unbox: (value: Box<a>) => a
```

Retrieves the current value from a box.

Parameters:

| param   | type     | description       |
| ------- | -------- | ----------------- |
| `value` | `Box<a>` | The box to unwrap |

Returns:

| type | description              |
| ---- | ------------------------ |
| `a`  | The value inside the box |

