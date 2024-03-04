---
title: Numbers
---

## Values

Functions and constants included in the Numbers module.

### Numbers.**tagSimple**

```grain
tagSimple : (x: WasmI32) => WasmI32
```

### Numbers.**isBoxedNumber**

```grain
isBoxedNumber : (x: WasmI32) => Bool
```

### Numbers.**isFloat**

```grain
isFloat : (x: WasmI32) => Bool
```

### Numbers.**isInteger**

```grain
isInteger : (x: WasmI32) => Bool
```

### Numbers.**isRational**

```grain
isRational : (x: WasmI32) => Bool
```

### Numbers.**isNaN**

```grain
isNaN : (x: WasmI32) => Bool
```

### Numbers.**isNumber**

```grain
isNumber : (x: WasmI32) => Bool
```

### Numbers.**reducedInteger**

```grain
reducedInteger : (x: WasmI64) => WasmI32
```

### Numbers.**reducedUnsignedInteger**

```grain
reducedUnsignedInteger : (x: WasmI64) => WasmI32
```

### Numbers.**boxedNumberTag**

```grain
boxedNumberTag : (xptr: WasmI32) => WasmI32
```

### Numbers.**boxedInt64Number**

```grain
boxedInt64Number : (xptr: WasmI32) => WasmI64
```

### Numbers.**boxedFloat64Number**

```grain
boxedFloat64Number : (xptr: WasmI32) => WasmF64
```

### Numbers.**boxedRationalNumerator**

```grain
boxedRationalNumerator : (xptr: WasmI32) => WasmI32
```

### Numbers.**boxedRationalDenominator**

```grain
boxedRationalDenominator : (xptr: WasmI32) => WasmI32
```

### Numbers.**coerceNumberToWasmF32**

```grain
coerceNumberToWasmF32 : (x: Number) => WasmF32
```

### Numbers.**coerceNumberToWasmF64**

```grain
coerceNumberToWasmF64 : (x: Number) => WasmF64
```

### Numbers.**coerceNumberToWasmI64**

```grain
coerceNumberToWasmI64 : (x: Number) => WasmI64
```

### Numbers.**coerceNumberToWasmI32**

```grain
coerceNumberToWasmI32 : (x: Number) => WasmI32
```

### Numbers.**coerceNumberToUnsignedWasmI64**

```grain
coerceNumberToUnsignedWasmI64 : (x: Number) => WasmI64
```

### Numbers.**coerceNumberToUnsignedWasmI32**

```grain
coerceNumberToUnsignedWasmI32 : (x: Number) => WasmI32
```

### Numbers.**numberEqual**

```grain
numberEqual : (x: WasmI32, y: WasmI32) => Bool
```

### Numbers.**addSubRational**

```grain
addSubRational :
  (x: WasmI32, y: WasmI32, isSub: Bool, keepRational: Bool) => WasmI32
```

### Numbers.**timesDivideRational**

```grain
timesDivideRational :
  (x: WasmI32, y: WasmI32, isDivide: Bool, keepRational: Bool) => WasmI32
```

### Numbers.**rationalsEqual**

```grain
rationalsEqual : (x: WasmI32, y: WasmI32) => Bool
```

### Numbers.**cmpRationals**

```grain
cmpRationals : (x: WasmI32, y: WasmI32) => WasmI32
```

### Numbers.**rationalNumerator**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rationalNumerator : (x: Rational) => Number
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

### Numbers.**rationalDenominator**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
rationalDenominator : (x: Rational) => Number
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

### Numbers.**cmp**

```grain
cmp : (x: WasmI32, y: WasmI32) => WasmI32
```

### Numbers.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(<) : (num1: Number, num2: Number) => Bool
```

Checks if the first operand is less than the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first operand is less than the second operand or `false` otherwise|

### Numbers.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(>) : (num1: Number, num2: Number) => Bool
```

Checks if the first operand is greater than the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first operand is greater than the second operand or `false` otherwise|

### Numbers.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(<=) : (num1: Number, num2: Number) => Bool
```

Checks if the first operand is less than or equal to the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first operand is less than or equal to the second operand or `false` otherwise|

### Numbers.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(>=) : (num1: Number, num2: Number) => Bool
```

Checks if the first operand is greater than or equal to the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first operand is greater than or equal to the second operand or `false` otherwise|

### Numbers.**compare**

```grain
compare : (x: Number, y: Number) => Number
```

### Numbers.**numberEq**

```grain
numberEq : (x: Number, y: Number) => Bool
```

### Numbers.**lnot**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
lnot : (value: Number) => Number
```

Computes the bitwise NOT of the operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The operand|

Returns:

|type|description|
|----|-----------|
|`Number`|Containing the inverted bits of the operand|

### Numbers.**(<<)**

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
(<<) : (value: Number, amount: Number) => Number
```

Shifts the bits of the value left by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The value to shift|
|`amount`|`Number`|The number of bits to shift by|

Returns:

|type|description|
|----|-----------|
|`Number`|The shifted value|

### Numbers.**(>>>)**

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
(>>>) : (value: Number, amount: Number) => Number
```

Shifts the bits of the value right by the given number of bits, preserving the sign bit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The value to shift|
|`amount`|`Number`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Number`|The shifted value|

### Numbers.**(&)**

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
(&) : (value1: Number, value2: Number) => Number
```

Computes the bitwise AND (`&`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value1`|`Number`|The first operand|
|`value2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|Containing a `1` in each bit position for which the corresponding bits of both operands are `1`|

### Numbers.**(|)**

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
(|) : (value1: Number, value2: Number) => Number
```

Computes the bitwise OR (`|`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value1`|`Number`|The first operand|
|`value2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|Containing a `1` in each bit position for which the corresponding bits of either or both operands are `1`|

### Numbers.**(^)**

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
(^) : (value1: Number, value2: Number) => Number
```

Computes the bitwise XOR (`^`) on the given operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value1`|`Number`|The first operand|
|`value2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|Containing a `1` in each bit position for which the corresponding bits of either but not both operands are `1`|

### Numbers.**(>>)**

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
(>>) : (value: Number, amount: Number) => Number
```

Shifts the bits of the value right by the given number of bits.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The value to shift|
|`amount`|`Number`|The amount to shift by|

Returns:

|type|description|
|----|-----------|
|`Number`|The shifted value|

### Numbers.**coerceNumberToInt8**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToInt8 : (number: Number) => Int8
```

Converts a Number to an Int8.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int8`|The Number represented as an Int8|

### Numbers.**coerceNumberToInt16**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToInt16 : (number: Number) => Int16
```

Converts a Number to an Int16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int16`|The Number represented as an Int16|

### Numbers.**coerceNumberToUint8**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToUint8 : (number: Number) => Uint8
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

### Numbers.**coerceNumberToUint16**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToUint16 : (number: Number) => Uint16
```

Converts a Number to a Uint16.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Uint16`|The Number represented as a Uint16|

### Numbers.**coerceNumberToInt32**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToInt32 : (number: Number) => Int32
```

Converts a Number to an Int32.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int32`|The Number represented as an Int32|

### Numbers.**coerceNumberToInt64**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToInt64 : (number: Number) => Int64
```

Converts a Number to an Int64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Int64`|The Number represented as an Int64|

### Numbers.**coerceNumberToBigInt**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToBigInt : (number: Number) => BigInt
```

Converts a Number to a BigInt.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`BigInt`|The Number represented as a BigInt|

### Numbers.**coerceNumberToRational**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToRational : (number: Number) => Rational
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

### Numbers.**coerceNumberToFloat32**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToFloat32 : (number: Number) => Float32
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

### Numbers.**coerceNumberToFloat64**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
coerceNumberToFloat64 : (number: Number) => Float64
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

### Numbers.**coerceInt8ToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceInt8ToNumber : (value: Int8) => Number
```

Converts an Int8 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int8`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int8 represented as a Number|

### Numbers.**coerceInt16ToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceInt16ToNumber : (value: Int16) => Number
```

Converts an Int16 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int16`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int16 represented as a Number|

### Numbers.**coerceUint8ToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceUint8ToNumber : (value: Uint8) => Number
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

### Numbers.**coerceUint16ToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceUint16ToNumber : (value: Uint16) => Number
```

Converts a Uint16 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Uint16`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Uint16 represented as a Number|

### Numbers.**coerceInt32ToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
coerceInt32ToNumber : (value: Int32) => Number
```

Converts an Int32 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int32`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int32 represented as a Number|

### Numbers.**coerceInt64ToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
coerceInt64ToNumber : (value: Int64) => Number
```

Converts an Int64 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Int64`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Int64 represented as a Number|

### Numbers.**coerceBigIntToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.0</code></summary>
No other changes yet.
</details>

```grain
coerceBigIntToNumber : (num: BigInt) => Number
```

Converts a BigInt to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num`|`BigInt`|The value to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The BigInt represented as a Number|

### Numbers.**coerceRationalToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
coerceRationalToNumber : (rational: Rational) => Number
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

### Numbers.**coerceFloat32ToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
coerceFloat32ToNumber : (float: Float32) => Number
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

### Numbers.**coerceFloat64ToNumber**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
coerceFloat64ToNumber : (float: Float64) => Number
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

### Numbers.**convertExactToInexact**

```grain
convertExactToInexact : (x: Number) => Number
```

### Numbers.**convertInexactToExact**

```grain
convertInexactToExact : (x: Number) => Number
```

### Numbers.**(+)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(+) : (num1: Number, num2: Number) => Number
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The sum of the two operands|

### Numbers.**(-)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(-) : (num1: Number, num2: Number) => Number
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The difference of the two operands|

### Numbers.**(*)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(*) : (num1: Number, num2: Number) => Number
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The product of the two operands|

### Numbers.**(/)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(/) : (num1: Number, num2: Number) => Number
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The quotient of the two operands|

### Numbers.**(%)**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
(%) : (num1: Number, num2: Number) => Number
```

Computes the remainder of the division of the first operand by the second.
The result will have the sign of the second operand.

Parameters:

|param|type|description|
|-----|----|-----------|
|`num1`|`Number`|The first operand|
|`num2`|`Number`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Number`|The modulus of its operands|

### Numbers.**incr**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
incr : (value: Number) => Number
```

Increments the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The value to increment|

Returns:

|type|description|
|----|-----------|
|`Number`|The incremented value|

### Numbers.**decr**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
decr : (value: Number) => Number
```

Decrements the value by one.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The value to decrement|

Returns:

|type|description|
|----|-----------|
|`Number`|The decremented value|

### Numbers.**isBigInt**

```grain
isBigInt : (x: a) => Bool
```

### Numbers.**scalbn**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
scalbn : (x: WasmF64, n: WasmI32) => WasmF64
```

Multiplies a floating-point number by an integral power of 2.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`WasmF64`|The floating-point value|
|`n`|`WasmI32`|The Integer exponent|

Returns:

|type|description|
|----|-----------|
|`WasmF64`|The result of x * 2^n|

### Numbers.**(\*\*)**

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
(**) : (base: Number, power: Number) => Number
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

