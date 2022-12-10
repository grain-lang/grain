### Numbers.**isBoxedNumber**

```grain
isBoxedNumber : WasmI32 -> Bool
```

### Numbers.**isFloat**

```grain
isFloat : WasmI32 -> Bool
```

### Numbers.**isInteger**

```grain
isInteger : WasmI32 -> Bool
```

### Numbers.**isRational**

```grain
isRational : WasmI32 -> Bool
```

### Numbers.**isNaN**

```grain
isNaN : WasmI32 -> Bool
```

### Numbers.**isNumber**

```grain
isNumber : WasmI32 -> Bool
```

### Numbers.**reducedInteger**

```grain
reducedInteger : WasmI64 -> WasmI32
```

### Numbers.**boxedNumberTag**

```grain
boxedNumberTag : WasmI32 -> WasmI32
```

### Numbers.**boxedInt32Number**

```grain
boxedInt32Number : WasmI32 -> WasmI32
```

### Numbers.**boxedInt64Number**

```grain
boxedInt64Number : WasmI32 -> WasmI64
```

### Numbers.**boxedFloat32Number**

```grain
boxedFloat32Number : WasmI32 -> WasmF32
```

### Numbers.**boxedFloat64Number**

```grain
boxedFloat64Number : WasmI32 -> WasmF64
```

### Numbers.**boxedRationalNumerator**

```grain
boxedRationalNumerator : WasmI32 -> WasmI32
```

### Numbers.**boxedRationalDenominator**

```grain
boxedRationalDenominator : WasmI32 -> WasmI32
```

### Numbers.**coerceNumberToWasmF32**

```grain
coerceNumberToWasmF32 : Number -> WasmF32
```

### Numbers.**coerceNumberToWasmF64**

```grain
coerceNumberToWasmF64 : Number -> WasmF64
```

### Numbers.**coerceNumberToWasmI64**

```grain
coerceNumberToWasmI64 : Number -> WasmI64
```

### Numbers.**coerceNumberToWasmI32**

```grain
coerceNumberToWasmI32 : Number -> WasmI32
```

### Numbers.**numberEqual**

```grain
numberEqual : (WasmI32, WasmI32) -> Bool
```

### Numbers.**cmp**

```grain
cmp : (WasmI32, WasmI32) -> WasmI32
```

### Numbers.**(<)**

```grain
(<) : (Number, Number) -> Bool
```

### Numbers.**(>)**

```grain
(>) : (Number, Number) -> Bool
```

### Numbers.**(<=)**

```grain
(<=) : (Number, Number) -> Bool
```

### Numbers.**(>=)**

```grain
(>=) : (Number, Number) -> Bool
```

### Numbers.**compare**

```grain
compare : (Number, Number) -> Number
```

### Numbers.**numberEq**

```grain
numberEq : (Number, Number) -> Bool
```

### Numbers.**lnot**

```grain
lnot : Number -> Number
```

### Numbers.**(<<)**

```grain
(<<) : (Number, Number) -> Number
```

### Numbers.**(>>>)**

```grain
(>>>) : (Number, Number) -> Number
```

### Numbers.**(&)**

```grain
(&) : (Number, Number) -> Number
```

### Numbers.**(|)**

```grain
(|) : (Number, Number) -> Number
```

### Numbers.**(^)**

```grain
(^) : (Number, Number) -> Number
```

### Numbers.**(>>)**

```grain
(>>) : (Number, Number) -> Number
```

### Numbers.**coerceNumberToInt32**

```grain
coerceNumberToInt32 : Number -> Int32
```

### Numbers.**coerceNumberToInt64**

```grain
coerceNumberToInt64 : Number -> Int64
```

### Numbers.**coerceNumberToBigInt**

```grain
coerceNumberToBigInt : Number -> BigInt
```

### Numbers.**coerceNumberToRational**

```grain
coerceNumberToRational : Number -> Rational
```

### Numbers.**coerceNumberToFloat32**

```grain
coerceNumberToFloat32 : Number -> Float32
```

### Numbers.**coerceNumberToFloat64**

```grain
coerceNumberToFloat64 : Number -> Float64
```

### Numbers.**coerceInt32ToNumber**

```grain
coerceInt32ToNumber : Int32 -> Number
```

### Numbers.**coerceInt64ToNumber**

```grain
coerceInt64ToNumber : Int64 -> Number
```

### Numbers.**coerceBigIntToNumber**

```grain
coerceBigIntToNumber : BigInt -> Number
```

### Numbers.**coerceRationalToNumber**

```grain
coerceRationalToNumber : Rational -> Number
```

### Numbers.**coerceFloat32ToNumber**

```grain
coerceFloat32ToNumber : Float32 -> Number
```

### Numbers.**coerceFloat64ToNumber**

```grain
coerceFloat64ToNumber : Float64 -> Number
```

### Numbers.**convertExactToInexact**

```grain
convertExactToInexact : Number -> Number
```

### Numbers.**convertInexactToExact**

```grain
convertInexactToExact : Number -> Number
```

### Numbers.**(+)**

```grain
(+) : (Number, Number) -> Number
```

### Numbers.**(-)**

```grain
(-) : (Number, Number) -> Number
```

### Numbers.**(*)**

```grain
(*) : (Number, Number) -> Number
```

### Numbers.**(/)**

```grain
(/) : (Number, Number) -> Number
```

### Numbers.**(%)**

```grain
(%) : (Number, Number) -> Number
```

### Numbers.**incr**

```grain
incr : Number -> Number
```

### Numbers.**decr**

```grain
decr : Number -> Number
```

### Numbers.**isBigInt**

```grain
isBigInt : a -> Bool
```

### Numbers.**scalbn**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
scalbn : (WasmF64, WasmI32) -> WasmF64
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

