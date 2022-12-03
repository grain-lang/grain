### Common.**BiasedFp**

```grain
record BiasedFp {
  f: Int64,
  e: Int32,
}
```

### Common.**_MINIMUM_EXPONENT**

```grain
_MINIMUM_EXPONENT : WasmI32
```

### Common.**_MIN_EXPONENT_ROUND_TO_EVEN**

```grain
_MIN_EXPONENT_ROUND_TO_EVEN : WasmI64
```

### Common.**_MAX_EXPONENT_ROUND_TO_EVEN**

```grain
_MAX_EXPONENT_ROUND_TO_EVEN : WasmI64
```

### Common.**_MIN_EXPONENT_FAST_PATH**

```grain
_MIN_EXPONENT_FAST_PATH : WasmI32
```

### Common.**_MAX_EXPONENT_FAST_PATH**

```grain
_MAX_EXPONENT_FAST_PATH : WasmI32
```

### Common.**_MAX_EXPONENT_DISGUISED_FAST_PATH**

```grain
_MAX_EXPONENT_DISGUISED_FAST_PATH : WasmI32
```

### Common.**_MAX_MANTISSA_FAST_PATH**

```grain
_MAX_MANTISSA_FAST_PATH : WasmI64
```

### Common.**_MANTISSA_EXPLICIT_BITS_64**

```grain
_MANTISSA_EXPLICIT_BITS_64 : WasmI64
```

### Common.**_MANTISSA_EXPLICIT_BITS_32**

```grain
_MANTISSA_EXPLICIT_BITS_32 : WasmI32
```

### Common.**_INFINITE_POWER**

```grain
_INFINITE_POWER : WasmI32
```

### Common.**_SMALLEST_POWER_OF_TEN**

```grain
_SMALLEST_POWER_OF_TEN : WasmI64
```

### Common.**_LARGEST_POWER_OF_TEN**

```grain
_LARGEST_POWER_OF_TEN : WasmI64
```

### Common.**_SMALLEST_POWER_OF_FIVE**

```grain
_SMALLEST_POWER_OF_FIVE : WasmI64
```

### Common.**_LARGEST_POWER_OF_FIVE**

```grain
_LARGEST_POWER_OF_FIVE : WasmI64
```

### Common.**_CHAR_CODE_UNDERSCORE**

```grain
_CHAR_CODE_UNDERSCORE : WasmI32
```

### Common.**_CHAR_CODE_PLUS**

```grain
_CHAR_CODE_PLUS : WasmI32
```

### Common.**_CHAR_CODE_MINUS**

```grain
_CHAR_CODE_MINUS : WasmI32
```

### Common.**_CHAR_CODE_0**

```grain
_CHAR_CODE_0 : WasmI32
```

### Common.**_CHAR_CODE_e**

```grain
_CHAR_CODE_e : WasmI32
```

### Common.**_CHAR_CODE_E**

```grain
_CHAR_CODE_E : WasmI32
```

### Common.**_CHAR_CODE_DOT**

```grain
_CHAR_CODE_DOT : WasmI32
```

### Common.**_CHAR_CODE_A**

```grain
_CHAR_CODE_A : WasmI32
```

### Common.**_CHAR_CODE_Z**

```grain
_CHAR_CODE_Z : WasmI32
```

### Common.**_CHAR_CODE_a**

```grain
_CHAR_CODE_a : WasmI32
```

### Common.**_CHAR_CODE_f**

```grain
_CHAR_CODE_f : WasmI32
```

### Common.**_CHAR_CODE_i**

```grain
_CHAR_CODE_i : WasmI32
```

### Common.**_CHAR_CODE_n**

```grain
_CHAR_CODE_n : WasmI32
```

### Common.**_CHAR_CODE_t**

```grain
_CHAR_CODE_t : WasmI32
```

### Common.**_CHAR_CODE_y**

```grain
_CHAR_CODE_y : WasmI32
```

### Common.**fpZero**

```grain
fpZero : () -> BiasedFp
```

### Common.**fpInf**

```grain
fpInf : () -> BiasedFp
```

### Common.**fpErr**

```grain
fpErr : () -> BiasedFp
```

### Common.**fpNan**

```grain
fpNan : () -> BiasedFp
```

### Common.**getPowers10**

```grain
getPowers10 : WasmI32 -> WasmI32
```

### Common.**getPowers10FastPath**

```grain
getPowers10FastPath : WasmI32 -> WasmF64
```

### Common.**is8Digits**

```grain
is8Digits : WasmI64 -> Bool
```

### Common.**power**

```grain
power : WasmI32 -> WasmI32
```

### Common.**fullMultiplication**

```grain
fullMultiplication : (WasmI64, WasmI64) -> (Int64, Int64)
```

### Common.**biasedFpToNumber**

```grain
biasedFpToNumber : (BiasedFp, Bool) -> Number
```

