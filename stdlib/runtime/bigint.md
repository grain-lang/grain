---
title: Bigint
---

## Values

Functions and constants included in the Bigint module.

### Bigint.**debugDumpNumber**

```grain
debugDumpNumber : (num: WasmI32) => Void
```

### Bigint.**getSize**

```grain
getSize : (ptr: WasmI32) => WasmI32
```

### Bigint.**getFlags**

```grain
getFlags : (ptr: WasmI32) => WasmI32
```

### Bigint.**getLimb**

```grain
getLimb : (ptr: WasmI32, i: WasmI32) => WasmI64
```

### Bigint.**makeWrappedInt32**

```grain
makeWrappedInt32 : (value: WasmI32) => WasmI32
```

### Bigint.**makeWrappedUint32**

```grain
makeWrappedUint32 : (value: WasmI32) => WasmI32
```

### Bigint.**makeWrappedInt64**

```grain
makeWrappedInt64 : (value: WasmI64) => WasmI32
```

### Bigint.**makeWrappedUint64**

```grain
makeWrappedUint64 : (value: WasmI64) => WasmI32
```

### Bigint.**isNegative**

```grain
isNegative : (num: WasmI32) => Bool
```

### Bigint.**eqz**

```grain
eqz : (num: WasmI32) => Bool
```

Returns true if the given bigint is equal to zero

### Bigint.**negate**

```grain
negate : (num: WasmI32) => WasmI32
```

### Bigint.**abs**

```grain
abs : (num: WasmI32) => WasmI32
```

### Bigint.**canConvertToInt32**

```grain
canConvertToInt32 : (num: WasmI32) => Bool
```

### Bigint.**toInt32**

```grain
toInt32 : (num: WasmI32) => WasmI32
```

### Bigint.**canConvertToInt64**

```grain
canConvertToInt64 : (num: WasmI32) => Bool
```

### Bigint.**toInt64**

```grain
toInt64 : (num: WasmI32) => WasmI64
```

### Bigint.**toUnsignedInt64**

```grain
toUnsignedInt64 : (num: WasmI32) => WasmI64
```

### Bigint.**toFloat64**

```grain
toFloat64 : (num: WasmI32) => WasmF64
```

### Bigint.**toFloat32**

```grain
toFloat32 : (num: WasmI32) => WasmF32
```

### Bigint.**cmpI64**

```grain
cmpI64 : (num1: WasmI32, num2: WasmI64) => WasmI32
```

### Bigint.**cmpU64**

```grain
cmpU64 : (num1: WasmI32, num2: WasmI64) => WasmI32
```

### Bigint.**cmpF64**

```grain
cmpF64 : (num1: WasmI32, num2: WasmF64) => WasmI32
```

### Bigint.**cmpF32**

```grain
cmpF32 : (num1: WasmI32, num2: WasmF32) => WasmI32
```

### Bigint.**cmp**

```grain
cmp : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**eq**

```grain
eq : (num1: WasmI32, num2: WasmI32) => Bool
```

### Bigint.**ne**

```grain
ne : (num1: WasmI32, num2: WasmI32) => Bool
```

### Bigint.**lt**

```grain
lt : (num1: WasmI32, num2: WasmI32) => Bool
```

### Bigint.**lte**

```grain
lte : (num1: WasmI32, num2: WasmI32) => Bool
```

### Bigint.**gt**

```grain
gt : (num1: WasmI32, num2: WasmI32) => Bool
```

### Bigint.**gte**

```grain
gte : (num1: WasmI32, num2: WasmI32) => Bool
```

### Bigint.**bigIntToString**

```grain
bigIntToString : (num: WasmI32, base: WasmI32) => String
```

### Bigint.**bigIntToString10**

```grain
bigIntToString10 : (num: WasmI32) => String
```

### Bigint.**add**

```grain
add : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**addInt**

```grain
addInt : (num1: WasmI32, int: WasmI64) => WasmI32
```

### Bigint.**sub**

```grain
sub : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**subInt**

```grain
subInt : (num1: WasmI32, int: WasmI64) => WasmI32
```

### Bigint.**incr**

```grain
incr : (num: WasmI32) => WasmI32
```

### Bigint.**decr**

```grain
decr : (num: WasmI32) => WasmI32
```

### Bigint.**mul**

```grain
mul : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**shl**

```grain
shl : (num: WasmI32, places: WasmI32) => WasmI32
```

### Bigint.**shrS**

```grain
shrS : (num: WasmI32, places: WasmI32) => WasmI32
```

### Bigint.**bitwiseNot**

```grain
bitwiseNot : (num: WasmI32) => WasmI32
```

### Bigint.**bitwiseAnd**

```grain
bitwiseAnd : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**bitwiseOr**

```grain
bitwiseOr : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**bitwiseXor**

```grain
bitwiseXor : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**countLeadingZeros**

```grain
countLeadingZeros : (num: WasmI32) => WasmI32
```

### Bigint.**countTrailingZeros**

```grain
countTrailingZeros : (num: WasmI32) => WasmI64
```

### Bigint.**popcnt**

```grain
popcnt : (num: WasmI32, flagDest: WasmI32) => WasmI64
```

### Bigint.**gcd**

```grain
gcd : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**quotRem**

```grain
quotRem : (num1: WasmI32, num2: WasmI32, dest: WasmI32) => Void
```

### Bigint.**divMod**

```grain
divMod : (num1: WasmI32, num2: WasmI32, dest: WasmI32) => Void
```

### Bigint.**quot**

```grain
quot : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**div**

```grain
div : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**rem**

```grain
rem : (num1: WasmI32, num2: WasmI32) => WasmI32
```

### Bigint.**mod**

```grain
mod : (num1: WasmI32, num2: WasmI32) => WasmI32
```

