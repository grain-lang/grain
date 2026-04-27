---
title: Bigint
---

## Values

Functions and constants included in the Bigint module.

### Bigint.**getSize**

```grain
getSize: (ref: WasmRef) => WasmI32
```

### Bigint.**getFlags**

```grain
getFlags: (ref: WasmRef) => WasmI32
```

### Bigint.**setFlags**

```grain
setFlags: (ref: WasmRef, flags: WasmI32) => Void
```

### Bigint.**getLimb**

```grain
getLimb: (ref: WasmRef, i: WasmI32) => WasmI64
```

### Bigint.**makeWrappedInt32**

```grain
makeWrappedInt32: (value: WasmI32) => WasmRef
```

### Bigint.**makeWrappedUint32**

```grain
makeWrappedUint32: (value: WasmI32) => WasmRef
```

### Bigint.**makeWrappedInt64**

```grain
makeWrappedInt64: (value: WasmI64) => WasmRef
```

### Bigint.**makeWrappedUint64**

```grain
makeWrappedUint64: (value: WasmI64) => WasmRef
```

### Bigint.**isNegative**

```grain
isNegative: (num: WasmRef) => Bool
```

### Bigint.**eqz**

```grain
eqz: (num: WasmRef) => Bool
```

Returns true if the given bigint is equal to zero

### Bigint.**negate**

```grain
negate: (num: WasmRef) => WasmRef
```

### Bigint.**abs**

```grain
abs: (num: WasmRef) => WasmRef
```

### Bigint.**canConvertToInt32**

```grain
canConvertToInt32: (num: WasmRef) => Bool
```

### Bigint.**toInt32**

```grain
toInt32: (num: WasmRef) => WasmI32
```

### Bigint.**canConvertToInt64**

```grain
canConvertToInt64: (num: WasmRef) => Bool
```

### Bigint.**toInt64**

```grain
toInt64: (num: WasmRef) => WasmI64
```

### Bigint.**toUnsignedInt64**

```grain
toUnsignedInt64: (num: WasmRef) => WasmI64
```

### Bigint.**toFloat64**

```grain
toFloat64: (num: WasmRef) => WasmF64
```

### Bigint.**toFloat32**

```grain
toFloat32: (num: WasmRef) => WasmF32
```

### Bigint.**cmpI64**

```grain
cmpI64: (num1: WasmRef, num2: WasmI64) => WasmI32
```

### Bigint.**cmpU64**

```grain
cmpU64: (num1: WasmRef, num2: WasmI64) => WasmI32
```

### Bigint.**cmpF64**

```grain
cmpF64: (num1: WasmRef, num2: WasmF64) => WasmI32
```

### Bigint.**cmpF32**

```grain
cmpF32: (num1: WasmRef, num2: WasmF32) => WasmI32
```

### Bigint.**cmp**

```grain
cmp: (num1: WasmRef, num2: WasmRef) => WasmI32
```

### Bigint.**eq**

```grain
eq: (num1: WasmRef, num2: WasmRef) => Bool
```

### Bigint.**ne**

```grain
ne: (num1: WasmRef, num2: WasmRef) => Bool
```

### Bigint.**lt**

```grain
lt: (num1: WasmRef, num2: WasmRef) => Bool
```

### Bigint.**lte**

```grain
lte: (num1: WasmRef, num2: WasmRef) => Bool
```

### Bigint.**gt**

```grain
gt: (num1: WasmRef, num2: WasmRef) => Bool
```

### Bigint.**gte**

```grain
gte: (num1: WasmRef, num2: WasmRef) => Bool
```

### Bigint.**bigIntToString**

```grain
bigIntToString: (num: WasmRef, base: WasmI32) => String
```

### Bigint.**bigIntToString10**

```grain
bigIntToString10: (num: WasmRef) => String
```

### Bigint.**add**

```grain
add: (num1: WasmRef, num2: WasmRef) => WasmRef
```

### Bigint.**addInt**

```grain
addInt: (num1: WasmRef, int: WasmI64) => WasmRef
```

### Bigint.**sub**

```grain
sub: (num1: WasmRef, num2: WasmRef) => WasmRef
```

### Bigint.**subInt**

```grain
subInt: (num1: WasmRef, int: WasmI64) => WasmRef
```

### Bigint.**incr**

```grain
incr: (num: WasmRef) => WasmRef
```

### Bigint.**decr**

```grain
decr: (num: WasmRef) => WasmRef
```

### Bigint.**mul**

```grain
mul: (num1: WasmRef, num2: WasmRef) => WasmRef
```

### Bigint.**shl**

```grain
shl: (num: WasmRef, places: WasmI32) => WasmRef
```

### Bigint.**shrS**

```grain
shrS: (num: WasmRef, places: WasmI32) => WasmRef
```

### Bigint.**bitwiseNot**

```grain
bitwiseNot: (num: WasmRef) => WasmRef
```

### Bigint.**bitwiseAnd**

```grain
bitwiseAnd: (num1: a, num2: b) => WasmRef
```

### Bigint.**bitwiseOr**

```grain
bitwiseOr: (num1: a, num2: b) => WasmRef
```

### Bigint.**bitwiseXor**

```grain
bitwiseXor: (num1: a, num2: b) => WasmRef
```

### Bigint.**countLeadingZeros**

```grain
countLeadingZeros: (num: WasmRef) => WasmI32
```

### Bigint.**countTrailingZeros**

```grain
countTrailingZeros: (num: WasmRef) => WasmI64
```

### Bigint.**popcnt**

```grain
popcnt: (num: WasmRef) => Option<Int64>
```

### Bigint.**gcd**

```grain
gcd: (num1: WasmRef, num2: WasmRef) => WasmRef
```

### Bigint.**quotRem**

```grain
quotRem: (num1: WasmRef, num2: WasmRef) => (WasmRef, WasmRef)
```

### Bigint.**divMod**

```grain
divMod: (num1: WasmRef, num2: WasmRef) => (WasmRef, WasmRef)
```

### Bigint.**quot**

```grain
quot: (num1: WasmRef, num2: WasmRef) => WasmRef
```

### Bigint.**div**

```grain
div: (num1: WasmRef, num2: WasmRef) => WasmRef
```

### Bigint.**rem**

```grain
rem: (num1: WasmRef, num2: WasmRef) => WasmRef
```

### Bigint.**mod**

```grain
mod: (num1: WasmRef, num2: WasmRef) => WasmRef
```

