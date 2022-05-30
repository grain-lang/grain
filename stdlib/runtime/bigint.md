### Bigint.**List**

```grain
type List<a>
```

### Bigint.**debugDumpNumber**

```grain
debugDumpNumber : WasmI32 -> Void
```

### Bigint.**getSize**

```grain
getSize : WasmI32 -> WasmI32
```

### Bigint.**getFlags**

```grain
getFlags : WasmI32 -> WasmI32
```

### Bigint.**getLimb**

```grain
getLimb : (WasmI32, WasmI32) -> WasmI64
```

### Bigint.**makeWrappedInt32**

```grain
makeWrappedInt32 : WasmI32 -> WasmI32
```

### Bigint.**makeWrappedUint32**

```grain
makeWrappedUint32 : WasmI32 -> WasmI32
```

### Bigint.**makeWrappedInt64**

```grain
makeWrappedInt64 : WasmI64 -> WasmI32
```

### Bigint.**makeWrappedUint64**

```grain
makeWrappedUint64 : WasmI64 -> WasmI32
```

### Bigint.**isNegative**

```grain
isNegative : WasmI32 -> Bool
```

### Bigint.**eqz**

```grain
eqz : WasmI32 -> Bool
```

Returns true if the given bigint is equal to zero

### Bigint.**negate**

```grain
negate : WasmI32 -> WasmI32
```

### Bigint.**abs**

```grain
abs : WasmI32 -> WasmI32
```

### Bigint.**canConvertToInt32**

```grain
canConvertToInt32 : WasmI32 -> Bool
```

### Bigint.**toInt32**

```grain
toInt32 : WasmI32 -> WasmI32
```

### Bigint.**canConvertToInt64**

```grain
canConvertToInt64 : WasmI32 -> Bool
```

### Bigint.**toInt64**

```grain
toInt64 : WasmI32 -> WasmI64
```

### Bigint.**toFloat64**

```grain
toFloat64 : WasmI32 -> WasmF64
```

### Bigint.**toFloat32**

```grain
toFloat32 : WasmI32 -> WasmF32
```

### Bigint.**cmpI64**

```grain
cmpI64 : (WasmI32, WasmI64) -> WasmI32
```

### Bigint.**cmpF64**

```grain
cmpF64 : (WasmI32, WasmF64) -> WasmI32
```

### Bigint.**cmpF32**

```grain
cmpF32 : (WasmI32, WasmF32) -> WasmI32
```

### Bigint.**cmp**

```grain
cmp : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**eq**

```grain
eq : (WasmI32, WasmI32) -> Bool
```

### Bigint.**ne**

```grain
ne : (WasmI32, WasmI32) -> Bool
```

### Bigint.**lt**

```grain
lt : (WasmI32, WasmI32) -> Bool
```

### Bigint.**lte**

```grain
lte : (WasmI32, WasmI32) -> Bool
```

### Bigint.**gt**

```grain
gt : (WasmI32, WasmI32) -> Bool
```

### Bigint.**gte**

```grain
gte : (WasmI32, WasmI32) -> Bool
```

### Bigint.**bigIntToString**

```grain
bigIntToString : (WasmI32, WasmI32) -> String
```

### Bigint.**bigIntToString10**

```grain
bigIntToString10 : WasmI32 -> String
```

### Bigint.**add**

```grain
add : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**addInt**

```grain
addInt : (WasmI32, WasmI64) -> WasmI32
```

### Bigint.**sub**

```grain
sub : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**subInt**

```grain
subInt : (WasmI32, WasmI64) -> WasmI32
```

### Bigint.**incr**

```grain
incr : WasmI32 -> WasmI32
```

### Bigint.**decr**

```grain
decr : WasmI32 -> WasmI32
```

### Bigint.**mul**

```grain
mul : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**shl**

```grain
shl : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**shrS**

```grain
shrS : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**bitwiseNot**

```grain
bitwiseNot : WasmI32 -> WasmI32
```

### Bigint.**bitwiseAnd**

```grain
bitwiseAnd : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**bitwiseOr**

```grain
bitwiseOr : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**bitwiseXor**

```grain
bitwiseXor : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**countLeadingZeros**

```grain
countLeadingZeros : WasmI32 -> WasmI32
```

### Bigint.**countTrailingZeros**

```grain
countTrailingZeros : WasmI32 -> WasmI64
```

### Bigint.**popcnt**

```grain
popcnt : (WasmI32, WasmI32) -> WasmI64
```

### Bigint.**gcd**

```grain
gcd : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**quotRem**

```grain
quotRem : (WasmI32, WasmI32, WasmI32) -> Void
```

### Bigint.**divMod**

```grain
divMod : (WasmI32, WasmI32, WasmI32) -> Void
```

### Bigint.**quot**

```grain
quot : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**div**

```grain
div : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**rem**

```grain
rem : (WasmI32, WasmI32) -> WasmI32
```

### Bigint.**mod**

```grain
mod : (WasmI32, WasmI32) -> WasmI32
```

