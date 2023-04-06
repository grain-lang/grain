---
title: WasmI32
---

## Values

Functions and constants included in the WasmI32 module.

### WasmI32.**load**

```grain
load : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**load8S**

```grain
load8S : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**load8U**

```grain
load8U : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**load16S**

```grain
load16S : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**load16U**

```grain
load16U : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**store**

```grain
store : (WasmI32, WasmI32, WasmI32) -> Void
```

### WasmI32.**store8**

```grain
store8 : (WasmI32, WasmI32, WasmI32) -> Void
```

### WasmI32.**store16**

```grain
store16 : (WasmI32, WasmI32, WasmI32) -> Void
```

### WasmI32.**clz**

```grain
clz : WasmI32 -> WasmI32
```

### WasmI32.**ctz**

```grain
ctz : WasmI32 -> WasmI32
```

### WasmI32.**popcnt**

```grain
popcnt : WasmI32 -> WasmI32
```

### WasmI32.**eqz**

```grain
eqz : WasmI32 -> Bool
```

### WasmI32.**(+)**

```grain
(+) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(-)**

```grain
(-) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(*)**

```grain
(*) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(/)**

```grain
(/) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**divU**

```grain
divU : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**remS**

```grain
remS : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**remU**

```grain
remU : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(&)**

```grain
(&) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(|)**

```grain
(|) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(^)**

```grain
(^) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(<<)**

```grain
(<<) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(>>)**

```grain
(>>) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(>>>)**

```grain
(>>>) : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**rotl**

```grain
rotl : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**rotr**

```grain
rotr : (WasmI32, WasmI32) -> WasmI32
```

### WasmI32.**(==)**

```grain
(==) : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**(!=)**

```grain
(!=) : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**(<)**

```grain
(<) : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**ltU**

```grain
ltU : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**(<=)**

```grain
(<=) : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**leU**

```grain
leU : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**(>)**

```grain
(>) : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**gtU**

```grain
gtU : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**(>=)**

```grain
(>=) : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**geU**

```grain
geU : (WasmI32, WasmI32) -> Bool
```

### WasmI32.**wrapI64**

```grain
wrapI64 : WasmI64 -> WasmI32
```

### WasmI32.**truncF32S**

```grain
truncF32S : WasmF32 -> WasmI32
```

### WasmI32.**truncF32U**

```grain
truncF32U : WasmF32 -> WasmI32
```

### WasmI32.**truncF64S**

```grain
truncF64S : WasmF64 -> WasmI32
```

### WasmI32.**truncF64U**

```grain
truncF64U : WasmF64 -> WasmI32
```

### WasmI32.**reinterpretF32**

```grain
reinterpretF32 : WasmF32 -> WasmI32
```

### WasmI32.**extendS8**

```grain
extendS8 : WasmI32 -> WasmI32
```

### WasmI32.**extendS16**

```grain
extendS16 : WasmI32 -> WasmI32
```

### WasmI32.**fromGrain**

```grain
fromGrain : a -> WasmI32
```

### WasmI32.**toGrain**

```grain
toGrain : WasmI32 -> a
```

