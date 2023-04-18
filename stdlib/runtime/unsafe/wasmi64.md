---
title: WasmI64
---

## Values

Functions and constants included in the WasmI64 module.

### WasmI64.**load**

```grain
load : (WasmI32, WasmI32) -> WasmI64
```

### WasmI64.**load8S**

```grain
load8S : (WasmI32, WasmI32) -> WasmI64
```

### WasmI64.**load8U**

```grain
load8U : (WasmI32, WasmI32) -> WasmI64
```

### WasmI64.**load16S**

```grain
load16S : (WasmI32, WasmI32) -> WasmI64
```

### WasmI64.**load16U**

```grain
load16U : (WasmI32, WasmI32) -> WasmI64
```

### WasmI64.**load32S**

```grain
load32S : (WasmI32, WasmI32) -> WasmI64
```

### WasmI64.**load32U**

```grain
load32U : (WasmI32, WasmI32) -> WasmI64
```

### WasmI64.**store**

```grain
store : (WasmI32, WasmI64, WasmI32) -> Void
```

### WasmI64.**store8**

```grain
store8 : (WasmI32, WasmI64, WasmI32) -> Void
```

### WasmI64.**store16**

```grain
store16 : (WasmI32, WasmI64, WasmI32) -> Void
```

### WasmI64.**store32**

```grain
store32 : (WasmI32, WasmI64, WasmI32) -> Void
```

### WasmI64.**clz**

```grain
clz : WasmI64 -> WasmI64
```

### WasmI64.**ctz**

```grain
ctz : WasmI64 -> WasmI64
```

### WasmI64.**popcnt**

```grain
popcnt : WasmI64 -> WasmI64
```

### WasmI64.**eqz**

```grain
eqz : WasmI64 -> Bool
```

### WasmI64.**(+)**

```grain
(+) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(-)**

```grain
(-) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(*)**

```grain
(*) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(/)**

```grain
(/) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**divU**

```grain
divU : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**remS**

```grain
remS : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**remU**

```grain
remU : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(&)**

```grain
(&) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(|)**

```grain
(|) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(^)**

```grain
(^) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(<<)**

```grain
(<<) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(>>>)**

```grain
(>>>) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(>>)**

```grain
(>>) : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**rotl**

```grain
rotl : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**rotr**

```grain
rotr : (WasmI64, WasmI64) -> WasmI64
```

### WasmI64.**(==)**

```grain
(==) : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**(!=)**

```grain
(!=) : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**(<)**

```grain
(<) : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**ltU**

```grain
ltU : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**(<=)**

```grain
(<=) : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**leU**

```grain
leU : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**(>)**

```grain
(>) : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**gtU**

```grain
gtU : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**(>=)**

```grain
(>=) : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**geU**

```grain
geU : (WasmI64, WasmI64) -> Bool
```

### WasmI64.**extendI32S**

```grain
extendI32S : WasmI32 -> WasmI64
```

### WasmI64.**extendI32U**

```grain
extendI32U : WasmI32 -> WasmI64
```

### WasmI64.**truncF32S**

```grain
truncF32S : WasmF32 -> WasmI64
```

### WasmI64.**truncF32U**

```grain
truncF32U : WasmF32 -> WasmI64
```

### WasmI64.**truncF64S**

```grain
truncF64S : WasmF64 -> WasmI64
```

### WasmI64.**truncF64U**

```grain
truncF64U : WasmF64 -> WasmI64
```

### WasmI64.**reinterpretF64**

```grain
reinterpretF64 : WasmF64 -> WasmI64
```

### WasmI64.**extendS8**

```grain
extendS8 : WasmI64 -> WasmI64
```

### WasmI64.**extendS16**

```grain
extendS16 : WasmI64 -> WasmI64
```

### WasmI64.**extendS32**

```grain
extendS32 : WasmI64 -> WasmI64
```

