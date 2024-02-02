---
title: WasmI64
---

## Values

Functions and constants included in the WasmI64 module.

### WasmI64.**load**

```grain
load : (ptr: WasmI32, offset: WasmI32) => WasmI64
```

### WasmI64.**load8S**

```grain
load8S : (ptr: WasmI32, offset: WasmI32) => WasmI64
```

### WasmI64.**load8U**

```grain
load8U : (ptr: WasmI32, offset: WasmI32) => WasmI64
```

### WasmI64.**load16S**

```grain
load16S : (ptr: WasmI32, offset: WasmI32) => WasmI64
```

### WasmI64.**load16U**

```grain
load16U : (ptr: WasmI32, offset: WasmI32) => WasmI64
```

### WasmI64.**load32S**

```grain
load32S : (ptr: WasmI32, offset: WasmI32) => WasmI64
```

### WasmI64.**load32U**

```grain
load32U : (ptr: WasmI32, offset: WasmI32) => WasmI64
```

### WasmI64.**store**

```grain
store : (ptr: WasmI32, value: WasmI64, offset: WasmI32) => Void
```

### WasmI64.**store8**

```grain
store8 : (ptr: WasmI32, value: WasmI64, offset: WasmI32) => Void
```

### WasmI64.**store16**

```grain
store16 : (ptr: WasmI32, value: WasmI64, offset: WasmI32) => Void
```

### WasmI64.**store32**

```grain
store32 : (ptr: WasmI32, value: WasmI64, offset: WasmI32) => Void
```

### WasmI64.**clz**

```grain
clz : (num: WasmI64) => WasmI64
```

### WasmI64.**ctz**

```grain
ctz : (num: WasmI64) => WasmI64
```

### WasmI64.**popcnt**

```grain
popcnt : (num: WasmI64) => WasmI64
```

### WasmI64.**eqz**

```grain
eqz : (num: WasmI64) => Bool
```

### WasmI64.**(+)**

```grain
(+) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(-)**

```grain
(-) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(*)**

```grain
(*) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(/)**

```grain
(/) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**divU**

```grain
divU : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**remS**

```grain
remS : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**remU**

```grain
remU : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(&)**

```grain
(&) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(|)**

```grain
(|) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(^)**

```grain
(^) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(<<)**

```grain
(<<) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(>>>)**

```grain
(>>>) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(>>)**

```grain
(>>) : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**rotl**

```grain
rotl : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**rotr**

```grain
rotr : (left: WasmI64, right: WasmI64) => WasmI64
```

### WasmI64.**(==)**

```grain
(==) : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**(!=)**

```grain
(!=) : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**(<)**

```grain
(<) : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**ltU**

```grain
ltU : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**(<=)**

```grain
(<=) : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**leU**

```grain
leU : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**(>)**

```grain
(>) : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**gtU**

```grain
gtU : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**(>=)**

```grain
(>=) : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**geU**

```grain
geU : (left: WasmI64, right: WasmI64) => Bool
```

### WasmI64.**extendI32S**

```grain
extendI32S : (num: WasmI32) => WasmI64
```

### WasmI64.**extendI32U**

```grain
extendI32U : (num: WasmI32) => WasmI64
```

### WasmI64.**truncF32S**

```grain
truncF32S : (num: WasmF32) => WasmI64
```

### WasmI64.**truncF32U**

```grain
truncF32U : (num: WasmF32) => WasmI64
```

### WasmI64.**truncF64S**

```grain
truncF64S : (num: WasmF64) => WasmI64
```

### WasmI64.**truncF64U**

```grain
truncF64U : (num: WasmF64) => WasmI64
```

### WasmI64.**reinterpretF64**

```grain
reinterpretF64 : (num: WasmF64) => WasmI64
```

### WasmI64.**extendS8**

```grain
extendS8 : (num: WasmI64) => WasmI64
```

### WasmI64.**extendS16**

```grain
extendS16 : (num: WasmI64) => WasmI64
```

### WasmI64.**extendS32**

```grain
extendS32 : (num: WasmI64) => WasmI64
```

