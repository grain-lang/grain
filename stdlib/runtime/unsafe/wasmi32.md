---
title: WasmI32
---

## Values

Functions and constants included in the WasmI32 module.

### WasmI32.**load**

```grain
load : (ptr: WasmI32, offset: WasmI32) => WasmI32
```

### WasmI32.**load8S**

```grain
load8S : (ptr: WasmI32, offset: WasmI32) => WasmI32
```

### WasmI32.**load8U**

```grain
load8U : (ptr: WasmI32, offset: WasmI32) => WasmI32
```

### WasmI32.**load16S**

```grain
load16S : (ptr: WasmI32, offset: WasmI32) => WasmI32
```

### WasmI32.**load16U**

```grain
load16U : (ptr: WasmI32, offset: WasmI32) => WasmI32
```

### WasmI32.**store**

```grain
store : (ptr: WasmI32, value: WasmI32, offset: WasmI32) => Void
```

### WasmI32.**store8**

```grain
store8 : (ptr: WasmI32, value: WasmI32, offset: WasmI32) => Void
```

### WasmI32.**store16**

```grain
store16 : (ptr: WasmI32, value: WasmI32, offset: WasmI32) => Void
```

### WasmI32.**clz**

```grain
clz : (num: WasmI32) => WasmI32
```

### WasmI32.**ctz**

```grain
ctz : (num: WasmI32) => WasmI32
```

### WasmI32.**popcnt**

```grain
popcnt : (num: WasmI32) => WasmI32
```

### WasmI32.**eqz**

```grain
eqz : (num: WasmI32) => Bool
```

### WasmI32.**(+)**

```grain
(+) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(-)**

```grain
(-) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(*)**

```grain
(*) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(/)**

```grain
(/) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**divU**

```grain
divU : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**remS**

```grain
remS : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**remU**

```grain
remU : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(&)**

```grain
(&) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(|)**

```grain
(|) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(^)**

```grain
(^) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(<<)**

```grain
(<<) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(>>)**

```grain
(>>) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(>>>)**

```grain
(>>>) : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**rotl**

```grain
rotl : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**rotr**

```grain
rotr : (left: WasmI32, right: WasmI32) => WasmI32
```

### WasmI32.**(==)**

```grain
(==) : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**(!=)**

```grain
(!=) : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**(<)**

```grain
(<) : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**ltU**

```grain
ltU : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**(<=)**

```grain
(<=) : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**leU**

```grain
leU : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**(>)**

```grain
(>) : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**gtU**

```grain
gtU : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**(>=)**

```grain
(>=) : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**geU**

```grain
geU : (left: WasmI32, right: WasmI32) => Bool
```

### WasmI32.**wrapI64**

```grain
wrapI64 : (num: WasmI64) => WasmI32
```

### WasmI32.**truncF32S**

```grain
truncF32S : (num: WasmF32) => WasmI32
```

### WasmI32.**truncF32U**

```grain
truncF32U : (num: WasmF32) => WasmI32
```

### WasmI32.**truncF64S**

```grain
truncF64S : (num: WasmF64) => WasmI32
```

### WasmI32.**truncF64U**

```grain
truncF64U : (num: WasmF64) => WasmI32
```

### WasmI32.**reinterpretF32**

```grain
reinterpretF32 : (num: WasmF32) => WasmI32
```

### WasmI32.**extendS8**

```grain
extendS8 : (num: WasmI32) => WasmI32
```

### WasmI32.**extendS16**

```grain
extendS16 : (num: WasmI32) => WasmI32
```

### WasmI32.**fromGrain**

```grain
fromGrain : (value: a) => WasmI32
```

### WasmI32.**toGrain**

```grain
toGrain : (value: WasmI32) => a
```

