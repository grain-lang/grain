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
clz : (int: WasmI32) => WasmI32
```

### WasmI32.**ctz**

```grain
ctz : (int: WasmI32) => WasmI32
```

### WasmI32.**popcnt**

```grain
popcnt : (int: WasmI32) => WasmI32
```

### WasmI32.**eqz**

```grain
eqz : (int: WasmI32) => Bool
```

### WasmI32.**(+)**

```grain
(+) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(-)**

```grain
(-) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(*)**

```grain
(*) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(/)**

```grain
(/) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**divU**

```grain
divU : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**remS**

```grain
remS : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**remU**

```grain
remU : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(&)**

```grain
(&) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(|)**

```grain
(|) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(^)**

```grain
(^) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(<<)**

```grain
(<<) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(>>)**

```grain
(>>) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(>>>)**

```grain
(>>>) : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**rotl**

```grain
rotl : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**rotr**

```grain
rotr : (value1: WasmI32, value2: WasmI32) => WasmI32
```

### WasmI32.**(==)**

```grain
(==) : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**(!=)**

```grain
(!=) : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**(<)**

```grain
(<) : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**ltU**

```grain
ltU : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**(<=)**

```grain
(<=) : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**leU**

```grain
leU : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**(>)**

```grain
(>) : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**gtU**

```grain
gtU : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**(>=)**

```grain
(>=) : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**geU**

```grain
geU : (value1: WasmI32, value2: WasmI32) => Bool
```

### WasmI32.**wrapI64**

```grain
wrapI64 : (int: WasmI64) => WasmI32
```

### WasmI32.**truncF32S**

```grain
truncF32S : (int: WasmF32) => WasmI32
```

### WasmI32.**truncF32U**

```grain
truncF32U : (int: WasmF32) => WasmI32
```

### WasmI32.**truncF64S**

```grain
truncF64S : (int: WasmF64) => WasmI32
```

### WasmI32.**truncF64U**

```grain
truncF64U : (int: WasmF64) => WasmI32
```

### WasmI32.**reinterpretF32**

```grain
reinterpretF32 : (int: WasmF32) => WasmI32
```

### WasmI32.**extendS8**

```grain
extendS8 : (int: WasmI32) => WasmI32
```

### WasmI32.**extendS16**

```grain
extendS16 : (int: WasmI32) => WasmI32
```

### WasmI32.**fromGrain**

```grain
fromGrain : (value: a) => WasmI32
```

### WasmI32.**toGrain**

```grain
toGrain : (value: WasmI32) => a
```

