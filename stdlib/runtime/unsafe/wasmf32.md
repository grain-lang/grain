---
title: WasmF32
---

## Values

Functions and constants included in the WasmF32 module.

### WasmF32.**load**

```grain
load : (ptr: WasmI32, offset: WasmI32) => WasmF32
```

### WasmF32.**store**

```grain
store : (ptr: WasmI32, value: WasmF32, offset: WasmI32) => Void
```

### WasmF32.**neg**

```grain
neg : (num: WasmF32) => WasmF32
```

### WasmF32.**abs**

```grain
abs : (num: WasmF32) => WasmF32
```

### WasmF32.**ceil**

```grain
ceil : (num: WasmF32) => WasmF32
```

### WasmF32.**floor**

```grain
floor : (num: WasmF32) => WasmF32
```

### WasmF32.**trunc**

```grain
trunc : (num: WasmF32) => WasmF32
```

### WasmF32.**nearest**

```grain
nearest : (num: WasmF32) => WasmF32
```

### WasmF32.**sqrt**

```grain
sqrt : (num: WasmF32) => WasmF32
```

### WasmF32.**(+)**

```grain
(+) : (left: WasmF32, right: WasmF32) => WasmF32
```

### WasmF32.**(-)**

```grain
(-) : (left: WasmF32, right: WasmF32) => WasmF32
```

### WasmF32.**(*)**

```grain
(*) : (left: WasmF32, right: WasmF32) => WasmF32
```

### WasmF32.**(/)**

```grain
(/) : (left: WasmF32, right: WasmF32) => WasmF32
```

### WasmF32.**copySign**

```grain
copySign : (left: WasmF32, right: WasmF32) => WasmF32
```

### WasmF32.**min**

```grain
min : (left: WasmF32, right: WasmF32) => WasmF32
```

### WasmF32.**max**

```grain
max : (left: WasmF32, right: WasmF32) => WasmF32
```

### WasmF32.**(==)**

```grain
(==) : (left: WasmF32, right: WasmF32) => Bool
```

### WasmF32.**(!=)**

```grain
(!=) : (left: WasmF32, right: WasmF32) => Bool
```

### WasmF32.**(<)**

```grain
(<) : (left: WasmF32, right: WasmF32) => Bool
```

### WasmF32.**(<=)**

```grain
(<=) : (left: WasmF32, right: WasmF32) => Bool
```

### WasmF32.**(>)**

```grain
(>) : (left: WasmF32, right: WasmF32) => Bool
```

### WasmF32.**(>=)**

```grain
(>=) : (left: WasmF32, right: WasmF32) => Bool
```

### WasmF32.**reinterpretI32**

```grain
reinterpretI32 : (num: WasmI32) => WasmF32
```

### WasmF32.**convertI32S**

```grain
convertI32S : (num: WasmI32) => WasmF32
```

### WasmF32.**convertI32U**

```grain
convertI32U : (num: WasmI32) => WasmF32
```

### WasmF32.**convertI64S**

```grain
convertI64S : (num: WasmI64) => WasmF32
```

### WasmF32.**convertI64U**

```grain
convertI64U : (num: WasmI64) => WasmF32
```

### WasmF32.**demoteF64**

```grain
demoteF64 : (num: WasmF64) => WasmF32
```

