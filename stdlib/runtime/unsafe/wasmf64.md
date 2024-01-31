---
title: WasmF64
---

## Values

Functions and constants included in the WasmF64 module.

### WasmF64.**load**

```grain
load : (ptr: WasmI32, offset: WasmI32) => WasmF64
```

### WasmF64.**store**

```grain
store : (ptr: WasmI32, value: WasmF64, offset: WasmI32) => Void
```

### WasmF64.**neg**

```grain
neg : (num: WasmF64) => WasmF64
```

### WasmF64.**abs**

```grain
abs : (num: WasmF64) => WasmF64
```

### WasmF64.**ceil**

```grain
ceil : (num: WasmF64) => WasmF64
```

### WasmF64.**floor**

```grain
floor : (num: WasmF64) => WasmF64
```

### WasmF64.**trunc**

```grain
trunc : (num: WasmF64) => WasmF64
```

### WasmF64.**nearest**

```grain
nearest : (num: WasmF64) => WasmF64
```

### WasmF64.**sqrt**

```grain
sqrt : (num: WasmF64) => WasmF64
```

### WasmF64.**(+)**

```grain
(+) : (left: WasmF64, right: WasmF64) => WasmF64
```

### WasmF64.**(-)**

```grain
(-) : (left: WasmF64, right: WasmF64) => WasmF64
```

### WasmF64.**(*)**

```grain
(*) : (left: WasmF64, right: WasmF64) => WasmF64
```

### WasmF64.**(/)**

```grain
(/) : (left: WasmF64, right: WasmF64) => WasmF64
```

### WasmF64.**copySign**

```grain
copySign : (left: WasmF64, right: WasmF64) => WasmF64
```

### WasmF64.**min**

```grain
min : (left: WasmF64, right: WasmF64) => WasmF64
```

### WasmF64.**max**

```grain
max : (left: WasmF64, right: WasmF64) => WasmF64
```

### WasmF64.**(==)**

```grain
(==) : (left: WasmF64, right: WasmF64) => Bool
```

### WasmF64.**(!=)**

```grain
(!=) : (left: WasmF64, right: WasmF64) => Bool
```

### WasmF64.**(<)**

```grain
(<) : (left: WasmF64, right: WasmF64) => Bool
```

### WasmF64.**(<=)**

```grain
(<=) : (left: WasmF64, right: WasmF64) => Bool
```

### WasmF64.**(>)**

```grain
(>) : (left: WasmF64, right: WasmF64) => Bool
```

### WasmF64.**(>=)**

```grain
(>=) : (left: WasmF64, right: WasmF64) => Bool
```

### WasmF64.**reinterpretI64**

```grain
reinterpretI64 : (num: WasmI64) => WasmF64
```

### WasmF64.**convertI32S**

```grain
convertI32S : (num: WasmI32) => WasmF64
```

### WasmF64.**convertI32U**

```grain
convertI32U : (num: WasmI32) => WasmF64
```

### WasmF64.**convertI64S**

```grain
convertI64S : (num: WasmI64) => WasmF64
```

### WasmF64.**convertI64U**

```grain
convertI64U : (num: WasmI64) => WasmF64
```

### WasmF64.**promoteF32**

```grain
promoteF32 : (num: WasmF32) => WasmF64
```

