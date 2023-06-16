---
title: WasmF32
---

## Values

Functions and constants included in the WasmF32 module.

### WasmF32.**load**

```grain
load : (WasmI32, WasmI32) => WasmF32
```

### WasmF32.**store**

```grain
store : (WasmI32, WasmF32, WasmI32) => Void
```

### WasmF32.**neg**

```grain
neg : WasmF32 => WasmF32
```

### WasmF32.**abs**

```grain
abs : WasmF32 => WasmF32
```

### WasmF32.**ceil**

```grain
ceil : WasmF32 => WasmF32
```

### WasmF32.**floor**

```grain
floor : WasmF32 => WasmF32
```

### WasmF32.**trunc**

```grain
trunc : WasmF32 => WasmF32
```

### WasmF32.**nearest**

```grain
nearest : WasmF32 => WasmF32
```

### WasmF32.**sqrt**

```grain
sqrt : WasmF32 => WasmF32
```

### WasmF32.**(+)**

```grain
(+) : (WasmF32, WasmF32) => WasmF32
```

### WasmF32.**(-)**

```grain
(-) : (WasmF32, WasmF32) => WasmF32
```

### WasmF32.**(*)**

```grain
(*) : (WasmF32, WasmF32) => WasmF32
```

### WasmF32.**(/)**

```grain
(/) : (WasmF32, WasmF32) => WasmF32
```

### WasmF32.**copySign**

```grain
copySign : (WasmF32, WasmF32) => WasmF32
```

### WasmF32.**min**

```grain
min : (WasmF32, WasmF32) => WasmF32
```

### WasmF32.**max**

```grain
max : (WasmF32, WasmF32) => WasmF32
```

### WasmF32.**(==)**

```grain
(==) : (WasmF32, WasmF32) => Bool
```

### WasmF32.**(!=)**

```grain
(!=) : (WasmF32, WasmF32) => Bool
```

### WasmF32.**(<)**

```grain
(<) : (WasmF32, WasmF32) => Bool
```

### WasmF32.**(<=)**

```grain
(<=) : (WasmF32, WasmF32) => Bool
```

### WasmF32.**(>)**

```grain
(>) : (WasmF32, WasmF32) => Bool
```

### WasmF32.**(>=)**

```grain
(>=) : (WasmF32, WasmF32) => Bool
```

### WasmF32.**reinterpretI32**

```grain
reinterpretI32 : WasmI32 => WasmF32
```

### WasmF32.**convertI32S**

```grain
convertI32S : WasmI32 => WasmF32
```

### WasmF32.**convertI32U**

```grain
convertI32U : WasmI32 => WasmF32
```

### WasmF32.**convertI64S**

```grain
convertI64S : WasmI64 => WasmF32
```

### WasmF32.**convertI64U**

```grain
convertI64U : WasmI64 => WasmF32
```

### WasmF32.**demoteF64**

```grain
demoteF64 : WasmF64 => WasmF32
```

