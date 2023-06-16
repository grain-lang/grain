---
title: WasmF64
---

## Values

Functions and constants included in the WasmF64 module.

### WasmF64.**load**

```grain
load : (WasmI32, WasmI32) => WasmF64
```

### WasmF64.**store**

```grain
store : (WasmI32, WasmF64, WasmI32) => Void
```

### WasmF64.**neg**

```grain
neg : WasmF64 => WasmF64
```

### WasmF64.**abs**

```grain
abs : WasmF64 => WasmF64
```

### WasmF64.**ceil**

```grain
ceil : WasmF64 => WasmF64
```

### WasmF64.**floor**

```grain
floor : WasmF64 => WasmF64
```

### WasmF64.**trunc**

```grain
trunc : WasmF64 => WasmF64
```

### WasmF64.**nearest**

```grain
nearest : WasmF64 => WasmF64
```

### WasmF64.**sqrt**

```grain
sqrt : WasmF64 => WasmF64
```

### WasmF64.**(+)**

```grain
(+) : (WasmF64, WasmF64) => WasmF64
```

### WasmF64.**(-)**

```grain
(-) : (WasmF64, WasmF64) => WasmF64
```

### WasmF64.**(*)**

```grain
(*) : (WasmF64, WasmF64) => WasmF64
```

### WasmF64.**(/)**

```grain
(/) : (WasmF64, WasmF64) => WasmF64
```

### WasmF64.**copySign**

```grain
copySign : (WasmF64, WasmF64) => WasmF64
```

### WasmF64.**min**

```grain
min : (WasmF64, WasmF64) => WasmF64
```

### WasmF64.**max**

```grain
max : (WasmF64, WasmF64) => WasmF64
```

### WasmF64.**(==)**

```grain
(==) : (WasmF64, WasmF64) => Bool
```

### WasmF64.**(!=)**

```grain
(!=) : (WasmF64, WasmF64) => Bool
```

### WasmF64.**(<)**

```grain
(<) : (WasmF64, WasmF64) => Bool
```

### WasmF64.**(<=)**

```grain
(<=) : (WasmF64, WasmF64) => Bool
```

### WasmF64.**(>)**

```grain
(>) : (WasmF64, WasmF64) => Bool
```

### WasmF64.**(>=)**

```grain
(>=) : (WasmF64, WasmF64) => Bool
```

### WasmF64.**reinterpretI64**

```grain
reinterpretI64 : WasmI64 => WasmF64
```

### WasmF64.**convertI32S**

```grain
convertI32S : WasmI32 => WasmF64
```

### WasmF64.**convertI32U**

```grain
convertI32U : WasmI32 => WasmF64
```

### WasmF64.**convertI64S**

```grain
convertI64S : WasmI64 => WasmF64
```

### WasmF64.**convertI64U**

```grain
convertI64U : WasmI64 => WasmF64
```

### WasmF64.**promoteF32**

```grain
promoteF32 : WasmF32 => WasmF64
```

