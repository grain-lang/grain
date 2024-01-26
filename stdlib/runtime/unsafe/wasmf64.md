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
neg : (int: WasmF64) => WasmF64
```

### WasmF64.**abs**

```grain
abs : (int: WasmF64) => WasmF64
```

### WasmF64.**ceil**

```grain
ceil : (int: WasmF64) => WasmF64
```

### WasmF64.**floor**

```grain
floor : (int: WasmF64) => WasmF64
```

### WasmF64.**trunc**

```grain
trunc : (int: WasmF64) => WasmF64
```

### WasmF64.**nearest**

```grain
nearest : (int: WasmF64) => WasmF64
```

### WasmF64.**sqrt**

```grain
sqrt : (int: WasmF64) => WasmF64
```

### WasmF64.**(+)**

```grain
(+) : (value1: WasmF64, value2: WasmF64) => WasmF64
```

### WasmF64.**(-)**

```grain
(-) : (value1: WasmF64, value2: WasmF64) => WasmF64
```

### WasmF64.**(*)**

```grain
(*) : (value1: WasmF64, value2: WasmF64) => WasmF64
```

### WasmF64.**(/)**

```grain
(/) : (value1: WasmF64, value2: WasmF64) => WasmF64
```

### WasmF64.**copySign**

```grain
copySign : (value1: WasmF64, value2: WasmF64) => WasmF64
```

### WasmF64.**min**

```grain
min : (value1: WasmF64, value2: WasmF64) => WasmF64
```

### WasmF64.**max**

```grain
max : (value1: WasmF64, value2: WasmF64) => WasmF64
```

### WasmF64.**(==)**

```grain
(==) : (value1: WasmF64, value2: WasmF64) => Bool
```

### WasmF64.**(!=)**

```grain
(!=) : (value1: WasmF64, value2: WasmF64) => Bool
```

### WasmF64.**(<)**

```grain
(<) : (value1: WasmF64, value2: WasmF64) => Bool
```

### WasmF64.**(<=)**

```grain
(<=) : (value1: WasmF64, value2: WasmF64) => Bool
```

### WasmF64.**(>)**

```grain
(>) : (value1: WasmF64, value2: WasmF64) => Bool
```

### WasmF64.**(>=)**

```grain
(>=) : (value1: WasmF64, value2: WasmF64) => Bool
```

### WasmF64.**reinterpretI64**

```grain
reinterpretI64 : (int: WasmI64) => WasmF64
```

### WasmF64.**convertI32S**

```grain
convertI32S : (int: WasmI32) => WasmF64
```

### WasmF64.**convertI32U**

```grain
convertI32U : (int: WasmI32) => WasmF64
```

### WasmF64.**convertI64S**

```grain
convertI64S : (int: WasmI64) => WasmF64
```

### WasmF64.**convertI64U**

```grain
convertI64U : (int: WasmI64) => WasmF64
```

### WasmF64.**promoteF32**

```grain
promoteF32 : (int: WasmF32) => WasmF64
```

