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
neg : (int: WasmF32) => WasmF32
```

### WasmF32.**abs**

```grain
abs : (int: WasmF32) => WasmF32
```

### WasmF32.**ceil**

```grain
ceil : (int: WasmF32) => WasmF32
```

### WasmF32.**floor**

```grain
floor : (int: WasmF32) => WasmF32
```

### WasmF32.**trunc**

```grain
trunc : (int: WasmF32) => WasmF32
```

### WasmF32.**nearest**

```grain
nearest : (int: WasmF32) => WasmF32
```

### WasmF32.**sqrt**

```grain
sqrt : (int: WasmF32) => WasmF32
```

### WasmF32.**(+)**

```grain
(+) : (value1: WasmF32, value2: WasmF32) => WasmF32
```

### WasmF32.**(-)**

```grain
(-) : (value1: WasmF32, value2: WasmF32) => WasmF32
```

### WasmF32.**(*)**

```grain
(*) : (value1: WasmF32, value2: WasmF32) => WasmF32
```

### WasmF32.**(/)**

```grain
(/) : (value1: WasmF32, value2: WasmF32) => WasmF32
```

### WasmF32.**copySign**

```grain
copySign : (value1: WasmF32, value2: WasmF32) => WasmF32
```

### WasmF32.**min**

```grain
min : (value1: WasmF32, value2: WasmF32) => WasmF32
```

### WasmF32.**max**

```grain
max : (value1: WasmF32, value2: WasmF32) => WasmF32
```

### WasmF32.**(==)**

```grain
(==) : (value1: WasmF32, value2: WasmF32) => Bool
```

### WasmF32.**(!=)**

```grain
(!=) : (value1: WasmF32, value2: WasmF32) => Bool
```

### WasmF32.**(<)**

```grain
(<) : (value1: WasmF32, value2: WasmF32) => Bool
```

### WasmF32.**(<=)**

```grain
(<=) : (value1: WasmF32, value2: WasmF32) => Bool
```

### WasmF32.**(>)**

```grain
(>) : (value1: WasmF32, value2: WasmF32) => Bool
```

### WasmF32.**(>=)**

```grain
(>=) : (value1: WasmF32, value2: WasmF32) => Bool
```

### WasmF32.**reinterpretI32**

```grain
reinterpretI32 : (int: WasmI32) => WasmF32
```

### WasmF32.**convertI32S**

```grain
convertI32S : (int: WasmI32) => WasmF32
```

### WasmF32.**convertI32U**

```grain
convertI32U : (int: WasmI32) => WasmF32
```

### WasmF32.**convertI64S**

```grain
convertI64S : (int: WasmI64) => WasmF32
```

### WasmF32.**convertI64U**

```grain
convertI64U : (int: WasmI64) => WasmF32
```

### WasmF32.**demoteF64**

```grain
demoteF64 : (int: WasmF64) => WasmF32
```

