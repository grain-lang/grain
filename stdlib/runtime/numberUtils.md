---
title: NumberUtils
---

## Values

Functions and constants included in the NumberUtils module.

### NumberUtils.**_MAX_DOUBLE_LENGTH**

```grain
_MAX_DOUBLE_LENGTH : WasmI32
```

### NumberUtils.**get_POWERS10**

```grain
get_POWERS10 : () => WasmI32
```

### NumberUtils.**get_HEX_DIGITS**

```grain
get_HEX_DIGITS : () => WasmI32
```

### NumberUtils.**decimalCount32**

```grain
decimalCount32 : (value: WasmI32) => WasmI32
```

### NumberUtils.**utoa32Buffered**

```grain
utoa32Buffered : (buf: WasmI32, value: WasmI32, radix: WasmI32) => Void
```

### NumberUtils.**utoa32**

```grain
utoa32 : (value: WasmI32, radix: WasmI32) => String
```

### NumberUtils.**itoa32**

```grain
itoa32 : (value: WasmI32, radix: WasmI32) => String
```

### NumberUtils.**utoa64**

```grain
utoa64 : (value: WasmI64, radix: WasmI32) => String
```

### NumberUtils.**itoa64**

```grain
itoa64 : (value: WasmI64, radix: WasmI32) => String
```

### NumberUtils.**isFinite**

```grain
isFinite : (value: WasmF64) => Bool
```

### NumberUtils.**isNaN**

```grain
isNaN : (value: WasmF64) => Bool
```

### NumberUtils.**dtoa**

```grain
dtoa : (value: WasmF64) => String
```

