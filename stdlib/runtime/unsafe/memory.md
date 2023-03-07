---
title: Memory
---

## Values

Functions and constants included in the Memory module.

### Memory.**malloc**

```grain
malloc : WasmI32 -> WasmI32
```

### Memory.**free**

```grain
free : WasmI32 -> Void
```

### Memory.**incRef**

```grain
incRef : WasmI32 -> WasmI32
```

### Memory.**decRef**

```grain
decRef : WasmI32 -> WasmI32
```

### Memory.**copy**

```grain
copy : (WasmI32, WasmI32, WasmI32) -> Void
```

### Memory.**fill**

```grain
fill : (WasmI32, WasmI32, WasmI32) -> Void
```

### Memory.**compare**

```grain
compare : (WasmI32, WasmI32, WasmI32) -> WasmI32
```

