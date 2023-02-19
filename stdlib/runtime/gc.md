---
title: GC
---

## Values

Functions and constants included in the GC module.

### GC.**decimalCount32**

```grain
decimalCount32 : Box<WasmI32 -> WasmI32>
```

### GC.**utoa32Buffered**

```grain
utoa32Buffered : Box<(WasmI32, WasmI32, WasmI32) -> Void>
```

### GC.**malloc**

```grain
malloc : WasmI32 -> WasmI32
```

### GC.**free**

```grain
free : WasmI32 -> Void
```

### GC.**incRef**

```grain
incRef : WasmI32 -> WasmI32
```

### GC.**decRef**

```grain
decRef : WasmI32 -> WasmI32
```

