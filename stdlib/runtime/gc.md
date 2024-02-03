---
title: GC
---

## Values

Functions and constants included in the GC module.

### GC.**malloc**

```grain
malloc : (size: WasmI32) => WasmI32
```

### GC.**free**

```grain
free : (userPtr: WasmI32) => Void
```

### GC.**incRef**

```grain
incRef : (userPtr: WasmI32) => WasmI32
```

### GC.**decRef**

```grain
decRef : (userPtr: WasmI32) => WasmI32
```

