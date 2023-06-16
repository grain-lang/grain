---
title: Memory
---

## Values

Functions and constants included in the Memory module.

### Memory.**malloc**

```grain
malloc : (size: WasmI32) => WasmI32
```

### Memory.**free**

```grain
free : (userPtr: WasmI32) => Void
```

### Memory.**incRef**

```grain
incRef : (userPtr: WasmI32) => WasmI32
```

### Memory.**decRef**

```grain
decRef : (userPtr: WasmI32) => WasmI32
```

### Memory.**copy**

```grain
copy : (dest: WasmI32, src: WasmI32, n: WasmI32) => Void
```

### Memory.**fill**

```grain
fill : (dest: WasmI32, c: WasmI32, n: WasmI32) => Void
```

### Memory.**compare**

```grain
compare : (WasmI32, WasmI32, WasmI32) => WasmI32
```

