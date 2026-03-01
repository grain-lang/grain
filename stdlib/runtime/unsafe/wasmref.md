---
title: WasmRef
---

## Values

Functions and constants included in the WasmRef module.

### WasmRef.**fromGrain**

```grain
fromGrain: (value: a) => WasmRef
```

### WasmRef.**toGrain**

```grain
toGrain: (value: WasmRef) => a
```

### WasmRef.**isRefI31**

```grain
isRefI31: (ref: WasmRef) => Bool
```

### WasmRef.**isGrainHeapValue**

```grain
isGrainHeapValue: (ref: WasmRef) => Bool
```

### WasmRef.**i31GetS**

```grain
i31GetS: (ref: WasmRef) => WasmI32
```

### WasmRef.**i31GetU**

```grain
i31GetU: (ref: WasmRef) => WasmI32
```

### WasmRef.**arrayLen**

```grain
arrayLen: (array: WasmRef) => WasmI32
```

### WasmRef.**arrayI8GetS**

```grain
arrayI8GetS: (array: WasmRef, offset: WasmI32) => WasmI32
```

### WasmRef.**arrayI8GetU**

```grain
arrayI8GetU: (array: WasmRef, offset: WasmI32) => WasmI32
```

### WasmRef.**arrayI8Set**

```grain
arrayI8Set: (array: WasmRef, offset: WasmI32, value: WasmI32) => Void
```

### WasmRef.**arrayI64Get**

```grain
arrayI64Get: (array: WasmRef, offset: WasmI32) => WasmI64
```

### WasmRef.**arrayI64Set**

```grain
arrayI64Set: (array: WasmRef, offset: WasmI32, value: WasmI64) => Void
```

### WasmRef.**arrayAnyGet**

```grain
arrayAnyGet: (array: WasmRef, offset: WasmI32) => WasmRef
```

### WasmRef.**arrayAnySet**

```grain
arrayAnySet: (array: WasmRef, offset: WasmI32, value: WasmRef) => Void
```

