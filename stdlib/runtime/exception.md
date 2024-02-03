---
title: Exception
---

## Values

Functions and constants included in the Exception module.

### Exception.**printers**

```grain
printers : WasmI32
```

### Exception.**dangerouslyRegisterBasePrinter**

```grain
dangerouslyRegisterBasePrinter : (f: a) => Void
```

### Exception.**dangerouslyRegisterPrinter**

```grain
dangerouslyRegisterPrinter : (f: a) => Void
```

### Exception.**panic**

```grain
panic : (msg: String) => a
```

### Exception.**panicWithException**

```grain
panicWithException : (e: Exception) => a
```

