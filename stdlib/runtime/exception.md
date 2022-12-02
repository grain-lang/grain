### Exception.**Option**

```grain
type Option<a>
```

### Exception.**printers**

```grain
printers : WasmI32
```

### Exception.**dangerouslyRegisterBasePrinter**

```grain
dangerouslyRegisterBasePrinter : a -> Void
```

### Exception.**dangerouslyRegisterPrinter**

```grain
dangerouslyRegisterPrinter : a -> Void
```

### Exception.**panic**

```grain
panic : String -> a
```

### Exception.**panicWithException**

```grain
panicWithException : Exception -> a
```

