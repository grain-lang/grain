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

### Memory.**utoa32Buffered**

```grain
utoa32Buffered : Box<(WasmI32, WasmI32, WasmI32) -> Void>
```

### Memory.**decimalCount32**

```grain
decimalCount32 : Box<WasmI32 -> WasmI32>
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

