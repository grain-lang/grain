### Decimal.**Decimal**

```grain
record Decimal {
  numDigits: Int32,
  decimalPoint: Int32,
  truncated: Bool,
  digits: Bytes,
}
```

### Decimal.**_DECIMAL_POINT_RANGE**

```grain
_DECIMAL_POINT_RANGE : WasmI32
```

### Decimal.**tryAddDigit**

```grain
tryAddDigit : (Decimal, WasmI32) -> Void
```

### Decimal.**round**

```grain
round : Decimal -> WasmI64
```

### Decimal.**get_TABLE**

```grain
get_TABLE : () -> WasmI32
```

### Decimal.**get_TABLE_POW5**

```grain
get_TABLE_POW5 : () -> WasmI32
```

### Decimal.**leftShift**

```grain
leftShift : (Decimal, WasmI32) -> Void
```

### Decimal.**rightShift**

```grain
rightShift : (Decimal, WasmI32) -> Void
```

### Decimal.**parseDecimal**

```grain
parseDecimal : String -> Decimal
```

