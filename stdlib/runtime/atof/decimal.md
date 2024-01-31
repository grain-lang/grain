---
title: Decimal
---

## Types

Type declarations included in the Decimal module.

### Decimal.**Decimal**

```grain
record Decimal {
  numDigits: Int32,
  decimalPoint: Int32,
  truncated: Bool,
  digits: Bytes,
}
```

Fields:

|name|type|description|
|----|----|-----------|
|`numDigits`|`Int32`|The number of significant digits in the decimal.|
|`decimalPoint`|`Int32`|The offset of the decimal point in the significant digits.|
|`truncated`|`Bool`|If the number of significant digits stored in the decimal is truncated.|
|`digits`|`Bytes`|Buffer of the raw digits, in the range [0, 9].|

## Values

Functions and constants included in the Decimal module.

### Decimal.**_DECIMAL_POINT_RANGE**

```grain
_DECIMAL_POINT_RANGE : WasmI32
```

### Decimal.**tryAddDigit**

```grain
tryAddDigit : (d: Decimal, digit: WasmI32) => Void
```

### Decimal.**round**

```grain
round : (d: Decimal) => WasmI64
```

### Decimal.**get_TABLE**

```grain
get_TABLE : () => WasmI32
```

### Decimal.**get_TABLE_POW5**

```grain
get_TABLE_POW5 : () => WasmI32
```

### Decimal.**leftShift**

```grain
leftShift : (d: Decimal, shift: WasmI32) => Void
```

### Decimal.**rightShift**

```grain
rightShift : (d: Decimal, shift: WasmI32) => Void
```

### Decimal.**parseDecimal**

```grain
parseDecimal : (s: String) => Decimal
```

