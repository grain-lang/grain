---
title: Parse
---

## Values

Functions and constants included in the Parse module.

### Parse.**isFastPath**

```grain
isFastPath :
  (exponent: WasmI32, mantissa: WasmI64, negative: Bool, manyDigits: Bool) =>
   Bool
```

### Parse.**parseFloat**

```grain
parseFloat : (string: String) => Result<Number, String>
```

