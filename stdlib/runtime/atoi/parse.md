---
title: Parse
---

## Types

Type declarations included in the Parse module.

### Parse.**ParseIntError**

```grain
enum ParseIntError {
  EmptyString,
  InvalidDigit,
  InvalidRadix,
}
```

Represents an error that can occur when parsing ints.

## Values

Functions and constants included in the Parse module.

### Parse.**parseInt**

```grain
parseInt : (string: String, radix: Number) => Result<Number, ParseIntError>
```

