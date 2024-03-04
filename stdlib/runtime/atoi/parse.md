---
title: Parse
---

## Types

Type declarations included in the Parse module.

### Parse.**ParseIntError**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
enum ParseIntError {
  ParseIntEmptyString,
  ParseIntInvalidDigit,
  ParseIntInvalidRadix,
}
```

Represents an error that occurred trying to parse an integer.

Variants:

```grain
ParseIntEmptyString
```

Represents an error caused by trying to parse an empty string.

```grain
ParseIntInvalidDigit
```

Represents an error caused by trying to parse a string with an invalid character.

```grain
ParseIntInvalidRadix
```

Represents an error caused by trying to parse with an invalid radix.

## Values

Functions and constants included in the Parse module.

### Parse.**parseInt**

```grain
parseInt : (string: String, radix: Number) => Result<Number, ParseIntError>
```

