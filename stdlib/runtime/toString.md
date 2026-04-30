---
title: ToString
---

Utilities for converting Grain values to their string representations.

```grain
ToString.toString(true) == "true"
```

```grain
ToString.toString(123) == "123"
```

## Values

Functions and constants included in the ToString module.

### ToString.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
toString: (value: a) => String
```

Converts any Grain value to its string representation.

Parameters:

| param   | type | description                |
| ------- | ---- | -------------------------- |
| `value` | `a`  | The Grain value to convert |

Returns:

| type     | description                                  |
| -------- | -------------------------------------------- |
| `String` | The string representation of the Grain value |

