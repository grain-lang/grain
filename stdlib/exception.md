---
title: Exception
---

Utilities for working with the Exception type.

The Exception type represents an error that has occurred during computation.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
from "exception" include Exception
```

```grain
exception ExampleError(Number)
```

```grain
exception ExampleError
```

## Values

Functions and constants included in the Exception module.

### Exception.**registerPrinter**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
registerPrinter : (printer: (Exception => Option<String>)) => Void
```

Registers an exception printer. When an exception is thrown, all registered
printers are called in order from the most recently registered printer to
the least recently registered printer. The first `Some` value returned is
used as the exception's string value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`printer`|`Exception => Option<String>`|The exception printer to register|

Examples:

```grain
exception ExampleError(Number)

Exception.registerPrinter(e => {
  match (e) {
    ExampleError(lineNumber) =>
      Some("Error found on line: " ++ toString(lineNumber)),
    _ => None,
  }
})

throw ExampleError(1) // Error found on line: 1
```

