---
title: Exception
---

Utilities for working with the Exception type.

The Exception type represents an error that has occured during computation.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
import Exception from "exception"
```

## Values

Functions included in the Exception module.

### Exception.**registerPrinter**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
registerPrinter : (Exception -> Option<String>) -> Void
```

Registers an exception printer. When an exception is thrown, all registered
printers are called in order from the most recently registered printer to
the least recently registered printer. The first `Some` value returned is
used as the exception's string value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`printer`|`Exception -> Option<String>`|The exception printer to register|

