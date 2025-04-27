---
title: Exception
---

## Values

Functions and constants included in the Exception module.

### Exception.**registerBasePrinter**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
registerBasePrinter: (printer: (Exception => String)) => Void
```

Registers a base exception printer. If no other exception printers are
registered, the base printer is used to convert an exception to a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`printer`|`Exception => String`|The base exception printer to register|

### Exception.**registerPrinter**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
registerPrinter: (printer: (Exception => Option<String>)) => Void
```

Registers an exception printer. When an exception is thrown, all registered
printers are called in order from the most recently registered printer to
the least recently registered printer. The first `Some` value returned is
used as the exception's string value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`printer`|`Exception => Option<String>`|The exception printer to register|

### Exception.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
toString: (e: Exception) => String
```

Gets the string representation of the given exception.

Parameters:

|param|type|description|
|-----|----|-----------|
|`e`|`Exception`|The exception to stringify|

Returns:

|type|description|
|----|-----------|
|`String`|The string representation of the exception|

### Exception.**panicWithException**

```grain
panicWithException: (e: Exception) => a
```

Throws an uncatchable exception and traps.

Parameters:

|param|type|description|
|-----|----|-----------|
|`e`|`Exception`|The exception to throw|

