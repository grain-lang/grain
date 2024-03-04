---
title: String
---

## Values

Functions and constants included in the String module.

### String.**concat**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
concat : (str1: String, str2: String) => String
```

Concatenate two strings.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str1`|`String`|The beginning string|
|`str2`|`String`|The ending string|

Returns:

|type|description|
|----|-----------|
|`String`|The combined string|

Examples:

```grain
"Foo" ++ "Bar" == "FooBar"
```

### String.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
toString : (value: a) => String
```

Converts the given operand to a string.
Provides a better representation of data types if those types are provided from the module.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The operand|

Returns:

|type|description|
|----|-----------|
|`String`|The operand, as a string|

### String.**print**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
print : (value: a, ?suffix: String) => Void
```

Prints the given operand to the console. Works for any type. Internally, calls `toString`
on the operand, so a better representation of data type will be printed if those types
are provided from the module.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The operand|
|`?suffix`|`String`|The string to print after the argument|

### String.**getCodePoint**

```grain
getCodePoint : (ptr: WasmI32) => WasmI32
```

