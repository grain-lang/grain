---
title: Char
---

Utilities for working with the Char type.

The Char type represents a single [Unicode scalar value](https://www.unicode.org/glossary/#unicode_scalar_value).

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
import Char from "char"
```

## Values

Functions and constants included in the Char module.

### Char.**min**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
min : Number
```

The minimum valid Unicode scalar value.

### Char.**max**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
max : Number
```

The maximum valid Unicode scalar value.

### Char.**isValid**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
isValid : Number -> Bool
```

Determines whether the given character code is a valid Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`charCode`|`Number`|The number to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the number refers to a valid Unicode scalar value or `false` otherwise|

### Char.**code**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
code : Char -> Number
```

Determines the Unicode scalar value for a character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character|

Returns:

|type|description|
|----|-----------|
|`Number`|The Unicode scalar value for the given character|

### Char.**fromCode**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
fromCode : Number -> Char
```

Creates a character from the given Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`usv`|`Number`|The Unicode scalar value|

Returns:

|type|description|
|----|-----------|
|`Char`|The character for the given Unicode scalar value|

Throws:

`InvalidArgument(String)`

* When the Unicode scalar value is invalid

### Char.**succ**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
succ : Char -> Char
```

Returns the next valid character by Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character|

Returns:

|type|description|
|----|-----------|
|`Char`|The next valid character by Unicode scalar value|

Throws:

`Failure(String)`

* When the input character is the maximum valid Unicode scalar value

### Char.**pred**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
pred : Char -> Char
```

Returns the previous valid character by Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character|

Returns:

|type|description|
|----|-----------|
|`Char`|The previous valid character by Unicode scalar value|

Throws:

`Failure(String)`

* When the input character is the minimum valid Unicode scalar value

### Char.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
toString : Char -> String
```

Converts the given character to a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to convert|

Returns:

|type|description|
|----|-----------|
|`String`|A string containing the given character|

