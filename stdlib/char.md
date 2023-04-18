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
include "char"
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
isValid : (charCode: Number) -> Bool
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
code : (char: Char) -> Number
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
fromCode : (usv: Number) -> Char
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
succ : (char: Char) -> Char
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
pred : (char: Char) -> Char
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
toString : (char: Char) -> String
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

### Char.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<) : (x: Char, y: Char) -> Bool
```

Checks if the first character is less than the second character by Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Char`|The first character|
|`y`|`Char`|The second character|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first character is less than the second character or `false` otherwise|

### Char.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(<=) : (x: Char, y: Char) -> Bool
```

Checks if the first character is less than or equal to the second character by Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Char`|The first character|
|`y`|`Char`|The second character|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first character is less than or equal to the second character or `false` otherwise|

### Char.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>) : (x: Char, y: Char) -> Bool
```

Checks if the first character is greater than the second character by Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Char`|The first character|
|`y`|`Char`|The second character|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first character is greater than the second character or `false` otherwise|

### Char.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
(>=) : (x: Char, y: Char) -> Bool
```

Checks if the first character is greater than or equal to the second character by Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Char`|The first character|
|`y`|`Char`|The second character|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first character is greater than or equal to the second character or `false` otherwise|

