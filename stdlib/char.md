---
title: Char
---

Utilities for working with chars.

A Grain `Char` represents a single [Unicode scalar value](https://www.unicode.org/glossary/#unicode_scalar_value)

```grain
import Char from "char"
```

### Char.**min**

```grain
min : Number
```

The minimum valid Unicode charCode.

### Char.**max**

```grain
max : Number
```

The maximum valid Unicode charCode.

### Char.**isValid**

```grain
isValid : Number -> Bool
```

Determines whether the given character code is a valid Unicode Scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`charCode`|`Number`|The value to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the number refers to a valid Unicode character, `false` otherwise|

### Char.**code**

```grain
code : Char -> Number
```

Determines the Unicode code point for the given character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`character`|`Char`|The character|

Returns:

|type|description|
|----|-----------|
|`Number`|The integer with the value of the Unicode code point for the given character|

### Char.**fromCode**

```grain
fromCode : Number -> Char
```

Determines the character from the given Unicode code point.

Parameters:

|param|type|description|
|-----|----|-----------|
|`codePoint`|`Number`|The Unicode code point|

Returns:

|type|description|
|----|-----------|
|`Char`|The character for the given code point if valid|

### Char.**succ**

```grain
succ : Char -> Char
```

Returns the next valid Unicode character by code point. Fails if the input character is U+10FFFF.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The input character|

Returns:

|type|description|
|----|-----------|
|`Char`|The next valid Unicode character by code point|

### Char.**pred**

```grain
pred : Char -> Char
```

Returns the previous valid Unicode character by code point. Fails if the input character is U+0000.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The input character|

Returns:

|type|description|
|----|-----------|
|`Char`|The previous valid Unicode character by code point|

### Char.**toString**

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

