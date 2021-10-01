---
title: Char
---

Utilities for working with chars.

```grain
import Char from "char"
```

### Char.**min**

```grain
min : Number
```

The minimum value of Unicode characters.

### Char.**max**

```grain
max : Number
```

The maximum value of Unicode characters.

### Char.**isValid**

```grain
isValid : Number -> Bool
```

Returns true if the given number is a valid Unicode scalar value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`n`|`Number`|The value to check.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the number refers to a valid character.|

### Char.**code**

```grain
code : Char -> Number
```

Returns the Unicode code point for the character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The input character.|

Returns:

|type|description|
|----|-----------|
|`Number`|The characterCode.|

### Char.**fromCode**

```grain
fromCode : Number -> Char
```

Returns the Char for the given code point. Fails if the code point is invalid.

Parameters:

|param|type|description|
|-----|----|-----------|
|`codePoint`|`Number`|The Unicode code point.|

Returns:

|type|description|
|----|-----------|
|`Char`|The character that the charCode represents.|

### Char.**succ**

```grain
succ : Char -> Char
```

Returns the next valid Unicode character by code point. Fails if the input character is U+10FFFF.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|the input character.|

Returns:

|type|description|
|----|-----------|
|`Char`|The next valid Unicode character by code point.|

### Char.**pred**

```grain
pred : Char -> Char
```

### Char.**toString**

```grain
toString : Char -> String
```

Creates a new string containing the character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to convert.|

Returns:

|type|description|
|----|-----------|
|`String`|The character as a string.|

