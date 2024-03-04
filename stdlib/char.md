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
from "char" include Char
```

```grain
'a'
```

```grain
'1'
```

```grain
'🌾'
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
isValid : (charCode: Number) => Bool
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

Examples:

```grain
Char.isValid(0) == true
```

```grain
Char.isValid(-1) == false
```

### Char.**code**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
code : (char: Char) => Number
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

Examples:

```grain
Char.code('a') == 97
```

```grain
Char.code('🌾') == 127806
```

### Char.**fromCode**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
fromCode : (usv: Number) => Char
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

Examples:

```grain
Char.fromCode(97) == 'a'
```

```grain
Char.fromCode(127806) == '🌾'
```

### Char.**succ**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
succ : (char: Char) => Char
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

Examples:

```grain
Char.succ('a') == 'b'
```

```grain
Char.succ('1') == '2'
```

### Char.**pred**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
pred : (char: Char) => Char
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

Examples:

```grain
Char.pred('b') == 'a'
```

```grain
Char.pred('2') == '1'
```

### Char.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
toString : (char: Char) => String
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

Examples:

```grain
Char.toString('a') == "a"
```

```grain
Char.toString('🌾') == "🌾"
```

### Char.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<) : (x: Char, y: Char) => Bool
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

Examples:

```grain
use Char.{ (<) }
assert 'a' < 'b'
```

```grain
use Char.{ (<) }
assert '1' < '2'
```

### Char.**(<=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<=) : (x: Char, y: Char) => Bool
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

Examples:

```grain
use Char.{ (<=) }
assert 'a' <= 'b'
```

```grain
use Char.{ (<=) }
assert '1' <= '2'
```

```grain
use Char.{ (<=) }
assert 'a' <= 'a'
```

### Char.**(>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>) : (x: Char, y: Char) => Bool
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

Examples:

```grain
use Char.{ (>) }
assert 'b' > 'a'
```

```grain
use Char.{ (>) }
assert '2' > '1'
```

### Char.**(>=)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(>=) : (x: Char, y: Char) => Bool
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

Examples:

```grain
use Char.{ (>=) }
assert 'b' >= 'a'
```

```grain
use Char.{ (>=) }
assert '2' >= '1'
```

```grain
use Char.{ (>=) }
assert 'a' >= 'a'
```

### Char.**isAsciiDigit**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
isAsciiDigit : (char: Char) => Bool
```

Checks if the character is an ASCII digit.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the character is an ASCII digit or `false` otherwise|

Examples:

```grain
assert Char.isAsciiDigit('1')
```

```grain
assert !Char.isAsciiDigit('a')
```

### Char.**isAsciiAlpha**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
isAsciiAlpha : (char: Char) => Bool
```

Checks if the character is an ASCII alphabetical character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the character is an ASCII alphabetical or `false` otherwise|

Examples:

```grain
assert Char.isAsciiAlpha('a')
```

```grain
assert !Char.isAsciiAlpha('1')
```

### Char.**toAsciiLowercase**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toAsciiLowercase : (char: Char) => Char
```

Converts the character to ASCII lowercase if it is an ASCII uppercase character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to convert|

Returns:

|type|description|
|----|-----------|
|`Char`|The lowercased character|

Examples:

```grain
assert Char.toAsciiLowercase('B') == 'b'
```

### Char.**toAsciiUppercase**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toAsciiUppercase : (char: Char) => Char
```

Converts the character to ASCII uppercase if it is an ASCII lowercase character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to convert|

Returns:

|type|description|
|----|-----------|
|`Char`|The uppercased character|

Examples:

```grain
assert Char.toAsciiUppercase('b') == 'B'
```

