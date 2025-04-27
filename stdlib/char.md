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

## Types

Type declarations included in the Char module.

### Char.**Encoding**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
enum Encoding {
  UTF8,
  UTF16,
  UTF32,
}
```

Byte encodings

## Values

Functions and constants included in the Char module.

### Char.**min**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
min: Number
```

The minimum valid Unicode scalar value.

### Char.**max**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
max: Number
```

The maximum valid Unicode scalar value.

### Char.**isValid**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
isValid: (charCode: Number) => Bool
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
code: (char: Char) => Number
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
fromCode: (usv: Number) => Char
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
succ: (char: Char) => Char
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
pred: (char: Char) => Char
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
toString: (char: Char) => String
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

### Char.**encodedLength**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
encodedLength: (encoding: Encoding, char: Char) => Number
```

Returns the byte count of a character if encoded in the given encoding.

Parameters:

|param|type|description|
|-----|----|-----------|
|`encoding`|`Encoding`|The encoding to check|
|`char`|`Char`|The character|

Returns:

|type|description|
|----|-----------|
|`Number`|The byte count of the character in the given encoding|

Examples:

```grain
Char.encodedLength(Char.UTF8, 'a') == 1
```

```grain
Char.encodedLength(Char.UTF8, '🌾') == 4
```

```grain
Char.encodedLength(Char.UTF16, '©') == 1
```

### Char.**(<)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(<): (x: Char, y: Char) => Bool
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
(<=): (x: Char, y: Char) => Bool
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
(>): (x: Char, y: Char) => Bool
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
(>=): (x: Char, y: Char) => Bool
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

## Char.Ascii

Utilities for working with ASCII characters.

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
Char.Ascii.isAscii('1')
```

### Values

Functions and constants included in the Char.Ascii module.

#### Char.Ascii.**min**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
min: Number
```

The minimum valid ASCII character code.

#### Char.Ascii.**max**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
max: Number
```

The maximum valid ASCII character code.

#### Char.Ascii.**isValid**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isValid: (char: Char) => Bool
```

Checks if the character is a valid ASCII character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the character is an ASCII character or `false` otherwise|

Examples:

```grain
assert Char.Ascii.isValid('1')
```

```grain
assert Char.Ascii.isValid('a')
```

```grain
assert !Char.Ascii.isValid('🌾')
```

#### Char.Ascii.**isDigit**

<details>
<summary>Added in <code>0.7.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Originally `Char.isAsciiDigit`</td></tr>
</tbody>
</table>
</details>

```grain
isDigit: (char: Char) => Bool
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
assert Char.Ascii.isDigit('1')
```

```grain
assert !Char.Ascii.isDigit('a')
```

#### Char.Ascii.**isAlpha**

<details>
<summary>Added in <code>0.7.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Originally `Char.isAsciiAlpha`</td></tr>
</tbody>
</table>
</details>

```grain
isAlpha: (char: Char) => Bool
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
assert Char.Ascii.isAlpha('a')
```

```grain
assert !Char.Ascii.isAlpha('1')
```

#### Char.Ascii.**isControl**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isControl: (char: Char) => Bool
```

Checks if the character is an ASCII control character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the character is an ASCII control character or `false` otherwise|

Examples:

```grain
assert Char.Ascii.isControl('\t')
```

```grain
assert Char.Ascii.isControl('\n')
```

```grain
assert !Char.Ascii.isControl('1')
```

```grain
assert !Char.Ascii.isControl('a')
```

#### Char.Ascii.**isWhitespace**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isWhitespace: (char: Char) => Bool
```

Checks if the character is an ASCII whitespace character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the character is an ASCII whitespace character or `false` otherwise|

Examples:

```grain
assert Char.isWhitespace('\t')
```

```grain
assert Char.isWhitespace('\n')
```

```grain
assert !Char.isWhitespace('1')
```

```grain
assert !Char.isWhitespace('a')
```

#### Char.Ascii.**isPunctuation**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isPunctuation: (char: Char) => Bool
```

Checks if the character is an ASCII punctuation character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the character is an ASCII punctuation character or `false` otherwise|

Examples:

```grain
assert Char.Ascii.isPunctuation('!')
```

```grain
assert !Char.Ascii.isPunctuation('1')
```

#### Char.Ascii.**isGraphic**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
isGraphic: (char: Char) => Bool
```

Checks if the character is an ASCII graphic character.

Parameters:

|param|type|description|
|-----|----|-----------|
|`char`|`Char`|The character to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the character is an ASCII graphic character or `false` otherwise|

Examples:

```grain
assert Char.Ascii.isGraphic('!')
```

```grain
assert !Char.Ascii.isGraphic('\t')
```

#### Char.Ascii.**toLowercase**

<details>
<summary>Added in <code>0.7.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Originally `Char.toAsciiLowercase`</td></tr>
</tbody>
</table>
</details>

```grain
toLowercase: (char: Char) => Char
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
assert Char.Ascii.toLowercase('B') == 'b'
```

#### Char.Ascii.**toUppercase**

<details>
<summary>Added in <code>0.7.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Originally `Char.toAsciiUppercase`</td></tr>
</tbody>
</table>
</details>

```grain
toUppercase: (char: Char) => Char
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
assert Char.Ascii.toUppercase('b') == 'B'
```

