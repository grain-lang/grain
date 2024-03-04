---
title: String
---

Utilities for working with strings.

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally named `strings`</td></tr>
<tr><td><code>0.2.0</code></td><td>Renamed to `string`</td></tr>
</tbody>
</table>
</details>

```grain
from "string" include String
```

## Types

Type declarations included in the String module.

### String.**Encoding**

```grain
enum Encoding {
  UTF8,
  UTF16_BE,
  UTF16_LE,
  UTF32_BE,
  UTF32_LE,
}
```

Byte encodings

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
String.concat("Foo", "Bar") == "FooBar"
```

### String.**length**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
length : (string: String) => Number
```

Returns the character length of the input string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of characters in the string|

Examples:

```grain
String.length("Hello world") == 11
```

### String.**byteLength**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
byteLength : (string: String) => Number
```

Returns the byte length of the input string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of bytes in the string|

Examples:

```grain
String.byteLength("🌾") == 4
```

### String.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty : (string: String) => Bool
```

Determines if the string contains no characters.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the string is empty and `false` otherwise|

### String.**indexOf**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
indexOf : (search: String, string: String) => Option<Number>
```

Finds the first position of a substring in the input string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`search`|`String`|The substring to find|
|`string`|`String`|The string to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<Number>`|`Some(position)` containing the starting position of the substring if found or `None` otherwise|

Examples:

```grain
String.indexOf("world", "Hello world") == Some(6)
```

### String.**lastIndexOf**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
lastIndexOf : (search: String, string: String) => Option<Number>
```

Finds the last position of a substring in the input string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`search`|`String`|The substring to find|
|`string`|`String`|The string to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<Number>`|`Some(position)` containing the starting position of the substring if found or `None` otherwise|

Examples:

```grain
String.lastIndexOf("world", "Hello world world") == Some(12)
```

### String.**charCodeAt**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
charCodeAt : (position: Number, string: String) => Number
```

Get the Unicode code point at the position in the input string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`position`|`Number`|The position to check|
|`string`|`String`|The string to search|

Returns:

|type|description|
|----|-----------|
|`Number`|The character code at the provided position|

Throws:

`Failure(String)`

* When the `position` is out of bounds

`MalformedUnicode`

* When the `string` is malformed

Examples:

```grain
String.charCodeAt(5, "Hello world") == 32
```

### String.**charAt**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
charAt : (position: Number, string: String) => Char
```

Get the character at the position in the input string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`position`|`Number`|The position to check|
|`string`|`String`|The string to search|

Returns:

|type|description|
|----|-----------|
|`Char`|The character at the provided position|

Throws:

`Failure(String)`

* When the `position` is out of bounds

`MalformedUnicode`

* When the `string` is malformed

Examples:

```grain
String.charAt(5, "Hello world") == ' '
```

### String.**explode**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
explode : (string: String) => Array<Char>
```

Split a string into its Unicode characters.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to split|

Returns:

|type|description|
|----|-----------|
|`Array<Char>`|An array containing all characters in the string|

Throws:

`MalformedUnicode`

* When the `string` is malformed

Examples:

```grain
String.explode("Hello") == [> 'H', 'e', 'l', 'l', 'o']
```

### String.**implode**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
implode : (arr: Array<Char>) => String
```

Create a string from an array of characters.

Parameters:

|param|type|description|
|-----|----|-----------|
|`arr`|`Array<Char>`|The array to combine|

Returns:

|type|description|
|----|-----------|
|`String`|A string representation of the array of characters|

Examples:

```grain
String.implode([> 'H', 'e', 'l', 'l', 'o']) == "Hello"
```

### String.**reverse**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.5</code></summary>
No other changes yet.
</details>

```grain
reverse : (string: String) => String
```

Create a string that is the given string reversed.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to reverse|

Returns:

|type|description|
|----|-----------|
|`String`|A string whose characters are in the reverse order of the given string|

Examples:

```grain
String.reverse("olleH") == "Hello"
```

### String.**split**

```grain
split : (separator: String, string: String) => Array<String>
```

Split a string by the given separator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`separator`|`String`|The separator to split on|
|`string`|`String`|The string to split|

Returns:

|type|description|
|----|-----------|
|`Array<String>`|An array of substrings from the initial string|

Throws:

`MalformedUnicode`

* When the `string` is malformed

Examples:

```grain
String.split(" ", "Hello world") == [> "Hello", "world"]
```

### String.**slice**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Default `end` to the String length</td></tr>
</tbody>
</table>
</details>

```grain
slice : (start: Number, ?end: Number, string: String) => String
```

Get a portion of a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The start position of the substring|
|`?end`|`Number`|The end position of the substring, exclusive|
|`string`|`String`|The input string|

Returns:

|type|description|
|----|-----------|
|`String`|The substring from the initial string|

Throws:

`IndexOutOfBounds`

* When `start` is out of bounds
* When `end` is out of bounds

`InvalidArgument(String)`

* When the `start` index is not an integer
* When the `to` index is not an integer
* When `start` is greater than `end`

Examples:

```grain
String.slice(0, end=5, "Hello world") == "Hello"
```

```grain
String.slice(0, "Hello world") == "Hello world"
```

### String.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
contains : (search: String, string: String) => Bool
```

Check if a string contains a substring.

Parameters:

|param|type|description|
|-----|----|-----------|
|`search`|`String`|The substring to check|
|`string`|`String`|The string to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the input string contains the search value or `false` otherwise|

Examples:

```grain
String.contains("world", "Hello world") == true
```

### String.**startsWith**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
startsWith : (search: String, string: String) => Bool
```

Check if a string begins with another string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`search`|`String`|The string to compare to the start|
|`string`|`String`|The string to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the input string starts with the search value or `false` otherwise|

Examples:

```grain
String.startsWith("Hello", "Hello world") == true
```

### String.**endsWith**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
endsWith : (search: String, string: String) => Bool
```

Check if a string ends with another string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`search`|`String`|The string to compare to the end|
|`string`|`String`|The string to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the input string ends with the search value or `false` otherwise|

Examples:

```grain
String.endsWith("world", "Hello world") == true
```

### String.**replaceFirst**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
replaceFirst :
  (searchPattern: String, replacement: String, string: String) => String
```

Replaces the first appearance of the search pattern in the string with the replacement value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`searchPattern`|`String`|The string to replace|
|`replacement`|`String`|The replacement|
|`string`|`String`|The string to change|

Returns:

|type|description|
|----|-----------|
|`String`|A new string with the first occurrence of the search pattern replaced|

Examples:

```grain
String.replaceFirst("🌾", "🌎", "Hello 🌾🌾") == "Hello 🌎🌾"
```

### String.**replaceLast**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
replaceLast :
  (searchPattern: String, replacement: String, string: String) => String
```

Replaces the last appearance of the search pattern in the string with the replacement value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`searchPattern`|`String`|The string to replace|
|`replacement`|`String`|The replacement|
|`string`|`String`|The string to change|

Returns:

|type|description|
|----|-----------|
|`String`|A new string with the last occurrence of the search pattern replaced|

Examples:

```grain
String.replaceLast("🌾", "🌎", "Hello 🌾🌾") == "Hello 🌾🌎"
```

### String.**replaceAll**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
replaceAll :
  (searchPattern: String, replacement: String, string: String) => String
```

Replaces every appearance of the search pattern in the string with the replacement value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`searchPattern`|`String`|The string to replace|
|`replacement`|`String`|The replacement|
|`string`|`String`|The string to change|

Returns:

|type|description|
|----|-----------|
|`String`|A new string with each occurrence of the search pattern replaced|

Examples:

```grain
String.replaceAll("🌾", "🌎", "Hello 🌾🌾") == "Hello 🌎🌎"
```

### String.**encodeAt**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Added `includeBom` default argument</td></tr>
</tbody>
</table>
</details>

```grain
encodeAt :
  (string: String, encoding: Encoding, dest: Bytes, destPos: Number,
   ?includeBom: Bool) => Bytes
```

Encodes the given string into a byte sequence at the supplied position using the encoding scheme provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|
|`dest`|`Bytes`|The byte sequence that will be copied|
|`destPos`|`Number`|The location in the byte sequence to write the output|
|`?includeBom`|`Bool`|Whether or not to include a byte order marker (false by default)|

Returns:

|type|description|
|----|-----------|
|`Bytes`|A copy of the input bytes with the encoded string replaced at the given position|

Throws:

`InvalidArgument(String)`

* When `destPos` is not an integer
* When `destPos` is negative

### String.**encode**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Added `includeBom` default argument</td></tr>
</tbody>
</table>
</details>

```grain
encode : (string: String, encoding: Encoding, ?includeBom: Bool) => Bytes
```

Encodes the given string using the given encoding scheme.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|
|`?includeBom`|`Bool`|Whether or not to include a byte order marker (false by default)|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The byte representation of the string in the given encoding|

### String.**decodeRange**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Added `keepBom` default argument</td></tr>
</tbody>
</table>
</details>

```grain
decodeRange :
  (bytes: Bytes, encoding: Encoding, start: Number, size: Number,
   ?keepBom: Bool) => String
```

Decodes the given byte sequence of the specified range into a string using the encoding scheme provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The input bytes|
|`encoding`|`Encoding`|The encoding to use|
|`start`|`Number`|The byte offset to begin decoding from|
|`size`|`Number`|The maximum number of bytes to decode|
|`?keepBom`|`Bool`|Whether or not to include a byte order marker (false by default)|

Returns:

|type|description|
|----|-----------|
|`String`|The decoded string|

Throws:

`InvalidArgument(String)`

* When `start` is not an integer
* When `start` is negative
* When `size` is not an integer
* When `size` is negative

### String.**decode**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Added `keepBom` default argument</td></tr>
</tbody>
</table>
</details>

```grain
decode : (bytes: Bytes, encoding: Encoding, ?keepBom: Bool) => String
```

Decodes the given byte sequence into a string using the given encoding scheme.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The input bytes|
|`encoding`|`Encoding`|The encoding to use|
|`?keepBom`|`Bool`|Whether or not to include a byte order marker (false by default)|

Returns:

|type|description|
|----|-----------|
|`String`|The decoded string|

### String.**forEachCodePoint**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
forEachCodePoint : (fn: (Number => Void), str: String) => Void
```

Iterates over Unicode code points in a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`Number => Void`|The iterator function|
|`str`|`String`|The string to iterate|

Examples:

```grain
String.forEachCodePoint(print, "Hello world")
```

### String.**forEachCodePointi**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
forEachCodePointi : (fn: ((Number, Number) => Void), str: String) => Void
```

Iterates over Unicode code points in a string. This is the same as
`forEachCodePoint`, but provides the code point's index in the string
as the second argument to the iterator function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(Number, Number) => Void`|The iterator function|
|`str`|`String`|The string to iterate|

Examples:

```grain
String.forEachCodePointi((codepoint, index) => print((codepoint, index)), "Hello world")
```

### String.**trimStart**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.2</code></summary>
No other changes yet.
</details>

```grain
trimStart : (string: String) => String
```

Trims the beginning of a string—removing any leading whitespace characters.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to be trimmed|

Returns:

|type|description|
|----|-----------|
|`String`|The trimmed string|

Examples:

```grain
String.trimStart("   Hello World") == "Hello World"
```

### String.**trimEnd**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.2</code></summary>
No other changes yet.
</details>

```grain
trimEnd : (string: String) => String
```

Trims the end of a string—removing any trailing whitespace characters.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to be trimmed|

Returns:

|type|description|
|----|-----------|
|`String`|The trimmed string|

Examples:

```grain
String.trimEnd("Hello World   ") == "Hello World"
```

### String.**trim**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.2</code></summary>
No other changes yet.
</details>

```grain
trim : (string: String) => String
```

Trims a string—removing all leading and trailing whitespace characters.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to be trimmed|

Returns:

|type|description|
|----|-----------|
|`String`|The trimmed string|

Examples:

```grain
String.trim("   Hello World   ") == "Hello World"
```

### String.**toAsciiLowercase**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toAsciiLowercase : (string: String) => String
```

Converts all ASCII uppercase characters in the string to lowercase.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to convert|

Returns:

|type|description|
|----|-----------|
|`String`|The lowercased string|

Examples:

```grain
assert String.toAsciiLowercase("aBc123") == "abc123"
```

### String.**toAsciiUppercase**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toAsciiUppercase : (string: String) => String
```

Converts all ASCII lowercase characters in the string to uppercase.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The string to convert|

Returns:

|type|description|
|----|-----------|
|`String`|The uppercased string|

Examples:

```grain
assert String.toAsciiUppercase("aBc123") == "ABC123"
```

