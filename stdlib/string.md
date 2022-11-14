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
import String from "string"
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

Functions for working with the String data type.

### String.**concat**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
concat : (String, String) -> String
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
length : String -> Number
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
byteLength : String -> Number
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

### String.**indexOf**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
indexOf : (String, String) -> Option<Number>
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
lastIndexOf : (String, String) -> Option<Number>
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
charCodeAt : (Number, String) -> Number
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
charAt : (Number, String) -> Char
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
explode : String -> Array<Char>
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
implode : Array<Char> -> String
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
reverse : String -> String
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
split : (String, String) -> Array<String>
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

Examples:

```grain
String.split(" ", "Hello world") == [> "Hello", "world"]
```

### String.**slice**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
slice : (Number, Number, String) -> String
```

Get a portion of a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The start position of the substring|
|`to`|`Number`|The end position of the substring, exclusive|
|`string`|`String`|The input string|

Returns:

|type|description|
|----|-----------|
|`String`|The substring from the initial string|

Examples:

```grain
String.slice(0, 5, "Hello world") == "Hello"
```

### String.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
contains : (String, String) -> Bool
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
startsWith : (String, String) -> Bool
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
endsWith : (String, String) -> Bool
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
replaceFirst : (String, String, String) -> String
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
replaceLast : (String, String, String) -> String
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
replaceAll : (String, String, String) -> String
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

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
encodeAt : (String, Encoding, Bytes, Number) -> Bytes
```

Encodes the given string into a byte sequence at the supplied position, excluding any byte-order marker, using the encoding scheme provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|
|`dest`|`Bytes`|The byte sequence that will be copied|
|`destPos`|`Number`|The location in the byte sequence to write the output|

Returns:

|type|description|
|----|-----------|
|`Bytes`|A copy of the input bytes with the encoded string replaced at the given position|

### String.**encodeAtWithBom**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
encodeAtWithBom : (String, Encoding, Bytes, Number) -> Bytes
```

Encodes the given string into a byte sequence at the supplied position, including any byte-order marker, using the encoding scheme provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|
|`dest`|`Bytes`|The byte sequence that will be copied|
|`destPos`|`Number`|The location in the byte sequence to write the output|

Returns:

|type|description|
|----|-----------|
|`Bytes`|A copy of the input bytes with the encoded string replaced at the given position|

### String.**encode**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
encode : (String, Encoding) -> Bytes
```

Encodes the given string using the given encoding scheme, excluding any byte-order marker.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The byte representation of the string in the given encoding|

### String.**encodeWithBom**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
encodeWithBom : (String, Encoding) -> Bytes
```

Encodes the given string using the given encoding scheme, including any byte-order marker.

Parameters:

|param|type|description|
|-----|----|-----------|
|`string`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The byte representation of the string in the given encoding|

### String.**decodeRange**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
decodeRange : (Bytes, Encoding, Number, Number) -> String
```

Decodes the given byte sequence of the specified range into a string, excluding any byte-order marker, using encoding scheme provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The input bytes|
|`encoding`|`Encoding`|The encoding to use|
|`start`|`Number`|The byte offset to begin decoding from|
|`size`|`Number`|The maximum number of bytes to decode|

Returns:

|type|description|
|----|-----------|
|`String`|The decoded string|

### String.**decodeRangeKeepBom**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
decodeRangeKeepBom : (Bytes, Encoding, Number, Number) -> String
```

Decodes the given byte sequence of the specified range into a string, including any byte-order marker, using encoding scheme provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The input bytes|
|`encoding`|`Encoding`|The encoding to use|
|`start`|`Number`|The byte offset to begin decoding from|
|`size`|`Number`|The maximum number of bytes to decode|

Returns:

|type|description|
|----|-----------|
|`String`|The decoded string|

### String.**decode**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
decode : (Bytes, Encoding) -> String
```

Decodes the given byte sequence into a string using the given encoding scheme, excluding any byte-order marker.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The input bytes|
|`encoding`|`Encoding`|The encoding to use|

Returns:

|type|description|
|----|-----------|
|`String`|The decoded string|

### String.**decodeKeepBom**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
decodeKeepBom : (Bytes, Encoding) -> String
```

Decodes the given byte sequence into a string using the given encoding scheme, including any byte-order marker.

Parameters:

|param|type|description|
|-----|----|-----------|
|`bytes`|`Bytes`|The input bytes|
|`encoding`|`Encoding`|The encoding to use|

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
forEachCodePoint : ((Number -> Void), String) -> Void
```

Iterates over Unicode code points in a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`Number -> Void`|The iterator function|
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
forEachCodePointi : (((Number, Number) -> Void), String) -> Void
```

Iterates over Unicode code points in a string. This is the same as
`forEachCodePoint`, but provides the code point's index in the string
as the second argument to the iterator function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(Number, Number) -> Void`|The iterator function|
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
trimStart : String -> String
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
trimEnd : String -> String
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
trim : String -> String
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

