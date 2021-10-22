---
title: String
---

Utilities for working with strings.

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
String.concat("Foo", " Bar") == "FooBar"
```

### String.**length**

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

```grain
indexOf : (String, String) -> Option<Number>
```

Finds the position of a substring in the input string.

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

### String.**charAt**

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
String.explode("Hello") == [> 'H', 'e', 'l', 'l', 'o' ]
```

### String.**implode**

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
String.implode([> 'H', 'e', 'l', 'l', 'o' ]) == "Hello"
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
String.split(" ", "Hello world") == [> "Hello", "world" ]
```

### String.**slice**

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

### String.**encodeAt**

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

