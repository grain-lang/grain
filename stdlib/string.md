---
title: String
---

Utilities for working with strings.

```grain
import String from "string"
```

### String.**concat**

```grain
concat : (String, String) -> String
```

Concatenate two strings.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str1`|`String`|The first string|
|`str2`|`String`|The second string|

Returns:

|type|description|
|----|-----------|
|`String`|The result of the joined strings|

Examples:

```grain
String.concat("Hello", " world")
```

### String.**length**

```grain
length : String -> Number
```

Get the character length of a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`input`|`String`|The string to check|

Returns:

|type|description|
|----|-----------|
|`Number`|The length of the provided string|

Examples:

```grain
String.length("Hello world")
```

### String.**byteLength**

```grain
byteLength : String -> Number
```

Get the byte length of a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`input`|`String`|The string to check|

Returns:

|type|description|
|----|-----------|
|`Number`|The byte length of the string|

Examples:

```grain
String.byteLength("Hello world")
```

### String.**indexOf**

```grain
indexOf : (String, String) -> Option<Number>
```

Find the start index of a substring.

Parameters:

|param|type|description|
|-----|----|-----------|
|`sub`|`String`|The substring to find|
|`input`|`String`|The string to check|

Returns:

|type|description|
|----|-----------|
|`Option<Number>`|The starting index of he substring|

Examples:

```grain
String.indexOf("world", "Hello world")
```

### String.**charAt**

```grain
charAt : (Number, String) -> Char
```

Find the Char at a specific index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`idx`|`Number`|The index to check|
|`s`|`String`|The string to search|

Returns:

|type|description|
|----|-----------|
|`Char`|The char at the provided index|

Examples:

```grain
String.charAt(5, "Hello world")
```

### String.**explode**

```grain
explode : String -> Array<Char>
```

Split a string into its Unicode characters.

Parameters:

|param|type|description|
|-----|----|-----------|
|`input`|`String`|The string to split|

Returns:

|type|description|
|----|-----------|
|`Array<Char>`|The string split into an array of characters|

Examples:

```grain
String.explode("Hello world")
```

### String.**implode**

```grain
implode : Array<Char> -> String
```

Create a string from an array of characters.

Parameters:

|param|type|description|
|-----|----|-----------|
|`input`|`Array<Char>`|The array to implode|

Returns:

|type|description|
|----|-----------|
|`String`|A string representation of the array of characters|

Examples:

```grain
String.implode([> 'H', 'e', 'l', 'l', 'o' ])
```

### String.**split**

```grain
split : (String, String) -> Array<String>
```

Split a string by the given sequence.

Parameters:

|param|type|description|
|-----|----|-----------|
|`sequence`|`String`|The sequence to split on|
|`input`|`String`|The string to split|

Returns:

|type|description|
|----|-----------|
|`Array<String>`|An array of substrings from the initial string|

Examples:

```grain
String.split(" ", "Hello world")
```

### String.**slice**

```grain
slice : (Number, Number, String) -> String
```

Get a portion of a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`from`|`Number`|The start index of the substring|
|`to`|`Number`|The end index of the substring|
|`input`|`String`|The input string|

Returns:

|type|description|
|----|-----------|
|`String`|The substring from the initial string|

Examples:

```grain
String.slice(0, 5, "Hello world")
```

### String.**contains**

```grain
contains : (String, String) -> Bool
```

Check if a string contains a substring.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pattern`|`String`|The substring to check|
|`input`|`String`|The input string|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the string contains the substring|

Examples:

```grain
String.contains("world", "Hello world")
```

### String.**startsWith**

```grain
startsWith : (String, String) -> Bool
```

Check if a string begins with another string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pattern`|`String`|The substring to check|
|`input`|`String`|The input string|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the string starts with the substring|

Examples:

```grain
String.startsWith("world", "Hello world")
```

### String.**endsWith**

```grain
endsWith : (String, String) -> Bool
```

Check if a string ends with another string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pattern`|`String`|The substring to check|
|`input`|`String`|The input string|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the ends starts with the substring|

Examples:

```grain
String.endsWith("world", "Hello world")
```

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

Byte Encodings

### String.**encodeAt**

```grain
encodeAt : (String, Encoding, Bytes, Number) -> Bytes
```

Encodes the given string using the given encoding scheme at a supplied offset in a Bytes instance, including byte-order marker.

Parameters:

|param|type|description|
|-----|----|-----------|
|`s`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|
|`includeBom`|`Bytes`|Whether to include the byte-order marker in the encoded output|
|`dest`|`Number`|The bytes object to write the encoded output into|
|`destPos`||The location in the byte array to write the output|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The encoded string|

### String.**encodeAtWithBom**

```grain
encodeAtWithBom : (String, Encoding, Bytes, Number) -> Bytes
```

Encodes the given string using the given encoding scheme.

Parameters:

|param|type|description|
|-----|----|-----------|
|`s`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|
|`includeBom`|`Bytes`|Whether to include the byte-order marker in the encoded output|
|`dest`|`Number`|The bytes object to write the encoded output into|
|`destPos`||The location in the byte array to write the output|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The encoded string|

### String.**encode**

```grain
encode : (String, Encoding) -> Bytes
```

Encodes the given string using the given encoding scheme. including a byte-order marker.
will not be included in the output.

Parameters:

|param|type|description|
|-----|----|-----------|
|`s`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The string encoded into bytes|

### String.**encodeWithBom**

```grain
encodeWithBom : (String, Encoding) -> Bytes
```

Encodes the given string using the given encoding scheme. A byte-order marker.

Parameters:

|param|type|description|
|-----|----|-----------|
|`s`|`String`|The input string|
|`encoding`|`Encoding`|The encoding to use|

Returns:

|type|description|
|----|-----------|
|`Bytes`|The string encoded into bytes|

### String.**decodeRange**

```grain
decodeRange : (Bytes, Encoding, Number, Number) -> String
```

Decodes the given byte sequence into a string using the given encoding scheme, skipping
the byte-order marker, if it's present.

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

Decodes the given byte sequence into a string using the given encoding scheme, including
the byte-order marker, if it's present.

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

Decodes the given byte sequence into a string using the given encoding scheme,
skipping the byte-order marker, if it's present.

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

Decodes the given byte sequence into a string using the given encoding scheme,
including the byte-order marker, if it's present.

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
String.forEachCodePoint(i => print(i), "Hello world")
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

