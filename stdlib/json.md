---
title: Json
---

@module Json: JSON (JavaScript Object Notation) parsing and printing.

## Types

Type declarations included in the Json module.

### Json.**JSON**

```grain
enum JSON {
  JSONNull,
  JSONBoolean(Bool),
  JSONNumber(Number),
  JSONString(String),
  JSONArray(List<JSON>),
  JSONObject(List<(String, JSON)>),
}
```

Data structure representing the result of parsing and the input of printing
JSON.

For example this object
```grain
JSONObject([
  ("currency", JSONString("€")),
  ("price", JSONNumber(99.99)),
])
```
is equivalent to the following JSON:
```json
{"currency":"€","price":99.99}
```

This data structure is semantically equivalent to the JSON format allowing
mostly lossless round trips of printing and parsing. Exceptions to this are
white spaces, multiple ways JSON allows escaping characters in strings and
some edge-cases related to Grain's `Number` type.

For example parsing the following JSON text will also result in the same
`JSON` object as above.
```json
{
  "currency": "\u20ac",
  "price": 99.99
}
```

### Json.**JSONToStringError**

```grain
enum JSONToStringError {
  InvalidNumber(String),
}
```

Represents errors for cases when a `JSON` object cannot be represente in the
JSON format along with a human readable text message. This can happen when
it contains number values `NaN`, `Infinity` or `-Infinity`.

### Json.**IndentationFormat**

```grain
enum IndentationFormat {
  NoIndentation,
  IndentWithTab,
  IndentWithSpaces(Number),
}
```

Controls how indentation is performed in custom formatting.

Following examples have whitespaces and line breaks replaced with visible
charactes for illustrative purposes.

`NoIndentation`
```
{↵
"currency":·"€",↵
"price":·99.9↵
}
```

`IndentWithTab`
```
{↵
→"currency":·"€",↵
→"price":·99.9↵
}
```

`IndentWithSpaces(2)`
```
{↵
··"currency":·"€",↵
··"price":·99.9↵
}
```

`IndentWithSpaces(4)`
```
{↵
····"currency":·"€",↵
····"price":·99.9↵
}
```

### Json.**ArrayFormat**

```grain
enum ArrayFormat {
  CompactArrayEntries,
  OneArrayEntryPerLine,
}
```

Controls how arrays are printed in custom formatting.

Following examples have whitespaces and line breaks replaced with visible
charactes for illustrative purposes.

`CompactArrayEntries`
```
[]
```

```
[1]
```

```
[1,2,3]
```

`OneArrayEntryPerLine`
```
[]
```

```
[↵
··1↵
]
```

```
[↵
··1,↵
··2,↵
··3↵
]
```

### Json.**ObjectFormat**

```grain
enum ObjectFormat {
  CompactObjectEntries,
  OneObjectEntryPerLine,
}
```

Controls how objects are printed in custom formatting.

Following examples have whitespaces and line breaks replaced with visible
charactes for illustrative purposes.

`CompactObjectEntries`
```
{}
```

```
{"a":1}
```

```
{"a":1,"b":2,"c":3}
```

`OneObjectEntryPerLine`
```
{}
```

```
{↵
··"a":·1↵
}
```

```
{↵
··"a":·1,↵
··"b":·2,↵
··"c":·3↵
}
```

### Json.**LineEnding**

```grain
enum LineEnding {
  NoLineEnding,
  LineFeed,
  CarriageReturnLineFeed,
  CarriageReturn,
}
```

Controls line ending type in custom formatting.

### Json.**FormattingSettings**

```grain
record FormattingSettings {
  indentation: IndentationFormat,
  arrayFormat: ArrayFormat,
  objectFormat: ObjectFormat,
  lineEnding: LineEnding,
  finishWithNewLine: Bool,
  escapeAllControlPoints: Bool,
  escapeHTMLUnsafeSequences: Bool,
  escapeNonASCII: Bool,
}
```

Allows fine grained control of formatting in JSON printing.

### Json.**JSONParseError**

```grain
enum JSONParseError {
  UnexpectedEndOfInput(String),
  UnexpectedToken(String),
  InvalidUTF16SurrogatePair(String),
}
```

Represents errors for JSON parsing along with a human readable text message.

## Values

Functions and constants included in the Json module.

### Json.**defaultCompactFormat**

```grain
defaultCompactFormat : () -> FormattingSettings
```

Compact formatting that minimizes the size of resulting JSON at cost of not
being easily human readable.

Only performs minimal string escaping as required by the ECMA-404 standard,
so the result needs to be treated as proper unicode and is not safe to be
transported in ASCII encoding.

The following example have whitespaces, line breaks and control points
replaced with visible characters.
```
{"currency":"€","price":99.9,"currencyDescription":"EURO␡","script·unembeddable":"You·cannot·pase·a·JSON·object·directly·into·a·<script>·tag·in·HTML·if·it·contains·</·with·unescaped·forward·slash"}
```

### Json.**defaultPrettyFormat**

```grain
defaultPrettyFormat : () -> FormattingSettings
```

Recommended human readable formatting.

Escapes all control points for the sake of clarity, but prints unicode
codepoints directly so the result needs to be treated as proper unicode and
is not safe to be transported in ASCII encoding.

The following example have whitespaces, line breaks and control points
replaced with visible characters.
```
{↵
··"currency":·"€",↵
··"price":·99.9,↵
··"currencyDescription":·"EURO\u007f",↵
··"script·unembeddable":·"You·cannot·pase·a·JSON·object·directly·into·a·<script>·tag·in·HTML·if·it·contains·</·with·unescaped·forward·slash"↵
}
```

### Json.**defaultCompactAndSafeFormat**

```grain
defaultCompactAndSafeFormat : () -> FormattingSettings
```

Compact and conservative formatting to maximize compatibility and
embeddability of the resulting JSON.

Should be safe to copy and paste directly into HTML and to transported in
plain ASCII.

The following example have whitespaces, line breaks and control points
replaced with visible characters.
```
{"currency":"\u20ac","price":99.9,"currencyDescription":"EURO\u007f","script·unembeddable":"You·cannot·pase·a·JSON·object·directly·into·a·<script>·tag·in·HTML·if·it·contains·<\/·with·unescaped·forward·slash"}
```

### Json.**defaultPrettyAndSafeFormat**

```grain
defaultPrettyAndSafeFormat : () -> FormattingSettings
```

Pretty and conservative formatting to maximize compatibility and
embeddability of the resulting JSON.

Should be safe to copy and paste directly into HTML and to transported in
plain ASCII.

The following example have whitespaces, line breaks and control points
replaced with visible characters.
```
{↵
··"currency":·"\u20ac",↵
··"price":·99.9,↵
··"currencyDescription":·"EURO\u007f",↵
··"script·unembeddable":·"You·cannot·pase·a·JSON·object·directly·into·a·<script>·tag·in·HTML·if·it·contains·<\/·with·unescaped·forward·slash"↵
}
```

### Json.**toString**

```grain
toString :
  (json: JSON, format: FormattingSettings) ->
   Result<String, JSONToStringError>
```

Prints the JSON object into a string with specific formatting settings.

Parameters:

|param|type|description|
|-----|----|-----------|
|`json`|`JSON`|The JSON object to print|
|`format`|`FormattingSettings`|Custom formatting setttings|

Returns:

|type|description|
|----|-----------|
|`Result<String, JSONToStringError>`|A `Result` object with either the string containing the printed JSON or an error if the input object cannot be represented in the JSON format.|

Examples:

```grain
print(Result.unwrap(toString(JSONObject([("currency", JSONString("€")), ("price", JSONNumber(99.9))]), defaultCompactAndSafeFormat())))

Example output:
```json
{"currency":"\u20ac","price":99.9}
```
```

### Json.**toStringCompact**

```grain
toStringCompact : (json: JSON) -> Result<String, JSONToStringError>
```

Prints the JSON object into a string with compact formatting optimized for
small size. Recommended for all uses where the intended consumer is a
machine program. For example in REST APIs.

Using this function can be preferred over `toString` with compact formatting
settings since it can be slightly faster.

Parameters:

|param|type|description|
|-----|----|-----------|
|`json`|`JSON`|The JSON object to print|

Returns:

|type|description|
|----|-----------|
|`Result<String, JSONToStringError>`|A `Result` object with either the string containing the printed JSON or an error if the input object cannot be represented in the JSON format.|

Examples:

```grain
print(Result.unwrap(toStringCompact(JSONObject([("currency", JSONString("€")), ("price", JSONNumber(99.9))]))))

Example output:
```json
{"currency":"€","price":99.9}
```
```

### Json.**toStringPretty**

```grain
toStringPretty : (json: JSON) -> Result<String, JSONToStringError>
```

Prints the JSON object into a string with default pretty formatting
settings. Recommended for uses where the intended consumer is a person or
the text should be easily inspectable. For example configuration files or
document file formats.

Parameters:

|param|type|description|
|-----|----|-----------|
|`json`|`JSON`|The JSON object to print|

Returns:

|type|description|
|----|-----------|
|`Result<String, JSONToStringError>`|A `Result` object with either the string containing the printed JSON or an error if the input object cannot be represented in the JSON format.

Example output:
```json
{
  "currency": "€",
  "price": 99.9
}
```|

Examples:

```grain
print(Result.unwrap(toStringPretty(JSONObject([("currency", JSONString("€")), ("price", JSONNumber(99.9))]))))
```

### Json.**parse**

```grain
parse : String -> Result<JSON, JSONParseError>
```

Parses JSON input from a string into a `JSON` object.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str`|`String`|The JSON text string|

Returns:

|type|description|
|----|-----------|
|`Result<JSON, JSONParseError>`|A `Result` object with either the parsed `JSON` object or an error.

Example output:
```
Ok(JSONObject([("currency", JSONString("$")), ("price", JSONNumber(119))]))
```|

Examples:

```grain
print(parse("{\"currency\":\"$\",\"price\":119}"))
```

