---
title: Json
---

JSON (JavaScript Object Notation) parsing, printing, and access utilities.

```grain
from "json" include Json
```

```grain
Json.parse("{\"currency\":\"€\",\"price\":99.99}")
```

```grain
print(
  toString(
    format=Pretty,
    JsonObject([("currency", JsonString("€")), ("price", JsonNumber(99.9))])
  )
)
```

## Types

Type declarations included in the Json module.

### Json.**Json**

```grain
enum Json {
  JsonNull,
  JsonBoolean(Bool),
  JsonNumber(Number),
  JsonString(String),
  JsonArray(List<Json>),
  JsonObject(List<(String, Json)>),
}
```

Data structure representing JSON in Grain.

Variants:

```grain
JsonNull
```

Represents the JSON `null` value.

```grain
JsonBoolean(Bool)
```

Represents a JSON boolean value.

```grain
JsonNumber(Number)
```

Represents a JSON number value.

```grain
JsonString(String)
```

Represents a JSON string value.

```grain
JsonArray(List<Json>)
```

Represents a JSON array value.

```grain
JsonObject(List<(String, Json)>)
```

Represents a JSON object value, as a list of (key, value).

Examples:

```grain
assert Json.parse("{\"currency\":\"€\",\"price\":99.99}") == JsonObject([
  ("currency", JsonString("€")),
  ("price", JsonNumber(99.99)),
])
```

```grain
assert Json.parse("{\n\"currency\":\"€\",\n\"price\":99.99\n}") == JsonObject([
  ("currency", JsonString("€")),
  ("price", JsonNumber(99.99)),
])
```

### Json.**JsonToStringError**

```grain
enum JsonToStringError {
  InvalidNumber(String),
}
```

Represents errors for cases where a `Json` data structure cannot be represented as a
JSON string.

Variants:

```grain
InvalidNumber(String)
```

The `Json` data structure contains a number value of `NaN`, `Infinity`, or `-Infinity`.

### Json.**IndentationFormat**

```grain
enum IndentationFormat {
  NoIndentation,
  IndentWithTab,
  IndentWithSpaces(Number),
}
```

Controls how indentation is output in custom formatting.

Variants:

```grain
NoIndentation
```

No indentation is emitted.

```json
{
"currency": "€",
"price": 99.9
}
```

```grain
IndentWithTab
```

Tabs are emitted.

```json
{
	"currency": "€",
	"price": 99.9
}
```

```grain
IndentWithSpaces(Number)
```

The desired number of spaces are emitted.

`IndentWithSpaces(2)`
```json
{
  "currency": "€",
  "price": 99.9
}
```

`IndentWithSpaces(4)`
```json
{
    "currency": "€",
    "price": 99.9
}
```

### Json.**ArrayFormat**

```grain
enum ArrayFormat {
  CompactArrayEntries,
  SpacedArrayEntries,
  OneArrayEntryPerLine,
}
```

Controls how arrays are output in custom formatting.

Variants:

```grain
CompactArrayEntries
```

Arrays are emitted in a compact manner.

```json
[]
```

```json
[1]
```

```json
[1,2,3]
```

```grain
SpacedArrayEntries
```

Arrays are emitted with spaces between elements.

```json
[ ]
```

```json
[1]
```

```json
[1, 2, 3]
```

```grain
OneArrayEntryPerLine
```

Arrays are emitted with newlines and indentation between each element.

```json
[]
```

```json
[
  1
]
```

```json
[
  1,
  2,
  3
]
```

### Json.**ObjectFormat**

```grain
enum ObjectFormat {
  CompactObjectEntries,
  SpacedObjectEntries,
  OneObjectEntryPerLine,
}
```

Controls how objects are output in custom formatting.

Variants:

```grain
CompactObjectEntries
```

Objects are emitted in a compact manner.

```json
{}
```

```json
{"a":1}
```

```json
{"a":1,"b":2,"c":3}
```

```grain
SpacedObjectEntries
```

Objects are emitted with spaces between entries.

```json
{ }
```

```json
{"a": 1}
```

```json
{"a": 1, "b": 2, "c": 3}
```

```grain
OneObjectEntryPerLine
```

Objects are emitted with each entry on a new line.

```
{}
```

```
{
  "a": 1
}
```

```
{
  "a": 1,
  "b": 2,
  "c": 3
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

Controls how line endings are output in custom formatting.

Variants:

```grain
NoLineEnding
```

No line endings will be emitted.

```grain
LineFeed
```

A `\n` will be emitted at the end of each line.

```grain
CarriageReturnLineFeed
```

A `\r\n` will be emitted at the end of each line.

```grain
CarriageReturn
```

A `\r` will be emitted at the end of each line.

### Json.**FormattingChoices**

```grain
enum FormattingChoices {
  Pretty,
  Compact,
  PrettyAndSafe,
  CompactAndSafe,
  Custom{
    indentation: IndentationFormat,
    arrayFormat: ArrayFormat,
    objectFormat: ObjectFormat,
    lineEnding: LineEnding,
    finishWithNewLine: Bool,
    escapeAllControlPoints: Bool,
    escapeHTMLUnsafeSequences: Bool,
    escapeNonASCII: Bool,
  },
}
```

Allows control of formatting in JSON output.

Variants:

```grain
Pretty
```

Recommended human readable formatting.

Escapes all control points for the sake of clarity, but outputs unicode
codepoints directly so the result needs to be treated as proper unicode and
is not safe to be transported in ASCII encoding.

Roughly Equivalent to:
```grain
Custom{
 indentation: IndentWithSpaces(2),
 arrayFormat: OneArrayEntryPerLine,
 objectFormat: OneObjectEntryPerLine,
 lineEnding: LineFeed,
 finishWithNewLine: true,
 escapeAllControlPoints: true,
 escapeHTMLUnsafeSequences: false,
 escapeNonASCII: false,
}
```

```json
{
  "currency": "€",
  "price": 99.9,
  "currencyDescription": "EURO\u007f",
}
```

```grain
Compact
```

Compact formatting that minimizes the size of resulting JSON at cost of not
being easily human readable.

Only performs minimal string escaping as required by the ECMA-404 standard,
so the result needs to be treated as proper unicode and is not safe to be
transported in ASCII encoding.

Roughly Equivalent to:
```grain
Custom{
 indentation: NoIndentation,
 arrayFormat: CompactArrayEntries,
 objectFormat: CompactObjectEntries,
 lineEnding: NoLineEnding,
 finishWithNewLine: false,
 escapeAllControlPoints: false,
 escapeHTMLUnsafeSequences: false,
 escapeNonASCII: false,
}
```

```json
{"currency":"€","price":99.9,"currencyDescription":"EURO␡"}
```

```grain
PrettyAndSafe
```

Pretty and conservative formatting to maximize compatibility and
embeddability of the resulting JSON.

Should be safe to copy and paste directly into HTML and to be transported in
plain ASCII.

Roughly Equivalent to:
```grain
Custom{
 indentation: IndentWithSpaces(2),
 arrayFormat: OneArrayEntryPerLine,
 objectFormat: OneObjectEntryPerLine,
 lineEnding: LineFeed,
 finishWithNewLine: true,
 escapeAllControlPoints: true,
 escapeHTMLUnsafeSequences: true,
 escapeNonASCII: true,
}
```

```json
{
  "currency": "\u20ac",
  "price": 99.9,
  "currencyDescription": "EURO\u007f",
}
```

```grain
CompactAndSafe
```

Compact and conservative formatting to maximize compatibility and
embeddability of the resulting JSON.

Should be safe to copy and paste directly into HTML and to transported in
plain ASCII.

Roughly Equivalent to:
```grain
Custom{
 indentation: NoIndentation,
 arrayFormat: CompactArrayEntries,
 objectFormat: CompactObjectEntries,
 lineEnding: NoLineEnding,
 finishWithNewLine: false,
 escapeAllControlPoints: true,
 escapeHTMLUnsafeSequences: true,
 escapeNonASCII: true,
}
```

```json
{"currency":"\u20ac","price":99.9,"currencyDescription":"EURO\u007f"}
```

```grain
Custom{
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

Allows for fined grained control of the formatting output.

### Json.**JsonParseError**

```grain
enum JsonParseError {
  UnexpectedEndOfInput(String),
  UnexpectedToken(String),
  InvalidUTF16SurrogatePair(String),
}
```

Represents errors for JSON parsing along with a human readable message.

## Values

Functions and constants included in the Json module.

### Json.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toString :
  (?format: FormattingChoices, json: Json) =>
   Result<String, JsonToStringError>
```

Converts the `Json` data structure into a JSON string with specific formatting settings.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?format`|`FormattingChoices`|Formatting options|
|`json`|`Json`|The `Json` data structure to convert|

Returns:

|type|description|
|----|-----------|
|`Result<String, JsonToStringError>`|`Ok(str)` containing the JSON string or `Err(err)` if the provided `Json` data structure cannot be converted to a string|

Examples:

```grain
assert toString(
  JsonObject([("currency", JsonString("€")), ("price", JsonNumber(99.9))]
) == Ok("{\"currency\":\"€\",\"price\":99.9}")
```

```grain
assert toString(
  format=Compact
  JsonObject([("currency", JsonString("€")), ("price", JsonNumber(99.9))])
) == Ok("{\"currency\":\"€\",\"price\":99.9}")
```

```grain
assert toString(
  format=Pretty,
  JsonObject([("currency", JsonString("€")), ("price", JsonNumber(99.9))])
) == Ok("{
  \"currency\": \"€\",
  \"price\": 99.9
}")
```

```grain
assert toString(
  format=Custom{
    indentation: NoIndentation,
    arrayFormat: CompactArrayEntries,
    objectFormat: CompactObjectEntries,
    lineEnding: NoLineEnding,
    finishWithNewLine: false,
    escapeAllControlPoints: true,
    escapeHTMLUnsafeSequences: true,
    escapeNonASCII: true,
  },
  JsonObject([("currency", JsonString("€")), ("price", JsonNumber(99.9))])
) == Ok("{\"currency\":\"\\u20ac\",\"price\":99.9}")
```

### Json.**parse**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
parse : (str: String) => Result<Json, JsonParseError>
```

Parses JSON string into a `Json` data structure.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str`|`String`|The JSON string to parse|

Returns:

|type|description|
|----|-----------|
|`Result<Json, JsonParseError>`|`Ok(json)` containing the parsed data structure on a successful parse or `Err(err)` containing a parse error otherwise|

Examples:

```grain
assert parse("{\"currency\":\"$\",\"price\":119}") == Ok(
 JsonObject([
   ("currency", JsonString("$")),
   ("price", JsonNumber(119))
 ])
)
```

