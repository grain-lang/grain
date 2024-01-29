---
title: Json
---

JSON (JavaScript Object Notation) parsing, printing, and access utilities.

```grain
include "json"
```

```grain
Json.parse("{\"currency\":\"€\",\"price\":99.99}") == JsonObject([
  ("currency", JsonString("€")),
  ("price", JsonNumber(99.99)),
])
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

Data structure representing the result of parsing and the input of printing JSON.

This data structure is semantically equivalent to the JSON format allowing
mostly lossless round trips of printing and parsing. Exceptions to this are
whitespace, multiple ways JSON allows escaping characters in strings, and
some edge cases related to Grain's `Number` type.

Examples:

```grain
Json.parse("{\"currency\":\"€\",\"price\":99.99}") == JsonObject([
  ("currency", JsonString("€")),
  ("price", JsonNumber(99.99)),
])
```

```grain
Json.parse("{\n\"currency\":\"€\",\n\"price\":99.99\n}") == JsonObject([
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

Represents errors for cases where a `JSON` object cannot be represented in the
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
characters for illustrative purposes.

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
  SpacedArrayEntries,
  OneArrayEntryPerLine,
}
```

Controls how arrays are printed in custom formatting.

Following examples have whitespaces and line breaks replaced with visible
characters for illustrative purposes.

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

`SpacedArrayEntries`
```
[ ]
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
  SpacedObjectEntries,
  OneObjectEntryPerLine,
}
```

Controls how objects are printed in custom formatting.

Following examples have whitespaces and line breaks replaced with visible
characters for illustrative purposes.

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

`SpacedObjectEntries`
```
{ }
```

```
{"a": 1}
```

```
{"a": 1, "b": 2, "c": 3}
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

Allows control of formatting in JSON printing.

### Json.**JsonParseError**

```grain
enum JsonParseError {
  UnexpectedEndOfInput(String),
  UnexpectedToken(String),
  InvalidUTF16SurrogatePair(String),
}
```

Represents errors for JSON parsing along with a human readable text message.

## Values

Functions and constants included in the Json module.

### Json.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toString :
  (?format: FormattingChoices, json: Json) =>
   Result<String, JsonToStringError>
```

Prints the JSON object into a string with specific formatting settings.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?format`|`FormattingChoices`|Formatting options|
|`json`|`Json`|The JSON object to print|

Returns:

|type|description|
|----|-----------|
|`Result<String, JsonToStringError>`|`Ok(str)` containing the printed JSON or `Err(err)` if the inputted object cannot be represented in the JSON format.,|

Examples:

```grain
print(
  toString(
    JsonObject([("currency", JsonString("€")), ("price", JsonNumber(99.9))]
  )
)
// Output: Ok("{\"currency\":\"€\",\"price\":99.9}")
```

```grain
print(
  toString(
    format=Compact
    JsonObject([("currency", JsonString("€")), ("price", JsonNumber(99.9))])
  )
)
// Output: Ok("{\"currency\":\"€\",\"price\":99.9}")
```

```grain
print(
  toString(
    format=Pretty,
    JsonObject([("currency", JsonString("€")), ("price", JsonNumber(99.9))])
  )
)
// Output: Ok("{
// \"currency\": \"€\",
// \"price\": 99.9
//}")
```

```grain
print(
  toString(
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
  )
)
// Output: Ok("{\"currency\":\"\\u20ac\",\"price\":99.9}")
```

### Json.**parse**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
parse : (str: String) => Result<Json, JsonParseError>
```

Parses JSON string into a `Json` data structure.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str`|`String`|The JSON text string|

Returns:

|type|description|
|----|-----------|
|`Result<Json, JsonParseError>`|`Ok(json)` containing the parsed data structure on a successful parse or `Err(err)` containing a parse error otherwise.|

Examples:

```grain
assert parse("{\"currency\":\"$\",\"price\":119}") == Ok(
 JsonObject([
   ("currency", JsonString("$")),
   ("price", JsonNumber(119))
 ])
)
```

