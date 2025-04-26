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
toString:
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
parse: (str: String) => Result<Json, JsonParseError>
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

## Json.Lenses

Utilities for accessing and updating JSON data.

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
let obj = JsonObject([("x", JsonNumber(123))])
assert get(property("x") ||> number, obj) == Some(123)
```

```grain
let obj = JsonObject([("x", JsonNumber(123))])
assert set(property("x") ||> number, 321, obj) ==
  Some(JsonObject([("x", JsonNumber(321))]))
```

### Types

Type declarations included in the Json.Lenses module.

#### Json.Lenses.**Lens**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
record Lens<a, b> {
  get: (subject: a) => Option<b>,
  set: (newValue: b, subject: a) => Option<a>,
}
```

A structure which provides functionality for accessing and setting JSON
data.

Fields:

|name|type|description|
|----|----|-----------|
|`get`|`(subject: a0) => Option<b0>`|A function which reads a value from the subject.|
|`set`|`(newValue: b0, subject: a0) => Option<a0>`|A function which immutably updates a value in the subject.|

### Values

Functions and constants included in the Json.Lenses module.

#### Json.Lenses.**get**

```grain
get: (lens: Lens<a, b>, subject: a) => Option<b>
```

Reads the value focused on by the given lens from the input data.

Parameters:

|param|type|description|
|-----|----|-----------|
|`lens`|`Lens<a, b>`|The lens to apply to the subject data|
|`subject`|`a`|The data which will have the lens applied to it|

Returns:

|type|description|
|----|-----------|
|`Option<b>`|`Some(data)` containing the data read by the lens if the lens matches the given data, or `None` if the data cannot be matched to the lens|

Examples:

```grain
assert get(number, JsonNumber(123)) == Some(123)
```

```grain
assert get(string, JsonString("abc")) == Some("abc")
```

```grain
assert get(number, JsonString("abc")) == None
```

#### Json.Lenses.**set**

```grain
set: (lens: Lens<a, b>, newValue: b, subject: a) => Option<a>
```

Sets the value focused on by the given lens from the input data to the
desired new value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`lens`|`Lens<a, b>`|The lens to apply to the subject data|
|`newValue`|`b`|The new value to set at the focus of the lens|
|`subject`|`a`|The data which will have the lens applied to it|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(data)` containing the new data after the lens substitution if the lens matches the given data, or `None` if the data cannot be matched to the lens|

Examples:

```grain
assert set(number, 123, JsonBoolean(true)) == Some(JsonNumber(123))
```

```grain
assert set(property("a"), JsonNumber(123), JsonObject([("a", JsonNull)])) == Some(JsonObject([("a", JsonNumber(123))]))
```

```grain
assert set(property("a"), JsonNumber(123), JsonBoolean(true)) == None
```

#### Json.Lenses.**map**

```grain
map: (lens: Lens<a, b>, fn: (b => b), subject: a) => Option<a>
```

Updates the value focused on by the given lens from the input data by
applying a function to it and setting the focus to the result of the function

Parameters:

|param|type|description|
|-----|----|-----------|
|`lens`|`Lens<a, b>`|The lens to apply to the subject data|
|`fn`|`b => b`|The function to apply to the matched data at the lens if matched|
|`subject`|`a`|The data which will have the lens applied to it|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(data)` containing the new data after the lens mapping has been applied if the lens matches the given data, or `None` if the data cannot be matched to the lens|

Examples:

```grain
assert map(number, x => x * 2, JsonNumber(5)) == Some(JsonNumber(10))
```

```grain
assert map(property("x"), x => JsonArray([x, x]), JsonObject([("x", JsonNumber(1))])) ==
  Some(JsonObject([("x", JsonArray([JsonNumber(1), JsonNumber(1)]))]))
```

```grain
assert map(number, x => x * 2, JsonString("abc")) == None
```

#### Json.Lenses.**json**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
json: Lens<Json, Json>
```

A lens whose focus is a JSON value.

Examples:

```grain
assert get(json, JsonString("abc")) == Some(JsonString("abc"))
```

#### Json.Lenses.**boolean**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
boolean: Lens<Json, Bool>
```

A lens whose focus is a JSON boolean value.

Examples:

```grain
assert get(boolean, JsonBoolean(true)) == Some(true)
```

#### Json.Lenses.**string**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
string: Lens<Json, String>
```

A lens whose focus is a JSON string value.

Examples:

```grain
assert get(string, JsonString("abc")) == Some("abc")
```

#### Json.Lenses.**number**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
number: Lens<Json, Number>
```

A lens whose focus is a JSON number value.

Examples:

```grain
assert get(number, JsonNumber(123)) == Some(123)
```

#### Json.Lenses.**array**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
array: Lens<Json, List<Json>>
```

A lens whose focus is a JSON array.

Examples:

```grain
assert get(array, JsonArray([JsonNumber(123)])) == Some([JsonNumber(123)])
```

#### Json.Lenses.**objectProperties**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
objectProperties: Lens<Json, List<(String, Json)>>
```

A lens whose focus is the property pair list of a JSON object.

Examples:

```grain
assert get(objectProperties, JsonObject([("a", JsonNumber(123))])) == Some([("a", JsonNumber(123))])
```

#### Json.Lenses.**property**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
property: (propertyName: String) => Lens<Json, Json>
```

Creates a lens whose focus is a given property of a JSON object.

Parameters:

|param|type|description|
|-----|----|-----------|
|`propertyName`|`String`|The property name of the JSON object to focus on|

Returns:

|type|description|
|----|-----------|
|`Lens<Json, Json>`|A lens whose focus is the given property of a JSON object|

Examples:

```grain
assert get(property("x"), JsonObject([("x", JsonNumber(123))])) == Some(JsonNumber(123))
```

```grain
assert set(property("x"), JsonString("new"), JsonObject([("x", JsonNumber(123))])) ==
  Some(JsonObject([("x", JsonString("new"))]))
```

#### Json.Lenses.**nullable**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
nullable: (lens: Lens<Json, a>) => Lens<Json, Option<a>>
```

Wraps a lens to permit nullable values in addition to the original value
type of the given lens. During a `get` operation if the lens matches then
the result will be enclosed in `Some`; if the lens does not match but the
value focused is null, then the lens will still successfully match and
`None` will be returned.

Examples:

```grain
assert get(nullable(number), JsonNumber(123)) == Some(Some(123))
```

```grain
assert get(nullable(number), JsonNull) == Some(None)
```

```grain
assert get(nullable(number), JsonString("abc")) == None
```

```grain
assert set(nullable(number), Some(123), JsonString("abc")) == Some(JsonNumber(123))
```

#### Json.Lenses.**propertyPath**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
propertyPath: (propertyNames: List<String>) => Lens<Json, Json>
```

Creates a lens whose focus is a given property path within a JSON object tree.

Parameters:

|param|type|description|
|-----|----|-----------|
|`propertyNames`|`List<String>`|The property path of the JSON object to create a focus on|

Returns:

|type|description|
|----|-----------|
|`Lens<Json, Json>`|A lens whose focus is the given property path of a JSON object|

Examples:

```grain
let nestedObj = JsonObject([("a", JsonObject([("b", JsonNumber(123))]))])
assert get(propertyPath(["a", "b"]), nestedObj) == Some(JsonNumber(123))
```

#### Json.Lenses.**(||>)**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
(||>): (lens1: Lens<a, b>, lens2: Lens<b, c>) => Lens<a, c>
```

Reverse lens composition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`lens1`|`Lens<a, b>`|The lens which will be applied first|
|`lens2`|`Lens<b, c>`|The lens which will be applied second|

Returns:

|type|description|
|----|-----------|
|`Lens<a, c>`|A lens which combines the two given lenses, passing through the first and then the second|

Examples:

```grain
assert get(property("x") ||> number, JsonObject([("x", JsonNumber(123))])) == Some(123)
```

```grain
assert set(property("x") ||> string, "new", JsonObject([("x", JsonNumber(123))])) ==
  Some(JsonObject([("x", JsonString("new"))]))
```

