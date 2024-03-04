---
title: Uri
---

Utilities for working with URIs.

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
from "uri" include Uri
```

## Types

Type declarations included in the Uri module.

### Uri.**Uri**

```grain
record Uri {
  scheme: Option<String>,
  userinfo: Option<String>,
  host: Option<String>,
  port: Option<Number>,
  path: String,
  query: Option<String>,
  fragment: Option<String>,
}
```

Represents a parsed RFC 3986 URI.

### Uri.**ParseError**

```grain
enum ParseError {
  ParseError,
}
```

Represents an error encountered while parsing a URI.

### Uri.**ConstructUriError**

```grain
enum ConstructUriError {
  UserinfoWithNoHost,
  PortWithNoHost,
  InvalidSchemeError,
  InvalidUserinfoError,
  InvalidHostError,
  InvalidPortError,
  InvalidPathError,
  InvalidQueryError,
  InvalidFragmentError,
}
```

Represents an error encountered while constructing a URI with `make` or `update`.

### Uri.**ResolveReferenceError**

```grain
enum ResolveReferenceError {
  BaseNotAbsolute,
}
```

Represents an error encountered while attempting to resolve a URI reference to a target URI.

### Uri.**DecodingError**

```grain
enum DecodingError {
  InvalidEncoding,
}
```

Represents an error encountered while attempting to percent-decode a string.

### Uri.**EncodeSet**

```grain
enum EncodeSet {
  EncodeNonUnreserved,
  EncodeUserinfo,
  EncodeRegisteredHost,
  EncodePath,
  EncodePathSegment,
  EncodeQueryOrFragment,
  EncodeCustom((Char => Bool)),
}
```

Used to specify which characters to percent-encode from a string.

## Values

Functions and constants included in the Uri module.

### Uri.**encode**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
encode : (str: String, ?encodeSet: EncodeSet) => String
```

Percent-encodes characters in a string based on the specified `EncodeSet`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str`|`String`|The string to encode|
|`?encodeSet`|`EncodeSet`|An indication for which characters to percent-encode. `EncodeNonUnreserved` by default|

Returns:

|type|description|
|----|-----------|
|`String`|A percent-encoding of the given string|

Examples:

```grain
Uri.encode("h3ll0_.w ?o+rld", encodeSet=Uri.EncodeNonUnreserved) // "h3ll0_.w%20%3Fo%2Brld"
```

```grain
Uri.encode("d+om@i:n.com", encodeSet=Uri.EncodeRegisteredHost) // "d+om%40i%3An.com"
```

```grain
Uri.encode("word", encodeSet=Uri.EncodeCustom(c => c == 'o')) // "w%6Frd"
```

### Uri.**decode**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
decode : (str: String) => Result<String, DecodingError>
```

Decodes any percent-encoded characters in a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str`|`String`|The string to decode|

Returns:

|type|description|
|----|-----------|
|`Result<String, DecodingError>`|`Ok(decoded)` containing the decoded string or `Err(err)` if the decoding failed|

### Uri.**encodeQuery**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
encodeQuery :
  (urlVals: List<(String, String)>, ?encodeSet: EncodeSet) => String
```

Encodes a list of key-value pairs into an query string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`urlVals`|`List<(String, String)>`|A list of key-value pairs|

Returns:

|type|description|
|----|-----------|
|`String`|A query string|

### Uri.**decodeQuery**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
decodeQuery : (str: String) => Result<List<(String, String)>, DecodingError>
```

Decodes a query string into a list of pairs.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str`|`String`|A query string|

Returns:

|type|description|
|----|-----------|
|`Result<List<(String, String)>, DecodingError>`|`Ok(decoded)` containing a list of key-value pairs from the decoded string or `Err(err)` if the decoding failed|

### Uri.**parse**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
parse : (str: String) => Result<Uri, ParseError>
```

Parses a string into a `Uri` according to RFC 3986. If the URI string has a
path it will be automatically normalized, removing unnecessary `.` and `..`
segments.

Parameters:

|param|type|description|
|-----|----|-----------|
|`str`|`String`|The RFC 3986 URI string to parse|

Returns:

|type|description|
|----|-----------|
|`Result<Uri, ParseError>`|`Ok(uri)` containing a `Uri` if the given string is a valid URI or `Err(ParseError)` otherwise|

Examples:

```grain
Uri.parse("https://grain-lang.org") == Ok(...)
```

```grain
Uri.parse("http://@*^%") == Err(Uri.ParseError)
```

### Uri.**resolveReference**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
resolveReference :
  (base: Uri, ref: Uri) => Result<Uri, ResolveReferenceError>
```

Transforms a base URI and a URI reference into a target URI

Parameters:

|param|type|description|
|-----|----|-----------|
|`base`|`Uri`|The base URI to resolve a URI reference on|
|`ref`|`Uri`|The URI reference to apply onto the base|

Returns:

|type|description|
|----|-----------|
|`Result<Uri, ResolveReferenceError>`|`Ok(uri)` containing the target `Uri` or `Err(err)` if the input is malformed|

Examples:

```grain
resolveReference(unwrap(parse("https://grain-lang.org/docs/stdlib/uri")), unwrap(parse("../intro"))) // https://grain-lang.org/docs/intro
```

```grain
resolveReference(unwrap(parse("https://grain-lang.org/docs")), unwrap(parse("?key=val"))) // https://grain-lang.org/docs?key=val
```

```grain
resolveReference(unwrap(parse("https://grain-lang.org/docs")), unwrap(parse("google.com/search"))) // https://google.com/search
```

### Uri.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
make :
  (?scheme: Option<String>, ?userinfo: Option<String>, ?host: Option<String>,
   ?port: Option<Number>, ?path: String, ?query: Option<String>,
   ?fragment: Option<String>, ?encodeComponents: Bool) =>
   Result<Uri, ConstructUriError>
```

Constructs a new `Uri` from components.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?scheme`|`Option<String>`|`Some(scheme)` containing the desired scheme component or `None` for a scheme-less URI|
|`?userinfo`|`Option<String>`|`Some(userinfo)` containing the desired userinfo component or `None` for a userinfo-less URI|
|`?host`|`Option<String>`|`Some(host)` containing the desired host component or `None` for a host-less URI|
|`?port`|`Option<Number>`|`Some(port)` containing the desired port component or `None` for a port-less URI|
|`?path`|`String`|The desired path for the URI. `""` by default|
|`?query`|`Option<String>`|`Some(query)` containing the desired query string component or `None` for a query-less URI|
|`?fragment`|`Option<String>`|`Some(fragment)` containing the desired fragment component or `None` for a fragment-less URI|
|`?encodeComponents`|`Bool`|Whether or not to apply percent encoding for each component to remove unsafe characters for each component|

Examples:

```grain
Uri.make(scheme=Some("https"), host=Some("grain-lang.org")) // https://grain-lang.org
```

```grain
Uri.make(host=Some("g/r@in"), encodeComponents=false) // Err(Uri.InvalidHostError)
```

```grain
Uri.make(scheme=Some("abc"), host=Some("g/r@in"), query=Some("k/ey=v^@l"), encodeComponents=true) // abc://g%2Fr%40in?k/ey=v%5E@l
```

```grain
Uri.make(port=Some(80)) // Err(Uri.PortWithNoHost)
```

### Uri.**update**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
update :
  (uri: Uri, ?scheme: Option<Option<String>>,
   ?userinfo: Option<Option<String>>, ?host: Option<Option<String>>,
   ?port: Option<Option<Number>>, ?path: Option<String>,
   ?query: Option<Option<String>>, ?fragment: Option<Option<String>>,
   ?encodeComponents: Bool) => Result<Uri, ConstructUriError>
```

Constructs a new `Uri` from a base `Uri` and components to update. The
pattern used to update each component is that `None` means the base URI's
component should be used and `Some(val)` means that a new value should be
used for that component.

Parameters:

|param|type|description|
|-----|----|-----------|
|`uri`|`Uri`|The `Uri` to update|
|`?scheme`|`Option<Option<String>>`|`Some(scheme)` containing the desired updated scheme component or `None` to maintain the base URI's scheme|
|`?userinfo`|`Option<Option<String>>`|`Some(userinfo)` containing the desired updated userinfo component or `None` to maintain the base URI's userinfo|
|`?host`|`Option<Option<String>>`|`Some(host)` containing the desired updated host component or `None` to maintain the base URI's host|
|`?port`|`Option<Option<Number>>`|`Some(port)` containing the desired updated port component or `None` to maintain the base URI's port|
|`?path`|`Option<String>`|`Some(path)` containing the desired updated path component or `None` to maintain the base URI's path|
|`?query`|`Option<Option<String>>`|`Some(query)` containing the desired updated query string component or `None` to maintain the base URI's query|
|`?fragment`|`Option<Option<String>>`|`Some(fragment)` containing the desired updated fragment component or `None` to maintain the base URI's fragment|
|`?encodeComponents`|`Bool`|Whether or not to apply percent encoding for each updated component to remove unsafe characters|

Examples:

```grain
let uri = Result.unwrap(Uri.parse("https://grain-lang.org/docs?k=v")) // Base URI for following examples
```

```grain
Uri.update(uri, scheme=Some(Some("ftp"))) // ftp://grain-lang.org/docs?k=v
```

```grain
Uri.update(uri, query=Some(None)) // https://grain-lang.org/docs
```

```grain
Uri.update(uri, host=Some(Some("g/r@in")), encodeComponents=true) // https://g%2Fr%40in/docs?k=v
```

```grain
Uri.update(uri, host=Some(None), port=Some(Some(80))) // Err(Uri.PortWithNoHost)
```

### Uri.**hasAuthority**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
hasAuthority : (uri: Uri) => Bool
```

Determines whether a `Uri` has an authority (i.e. has a host component)

Parameters:

|param|type|description|
|-----|----|-----------|
|`uri`|`Uri`|The `Uri` to consider|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the `Uri` has an authority component or `false` otherwise|

### Uri.**isAbsolute**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
isAbsolute : (uri: Uri) => Bool
```

Determines whether a `Uri` is an absolute URI (has a scheme component)

Parameters:

|param|type|description|
|-----|----|-----------|
|`uri`|`Uri`|The `Uri` to consider|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the `Uri` is absolute (has a scheme component) or `false` otherwise|

### Uri.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toString : (uri: Uri) => String
```

Converts the given `Uri` into a string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`uri`|`Uri`|The `Uri` to convert|

Returns:

|type|description|
|----|-----------|
|`String`|A string representation of the `Uri`|

