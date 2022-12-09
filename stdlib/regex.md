---
title: Regex
---

Regular Expressions.

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
import Regex from "regex"
```

## Values

Functions for working with regular expressions.

### Regex.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
make : String -> Result<RegularExpression, String>
```

Compiles the given pattern string into a regular expression object.

For a general overview of regular expressions, refer to
["Mastering Regular Expressions"](http://regex.info/book.html) by Friedl, or other online resources.

Regular expressions are a combination of normal and special characters. A normal
character in a pattern will match a one-character string containing that character.
Moreover, if there are two regular expressions `A` and `B`, they can be concatenated
into a regular expression `AB`. If a string `p` matches `A` and `q` matches `B`,
then `pq` will match `AB`.

The special character sequences are as follows:

- `.` - Matches any character, except for a newline in multi-line mode
- `^` - Matches the beginning of the input, or after a newline (`\n`) in multi-line mode
- `$` - Matches the end of the input, or right before a newline (`\n`) in multi-line mode
- `«re»*` - Matches `«re»` zero or more times
- `«re»+` - Matches `«re»` one or more times
- `«re»?` - Matches `«re»` zero or one times
- `«re»{«n»}` - Matches `«re»` exactly `«n»` times
- `«re»{«n»,}` - Matches `«re»` `«n»` or more times
- `«re»{,«m»}` - Matches `«re»` zero to `«m»` times
- `«re»{«n»,«m»}` - Matches `«re»` between `«n»` and `«m»` times
- `«re»{}` - Matches `«re»` zero or more times
- `[«rng»]` - Matches any character in `«rng»` (see below)
- `[^«rng»]` - Matches any character not in `«rng»` (see below)
- `\«n»` - Matches the latest match for group `«n»` (one-indexed)
- `\b` - Matches the boundary of `\w*` (`\w` defined below, under "basic classes")
- `\B` - Matches where `\b` does not
- `\p{«property»}` - Matches any character with Unicode property `«property»` (see below)
- `\P{«property»}` - Matches any character without Unicode property `«property»` (see below)
- `(«re»)` - Matches `«re»`, storing the result in a group
- `(?:«re»)` - Matches `«re»` without storing the result in a group
- `(?«mode»:«re») - Matches `«re»` with the mode settings specified by `«mode»` using the following syntax:
  - `«mode»i` - The same as `«mode»`, but with case-insensitivity enabled (temporarily not supported until grain-lang/grain#661 is resolved)
  - `«mode»-i` - The same as `«mode»`, but with case-insensitivity disabled (the default)
  - `«mode»m` / `«mode»-s` - The same as `«mode»`, but with multi-line mode enabled
  - `«mode»-m` / `«mode»s` - The same as `«mode»`, but with multi-line mode disabled
  - An empty string, which will not change any mode settings
- `(?«tst»«re1»|«re2»)` - Will match `«re1»` if `«tst»`, otherwise will match `«re2»`. The following options are available for `«tst»`
  - `(«n»)` - Will be true if group `«n»` has a match
  - `(?=«re»)` - Will be true if `«re»` matches the next sequence
  - `(?!«re»)` - Will be true if `«re»` does not match the next sequence
  - `(?<=«re»)` - Will be true if `«re»` matches the preceding sequence
  - `(?<!«re»)` - Will be true if `«re»` does not match the preceding sequence
- `(?«tst»«re»)` - Equivalent to `(?«tst»«re»|)`
- Finally, basic classes (defined below) can also appear outside of character ranges.

Character ranges (referred to as `«rng»` above) have the following syntax:
- `«c»` - Matches the character `«c»` exactly
- `«c1»-«c2»` - Matches any character with a character code between the character code for `«c1»` and the code for `«c2»`

These forms can be repeated any number of times, which will construct a range of their union. That is, `[ba-c]` and `[a-c]` are equivalent ranges.
Additionally, there are the following special cases:
- A `]` as the first character of the range will match a `]`
- A `-` as the first or last character of the range will match a `-`
- A `^` in any position other than the first position will match a `^`
- `\«c»`, where `«c»` is a non-alphabetic character, will match `«c»`

Furthermore, ranges can include character classes, which are predefined commonly-used
sets of characters. There are two "flavors" of these: *basic* classes and *POSIX* classes.
Both are provided for ease of use and to maximize compatibility with other regular
expression engines, so feel free to use whichever is most convenient.

The *basic* classes are as follows:
- `\d` - Matches `0-9`
- `\D` - Matches characters not in `\d`
- `\w` - Matches `a-z`, `A-Z`, `0-9`, and `_`
- `\W` - Matches characters not in `\w`
- `\s` - Matches space, tab, formfeed, and return
- `\S` - Matches characters not in `\s`
The *POSIX* classes are as follows:
- `[:alpha:]` - Matches `a-z` and `A-Z`
- `[:upper:]` - Matches `A-Z`
- `[:lower:]` - Matches `a-z`
- `[:digit:]` - Matches `0-9`
- `[:xdigit:]` - Matches `0-9`, `a-f`, and `A-F`
- `[:alnum:]` - Matches `a-z`, `A-Z`, and `0-9`
- `[:word:]` - Matches `a-z`, `A-Z`, `0-9`, and `_`
- `[:blank:]` - Matches space and tab
- `[:space:]` - Matches space, tab, newline, formfeed, and return
- `[:cntrl:]` - Contains all characters with code points < 32
- `[:ascii:]` - Contains all ASCII characters

Parameters:

|param|type|description|
|-----|----|-----------|
|`regexString`|`String`|The regular expression to compile|

Returns:

|type|description|
|----|-----------|
|`Result<RegularExpression, String>`|The compiled regular expression|

Examples:

```grain
Regex.make("(foo|bar)[0-9]+")
```

### Regex.**MatchResult**

```grain
record MatchResult {
  group: Number -> Option<String>,
  groupPosition: Number -> Option<(Number, Number)>,
  numGroups: Number,
  allGroups: () -> Array<Option<String>>,
  allGroupPositions: () -> Array<Option<(Number, Number)>>,
}
```

This object contains the results
of a regular expression match. The results can be obtained using
the following accessors:

```grain
group : Number -> Option<String>
```

Returns the contents of the given group. Note that group 0 contains
the entire matched substring, and group 1 contains the first parenthesized group.

```grain
groupPosition : Number -> Option<(Number, Number)>
```

Returns the position of the given group.

```grain
numGroups : Number
```

The number of defined groups in this match object (including group 0).

```grain
allGroups : () -> Array<Option<String>>
```

Returns the contents of all groups matched in this match object.

```grain
allGroupPositions : () -> Array<Option<(Number, Number)>>
```

Returns the positions of all groups matched in this match object.

### Regex.**isMatch**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
isMatch : (RegularExpression, String) -> Bool
```

Determines if the given regular expression has a match in the given string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to search for|
|`string`|`String`|The string to search within|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the RegExp matches the string or `false` otherwise|

Examples:

```grain
assert Regex.isMatch(Result.unwrap(Regex.make("ca+[at]")), "caaat") == true
```

### Regex.**isMatchRange**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
isMatchRange : (RegularExpression, String, Number, Number) -> Bool
```

Determines if the given regular expression has a match in the given string between the given start/end offsets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to search for|
|`string`|`String`|The string to search|
|`start`|`Number`|The start offset to search between|
|`end`|`Number`|The end offset to search between|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the RegExp matches the string in the given range, otherwise `false`|

Examples:

```grain
assert Regex.isMatchRange(Result.unwrap(Regex.make("ca+[at]")), "caaat", 0, 5) == true
```

```grain
assert Regex.isMatchRange(Result.unwrap(Regex.make("ca+[at]")), "caaat", 1, 5) == false
```

### Regex.**find**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
find : (RegularExpression, String) -> Option<MatchResult>
```

Returns the first match for the given regular expression contained within the given string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to search for|
|`string`|`String`|The string to search|

Returns:

|type|description|
|----|-----------|
|`Option<MatchResult>`|The match result, if any|

Examples:

```grain
Regex.find(Result.unwrap(Regex.make("ca+[at]")), "caaat")
```

### Regex.**findRange**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
findRange :
  (RegularExpression, String, Number, Number) -> Option<MatchResult>
```

Returns the first match for the given regular expression contained within the given string
between the given start/end range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to search for|
|`string`|`String`|The string to search|
|`start`|`Number`|The start offset to search between|
|`end`|`Number`|The end offset to search between|

Returns:

|type|description|
|----|-----------|
|`Option<MatchResult>`|The match result, if any|

Examples:

```grain
Regex.findRange(Result.unwrap(Regex.make("ca+[at]")), "caaat", 0, 5)
```

### Regex.**findAll**

```grain
findAll : (RegularExpression, String) -> List<MatchResult>
```

Returns all matches for the given regular expression contained within the given string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to search for|
|`string`|`String`|The string to search|

Returns:

|type|description|
|----|-----------|
|`List<MatchResult>`|The list of matches|

### Regex.**findAllRange**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
findAllRange :
  (RegularExpression, String, Number, Number) -> List<MatchResult>
```

Returns all matches for the given regular expression contained within the given string
between the given start/end range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to search for|
|`string`|`String`|The string to search|
|`start`|`Number`|The start offset to search between|
|`end`|`Number`|The end offset to search between|

Returns:

|type|description|
|----|-----------|
|`List<MatchResult>`|The list of matches|

Examples:

```grain
Regex.findAllRange(Result.unwrap(Regex.make("ca+[at]")), "caaat", 0, 5)
```

### Regex.**replace**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
replace : (RegularExpression, String, String) -> String
```

Replaces the first match for the given regular expression contained within the given string with the specified replacement.
Replacement strings support the following syntax:
- `$&` - Replaced with the text of the matching portion of input (e.g. for `(foo)`, the search string `foo bar`, and the replacement `baz $&`, the result will be `baz foo bar`)
- `$n` / `$nn` (where `n` is a digit) - Replaced with the text of group `nn`
- `$$` - Replaced with a literal `$`
- `$.` - Does nothing (this exists to support replacement strings such as `$4$.0`, which will place the contents of group 4 prior to a zero)
- `$\`` - Replaced with the text preceding the matched substring
- `$'` - Replaced with the text following the matched substring
- Any other character will be placed as-is in the replaced output.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to search for|
|`toSearch`|`String`|The string to search|
|`replacement`|`String`|The string that replaces matches|

Returns:

|type|description|
|----|-----------|
|`String`|The given string with the appropriate replacements, if any|

Examples:

```grain
assert Regex.replace(Result.unwrap(Regex.make("o")), "foo", "a") == "fao"
```

### Regex.**replaceAll**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.3</code></summary>
No other changes yet.
</details>

```grain
replaceAll : (RegularExpression, String, String) -> String
```

Replaces all matches for the given regular expression contained within the given string with the specified replacement.
See `replace` for replacement string syntax.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to search for|
|`toSearch`|`String`|The string to search|
|`replacement`|`String`|The string that replaces matches|

Returns:

|type|description|
|----|-----------|
|`String`|The input string with the appropriate replacements, if any|

Examples:

```grain
assert Regex.replaceAll(Result.unwrap(Regex.make("o")), "skoot", "r") == "skrrt"
```

### Regex.**split**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
split : (RegularExpression, String) -> List<String>
```

Splits the given string at the first match for the given regular expression.

If the regex pattern contains capture groups, the content of the groups
will be included in the output list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to match|
|`str`|`String`|The string to split|

Returns:

|type|description|
|----|-----------|
|`List<String>`|A list of the split segments|

Examples:

```grain
assert Regex.split(Result.unwrap(Regex.make(",")), "a,b,c") == [ "a", "b,c" ]
```

### Regex.**splitAll**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
splitAll : (RegularExpression, String) -> List<String>
```

Splits the given string at every match for the given regular expression.

If the regex pattern contains capture groups, the content of the groups
will be included in the output list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`rx`|`RegularExpression`|The regular expression to match|
|`str`|`String`|The string to split|

Returns:

|type|description|
|----|-----------|
|`List<String>`|A list of the split segments|

Examples:

```grain
assert Regex.splitAll(Result.unwrap(Regex.make(",")), "a,b,c") == [ "a", "b", "c" ]
```

