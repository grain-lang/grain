---
title: Option
---

Utilities for working with the Option data type.

The Option type is an enum that represents the possibility of something being present (with the `Some` variant), or not (with the `None` variant). There’s no standalone `null` or `nil` type in Grain; use an Option where you would normally reach for `null` or `nil`.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
import Option from "option"
```

```grain
let hasValue = Some(1234) // Creates an Option containing 1234
```

```grain
let noValue = None // Creates an Option containing nothing
```

## Values

Functions for working with the Option data type.

### Option.**isSome**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
isSome : Option<a> -> Bool
```

Checks if the Option is the `Some` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the Option is the `Some` variant or `false` otherwise|

### Option.**isNone**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
isNone : Option<a> -> Bool
```

Checks if the Option is the `None` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the Option is the `None` variant or `false` otherwise|

### Option.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
contains : (a, Option<a>) -> Bool
```

Checks if the Option is the `Some` variant and contains the given value. Uses the generic `==` equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value to search for|
|`option`|`Option<a>`|The option to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the Option is equivalent to `Some(value)` or `false` otherwise|

### Option.**expect**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
expect : (String, Option<a>) -> a
```

Extracts the value inside a `Some` option, otherwise throws an
exception containing the message provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`msg`|`String`|The message to use upon failure|
|`option`|`Option<a>`|The option to extract a value from|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value if the Option is the `Some` variant|

Throws:

`Failure(String)`

* When the `option` is `None`

### Option.**unwrap**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
unwrap : Option<a> -> a
```

Extracts the value inside a `Some` option, otherwise
throws an exception containing a default message.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to extract the value from|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value if the Option is the `Some` variant|

Throws:

`Failure(String)`

* When the `option` is `None`

### Option.**unwrapWithDefault**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
unwrapWithDefault : (a, Option<a>) -> a
```

Extracts the value inside a `Some` option or provide the default value if `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`default`|`a`|The default value|
|`option`|`Option<a>`|The option to unwrap|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value if the Option is the `Some` variant or the default value otherwise|

### Option.**map**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
map : ((a -> b), Option<a>) -> Option<b>
```

If the Option is `Some(value)`, applies the given function to the `value` and wraps the new value in a `Some` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of a `Some` variant|
|`option`|`Option<a>`|The option to map|

Returns:

|type|description|
|----|-----------|
|`Option<b>`|A new `Some` variant produced by the mapping function if the variant was `Some` or the unmodified `None` otherwise|

### Option.**mapWithDefault**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mapWithDefault : ((a -> b), b, Option<a>) -> b
```

If the Option is `Some(value)`, applies the given function to the `value` to produce a new value, otherwise uses the default value.
Useful for unwrapping an Option while providing a fallback for any `None` variants.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of a `Some` variant|
|`default`|`b`|A fallback value for a `None` variant|
|`option`|`Option<a>`|The option to map|

Returns:

|type|description|
|----|-----------|
|`b`|The value produced by the mapping function if the Option is of the `Some` variant or the default value otherwise|

### Option.**mapWithDefaultFn**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mapWithDefaultFn : ((a -> b), (() -> b), Option<a>) -> b
```

If the Option is `Some(value)`, applies the `fn` function to the `value` to produce a new value.
If the Option is `None`, calls the `defaultFn` function to produce a new value.
Useful for unwrapping an Option into a value, whether it is `Some` or `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of a `Some` variant|
|`defaultFn`|`() -> b`|The default function|
|`option`|`Option<a>`|The option to map|

Returns:

|type|description|
|----|-----------|
|`b`|The value produced by one of the mapping functions|

### Option.**flatMap**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
flatMap : ((a -> Option<b>), Option<a>) -> Option<b>
```

If the Option is `Some(value)`, applies the given function to the `value` to produce a new Option.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Option<b>`|The function to call on the value of a `Some` variant|
|`option`|`Option<a>`|The option to map|

Returns:

|type|description|
|----|-----------|
|`Option<b>`|A new Option produced by the mapping function if the variant was `Some` or the unmodified `None` otherwise|

### Option.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
filter : ((a -> Bool), Option<a>) -> Option<a>
```

Converts `Some(value)` variants to `None` variants where the predicate function returns `false`.
if the `fn` return `true` returns `Some(value)`, otherwise returns `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The predicate function to indicate if the option should remain `Some`|
|`option`|`Option<a>`|The option to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` if the variant was `Some` and the predicate returns `true` or `None` otherwise|

### Option.**zip**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
zip : (Option<a>, Option<b>) -> Option<(a, b)>
```

Combine two Options into a single Option containing a tuple of their values.

Parameters:

|param|type|description|
|-----|----|-----------|
|`optionA`|`Option<a>`|The first option to combine|
|`optionB`|`Option<b>`|The second option to combine|

Returns:

|type|description|
|----|-----------|
|`Option<(a, b)>`|`Some((valueA, valueB))` if both Options are `Some` variants or `None` otherwise|

### Option.**zipWith**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
zipWith : (((a, b) -> c), Option<a>, Option<b>) -> Option<c>
```

Combine two Options into a single Option. The new value is produced by applying the given function to both values.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> c`|The function to generate a new value|
|`optionA`|`Option<a>`|The first option to combine|
|`optionB`|`Option<b>`|The second option to combine|

Returns:

|type|description|
|----|-----------|
|`Option<c>`|`Some(newValue)` if both Options are `Some` variants or `None` otherwise|

### Option.**flatten**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
flatten : Option<Option<a>> -> Option<a>
```

Flattens nested Options.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<Option<a>>`|The option to flatten|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(innerValue)` if all nested options were the `Some` variant or `None` otherwise|

Examples:

```grain
Option.flatten(Some(Some(1))) == Some(1)
```

### Option.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toList : Option<a> -> List<a>
```

Converts an Option to a list with either zero or one item.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|`[value]` if the Option was the `Some` variant or `[]` otherwise|

### Option.**toArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toArray : Option<a> -> Array<a>
```

Converts an Option to an array with either zero or one item.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to convert|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|`[> value]` if the Option was the `Some` variant or `[> ]` otherwise|

### Option.**toResult**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toResult : (a, Option<b>) -> Result<b, a>
```

Converts the Option to a Result, using the provided error in case of the `None` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`err`|`a`|The error to use if the option is `None`|
|`option`|`Option<b>`|The option to convert|

Returns:

|type|description|
|----|-----------|
|`Result<b, a>`|`Ok(value)` if the Option is `Some(value)` or `Err(err)` if the Option is `None`|

### Option.**sideEffect**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
sideEffect : ((a -> Void), Option<a>) -> Void
```

If the Option is `Some(value)`, applies the `fn` function to the `value` without producing a new value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The function to call on the value of a `Some` variant|
|`option`|`Option<a>`|The option to inspect|

### Option.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
peek : ((a -> Void), Option<a>) -> Option<a>
```

If the Option is `Some(value)`, applies the `fn` function to the `value` without producing a new value.
Useful for inspecting Options without changing anything.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The function to call on the value of a `Some` variant|
|`option`|`Option<a>`|The option to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The unmodified option|

### Option.**or**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
or : (Option<a>, Option<a>) -> Option<a>
```

Behaves like a logical OR (`||`) where the first Option is only returned if it is the `Some` variant and falling back to the second Option in all other cases.

Parameters:

|param|type|description|
|-----|----|-----------|
|`optionA`|`Option<a>`|The first option|
|`optionB`|`Option<a>`|The second option|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The first Option if it is the `Some` variant or the second Option otherwise|

### Option.**and**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
and : (Option<a>, Option<a>) -> Option<a>
```

Behaves like a logical AND (`&&`) where the first Option is only returned if it is the `None` variant and falling back to the second Option Result in all other cases.

Parameters:

|param|type|description|
|-----|----|-----------|
|`optionA`|`Option<a>`|The first option|
|`optionB`|`Option<a>`|The second option|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The second Option if both are the `Some` variant or the first Option otherwise|

