---
title: Option
---

Utilities for working with the Option data type.

The Option type is an enum that represents the possibility of something being present (with the `Some` variant), or not (with the `None` variant). There’s no standalone `null` or `none` type in Grain; use an Option where you would normally reach for `null` or `none`



Create new `Option` values using `Some()` and `None`.

```grain
import Option from "option"
```

```grain
let has_value = Some(1234), no_value = None
```

## Values

Functions for working with the Option data type.

### Option.**isSome**

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

```grain
contains : (a, Option<a>) -> Bool
```

Checks if the Option is the `Some` variant and contains the given value. Uses the generic `==` equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value to expect|
|`option`|`Option<a>`|The option to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the Option is the `Some` variant and contains the given value, `false` otherwise|

### Option.**expect**

```grain
expect : (String, Option<a>) -> a
```

Extracts the value inside a `Some` result, otherwise throw an
exception containing the message provided.
If it is `None`, throws the provided failure message.

Parameters:

|param|type|description|
|-----|----|-----------|
|`msg`|`String`|The failure message|
|`option`|`Option<a>`|The option to unwrap|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value if the Option is the `Some` variant|

### Option.**unwrap**

```grain
unwrap : Option<a> -> a
```

Extracts the value inside a `Some` Option, otherwise
throw an exception containing a default message.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The Option to extract the value from|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value if the Option is the `Some` variant|

### Option.**unwrapWithDefault**

```grain
unwrapWithDefault : (a, Option<a>) -> a
```

Extracts the value inside a `Some` Option, otherwise
return the provided default value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`default`|`a`|The default value to return|
|`option`|`Option<a>`|The option to unwrap|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value if the Option is the `Some` variant, otherwise the default value provided|

### Option.**map**

```grain
map : ((a -> b), Option<a>) -> Option<b>
```

If the Option is `Some(value)`, applies the given function to the `value` to produce a new Option.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of an `Some` variant|
|`option`|`Option<a>`|The Option to map|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|A new Option produced by the mapping function if the variant was `Some` or the unmodified `None` otherwise|

### Option.**mapWithDefault**

```grain
mapWithDefault : ((a -> b), b, Option<a>) -> b
```

If the Option is `Some(value)`, applies the given function to the `value` to produce a new value, otherwise uses the default value.
Useful for unwrapping a `Some` Option while providing a fallback for any None values.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of a `Some` variant|
|`default`|`a`|A fallback value for a `None` variant|
|`option`|`Option<a>`|The Option to map|

Returns:

|type|description|
|----|-----------|
|`a`|The value produced by the mapping function if the Option is of the `Some` variant or the default value otherwise|

### Option.**mapWithDefaultFn**

```grain
mapWithDefaultFn : ((a -> b), (() -> b), Option<a>) -> b
```

If the Option is `Some(value)`, applies the `fn` function to the `value` to produce a new value.
If the Option is `None`, applies the `defaultFn` function to produce a new value.
Useful for unwrapping an Option into a value, whether it is `Some` or `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of a `Some` variant|
|`defaultFn`|`() -> a`|The default function to map with|
|`option`|`Option<a>`|The Option to map|

Returns:

|type|description|
|----|-----------|
|`a`|The value produced by one of the mapping functions|

### Option.**flatMap**

```grain
flatMap : ((a -> Option<b>), Option<a>) -> Option<b>
```

If the Option is `Some(value)`, applies the given function to the `value` to produce a new Option.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Option<b>`|The function to call on the value of a `Some` variant|
|`option`|`Option<a>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|A new Option produced by the mapping function if the variant was `Some` or the unmodified `None` otherwise|

### Option.**filter**

```grain
filter : ((a -> Bool), Option<a>) -> Option<a>
```

If the Option is `Some(value)` call `fn` with the `value`,
if the `fn` return `true` returns `Some(value)`, otherwise returns `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to filter with|
|`option`|`Option<a>`|The option to filter|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` if the filter function returns `true`, `None` otherwise|

### Option.**zip**

```grain
zip : (Option<a>, Option<b>) -> Option<(a, b)>
```

If both `optionA` and `optionB` are `Some(value)` return `Some((valueA, valueB))`, otherwise return `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`optionA`|`Option<a>`|The first Option to zip|
|`optionB`|`Option<a>`|The second Option to zip|

Returns:

|type|description|
|----|-----------|
|`Option<(a, b)>`|`Some((valueA, valueB))` if both Options are `Some` variants, otherwise `None`|

### Option.**zipWith**

```grain
zipWith : (((a, b) -> c), Option<a>, Option<b>) -> Option<c>
```

If both `optionA` and `optionB` are `Some(value)` return `Some((fn(valueA), fn(valueB)))`, otherwise return `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> c`|The function to zip with|
|`optionA`|`Option<a>`|The first Option to zip|
|`optionB`|`Option<a>`|The second Option to zip|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some((fn(valueA), fn(valueB)))` if both Options are `Some` variants, otherwise `None`|

### Option.**flatten**

```grain
flatten : Option<Option<a>> -> Option<a>
```

Flattens nested Options, like `Some(Some(1))` to `Some(1)`. If any of the Options are `None`, returns `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<Option<a>>`|The option to flatten|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(Some(1))` to `Some(1)`. If any of the Options are `None`, returns `None`|

### Option.**toList**

```grain
toList : Option<a> -> List<a>
```

If the Option is `Some(value)` returns `[ value ]`, Otherwise returns `[ ]`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|`Some(value)` returns `[ value ]`, Otherwise returns `[ ]`|

### Option.**toArray**

```grain
toArray : Option<a> -> Array<a>
```

If the Option is the `Some` variant, returns a `Array` containing the inner value as the only item. If it is `None`, returns an empty `Array`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to convert|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The option represented as a array|

### Option.**toResult**

```grain
toResult : (a, Option<b>) -> Result<b, a>
```

If the Option is the `Some(a)`, returns `Ok(a)`. If it is `None`, returns an `Err` of the provided error value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`a`|The option to convert|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The option represented as a result|

### Option.**sideEffect**

```grain
sideEffect : ((a -> Void), Option<a>) -> Void
```

If the Option is the `Some` variant, call `fn` with the inner value. Always returns `void`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The function to call with|
|`option`|`Option<a>`|The option to call on|

### Option.**peek**

```grain
peek : ((a -> Void), Option<a>) -> Option<a>
```

If the Option is the `Some` variant, call `fn` with the inner value. Always returns the Option it is called with; this method is a “chainable” `Option.sideEffect`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The function to call with|
|`option`|`Option<a>`|The option to call on|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The option|

### Option.**or**

```grain
( or ) : (Option<a>, Option<a>) -> Option<a>
```

If the first Option is the `Some` variant, returns the second Option. Returns `None` otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`optionA`|`Option<a>`|The first option|
|`optionB`|`Option<a>`|The first option|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The second option or none depending on the first option|

### Option.**and**

```grain
and : (Option<a>, Option<a>) -> Option<a>
```

Returns the first Option if it is the `Some` variant. Returns the second Option otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`optionA`|`Option<a>`|The first option|
|`optionB`|`Option<a>`|The first option|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The option meeting the conditions|

