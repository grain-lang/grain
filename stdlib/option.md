---
title: Option
---

Utilities for working with the Option data type.

The `Option` type is a special type of enum that exists to represent the possibility of something being present (with the `Some` variant), or not (with the `None` variant). There’s no standalone `null` or `none` type in Grain; use an Option where you would normally reach for `null` or `none`



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
|`option`|`Option<a>`|The option|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the argument is of the `Some` variant|

### Option.**isNone**

```grain
isNone : Option<a> -> Bool
```

Checks if the Option is the `None` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the argument is of the `None` variant|

### Option.**contains**

```grain
contains : (a, Option<a>) -> Bool
```

Checks if the Option is the `Some` variant and contains the given value. Uses the generic `==` equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`val`|`a`|The value to look for|
|`option`|`Option<a>`|The option|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the option contains the given value|

### Option.**expect**

```grain
expect : (String, Option<a>) -> a
```

Attempts to unwrap the inner value from the `Some` variant. If it is `None`, raises `Failure` with the provided message.

Parameters:

|param|type|description|
|-----|----|-----------|
|`msg`|`String`|The failure message|
|`option`|`Option<a>`|The option to unwrap|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value|

### Option.**unwrap**

```grain
unwrap : Option<a> -> a
```

Attempts to unwrap the inner value from the `Some` variant. If it is `None`, raises `Failure` with a generic message.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to unwrap|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value|

### Option.**unwrapWithDefault**

```grain
unwrapWithDefault : (a, Option<a>) -> a
```

Unwraps the inner value from the `Some` variant. If it is `None`, returns the default value provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`default`|`a`|The default value|
|`option`|`Option<a>`|The option to unwrap|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped value or the default value|

### Option.**map**

```grain
map : ((a -> b), Option<a>) -> Option<b>
```

If the Option is the `Some` variant, call `fn` with the inner value and returns a new `Some` variant with the produced value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map with|
|`option`|`Option<a>`|The option to map|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The mapped option|

### Option.**mapWithDefault**

```grain
mapWithDefault : ((a -> b), b, Option<a>) -> b
```

If the Option is the `Some` variant, call `fn` with the inner value and returns the result. If it is `None`, returns the default value provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map with|
|`default`|`a`|The default value|
|`option`|`Option<a>`|The option to map|

Returns:

|type|description|
|----|-----------|
|`a`|The mapped option|

### Option.**mapWithDefaultFn**

```grain
mapWithDefaultFn : ((a -> b), (() -> b), Option<a>) -> b
```

If the Option is the `Some` variant, call `fn` with the inner value and returns the result. If it is `None`, returns the result of the default function provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map with|
|`defaultFn`|`() -> a`|The default function to map with|
|`option`|`Option<a>`|The option to map|

Returns:

|type|description|
|----|-----------|
|`a`|The mapped option|

### Option.**flatMap**

```grain
flatMap : ((a -> Option<b>), Option<a>) -> Option<b>
```

If the Option is the `Some` variant, call `fn` with the inner value. The `fn` must produce its own Option to be returned.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Option<b>`|The function to map with|
|`option`|`Option<a>`|The option to map|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The mapped option|

### Option.**filter**

```grain
filter : ((a -> Bool), Option<a>) -> Option<a>
```

If the Option is the `Some` variant, call `fn` with the inner value. If the `fn` returns `false`, returns a `None` variant type.

Parameters:

|param|type|description|
|-----|----|-----------|
|`filter`|`a -> Bool`|The function to filter with|
|`option`|`Option<a>`|The option to filter|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The filtered option|

### Option.**zip**

```grain
zip : (Option<a>, Option<b>) -> Option<(a, b)>
```

If both Options are the `Some` variant, returns a new `Some` variant that contains a tuple of both values. If any of the Options are `None`, returns `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`optionA`|`Option<a>`|The first option|
|`optionB`|`Option<a>`|The second option|

Returns:

|type|description|
|----|-----------|
|`Option<(a, b)>`|The zipped option|

### Option.**zipWith**

```grain
zipWith : (((a, b) -> c), Option<a>, Option<b>) -> Option<c>
```

If both Options are the `Some` variant, call `fn` with both inner values and returns a new `Some` variant with the produced value. If any of the Options are `None`, returns `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> c`|The function to zip with|
|`optionA`|`Option<a>`|The first option|
|`optionB`|`Option<a>`|The second option|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The zipped option|

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
|`Option<a>`|The flattened option|

### Option.**toList**

```grain
toList : Option<a> -> List<a>
```

If the Option is the `Some` variant, returns a `List` containing the inner value as the only item. If it is `None`, returns an empty `List`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`option`|`Option<a>`|The option to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The option represented as a list|

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

