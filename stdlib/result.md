---
title: Result
---

Utilities for working with the Result data type.

The `Result` type is a special type of enum that exists to represent the possibility of a success case (with the `Ok` variant),
or an error case (with the `Err` variant). Use a `Result` as the return type of a function that may return an error.



Create new `Result` values using `Ok()` and `Err()`:

```grain
import Result from "result"
```

```grain
let success = Ok("Yay!")
```

```grain
let failure = Err("Something bad happened")
```

## Values

Functions for working with the `result` data type.

### Result.**isOk**

```grain
isOk : Result<a, b> -> Bool
```

Checks if the Result is of the `Ok` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result`|`Result<a, b>`|The result to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the result is of the `Ok` variant, otherwise returns `false`|

### Result.**isErr**

```grain
isErr : Result<a, b> -> Bool
```

Checks if the Result is of the `Err` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result`|`Result<a, b>`|The result to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the result is of the `Err` variant, otherwise returns `false`|

### Result.**toOption**

```grain
toOption : Result<a, b> -> Option<a>
```

Converts the Result to an Option. The error is discarded and replaced with `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result`|`Result<a, b>`|The result to convert|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(okValue)` with okValue representing the value inside of `Ok` if the result is of the `Ok` variant, otherwise returns `None`|

### Result.**flatMap**

```grain
flatMap : ((a -> Result<b, c>), Result<a, c>) -> Result<b, c>
```

If the Result is `Ok`, applies the given function to the `Ok` value and returns the result. Returns the unmodified `Err` otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Result<b, c>`|The function to apply|
|`result`|`Result<a, b>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|`fn(okValue)` if the result is of the `Ok` variant, okValue represent the value wrapped by the result, otherwise returns the unmodified `Err`|

### Result.**flatMapErr**

```grain
flatMapErr : ((a -> Result<b, c>), Result<b, a>) -> Result<b, c>
```

If the Result is an `Err`, applies the given function to the `Err` value and returns the result. Returns the unmodified `Ok` value otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Result<b, c>`|The function to apply|
|`result`|`Result<a, b>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|`fn(errValue)` if the result is of the `Err` variant, errValue represent the value wrapped by the result, otherwise returns the unmodified `Ok`|

### Result.**map**

```grain
map : ((a -> b), Result<a, c>) -> Result<b, c>
```

If the Result is `Ok(x)`, returns `Ok(fn(x))`. Returns the unmodified `Err` otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map|
|`result`|`Result<a, b>`|The result to map on|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|`Ok(fn(okValue))` if the result is of the `Ok` variant, otherwise returns the unmodified `Err`|

### Result.**mapErr**

```grain
mapErr : ((a -> b), Result<c, a>) -> Result<c, b>
```

If the Result is `Err(x)`, returns `Err(fn(x))`. Returns the unmodified `Ok` otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map|
|`result`|`Result<a, b>`|The result to map on|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|`Err(fn(errValue))` if the result is of the `Err` variant, otherwise returns the unmodified `Ok`|

### Result.**mapWithDefault**

```grain
mapWithDefault : ((a -> b), b, Result<a, c>) -> b
```

If the Result is `Ok(x)`, returns `fn(x)`. Returns the provided default otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map|
|`def`|`a`|The default value to return|
|`result`|`Result<a, b>`|The result to map on|

Returns:

|type|description|
|----|-----------|
|`a`|`Ok(fn(okValue))` if the result is of the `Ok` variant, otherwise returns the value given by the `def` param|

### Result.**mapWithDefaultFn**

```grain
mapWithDefaultFn : ((a -> b), (c -> b), Result<a, c>) -> b
```

If the Result is `Ok(x)`, returns `fnOk(x)`. If the Result is `Err(y)`, returns `fnErr(y)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map|
|`def`|`a -> b`|The default value to return|
|`result`|`Result<a, b>`|The result to map on|

Returns:

|type|description|
|----|-----------|
|`a`|`fnOk(okValue)` if the result is of the `Ok` variant, otherwise returns `fnErr(errValue)`|

### Result.**or**

```grain
( or ) : (Result<a, b>, Result<a, b>) -> Result<a, b>
```

If the first Result is `Ok`, returns the first Result. Returns the second Result otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result1`|`Result<a, b>`|The first result|
|`result2`|`Result<a, b>`|The second result|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|`result1` if result1 is of the `Ok` variant, otherwise returns `result2`|

### Result.**and**

```grain
and : (Result<a, b>, Result<a, b>) -> Result<a, b>
```

If the first Result is `Err`, returns the first Result. Returns the second Result otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result1`|`Result<a, b>`|The first result|
|`result2`|`Result<a, b>`|The second result|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|`result1` if result1 is of the `Err` variant, otherwise returns `result2`|

### Result.**peek**

```grain
peek : ((a -> b), (c -> d), Result<a, c>) -> Void
```

If the Result is `Ok(x)`, applies the first function to `x`. If the Result is `Err(y)`, applies the second function to `y`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fnOk`|`a -> b`|The function to apply on the result if it is an `Err`|
|`fnErr`|`a -> b`|The function to apply on the result if it is an `Err`|
|`result`|`Result<a, b>`|The result to apply the function on|

### Result.**peekOk**

```grain
peekOk : ((a -> b), Result<a, c>) -> Void
```

If the Result is `Ok(x)`, applies the function to `x`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to apply|
|`result`|`Result<a, b>`|The result to apply the function on|

### Result.**peekErr**

```grain
peekErr : ((a -> b), Result<c, a>) -> Void
```

If the Result is `Err(x)`, applies the function to `x`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to apply|
|`result`|`Result<a, b>`|The result to apply the function on|

### Result.**expect**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
expect : (String, Result<a, b>) -> a
```

Extracts the value inside an `Ok` result, otherwise throw an
exception containing the message and contents of the `Err`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`msg`|`String`|The message to prepend if the result contains an `Err`|
|`result`|`Result<a, b>`|The result to extract a value from|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped `ok` value if the result is of the `Ok` variant, otherwise throws `msg: err`|

Examples:

```grain
Result.expect("Unexpected error", Ok(1234)) == 1234
```

### Result.**unwrap**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
unwrap : Result<a, b> -> a
```

Extracts the value inside an `Ok` result, otherwise throw an
exception containing a default message and contents of the `Err`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result`|`Result<a, b>`|The result to extract a value from|

Returns:

|type|description|
|----|-----------|
|`a`|The unwrapped `ok` value if the result is of the `Ok` variant, otherwise it throws with `Could not unwrap Err value: This will throw`|

Examples:

```grain
Result.unwrap(Err("This will throw"))
```

