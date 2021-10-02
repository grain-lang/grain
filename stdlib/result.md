---
title: Result
---

Utilities for working with the Result data type.

The `Result` type is a special type of enum that exists to represent the possibility of a success case (with the `Ok` variant), or an error case (with the `Err` variant). Use a `Result` as the return type of a function that may return an error.



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

Checks if the Result is the `Ok` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`v`|`Result<a, b>`|The result to check.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the result isOk.|

### Result.**isErr**

```grain
isErr : Result<a, b> -> Bool
```

Checks if the Result is the `Err` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`v`|`Result<a, b>`|The result to check.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the result isErr.|

### Result.**toOption**

```grain
toOption : Result<a, b> -> Option<a>
```

Converts the Result to an Option. The error is discarded and replaced with `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`v`|`Result<a, b>`|The result to convert.|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The result as an option.|

### Result.**flatMap**

```grain
flatMap : ((a -> Result<b, c>), Result<a, c>) -> Result<b, c>
```

If the Result is `Ok`, applies the given function to the `Ok` value and returns the result. Returns the unmodified `Err` otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Result<b, c>`|The function to apply.|
|`v`|`Result<a, b>`|The result to map.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result of the applied function.|

### Result.**flatMapErr**

```grain
flatMapErr : ((a -> Result<b, c>), Result<b, a>) -> Result<b, c>
```

If the Result is an `Err`, applies the given function to the `Err` value and returns the result. Returns the unmodified `Ok` value otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Result<b, c>`|The function to apply.|
|`v`|`Result<a, b>`|The result to map.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result of the applied function.|

### Result.**map**

```grain
map : ((a -> b), Result<a, c>) -> Result<b, c>
```

If the Result is `Ok(x)`, returns `Ok(fn(x))`. Returns the unmodified `Err` otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map.|
|`v`|`Result<a, b>`|The result to map on.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result with the functions mapped.|

### Result.**mapErr**

```grain
mapErr : ((a -> b), Result<c, a>) -> Result<c, b>
```

If the Result is `Err(x)`, returns `Err(fn(x))`. Returns the unmodified `Ok` otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map.|
|`v`|`Result<a, b>`|The result to map on.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result with the functions mapped.|

### Result.**mapWithDefault**

```grain
mapWithDefault : ((a -> b), b, Result<a, c>) -> b
```

If the Result is `Ok(x)`, returns `fnOk(fn(x))`. If the Result is `Err(y)`, returns `fnErr(y)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map.|
|`v`|`a`|The result to map on.|

Returns:

|type|description|
|----|-----------|
|`a`|The result with the functions mapped.|

### Result.**mapWithDefaultFn**

```grain
mapWithDefaultFn : ((a -> b), (c -> b), Result<a, c>) -> b
```

If the Result is `Ok(x)`, returns `fnOk(x)`. If the Result is `Err(y)`, returns `fnErr(y)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to map.|
|`v`|`a -> b`|The result to map on.|

Returns:

|type|description|
|----|-----------|
|`a`|The result with the functions mapped.|

### Result.**or**

```grain
( or ) : (Result<a, b>, Result<a, b>) -> Result<a, b>
```

If the first Result is `Ok`, returns the first Result. Returns the second Result otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`r1`|`Result<a, b>`|The first result.|
|`r2`|`Result<a, b>`|The second result.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result that is Ok.|

### Result.**and**

```grain
and : (Result<a, b>, Result<a, b>) -> Result<a, b>
```

If the first Result is `Err`, returns the first Result. Returns the second Result otherwise.

Parameters:

|param|type|description|
|-----|----|-----------|
|`r1`|`Result<a, b>`|The first result.|
|`r2`|`Result<a, b>`|The second result.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result that is Err.|

### Result.**peek**

```grain
peek : ((a -> b), (c -> d), Result<a, c>) -> Result<a, c>
```

If the Result is `Ok(x)`, applies the first function to `x`. If the Result is `Err(y)`, applies the second function to `y`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fnOk`|`a -> b`|The function to apply on the result if it is an `Err`.|
|`fnErr`|`a -> b`|The function to apply on the result if it is an `Err`.|
|`r`|`Result<a, b>`|The result to apply the function on.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result with the appropriate function applied.|

### Result.**peekOk**

```grain
peekOk : ((a -> b), Result<a, c>) -> Result<a, c>
```

If the Result is `Ok(x)`, applies the function to `x`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to apply.|
|`r`|`Result<a, b>`|The result to apply the function on.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result with the function applied if it is `Ok`.|

### Result.**peekErr**

```grain
peekErr : ((a -> b), Result<c, a>) -> Result<c, a>
```

If the Result is `Err(x)`, applies the function to `x`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to apply.|
|`r`|`Result<a, b>`|The result to apply the function on.|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The result with the function applied if it is `Err`.|

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
|`a`|The value inside an `Ok` result|

Examples:

```grain
Result.expect("Unexpected error", Ok(1234))
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
|`a`|The value inside an `Ok` result|

Examples:

```grain
Result.unwrap(Err("This will throw"))
```

