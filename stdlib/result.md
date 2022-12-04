---
title: Result
---

Utilities for working with the Result data type.

The Result type is an enum that represents the possibility of a success case (with the `Ok` variant),
or an error case (with the `Err` variant). Use a Result as the return type of a function that may return an error.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
import Result from "result"
```

```grain
let success = Ok((x) => 1 + x) // Creates a successful Result containing (x) => 1 + x
```

```grain
let failure = Err("Something bad happened") // Creates an unsuccessful Result containing "Something bad happened"
```

## Values

Functions for working with the Result data type.

### Result.**isOk**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
isOk : Result<a, b> -> Bool
```

Checks if the Result is the `Ok` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result`|`Result<a, b>`|The result to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the Result is the `Ok` variant or `false` otherwise|

### Result.**isErr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
isErr : Result<a, b> -> Bool
```

Checks if the Result is the `Err` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result`|`Result<a, b>`|The result to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the Result is the `Err` variant or `false` otherwise|

### Result.**toOption**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toOption : Result<a, b> -> Option<a>
```

Converts the Result to an Option. An error value is discarded and replaced with `None`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result`|`Result<a, b>`|The result to convert|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` if the Result is `Ok(value)` or `None` if the Result is an `Err`|

### Result.**flatMap**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
flatMap : ((a -> Result<b, c>), Result<a, c>) -> Result<b, c>
```

If the Result is `Ok(value)`, applies the given function to the `value` to produce a new Result.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Result<b, c>`|The function to call on the value of an `Ok` variant|
|`result`|`Result<a, c>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`Result<b, c>`|A new Result produced by the mapping function if the variant was `Ok` or the unmodified `Err` otherwise|

### Result.**flatMapErr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
flatMapErr : ((a -> Result<b, c>), Result<b, a>) -> Result<b, c>
```

If the Result is an `Err(value)`, applies the given function to the `value` to produce a new Result.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Result<b, c>`|The function to call on the value of an `Err` variant|
|`result`|`Result<b, a>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`Result<b, c>`|A new Result produced by the mapping function if the variant was `Err` or the unmodified `Ok` otherwise|

### Result.**map**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
map : ((a -> b), Result<a, c>) -> Result<b, c>
```

If the Result is `Ok(value)`, applies the given function to the `value` and wraps the new value in an `Ok` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of an `Ok` variant|
|`result`|`Result<a, c>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`Result<b, c>`|A new `Ok` variant produced by the mapping function if the variant was `Ok` or the unmodified `Err` otherwise|

### Result.**mapErr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mapErr : ((a -> b), Result<c, a>) -> Result<c, b>
```

If the Result is `Err(value)`, applies the given function to the `value` and wraps the new value in an `Err` variant.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of an `Err` variant|
|`result`|`Result<c, a>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`Result<c, b>`|A new `Err` variant produced by the mapping function if the variant was `Err` or the unmodified `Ok` otherwise|

### Result.**mapWithDefault**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mapWithDefault : ((a -> b), b, Result<a, c>) -> b
```

If the Result is `Ok(value)`, applies the given function to the `value` to produce a new value, otherwise uses the default value.
Useful for unwrapping a successful Result while providing a fallback for any errors that can occur.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of an `Ok` variant|
|`def`|`b`|A fallback value for an `Err` variant|
|`result`|`Result<a, c>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`b`|The value produced by the mapping function if the result is of the `Ok` variant or the default value otherwise|

### Result.**mapWithDefaultFn**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
mapWithDefaultFn : ((a -> b), (c -> b), Result<a, c>) -> b
```

If the Result is `Ok(value)`, applies the `fnOk` function to the `value` to produce a new value.
If the Result is `Err(value)`, applies the `fnErr` function to the `value` to produce a new value.
Useful for unwrapping a Result into a value, whether it is successful or unsuccessful.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fnOk`|`a -> b`|The function to call on the value of an `Ok` variant|
|`fnErr`|`c -> b`|The function to call on the value of an `Err` variant|
|`result`|`Result<a, c>`|The result to map|

Returns:

|type|description|
|----|-----------|
|`b`|The value produced by one of the mapping functions|

### Result.**or**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
or : (Result<a, b>, Result<a, b>) -> Result<a, b>
```

Behaves like a logical OR (`||`) where the first Result is only returned if it is the `Ok` variant and falling back to the second Result in all other cases.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result1`|`Result<a, b>`|The first result|
|`result2`|`Result<a, b>`|The second result|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The first Result if it is the `Ok` variant or the second Result otherwise|

### Result.**and**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
and : (Result<a, b>, Result<a, b>) -> Result<a, b>
```

Behaves like a logical AND (`&&`) where the first Result is only returned if it is the `Err` variant and falling back to the second Result in all other cases.

Parameters:

|param|type|description|
|-----|----|-----------|
|`result1`|`Result<a, b>`|The first result|
|`result2`|`Result<a, b>`|The second result|

Returns:

|type|description|
|----|-----------|
|`Result<a, b>`|The second Result if both are the `Ok` variant or the first Result otherwise|

### Result.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
peek : ((a -> b), (c -> d), Result<a, c>) -> Void
```

If the Result is `Ok(value)`, applies the `fnOk` function to the `value` without producing a new value.
If the Result is `Err(value)`, applies the `fnErr` function to the `value` without producing a new value.
Useful for inspecting Results without changing anything.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fnOk`|`a -> b`|The function to call on the value of an `Ok` variant|
|`fnErr`|`c -> d`|The function to call on the value of an `Err` variant|
|`result`|`Result<a, c>`|The result to inspect|

### Result.**peekOk**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
peekOk : ((a -> b), Result<a, c>) -> Void
```

If the Result is `Ok(value)`, applies the given function to the `value` without producing a new value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of an `Ok` variant|
|`result`|`Result<a, c>`|The result to inspect|

### Result.**peekErr**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
peekErr : ((a -> b), Result<c, a>) -> Void
```

If the Result is `Err(value)`, applies the given function to the `value` without producing a new value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on the value of an `Err` variant|
|`result`|`Result<c, a>`|The result to inspect|

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
|`a`|The unwrapped value if the Result is the `Ok` variant|

Throws:

`Failure(String)`

* When the `result` is `Err`

Examples:

```grain
Result.expect("Unexpected error", Ok(1234)) + 42
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
|`a`|The unwrapped value if the result is the `Ok` variant|

Throws:

`Failure(String)`

* When the `result` is `Err`

Examples:

```grain
Result.unwrap(Err("This will throw"))
```

