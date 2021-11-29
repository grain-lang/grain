---
title: Range
---

Utilities for working with ranges. A range represents an interval—a set of values with a beginning and an end.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
import Range from "range"
```

## Types

Type declarations included in the Range module.

### Range.**Range**

```grain
enum Range {
  Inclusive(Number, Number),
  Exclusive(Number, Number),
}
```

Ranges can be inclusive or exclusive. When `Inclusive`, the end value will be included in operations. When `Exclusive`, the end value will be excluded from operations.

## Values

Functions and constants included in the Range module.

### Range.**inRange**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
inRange : (Number, Range) -> Bool
```

Checks if the given number is within the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The number being checked|
|`range`|`Range`|The range to check within|

Returns:

|type|description|
|----|-----------|
|`Bool`|Whether or not the value is within range|

Examples:

```grain
Range.inRange(1, Range.Inclusive(0, 2)) == true
```

```grain
Range.inRange(10, Range.Inclusive(0, 2)) == false
```

### Range.**forEach**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
forEach : ((Number -> Void), Range) -> Void
```

Calls the given function with each number in the range. For increasing ranges, the value is increased by `1` in each iteration, and for decreasing ranges, the value is decreased by `1`. The value is always changed by `1`, even if non-integer values were provided in the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`Number -> Void`|The function to be executed on each number in the range|
|`range`|`Range`|The range to iterate|

Examples:

```grain
Range.forEach(val => print(val), Range.Exclusive(0, 2))
```

### Range.**map**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
map : ((Number -> a), Range) -> List<a>
```

Produces a list by calling the given function on each number included in the range. For increasing ranges, the value is increased by `1` in each iteration, and for decreasing ranges, the value is decreased by `1`. The value is always changed by `1`, even if non-integer values were provided in the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`Number -> a`|The function called on each number in the range that returns the value for the output list|
|`range`|`Range`|The range to iterate|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all values returned from the `fn`|

Examples:

```grain
Range.map(val => val * 2, Range.Inclusive(0, 2)) == [0, 2, 4]
```

