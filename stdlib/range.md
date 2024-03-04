---
title: Range
---

Utilities for working with ranges.

A range represents an interval—a set of values with a beginning and an end.

All functions in this module treat ranges as exclusive, but inclusive versions
of all APIs are available in the `Inclusive` submodule.

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Treats all ranges as exclusive</td></tr>
</tbody>
</table>
</details>

```grain
from "range" include Range
```

## Values

Functions and constants included in the Range module.

### Range.**inRange**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Treats all ranges as exclusive</td></tr>
</tbody>
</table>
</details>

```grain
inRange : (value: Number, range: Range<Number>) => Bool
```

Checks if the given number is within the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The number being checked|
|`range`|`Range<Number>`|The range to check within|

Returns:

|type|description|
|----|-----------|
|`Bool`|Whether or not the value is within range|

Examples:

```grain
Range.inRange(1, { rangeStart: 0, rangeEnd: 2 }) == true
```

```grain
Range.inRange(10, { rangeStart: 0, rangeEnd: 2 }) == false
```

### Range.**forEach**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Treats all ranges as exclusive</td></tr>
</tbody>
</table>
</details>

```grain
forEach : (fn: (Number => Void), range: Range<Number>) => Void
```

Calls the given function with each number in the range.

For increasing ranges, the value is increased by `1` in each iteration,
and for decreasing ranges, the value is decreased by `1`. The value is
always changed by `1`, even if non-integer values were provided in the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`Number => Void`|The function to be executed on each number in the range|
|`range`|`Range<Number>`|The range to iterate|

Examples:

```grain
Range.forEach(val => print(val), { rangeStart: 0, rangeEnd: 2 })
```

### Range.**map**

<details>
<summary>Added in <code>0.3.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Treats all ranges as exclusive</td></tr>
</tbody>
</table>
</details>

```grain
map : (fn: (Number => a), range: Range<Number>) => List<a>
```

Produces a list by calling the given function on each number included in the range.

For increasing ranges, the value is increased by `1` in each iteration,
and for decreasing ranges, the value is decreased by `1`. The value is
always changed by `1`, even if non-integer values were provided in the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`Number => a`|The function called on each number in the range that returns the value for the output list|
|`range`|`Range<Number>`|The range to iterate|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all values returned from the `fn`|

Examples:

```grain
Range.map(val => val * 2, { rangeStart: 0, rangeEnd: 3 }) == [0, 2, 4]
```

## Range.Inclusive

### Values

Functions and constants included in the Range.Inclusive module.

#### Range.Inclusive.**inRange**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.0</code></td><td>Root APIs originally handled Inclusive & Exclusive variants</td></tr>
</tbody>
</table>
</details>

```grain
inRange : (value: Number, range: Range<Number>) => Bool
```

Checks if the given number is within the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`Number`|The number being checked|
|`range`|`Range<Number>`|The range to check within|

Returns:

|type|description|
|----|-----------|
|`Bool`|Whether or not the value is within range|

Examples:

```grain
Range.Inclusive.inRange(1, { rangeStart: 0, rangeEnd: 1 }) == true
```

```grain
Range.Inclusive.inRange(10, { rangeStart: 0, rangeEnd: 2 }) == false
```

#### Range.Inclusive.**forEach**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.0</code></td><td>Root APIs originally handled Inclusive & Exclusive variants</td></tr>
</tbody>
</table>
</details>

```grain
forEach : (fn: (Number => Void), range: Range<Number>) => Void
```

Calls the given function with each number in the range.

For increasing ranges, the value is increased by `1` in each iteration,
and for decreasing ranges, the value is decreased by `1`. The value is
always changed by `1`, even if non-integer values were provided in the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`Number => Void`|The function to be executed on each number in the range|
|`range`|`Range<Number>`|The range to iterate|

Examples:

```grain
Range.Inclusive.forEach(val => print(val), { rangeStart: 0, rangeEnd: 2 })
```

#### Range.Inclusive.**map**

<details>
<summary>Added in <code>0.3.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.0</code></td><td>Root APIs originally handled Inclusive & Exclusive variants</td></tr>
</tbody>
</table>
</details>

```grain
map : (fn: (Number => a), range: Range<Number>) => List<a>
```

Produces a list by calling the given function on each number included in the range.

For increasing ranges, the value is increased by `1` in each iteration,
and for decreasing ranges, the value is decreased by `1`. The value is
always changed by `1`, even if non-integer values were provided in the range.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`Number => a`|The function called on each number in the range that returns the value for the output list|
|`range`|`Range<Number>`|The range to iterate|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all values returned from the `fn`|

Examples:

```grain
Range.Inclusive.map(val => val * 2, { rangeStart: 0, rangeEnd: 2 }) == [0, 2, 4]
```

