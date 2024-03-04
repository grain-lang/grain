---
title: Set
---

A Set is an unordered collection of unique values. Operations on a Set mutate the internal state, so it never needs to be re-assigned.

An immutable set implementation is available in the `Immutable` submodule.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
from "set" include Set
```

## Types

Type declarations included in the Set module.

### Set.**Set**

```grain
type Set<k>
```

### Set.**InternalSetStats**

```grain
record InternalSetStats {
  currentSize: Number,
  bucketCount: Number,
}
```

Represents the internal state of a set.

## Values

Functions and constants included in the Set module.

### Set.**make**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Merged with `makeSized`; modified signature to accept size</td></tr>
</tbody>
</table>
</details>

```grain
make : (?size: Number) => Set<a>
```

Creates a new empty set with an initial storage of the given size. As
values are added or removed, the internal storage may grow or shrink.
Generally, you won't need to care about the storage size of your set and
can use the default size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?size`|`Number`|The initial storage size of the set|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|An empty set with the given initial storage size|

### Set.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
add : (key: a, set: Set<a>) => Void
```

Adds a new value to the set. If the value already exists, nothing happens.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to add|
|`set`|`Set<a>`|The set to update|

### Set.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
contains : (key: a, set: Set<a>) => Bool
```

Determines if the set contains the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to search for|
|`set`|`Set<a>`|The set to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the set contains the given value or `false` otherwise|

### Set.**remove**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
remove : (key: a, set: Set<a>) => Void
```

Removes the given value from the set. If the value doesn't exist, nothing happens.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to remove|
|`set`|`Set<a>`|The set to update|

### Set.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
size : (set: Set<a>) => Number
```

Provides the count of values within the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The count of elements in the set|

### Set.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty : (set: Set<a>) => Bool
```

Determines if the set contains no elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the given set is empty or `false` otherwise|

### Set.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
clear : (set: Set<a>) => Void
```

Resets the set by removing all values.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to reset|

### Set.**forEach**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Ensured the iterator function return type is always `Void`</td></tr>
</tbody>
</table>
</details>

```grain
forEach : (fn: (a => Void), set: Set<a>) => Void
```

Iterates the set, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Void`|The iterator function to call with each element|
|`set`|`Set<a>`|The set to iterate|

### Set.**reduce**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
reduce : (fn: ((a, b) => a), init: a, set: Set<b>) => a
```

Combines all elements of a set using a reducer function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) => a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`init`|`a`|The initial value to use for the accumulator on the first iteration|
|`set`|`Set<b>`|The set to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

### Set.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
filter : (fn: (a => Bool), set: Set<a>) => Void
```

Removes elements from a set where a predicate function returns `false`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The predicate function to indicate which elements to remove from the set, where returning `false` indicates the value should be removed|
|`set`|`Set<a>`|The set to iterate|

### Set.**reject**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
reject : (fn: (a => Bool), set: Set<a>) => Void
```

Removes elements from a set where a predicate function returns `true`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The predicate function to indicate which elements to remove from the set, where returning `true` indicates the value should be removed|
|`set`|`Set<a>`|The set to iterate|

### Set.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
toList : (set: Set<a>) => List<a>
```

Converts a set into a list of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all set values|

### Set.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
fromList : (list: List<a>) => Set<a>
```

Creates a set from a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to convert|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing all list values|

### Set.**toArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
toArray : (set: Set<a>) => Array<a>
```

Converts a set into an array of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to convert|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|An array containing all set values|

### Set.**fromArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
fromArray : (array: Array<a>) => Set<a>
```

Creates a set from an array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to convert|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing all array values|

### Set.**union**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
union : (set1: Set<a>, set2: Set<a>) => Set<a>
```

Combines two sets into a single set containing all elements from both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to combine|
|`set2`|`Set<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing all elements of both sets|

### Set.**diff**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
diff : (set1: Set<a>, set2: Set<a>) => Set<a>
```

Combines two sets into a single set containing only the elements not shared between both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to combine|
|`set2`|`Set<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing only unshared elements from both sets|

### Set.**intersect**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
intersect : (set1: Set<a>, set2: Set<a>) => Set<a>
```

Combines two sets into a single set containing only the elements shared between both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to combine|
|`set2`|`Set<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing only shared elements from both sets|

### Set.**getInternalStats**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Return `InternalSetStats` record instead of a tuple</td></tr>
</tbody>
</table>
</details>

```grain
getInternalStats : (set: Set<a>) => InternalSetStats
```

Provides data representing the internal state state of the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to inspect|

Returns:

|type|description|
|----|-----------|
|`InternalSetStats`|The internal state of the set|

## Set.Immutable

An immutable set implementation.

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

### Types

Type declarations included in the Set.Immutable module.

#### Set.Immutable.**Set**

```grain
type Set<a>
```

### Values

Functions and constants included in the Set.Immutable module.

#### Set.Immutable.**empty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
empty : Set<a>
```

An empty set

#### Set.Immutable.**size**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
size : (set: Set<a>) => Number
```

Provides the count of values within the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The count of elements in the set|

#### Set.Immutable.**isEmpty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
isEmpty : (set: Set<a>) => Bool
```

Determines if the set contains no elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the given set is empty or `false` otherwise|

#### Set.Immutable.**add**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
add : (key: a, set: Set<a>) => Set<a>
```

Produces a new set by inserting the given value into the set. If the value
already exists, the new set will have the same elements as the input set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to add|
|`set`|`Set<a>`|The base set|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A new set containing the new element|

#### Set.Immutable.**contains**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
contains : (key: a, set: Set<a>) => Bool
```

Determines if the set contains the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to search for|
|`set`|`Set<a>`|The set to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the set contains the given value or `false` otherwise|

#### Set.Immutable.**remove**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
remove : (key: a, set: Set<a>) => Set<a>
```

Produces a new set without the given element. If the value doesn't exist in
the set, the set will be returned unmodified.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to exclude|
|`set`|`Set<a>`|The set to exclude from|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A new set without the excluded element|

#### Set.Immutable.**forEach**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
forEach : (fn: (a => Void), set: Set<a>) => Void
```

Iterates the set, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Void`|The iterator function to call with each element|
|`set`|`Set<a>`|The set to iterate|

#### Set.Immutable.**reduce**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
reduce : (fn: ((a, b) => a), init: a, set: Set<b>) => a
```

Combines all elements of a set using a reducer function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) => a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`init`|`a`|The initial value to use for the accumulator on the first iteration|
|`set`|`Set<b>`|The set to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

#### Set.Immutable.**filter**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
filter : (fn: (a => Bool), set: Set<a>) => Set<a>
```

Produces a new set without the elements from the input set where a predicate function returns `false`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The predicate function to indicate which elements to exclude from the set, where returning `false` indicates the value should be excluded|
|`set`|`Set<a>`|The set to iterate|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A new set excluding the elements not fulfilling the predicate|

#### Set.Immutable.**reject**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
reject : (fn: (a => Bool), set: Set<a>) => Set<a>
```

Produces a new set without the elements from the input set where a predicate function returns `true`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The predicate function to indicate which elements to exclude from the set, where returning `true` indicates the value should be excluded|
|`set`|`Set<a>`|The set to iterate|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A new set excluding the elements fulfilling the predicate|

#### Set.Immutable.**union**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
union : (set1: Set<a>, set2: Set<a>) => Set<a>
```

Combines two sets into a single set containing all elements from both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to combine|
|`set2`|`Set<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing all elements of both sets|

#### Set.Immutable.**diff**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
diff : (set1: Set<a>, set2: Set<a>) => Set<a>
```

Combines two sets into a single set containing only the elements not shared between both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to combine|
|`set2`|`Set<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing only unshared elements from both sets|

#### Set.Immutable.**intersect**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
intersect : (set1: Set<a>, set2: Set<a>) => Set<a>
```

Combines two sets into a single set containing only the elements shared between both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to combine|
|`set2`|`Set<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing only shared elements from both sets|

#### Set.Immutable.**fromList**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
fromList : (list: List<a>) => Set<a>
```

Creates a set from a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to convert|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing all list values|

#### Set.Immutable.**toList**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
toList : (set: Set<a>) => List<a>
```

Converts a set into a list of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all set values|

#### Set.Immutable.**fromArray**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
fromArray : (array: Array<a>) => Set<a>
```

Creates a set from an array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to convert|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|A set containing all array values|

#### Set.Immutable.**toArray**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutableset"` module</td></tr>
</tbody>
</table>
</details>

```grain
toArray : (set: Set<a>) => Array<a>
```

Converts a set into an array of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to convert|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|An array containing all set values|

