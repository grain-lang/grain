---
title: Set
---

A Set is an unordered collection of unique values. Operations on a Set mutate the internal state, so it never needs to be re-assigned.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
import Set from "set"
```

## Types

Type declarations included in the Set module.

### Set.**Set**

```grain
type Set<k>
```

## Values

Functions for working with Sets.

### Set.**makeSized**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
makeSized : Number -> Set<a>
```

Creates a new empty set with an initial storage of the given size. As values are added or removed, the internal storage may grow or shrink. Generally, you won't need to care about the storage size of your set and can use `Set.make()` instead.

Parameters:

|param|type|description|
|-----|----|-----------|
|`size`|`Number`|The initial storage size of the set|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|An empty set with the given initial storage size|

### Set.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
make : () -> Set<a>
```

Creates a new, empty set.

Returns:

|type|description|
|----|-----------|
|`Set<a>`|An empty set|

### Set.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
add : (a, Set<a>) -> Void
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
contains : (a, Set<a>) -> Bool
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
remove : (a, Set<a>) -> Void
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
size : Set<a> -> Number
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
isEmpty : Set<a> -> Bool
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
clear : Set<a> -> Void
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
forEach : ((a -> Void), Set<a>) -> Void
```

Iterates the set, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The iterator function to call with each element|
|`set`|`Set<a>`|The set to iterate|

### Set.**reduce**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
reduce : (((a, b) -> a), a, Set<b>) -> a
```

Combines all elements of a set using a reducer function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
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
filter : ((a -> Bool), Set<a>) -> Void
```

Removes elements from a set where a predicate function returns `false`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The predicate function to indicate which elements to remove from the set, where returning `false` indicates the value should be removed|
|`set`|`Set<a>`|The set to iterate|

### Set.**reject**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
reject : ((a -> Bool), Set<a>) -> Void
```

Removes elements from a set where a predicate function returns `true`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The predicate function to indicate which elements to remove from the set, where returning `true` indicates the value should be removed|
|`set`|`Set<a>`|The set to iterate|

### Set.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
toList : Set<a> -> List<a>
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
fromList : List<a> -> Set<a>
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
toArray : Set<a> -> Array<a>
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
fromArray : Array<a> -> Set<a>
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
union : (Set<a>, Set<a>) -> Set<a>
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
diff : (Set<a>, Set<a>) -> Set<a>
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
intersect : (Set<a>, Set<a>) -> Set<a>
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

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
getInternalStats : Set<a> -> (Number, Number)
```

Provides data representing the internal state state of the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to inspect|

Returns:

|type|description|
|----|-----------|
|`(Number, Number)`|The internal state of the set|

