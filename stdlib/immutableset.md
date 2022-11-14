---
title: ImmutableSet
---

An ImmutableSet is a collection of unique values. Operations on an ImmutableSet do not mutate the set's internal state.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
import ImmutableSet from "immutableset"
```

## Types

Type declarations included in the ImmutableSet module.

### ImmutableSet.**ImmutableSet**

```grain
type ImmutableSet<a>
```

## Values

Functions and constants for working with ImmutableSets.

### ImmutableSet.**empty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
empty : ImmutableSet<a>
```

An empty set

### ImmutableSet.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
size : ImmutableSet<a> -> Number
```

Provides the count of values within the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`ImmutableSet<a>`|The set to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The count of elements in the set|

### ImmutableSet.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
isEmpty : ImmutableSet<a> -> Bool
```

Determines if the set contains no elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`ImmutableSet<a>`|The set to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the given set is empty or `false` otherwise|

### ImmutableSet.**add**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
add : (a, ImmutableSet<a>) -> ImmutableSet<a>
```

Produces a new set by inserting the given value into the set. If the value
already exists, the new set will have the same elements as the input set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to add|
|`set`|`ImmutableSet<a>`|The base set|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A new set containing the new element|

### ImmutableSet.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
contains : (a, ImmutableSet<a>) -> Bool
```

Determines if the set contains the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to search for|
|`set`|`ImmutableSet<a>`|The set to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the set contains the given value or `false` otherwise|

### ImmutableSet.**remove**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
remove : (a, ImmutableSet<a>) -> ImmutableSet<a>
```

Produces a new set without the given element. If the value doesn't exist in
the set, the set will be returned unmodified.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to exclude|
|`set`|`ImmutableSet<a>`|The set to exclude from|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A new set without the excluded element|

### ImmutableSet.**forEach**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
forEach : ((a -> Void), ImmutableSet<a>) -> Void
```

Iterates the set, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The iterator function to call with each element|
|`set`|`ImmutableSet<a>`|The set to iterate|

### ImmutableSet.**reduce**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
reduce : (((a, b) -> a), a, ImmutableSet<b>) -> a
```

Combines all elements of a set using a reducer function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`init`|`a`|The initial value to use for the accumulator on the first iteration|
|`set`|`ImmutableSet<b>`|The set to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

### ImmutableSet.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
filter : ((a -> Bool), ImmutableSet<a>) -> ImmutableSet<a>
```

Produces a new set without the elements from the input set where a predicate function returns `false`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The predicate function to indicate which elements to exclude from the set, where returning `false` indicates the value should be excluded|
|`set`|`ImmutableSet<a>`|The set to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A new set excluding the elements not fulfilling the predicate|

### ImmutableSet.**reject**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
reject : ((a -> Bool), ImmutableSet<a>) -> ImmutableSet<a>
```

Produces a new set without the elements from the input set where a predicate function returns `true`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The predicate function to indicate which elements to exclude from the set, where returning `true` indicates the value should be excluded|
|`set`|`ImmutableSet<a>`|The set to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A new set excluding the elements fulfilling the predicate|

### ImmutableSet.**union**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
union : (ImmutableSet<a>, ImmutableSet<a>) -> ImmutableSet<a>
```

Combines two sets into a single set containing all elements from both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`ImmutableSet<a>`|The first set to combine|
|`set2`|`ImmutableSet<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A set containing all elements of both sets|

### ImmutableSet.**diff**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
diff : (ImmutableSet<a>, ImmutableSet<a>) -> ImmutableSet<a>
```

Combines two sets into a single set containing only the elements not shared between both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`ImmutableSet<a>`|The first set to combine|
|`set2`|`ImmutableSet<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A set containing only unshared elements from both sets|

### ImmutableSet.**intersect**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
intersect : (ImmutableSet<a>, ImmutableSet<a>) -> ImmutableSet<a>
```

Combines two sets into a single set containing only the elements shared between both sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`ImmutableSet<a>`|The first set to combine|
|`set2`|`ImmutableSet<a>`|The second set to combine|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A set containing only shared elements from both sets|

### ImmutableSet.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
fromList : List<a> -> ImmutableSet<a>
```

Creates a set from a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to convert|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A set containing all list values|

### ImmutableSet.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
toList : ImmutableSet<a> -> List<a>
```

Converts a set into a list of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`ImmutableSet<a>`|The set to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all set values|

### ImmutableSet.**fromArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
fromArray : Array<a> -> ImmutableSet<a>
```

Creates a set from an array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to convert|

Returns:

|type|description|
|----|-----------|
|`ImmutableSet<a>`|A set containing all array values|

### ImmutableSet.**toArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
toArray : ImmutableSet<a> -> Array<a>
```

Converts a set into an array of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`ImmutableSet<a>`|The set to convert|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|An array containing all set values|

