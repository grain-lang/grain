---
title: Set
---

A Set is an unordered collection of unique values. Operations on a Set mutate the internal state, so it never needs to be re-assigned.

```grain
import Set from "set"
```

### Set.**Bucket**

```grain
record Bucket<t> {
  key: t,
  next: Option<Bucket<t>>,
}
```

### Set.**Set**

```grain
record Set<k> {
  size: Number,
  buckets: Array<Option<Bucket<k>>>,
}
```

### Set.**makeSized**

```grain
makeSized : Number -> Set<a>
```

Creates a new, empty set with an initial storage size for the given number of elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`size`|`Number`|The size of the set.|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|An empty set of the entered size.|

### Set.**make**

```grain
make : () -> Set<a>
```

Creates a new, empty set.

Returns:

|type|description|
|----|-----------|
|`Set<a>`|An empty set.|

### Set.**add**

```grain
add : (a, Set<a>) -> Void
```

Adds a new value to the set. If the value already exists, nothing happens.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to add to the set.|
|`set`|`Set<a>`|The set to append too.|

### Set.**contains**

```grain
contains : (a, Set<a>) -> Bool
```

Returns `true` if the set contains the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key to search for.|
|`set`|`Set<a>`|The set to be searched.|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the set contains the given value.|

### Set.**remove**

```grain
remove : (a, Set<a>) -> Void
```

Removes the given value from the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The value to remove from the set.|
|`set`|`Set<a>`|The set to remove from.|

### Set.**size**

```grain
size : Set<a> -> Number
```

Returns the number of values within the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to determine the length from.|

Returns:

|type|description|
|----|-----------|
|`Number`|The length of the set.|

### Set.**isEmpty**

```grain
isEmpty : Set<a> -> Bool
```

Returns a boolean indicating weather or not the set is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to check.|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the set contains no values.|

### Set.**clear**

```grain
clear : Set<a> -> Void
```

Removes all values from the given set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to clear.|

### Set.**forEach**

```grain
forEach : ((a -> b), Set<a>) -> Void
```

Iterates the given function over each value in the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The function to call on each element in the set.|
|`set`|`Set<a>`|The set to iterate.|

### Set.**reduce**

```grain
reduce : (((a, b) -> a), a, Set<b>) -> a
```

Reduces all values within a set to a single value. The reducer function is called with the accumulator and the current value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> a`|The function to call on each element in the set.|
|`init`|`a`|A value to which the previousValue is initialized with.|
|`set`|`Set<a>`|The set to reduce.|

Returns:

|type|description|
|----|-----------|
|`a`|The reduced set.|

### Set.**filter**

```grain
filter : ((a -> Bool), Set<a>) -> Void
```

Keeps all values that the predicate returned `true` for from the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function used to filter the set returning `true` indicates to keep the value.|
|`set`|`Set<a>`|The set to filter.|

### Set.**reject**

```grain
reject : ((a -> Bool), Set<a>) -> Void
```

Removes all values that the predicate returned true for from the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function used to filter the set returning `true` indicates to remove the value.|
|`set`|`Set<a>`|The set to filter.|

### Set.**toList**

```grain
toList : Set<a> -> List<a>
```

Returns a list from the values of a set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to convert.|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The generated List.|

### Set.**fromList**

```grain
fromList : List<a> -> Set<b>
```

Creates a set from a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to convert.|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|The generated set.|

### Set.**toArray**

```grain
toArray : Set<a> -> Array<a>
```

Returns an array from the values of a set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to convert.|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The generated array.|

### Set.**fromArray**

```grain
fromArray : Array<a> -> Set<b>
```

Creates a set from an array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to convert.|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|The generated set.|

### Set.**union**

```grain
union : (Set<a>, Set<b>) -> Set<c>
```

Creates a single set from the union of the given sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to union.|
|`set2`|`Set<a>`|The second set to union.|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|The generated set.|

### Set.**diff**

```grain
diff : (Set<a>, Set<a>) -> Set<b>
```

Creates a set from the difference of the given sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to merge.|
|`set2`|`Set<a>`|The second set to merge.|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|The generated set.|

### Set.**intersect**

```grain
intersect : (Set<a>, Set<a>) -> Set<b>
```

Creates a set from the intersection of the given sets.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set1`|`Set<a>`|The first set to merge.|
|`set2`|`Set<a>`|The second set to merge.|

Returns:

|type|description|
|----|-----------|
|`Set<a>`|The generated set.|

### Set.**getInternalStats**

```grain
getInternalStats : Set<a> -> (Number, Number)
```

Returns a tuple representing the internal state state of the set.

Parameters:

|param|type|description|
|-----|----|-----------|
|`set`|`Set<a>`|The set to operate on.|

Returns:

|type|description|
|----|-----------|
|`(Number, Number)`|The tuple representation of the internal state of the set.|

