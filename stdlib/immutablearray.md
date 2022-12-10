---
title: ImmutableArray
---

An immutable array implementation, providing fast arbitrary lookups and modifications.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
import ImmutableArray from "immutablearray"
```

## Types

Type declarations included in the ImmutableArray module.

### ImmutableArray.**ImmutableArray**

```grain
type ImmutableArray<a>
```

## Values

Functions and constants for working with immutable arrays.

### ImmutableArray.**empty**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
empty : ImmutableArray<a>
```

An empty array.

### ImmutableArray.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
isEmpty : ImmutableArray<a> -> Bool
```

Determines if the array contains no elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`ImmutableArray<a>`|The array to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the array is empty and `false` otherwise|

### ImmutableArray.**length**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
length : ImmutableArray<a> -> Number
```

Provides the length of the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`ImmutableArray<a>`|The array to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of elements in the array|

Examples:

```grain
length(fromList([1, 2, 3, 4, 5])) == 5
```

### ImmutableArray.**get**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
get : (Number, ImmutableArray<a>) -> a
```

Retrieves the element from the array at the specified index.
A negative index is treated as an offset from the end of the array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The index to access|
|`array`|`ImmutableArray<a>`|The array to access|

Returns:

|type|description|
|----|-----------|
|`a`|The element from the array|

Throws:

`IndexOutOfBounds`

* When the index being accessed is outside the array's bounds

Examples:

```grain
get(1, fromList([1, 2, 3, 4])) == 2
```

```grain
get(-1, fromList([1, 2, 3, 4])) == 4
```

### ImmutableArray.**set**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
set : (Number, a, ImmutableArray<a>) -> ImmutableArray<a>
```

Creates a new array in which the element at the specified index is set to a
new value. A negative index is treated as an offset from the end of the array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The index to update|
|`value`|`a`|The value to store|
|`array`|`ImmutableArray<a>`|The array to update|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|A new array containing the new element at the given index|

Throws:

`IndexOutOfBounds`

* When the index being updated is outside the array's bounds

Examples:

```grain
set(1, 9, fromList([1, 2, 3, 4, 5])) == fromList([1, 9, 3, 4, 5])
```

### ImmutableArray.**append**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
append : (ImmutableArray<a>, ImmutableArray<a>) -> ImmutableArray<a>
```

Creates a new array with the elements of the first array followed by
the elements of the second array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array1`|`ImmutableArray<a>`|The array containing elements to appear first|
|`array2`|`ImmutableArray<a>`|The array containing elements to appear second|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array containing elements from `array1` followed by elements from `array2`|

Examples:

```grain
append(fromList([1, 2]), fromList([3, 4, 5])) == fromList([1, 2, 3, 4, 5])
```

### ImmutableArray.**concat**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
concat : List<ImmutableArray<a>> -> ImmutableArray<a>
```

Creates a single array containing the elements of all arrays in the
provided list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`arrays`|`List<ImmutableArray<a>>`|A list containing all arrays to combine|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array|

Examples:

```grain
concat([fromList([1, 2]), fromList([3, 4]), fromList([5, 6])]) == fromList([1, 2, 3, 4, 5, 6])
```

### ImmutableArray.**init**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
init : (Number, (Number -> a)) -> ImmutableArray<a>
```

Creates a new array of the specified length where each element is
initialized with the result of an initializer function. The initializer
is called with the index of each array element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`length`|`Number`|The length of the new array|
|`fn`|`Number -> a`|The initializer function to call with each index, where the value returned will be used to initialize the element|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array|

Examples:

```grain
init(5, i => i + 3) == fromList([3, 4, 5, 6, 7])
```

### ImmutableArray.**make**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
make : (Number, a) -> ImmutableArray<a>
```

Creates a new array of the specified length with each element being
initialized with the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`length`|`Number`|The length of the new array|
|`value`|`a`|The value to store at each index|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array|

Examples:

```grain
make(5, "foo") == fromList(["foo", "foo", "foo", "foo", "foo"])
```

### ImmutableArray.**forEach**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
forEach : ((a -> Void), ImmutableArray<a>) -> Void
```

Iterates an array, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The iterator function to call with each element|
|`array`|`ImmutableArray<a>`|The array to iterate|

### ImmutableArray.**cycle**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
cycle : ((a -> Void), Number, ImmutableArray<a>) -> Void
```

Iterates an array a given number of times, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The iterator function to call with each element|
|`n`|`Number`|The number of times to iterate the given array|
|`array`|`ImmutableArray<a>`|The array to iterate|

### ImmutableArray.**map**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
map : ((a -> b), ImmutableArray<a>) -> ImmutableArray<b>
```

Produces a new array initialized with the results of a mapper function
called on each element of the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The mapper function to call on each element, where the value returned will be used to initialize the element in the new array|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<b>`|The new array with mapped values|

### ImmutableArray.**reduce**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
reduce : (((a, b) -> a), a, ImmutableArray<b>) -> a
```

Combines all elements of an array using a reducer function,
starting from the "head", or left side, of the array.

In `ImmutableArray.reduce(fn, initial, array)`, `fn` is called with
an accumulator and each element of the array, and returns
a new accumulator. The final value is the last accumulator
returned. The accumulator starts with value `initial`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`a`|The initial value to use for the accumulator on the first iteration|
|`array`|`ImmutableArray<b>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

Examples:

```grain
reduce((acc, x) => acc + x, 0, fromList([1, 2, 3])) == 6
```

### ImmutableArray.**reduceRight**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
reduceRight : (((a, b) -> b), b, ImmutableArray<a>) -> b
```

Combines all elements of an array using a reducer function,
starting from the "end", or right side, of the array.

In `ImmutableArray.reduceRight(fn, initial, array)`, `fn` is called with
each element of the array and an accumulator, and returns
a new accumulator. The final value is the last accumulator
returned. The accumulator starts with value `initial`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> b`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`b`|The initial value to use for the accumulator on the first iteration|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`b`|The final accumulator returned from `fn`|

Examples:

```grain
reduceRight((x, acc) => acc ++ x, "", fromList(["baz", "bar", "foo"])) == "foobarbaz"
```

### ImmutableArray.**flatMap**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
flatMap : ((a -> ImmutableArray<b>), ImmutableArray<a>) -> ImmutableArray<b>
```

Produces a new array by calling a function on each element
of the input array. Each iteration produces an intermediate
array, which are all appended to produce a "flattened" array
of all results.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> ImmutableArray<b>`|The function to be called on each element, where the value returned will be an array that gets appended to the new array|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<b>`|The new array|

Examples:

```grain
flatMap(n => fromList([n, n + 1]), fromList([1, 3, 5])) == fromList([1, 2, 3, 4, 5, 6])
```

### ImmutableArray.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromList : List<a> -> ImmutableArray<a>
```

Converts the input list to an array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to convert|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The array containing all elements from the list|

### ImmutableArray.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toList : ImmutableArray<a> -> List<a>
```

Converts the input array to a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`ImmutableArray<a>`|The array to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The list containing all elements from the array|

### ImmutableArray.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
filter : ((a -> Bool), ImmutableArray<a>) -> ImmutableArray<a>
```

Produces a new array by calling a function on each element of
the input array and only including it in the result array if the element satisfies
the condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array containing elements where `fn` returned `true`|

### ImmutableArray.**every**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
every : ((a -> Bool), ImmutableArray<a>) -> Bool
```

Checks that the given condition is satisfied for all
elements in the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if all elements satify the condition or `false` otherwise|

### ImmutableArray.**some**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
some : ((a -> Bool), ImmutableArray<a>) -> Bool
```

Checks that the given condition is satisfied **at least
once** by an element in the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if one or more elements satify the condition or `false` otherwise|

### ImmutableArray.**reverse**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
reverse : ImmutableArray<a> -> ImmutableArray<a>
```

Creates a new array with all elements in reverse order.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`ImmutableArray<a>`|The array to reverse|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array|

### ImmutableArray.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
contains : (a, ImmutableArray<a>) -> Bool
```

Checks if the value is an element of the input array.
Uses the generic `==` structural equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`search`|`a`|The value to compare|
|`array`|`ImmutableArray<a>`|The array to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value exists in the array or `false` otherwise|

### ImmutableArray.**find**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
find : ((a -> Bool), ImmutableArray<a>) -> Option<a>
```

Finds the first element in an array that satifies the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to search|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(element)` containing the first value found or `None` otherwise|

### ImmutableArray.**findIndex**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
findIndex : ((a -> Bool), ImmutableArray<a>) -> Option<Number>
```

Finds the first index in an array where the element satifies the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to search|

Returns:

|type|description|
|----|-----------|
|`Option<Number>`|`Some(index)` containing the index of the first element found or `None` otherwise|

### ImmutableArray.**product**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
product : (ImmutableArray<a>, ImmutableArray<b>) -> ImmutableArray<(a, b)>
```

Combines two arrays into a Cartesian product of tuples containing
all ordered pairs `(a, b)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array1`|`ImmutableArray<a>`|The array to provide values for the first tuple element|
|`array2`|`ImmutableArray<b>`|The array to provide values for the second tuple element|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<(a, b)>`|The new array containing all pairs of `(a, b)`|

### ImmutableArray.**count**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
count : ((a -> Bool), ImmutableArray<a>) -> Number
```

Counts the number of elements in an array that satisfy the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Number`|The total number of elements that satisfy the condition|

### ImmutableArray.**unique**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
unique : ImmutableArray<a> -> ImmutableArray<a>
```

Produces a new array with any duplicates removed.
Uses the generic `==` structural equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`ImmutableArray<a>`|The array to filter|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array with only unique values|

### ImmutableArray.**zip**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
zip : (ImmutableArray<a>, ImmutableArray<b>) -> ImmutableArray<(a, b)>
```

Produces a new array filled with tuples of elements from both given arrays.
The first tuple will contain the first item of each array, the second tuple
will contain the second item of each array, and so on.

Calling this function with arrays of different sizes will cause the returned
array to have the length of the smaller array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array1`|`ImmutableArray<a>`|The array to provide values for the first tuple element|
|`array2`|`ImmutableArray<b>`|The array to provide values for the second tuple element|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<(a, b)>`|The new array containing indexed pairs of `(a, b)`|

### ImmutableArray.**zipWith**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
zipWith :
  (((a, b) -> c), ImmutableArray<a>, ImmutableArray<b>) -> ImmutableArray<c>
```

Produces a new array filled with elements defined by applying a function on
pairs from both given arrays. The first element will contain the result of
applying the function to the first elements of each array, the second element
will contain the result of applying the function to the second elements of
each array, and so on.

Calling this function with arrays of different sizes will cause the returned
array to have the length of the smaller array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> c`|The function to apply to pairs of elements|
|`array1`|`ImmutableArray<a>`|The array whose elements will each be passed to the function as the first argument|
|`array2`|`ImmutableArray<b>`|The array whose elements will each be passed to the function as the second argument|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<c>`|The new array containing elements derived from applying the function to pairs of input array elements|

Examples:

```grain
zipWith((a, b) => a + b, fromList([1, 2, 3]), fromList([4, 5, 6])) == fromList([5, 7, 9])
```

```grain
zipWith((a, b) => a * b, fromList([1, 2, 3]), fromList([4, 5])) == fromList([4, 10])
```

### ImmutableArray.**unzip**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
unzip : ImmutableArray<(a, b)> -> (ImmutableArray<a>, ImmutableArray<b>)
```

Produces two arrays by splitting apart an array of tuples.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`ImmutableArray<(a, b)>`|The array of tuples to split|

Returns:

|type|description|
|----|-----------|
|`(ImmutableArray<a>, ImmutableArray<b>)`|An array containing all elements from the first tuple element and an array containing all elements from the second tuple element|

### ImmutableArray.**join**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
join : (String, ImmutableArray<String>) -> String
```

Concatenates an array of strings into a single string, separated by a separator string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`separator`|`String`|The separator to insert between items in the string|
|`array`|`ImmutableArray<String>`|The input strings|

Returns:

|type|description|
|----|-----------|
|`String`|The concatenated string|

### ImmutableArray.**slice**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
slice : (Number, Number, ImmutableArray<a>) -> ImmutableArray<a>
```

Slices an array given zero-based start and end indexes. The value
at the end index will not be included in the result.

If either index is a negative number, it will be treated as a reverse index from
the end of the array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The index of the array where the slice will begin (inclusive)|
|`end`|`Number`|The index of the array where the slice will end (exclusive)|
|`array`|`ImmutableArray<a>`|The array to be sliced|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The subset of the array that was sliced|

Examples:

```grain
slice(0, 2, fromList(['a', 'b', 'c'])) == fromList(['a', 'b'])
```

```grain
slice(1, -1, fromList(['a', 'b', 'c'])) == fromList(['b'])
```

### ImmutableArray.**sort**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
sort : (((a, a) -> Number), ImmutableArray<a>) -> ImmutableArray<a>
```

Sorts the given array based on a given comparator function.

Ordering is calculated using a comparator function which takes two array elements and must return 0 if both are equal, a positive number if the first is greater, and a negative number if the first is smaller.

Parameters:

|param|type|description|
|-----|----|-----------|
|`comp`|`(a, a) -> Number`|The comparator function used to indicate sort order|
|`array`|`ImmutableArray<a>`|The array to be sorted|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The sorted array|

### ImmutableArray.**rotate**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
rotate : (Number, ImmutableArray<a>) -> ImmutableArray<a>
```

Rotates array elements by the specified amount to the left.

If value is negative, array elements will be rotated by the
specified amount to the right. See examples.

Parameters:

|param|type|description|
|-----|----|-----------|
|`n`|`Number`|The number of elements to rotate by|
|`array`|`ImmutableArray<a>`|The array to be rotated|

Examples:

```grain
rotate(2, fromList([1, 2, 3, 4, 5])) == fromList([3, 4, 5, 1, 2])
```

```grain
rotate(-1, fromList([1, 2, 3, 4, 5])) == fromList([5, 1, 2, 3, 4])
```

