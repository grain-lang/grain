---
title: Array
---

Utilities for working with arrays.

An immutable array implementation is available in the `Immutable` submodule.

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally named `arrays`</td></tr>
<tr><td><code>0.2.0</code></td><td>Renamed to `array`</td></tr>
</tbody>
</table>
</details>

```grain
from "array" include Array
```

```grain
[> 1, 2, 3]
```

## Values

Functions and constants included in the Array module.

### Array.**length**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
length : (array: Array<a>) => Number
```

Provides the length of the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of elements in the array|

Examples:

```grain
Array.length([> 1, 2, 3, 4, 5]) == 5
```

### Array.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
make : (length: Number, item: a) => Array<a>
```

Creates a new array of the specified length with each element being
initialized with the given value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`length`|`Number`|The length of the new array|
|`item`|`a`|The value to store at each index|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array|

Throws:

`InvalidArgument(String)`

* When `length` is not an integer
* When `length` is negative

Examples:

```grain
Array.make(5, "foo") == [> "foo", "foo", "foo", "foo", "foo"]
```

### Array.**init**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
init : (length: Number, fn: (Number => a)) => Array<a>
```

Creates a new array of the specified length where each element is
initialized with the result of an initializer function. The initializer
is called with the index of each array element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`length`|`Number`|The length of the new array|
|`fn`|`Number => a`|The initializer function to call with each index, where the value returned will be used to initialize the element|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array|

Throws:

`InvalidArgument(String)`

* When `length` is not an integer
* When `length` is negative

Examples:

```grain
Array.init(5, n => n + 3) == [> 3, 4, 5, 6, 7]
```

### Array.**get**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Argument order changed to data-last</td></tr>
</tbody>
</table>
</details>

```grain
get : (index: Number, array: Array<a>) => a
```

An alias for normal syntactic array access, i.e. `array[n]`.

Retrieves the element from the array at the specified index.
A negative index is treated as an offset from the end of the array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The index to access|
|`array`|`Array<a>`|The array to access|

Returns:

|type|description|
|----|-----------|
|`a`|The element from the array|

Throws:

`IndexOutOfBounds`

* When `index` is not an integer
* When `index` is out of bounds

Examples:

```grain
Array.get(1, [> 1, 2, 3, 4, 5]) == 2
```

### Array.**set**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Argument order changed to data-last</td></tr>
</tbody>
</table>
</details>

```grain
set : (index: Number, value: a, array: Array<a>) => Void
```

An alias for normal syntactic array set, i.e. `array[n] = value`.

Sets the element at the specified index in the array to the new value.
A negative index is treated as an offset from the end of the array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The index to update|
|`value`|`a`|The value to store|
|`array`|`Array<a>`|The array to update|

Throws:

`IndexOutOfBounds`

* When `index` is not an integer
* When `index` is out of bounds

Examples:

```grain
let array = [> 1, 2, 3, 4, 5]
Array.set(1, 9, array)
assert array == [> 1, 9, 3, 4, 5]
```

### Array.**append**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
append : (array1: Array<a>, array2: Array<a>) => Array<a>
```

Creates a new array with the elements of the first array followed by
the elements of the second array. This does not modify the arguments.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array1`|`Array<a>`|The array containing elements to appear first|
|`array2`|`Array<a>`|The array containing elements to appear second|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array containing elements from `array1` followed by elements from `array2`|

Throws:

`InvalidArgument(String)`

* When the combined length of the two arrays is not an integer

Examples:

```grain
Array.append([> 1, 2], [> 3, 4, 5]) == [> 1, 2, 3, 4, 5]
```

### Array.**concat**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
concat : (arrays: List<Array<a>>) => Array<a>
```

Creates a single array containing the elements of all arrays in the
provided list. Does not modify any of the input arguments.

Parameters:

|param|type|description|
|-----|----|-----------|
|`arrays`|`List<Array<a>>`|A list containing all arrays to combine|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array|

Throws:

`InvalidArgument(String)`

* When the combined length of all arrays is not an integer

Examples:

```grain
Array.concat([[> 1, 2], [> 3, 4], [> 5, 6]]) == [> 1, 2, 3, 4, 5, 6]
```

### Array.**copy**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
copy : (array: Array<a>) => Array<a>
```

Produces a shallow copy of the input array. The new array contains the
same elements as the original.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to copy|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array containing the elements from the input|

Examples:

```grain
Array.copy([> 1, 2, 3]) == [> 1, 2, 3]
```

### Array.**cycle**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.4</code></summary>
No other changes yet.
</details>

```grain
cycle : (fn: (a => Void), n: Number, array: Array<a>) => Void
```

Iterates an array a given number of times, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Void`|The iterator function to call with each element|
|`n`|`Number`|The number of times to iterate the given array|
|`array`|`Array<a>`|The array to iterate|

Examples:

```grain
let mut str = ""
Array.cycle(s => str = str ++ s, 2, [> "a", "b", "c"])
assert str == "abcabc"
```

### Array.**forEach**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Argument order changed to data-last</td></tr>
</tbody>
</table>
</details>

```grain
forEach : (fn: (a => Void), array: Array<a>) => Void
```

Iterates an array, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Void`|The iterator function to call with each element|
|`array`|`Array<a>`|The array to iterate|

Examples:

```grain
let mut str = ""
Array.forEach(s => str = str ++ s, [> "a", "b", "c"])
assert str == "abc"
```

### Array.**forEachi**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Argument order changed to data-last</td></tr>
</tbody>
</table>
</details>

```grain
forEachi : (fn: ((a, Number) => Void), array: Array<a>) => Void
```

Iterates an array, calling an iterator function on each element.
Also passes the index as the second argument to the function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, Number) => Void`|The iterator function to call with each element|
|`array`|`Array<a>`|The array to iterate|

Examples:

```grain
let mut str = ""
Array.forEachi((s, i) => str = str ++ s ++ toString(i), [> "a", "b", "c"])
assert str == "a0b1c2"
```

### Array.**map**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Argument order changed to data-last</td></tr>
</tbody>
</table>
</details>

```grain
map : (fn: (a => b), array: Array<a>) => Array<b>
```

Produces a new array initialized with the results of a mapper function
called on each element of the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => b`|The mapper function to call on each element, where the value returned will be used to initialize the element in the new array|
|`array`|`Array<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Array<b>`|The new array with mapped values|

Examples:

```grain
Array.map(x => x * 2, [> 1, 2, 3]) == [> 2, 4, 6]
```

### Array.**mapi**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
mapi : (fn: ((a, Number) => b), array: Array<a>) => Array<b>
```

Produces a new array initialized with the results of a mapper function
called on each element of the input array and its index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, Number) => b`|The mapper function to call on each element, where the value returned will be used to initialize the element in the new array|
|`array`|`Array<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Array<b>`|The new array with mapped values|

Examples:

```grain
Array.mapi((x, i) => (x * 2, i), [> 1, 2, 3]) == [> (2, 0), (4, 1), (6, 2)]
```

### Array.**reduce**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
reduce : (fn: ((a, b) => a), initial: a, array: Array<b>) => a
```

Combines all elements of an array using a reducer function,
starting from the "head", or left side, of the array.

In `Array.reduce(fn, initial, array)`, `fn` is called with
an accumulator and each element of the array, and returns
a new accumulator. The final value is the last accumulator
returned. The accumulator starts with value `initial`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) => a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`a`|The initial value to use for the accumulator on the first iteration|
|`array`|`Array<b>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

Examples:

```grain
Array.reduce((acc, el) => acc + el, 0, [> 1, 2, 3]) == 6
```

```grain
Array.reduce((acc, el) => acc ++ el, "", [> "baz", "bar", "foo"]) == "bazbarfoo"
```

### Array.**reduceRight**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
reduceRight : (fn: ((a, b) => b), initial: b, array: Array<a>) => b
```

Combines all elements of an array using a reducer function,
starting from the "end", or right side, of the array.

In `Array.reduceRight(fn, initial, array)`, `fn` is called with
each element of the array and an accumulator, and returns
a new accumulator. The final value is the last accumulator
returned. The accumulator starts with value `initial`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) => b`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`b`|The initial value to use for the accumulator on the first iteration|
|`array`|`Array<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`b`|The final accumulator returned from `fn`|

Examples:

```grain
Array.reduceRight((el, acc) => acc ++ el, "", [> "baz", "bar", "foo"]) == "foobarbaz"
```

### Array.**reducei**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
reducei : (fn: ((a, b, Number) => a), initial: a, array: Array<b>) => a
```

Combines all elements of an array using a reducer function,
starting from the "head", or left side, of the array.

In `Array.reducei(fn, initial, array)`, `fn` is called with
an accumulator, each element of the array, and the index
of that element, and returns a new accumulator. The final
value is the last accumulator returned. The accumulator
starts with value `initial`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b, Number) => a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`a`|The initial value to use for the accumulator on the first iteration|
|`array`|`Array<b>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

Examples:

```grain
Array.reducei((acc, el, index) => acc + el + index, 0, [> 1, 2, 3]) == 9
```

```grain
let output = Array.reducei((acc, el, index) => {
  acc ++ el ++ toString(index)
}, "", [> "baz", "bar", "foo"])
assert output == "baz0bar1foo2"
```

### Array.**flatMap**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
flatMap : (fn: (b => Array<a>), array: Array<b>) => Array<a>
```

Produces a new array by calling a function on each element
of the input array. Each iteration produces an intermediate
array, which are all appended to produce a "flattened" array
of all results.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`b => Array<a>`|The function to be called on each element, where the value returned will be an array that gets appended to the new array|
|`array`|`Array<b>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array|

Throws:

`InvalidArgument(String)`

* When the combined length of all arrays is not an integer

Examples:

```grain
Array.flatMap(e => [> 1, e], [> 1, 2, 3]) == [> 1, 1, 1, 2, 1, 3]
```

### Array.**every**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
every : (fn: (a => Bool), array: Array<a>) => Bool
```

Checks that the given condition is satisfied for all
elements in the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`Array<a>`|The array to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if all elements satisfy the condition or `false` otherwise|

Examples:

```grain
Array.every(e => e % 2 == 0, [> 2, 4, 6]) == true
```

```grain
Array.every(e => e % 2 == 0, [> 2, 4, 7]) == false
```

### Array.**some**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
some : (fn: (a => Bool), array: Array<a>) => Bool
```

Checks that the given condition is satisfied **at least
once** by an element in the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`Array<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if one or more elements satisfy the condition or `false` otherwise|

Examples:

```grain
Array.some(e => e % 2 == 0, [> 2, 4, 6]) == true
```

```grain
Array.some(e => e % 2 == 0, [> 2, 4, 7]) == true
```

```grain
Array.some(e => e % 2 == 0, [> 3, 5, 7]) == false
```

### Array.**fill**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fill : (value: a, array: Array<a>) => Void
```

Replaces all elements in an array with the new value provided.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value replacing each element|
|`array`|`Array<a>`|The array to update|

Examples:

```grain
let arr = [> 2, 4, 6]
Array.fill(0, arr)
assert arr == [> 0, 0, 0]
```

### Array.**fillRange**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fillRange : (value: a, start: Number, stop: Number, array: Array<a>) => Void
```

Replaces all elements in the provided index range in the array
with the new value provided. Fails if the index is out-of-bounds.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value replacing each element between the indexes|
|`start`|`Number`|The index to begin replacement|
|`stop`|`Number`|The (exclusive) index to end replacement|
|`array`|`Array<a>`|The array to update|

Throws:

`IndexOutOfBounds`

* When the start index is out of bounds
* When the start index is greater then the stop index

Examples:

```grain
let arr = [> 2, 4, 6, 8]
Array.fillRange(0, 1, 3, arr)
assert arr == [> 2, 0, 0, 8]
```

### Array.**reverse**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
reverse : (array: Array<a>) => Array<a>
```

Creates a new array with all elements in reverse order.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to reverse|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array|

Examples:

```grain
Array.reverse([> 1, 2, 3]) == [> 3, 2, 1]
```

### Array.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
toList : (array: Array<a>) => List<a>
```

Converts the input array to a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The list containing all elements from the array|

Examples:

```grain
Array.toList([> 1, 2, 3]) == [1, 2, 3]
```

### Array.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
fromList : (list: List<a>) => Array<a>
```

Converts the input list to an array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to convert|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The array containing all elements from the list|

Examples:

```grain
Array.fromList([1, 2, 3]) == [> 1, 2, 3]
```

### Array.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
contains : (search: a, array: Array<a>) => Bool
```

Checks if the value is an element of the input array.
Uses the generic `==` structural equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`search`|`a`|The value to compare|
|`array`|`Array<a>`|The array to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value exists in the array or `false` otherwise|

Examples:

```grain
Array.contains(1, [> 1, 2, 3]) == true
```

```grain
Array.contains(0, [> 1, 2, 3]) == false
```

### Array.**find**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
find : (fn: (a => Bool), array: Array<a>) => Option<a>
```

Finds the first element in an array that satisfies the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`Array<a>`|The array to search|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(element)` containing the first value found or `None` otherwise|

Examples:

```grain
Array.find(e => e % 2 == 0, [> 1, 4, 3]) == Some(4)
```

```grain
Array.find(e => e % 2 == 0, [> 1, 2, 3, 4]) == Some(2)
```

```grain
Array.find(e => e % 2 == 0, [> 1, 3, 5]) == None
```

### Array.**findIndex**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
findIndex : (fn: (a => Bool), array: Array<a>) => Option<Number>
```

Finds the first index in an array where the element satisfies the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`Array<a>`|The array to search|

Returns:

|type|description|
|----|-----------|
|`Option<Number>`|`Some(index)` containing the index of the first element found or `None` otherwise|

Examples:

```grain
Array.findIndex(e => e % 2 == 0, [> 1, 4, 3]) == Some(1)
```

```grain
Array.findIndex(e => e % 2 == 0, [> 1, 2, 3, 4]) == Some(1)
```

```grain
Array.findIndex(e => e % 2 == 0, [> 1, 3, 5]) == None
```

### Array.**product**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
product : (array1: Array<a>, array2: Array<b>) => Array<(a, b)>
```

Combines two arrays into a Cartesian product of tuples containing
all ordered pairs `(a, b)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array1`|`Array<a>`|The array to provide values for the first tuple element|
|`array2`|`Array<b>`|The array to provide values for the second tuple element|

Returns:

|type|description|
|----|-----------|
|`Array<(a, b)>`|The new array containing all pairs of `(a, b)`|

Throws:

`InvalidArgument(String)`

* When the multiplied array lengths are not an integer

Examples:

```grain
Array.product([> 1, 2], [> 3, 4]) == [> (1, 3), (1, 4), (2, 3), (2, 4)]
```

### Array.**count**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
count : (fn: (a => Bool), array: Array<a>) => Number
```

Counts the number of elements in an array that satisfy the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`Array<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Number`|The total number of elements that satisfy the condition|

Examples:

```grain
Array.count(e => e % 2 == 0, [> 1, 2, 3, 4]) == 2
```

### Array.**counti**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
counti : (fn: ((a, Number) => Bool), array: Array<a>) => Number
```

Counts the number of elements in an array that satisfy the
given condition. Also passes the index to the function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, Number) => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`Array<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Number`|The total number of elements that satisfy the condition|

Examples:

```grain
Array.counti((e, i) => e % 2 == 0 && i % 2 == 0, [> 1, 2, 3, 4, 5]) == 0
```

```grain
Array.counti((e, i) => e % 2 == 0 && i % 2 == 0, [> 0, 1, 2, 3, 5]) == 2
```

### Array.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
filter : (fn: (a => Bool), array: Array<a>) => Array<a>
```

Produces a new array by calling a function on each element of
the input array and only including it in the result array if the element satisfies
the condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`Array<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array containing elements where `fn` returned `true`|

Examples:

```grain
Array.filter(e => e % 2 == 0, [> 1, 2, 3, 4]) == [> 2, 4]
```

### Array.**filteri**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
filteri : (fn: ((a, Number) => Bool), array: Array<a>) => Array<a>
```

Produces a new array by calling a function on each element of
the input array and only including it in the result array if the element satisfies
the condition. Also passes the index to the function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, Number) => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`Array<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array containing elements where `fn` returned `true`|

Examples:

```grain
Array.filteri((e, i) => e % 2 == 0, [> 1, 2, 3, 4]) == [> 2, 4]
```

```grain
Array.filteri((e, i) => e % 2 == 1 && i % 2 == 0, [> 1, 2, 3, 4, 5]) == [> 1, 3, 5]
```

### Array.**unique**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
unique : (array: Array<a>) => Array<a>
```

Produces a new array with any duplicates removed.
Uses the generic `==` structural equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|The array to filter|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The new array with only unique values|

Examples:

```grain
Array.unique([> 1, 2, 1, 2, 3, 1]) == [> 1, 2, 3]
```

### Array.**zip**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Support zipping arrays of different sizes</td></tr>
</tbody>
</table>
</details>

```grain
zip : (array1: Array<a>, array2: Array<b>) => Array<(a, b)>
```

Produces a new array filled with tuples of elements from both given arrays.
The first tuple will contain the first item of each array, the second tuple
will contain the second item of each array, and so on.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array1`|`Array<a>`|The array to provide values for the first tuple element|
|`array2`|`Array<b>`|The array to provide values for the second tuple element|

Returns:

|type|description|
|----|-----------|
|`Array<(a, b)>`|The new array containing indexed pairs of `(a, b)`|

Throws:

`IndexOutOfBounds`

* When the arrays have different sizes

Examples:

```grain
Array.zip([> 1, 2, 3], [> 4, 5, 6]) == [> (1, 4), (2, 5), (3, 6)]
```

### Array.**zipWith**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
zipWith : (fn: ((a, b) => c), array1: Array<a>, array2: Array<b>) => Array<c>
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
|`fn`|`(a, b) => c`|The function to apply to pairs of elements|
|`array1`|`Array<a>`|The array whose elements will each be passed to the function as the first argument|
|`array2`|`Array<b>`|The array whose elements will each be passed to the function as the second argument|

Returns:

|type|description|
|----|-----------|
|`Array<c>`|The new array containing elements derived from applying the function to pairs of input array elements|

Throws:

`IndexOutOfBounds`

* When the arrays have different sizes

Examples:

```grain
Array.zipWith((a, b) => a + b, [> 1, 2, 3], [> 4, 5, 6]) == [> 5, 7, 9]
```

```grain
Array.zipWith((a, b) => a * b, [> 1, 2, 3], [> 4, 5]) == [> 4, 10]
```

### Array.**unzip**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
unzip : (array: Array<(a, b)>) => (Array<a>, Array<b>)
```

Produces two arrays by splitting apart an array of tuples.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<(a, b)>`|The array of tuples to split|

Returns:

|type|description|
|----|-----------|
|`(Array<a>, Array<b>)`|An array containing all elements from the first tuple element, and an array containing all elements from the second tuple element|

Examples:

```grain
Array.unzip([> (1, 4), (2, 5), (3, 6)]) == ([> 1, 2, 3], [> 4, 5, 6])
```

### Array.**join**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
join : (separator: String, items: Array<String>) => String
```

Concatenates an array of strings into a single string, separated by a separator string.

Parameters:

|param|type|description|
|-----|----|-----------|
|`separator`|`String`|The separator to insert between items in the string|
|`items`|`Array<String>`|The input strings|

Returns:

|type|description|
|----|-----------|
|`String`|The concatenated string|

Examples:

```grain
Array.join(", ", [> "a", "b", "c"]) == "a, b, c"
```

### Array.**slice**

<details>
<summary>Added in <code>0.4.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Default `end` to the Array length</td></tr>
</tbody>
</table>
</details>

```grain
slice : (start: Number, ?end: Number, array: Array<a>) => Array<a>
```

Slices an array given zero-based start and end indexes. The value
at the end index will not be included in the result.

If either index is a negative number, it will be treated as a reverse index from
the end of the array. e.g. `slice(1, -1, [> 'a', 'b', 'c']) == [> 'b']`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The index of the array where the slice will begin (inclusive)|
|`?end`|`Number`|The index of the array where the slice will end (exclusive)|
|`array`|`Array<a>`|The array to be sliced|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|The subset of the array that was sliced|

Examples:

```grain
Array.slice(1, end=3, [> 1, 2, 3, 4]) == [> 2, 3]
```

```grain
Array.slice(1, [> 1, 2, 3, 4]) == [> 2, 3, 4]
```

### Array.**sort**

<details>
<summary>Added in <code>0.4.5</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Made `compare` a default argument</td></tr>
</tbody>
</table>
</details>

```grain
sort : (?compare: ((num1: a, num2: a) => Number), array: Array<a>) => Void
```

Sorts an array in-place.

Ordering is calculated using a comparator function which takes two array elements and must return 0 if both are equal, a positive number if the first is greater, and a negative number if the first is smaller.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?compare`|`(num1: a, num2: a) => Number`|The comparator function used to indicate sort order|
|`array`|`Array<a>`|The array to be sorted|

Examples:

```grain
let arr = [> 3, 2, 4, 1]
Array.sort(compare=(a, b) => a - b, arr)
assert arr == [> 1, 2, 3, 4]
```

### Array.**rotate**

<details>
<summary>Added in <code>0.4.5</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Behavior changed from right-rotation to left-rotation</td></tr>
</tbody>
</table>
</details>

```grain
rotate : (n: Number, arr: Array<a>) => Void
```

Rotates array elements in place by the specified amount to the left, such
that the `n`th element becomes the first in the array.

If value is negative, array elements will be rotated by the
specified amount to the right. See examples.

Parameters:

|param|type|description|
|-----|----|-----------|
|`n`|`Number`|The number of elements to rotate by|
|`arr`|`Array<a>`|The array to be rotated|

Examples:

```grain
let array = [> 1, 2, 3, 4, 5]
Array.rotate(2, array)
assert array == [> 3, 4, 5, 1, 2]
```

```grain
let array = [> 1, 2, 3, 4, 5]
Array.rotate(-1, array)
assert array == [> 5, 1, 2, 3, 4]
```

### Array.**chunk**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
chunk : (chunkSize: Number, arr: Array<a>) => Array<Array<a>>
```

Splits the given array into chunks of the provided size.
If the array cannot be split evenly, the final chunk will contain the remaining elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`chunkSize`|`Number`|The maximum size of each chunk|
|`arr`|`Array<a>`|The array to chunk|

Returns:

|type|description|
|----|-----------|
|`Array<Array<a>>`|An array of chunks|

Throws:

`InvalidArgument(String)`

* When `chunkSize` is not an integer
* When `chunkSize` is less than one

Examples:

```grain
Array.chunk(2, [> 1, 2, 3, 4, 5]) == [> [> 1, 2], [> 3, 4], [> 5]]
```

```grain
Array.chunk(2, [> 1, 2, 3, 4]) == [> [> 1, 2], [> 3, 4]]
```

## Array.Immutable

An immutable array implementation.

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
use Array.{ module Immutable }
```

```grain
Array.Immutable.empty
```

```grain
Immutable.fromList([1, 2, 3, 4, 5])
```

### Types

Type declarations included in the Array.Immutable module.

#### Array.Immutable.**ImmutableArray**

```grain
type ImmutableArray<a>
```

### Values

Functions and constants included in the Array.Immutable module.

#### Array.Immutable.**empty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
empty : ImmutableArray<a>
```

An empty array.

Examples:

```grain
Array.Immutable.empty == Array.Immutable.fromList([])
```

#### Array.Immutable.**isEmpty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
isEmpty : (array: ImmutableArray<a>) => Bool
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

Examples:

```grain
use Array.{ module Immutable }
assert Immutable.isEmpty(Immutable.fromList([1, 2, 3])) == false
```

```grain
use Array.{ module Immutable }
assert Immutable.isEmpty(Immutable.fromList([])) == true
```

#### Array.Immutable.**length**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
length : (array: ImmutableArray<a>) => Number
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
use Array.{ module Immutable }
assert Immutable.length(Immutable.fromList([1, 2, 3, 4, 5])) == 5
```

#### Array.Immutable.**get**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
get : (index: Number, array: ImmutableArray<a>) => a
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
use Array.{ module Immutable }
assert Immutable.get(1, Immutable.fromList([1, 2, 3, 4])) == 2
```

```grain
use Array.{ module Immutable }
assert Immutable.get(-1, Immutable.fromList([1, 2, 3, 4])) == 4
```

#### Array.Immutable.**set**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
set :
  (index: Number, value: a, array: ImmutableArray<a>) => ImmutableArray<a>
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
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 2, 3, 4, 5])
let array = Immutable.set(1, 9, arr)
assert arr == Immutable.fromList([1, 2, 3, 4, 5])
assert array == Immutable.fromList([1, 9, 3, 4, 5])
```

#### Array.Immutable.**append**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
append :
  (array1: ImmutableArray<a>, array2: ImmutableArray<a>) => ImmutableArray<a>
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
use Array.{ module Immutable }
let arr1 = Immutable.fromList([1, 2])
let arr2 = Immutable.fromList([3, 4, 5])
assert Immutable.append(arr1, arr2) == Immutable.fromList([1, 2, 3, 4, 5])
assert arr1 == Immutable.fromList([1, 2])
assert arr2 == Immutable.fromList([3, 4, 5])
```

#### Array.Immutable.**concat**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
concat : (arrays: List<ImmutableArray<a>>) => ImmutableArray<a>
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
use Array.{ module Immutable }
let arr1 = Immutable.fromList([1, 2])
let arr2 = Immutable.fromList([3, 4])
let arr3 = Immutable.fromList([5, 6])
assert Immutable.concat([arr1, arr2, arr3]) == Immutable.fromList([1, 2, 3, 4, 5, 6])
```

#### Array.Immutable.**init**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
init : (length: Number, fn: (Number => a)) => ImmutableArray<a>
```

Creates a new array of the specified length where each element is
initialized with the result of an initializer function. The initializer
is called with the index of each array element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`length`|`Number`|The length of the new array|
|`fn`|`Number => a`|The initializer function to call with each index, where the value returned will be used to initialize the element|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array|

Examples:

```grain
use Array.{ module Immutable }
assert Immutable.init(5, i => i) == Immutable.fromList([0, 1, 2, 3, 4])
```

```grain
use Array.{ module Immutable }
assert Immutable.init(5, i => i + 3) == Immutable.fromList([3, 4, 5, 6, 7])
```

#### Array.Immutable.**make**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
make : (length: Number, value: a) => ImmutableArray<a>
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
use Array.{ module Immutable }
assert Immutable.make(5, "🌾") == Immutable.fromList(["🌾", "🌾", "🌾", "🌾", "🌾"])
```

#### Array.Immutable.**forEach**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
forEach : (fn: (a => Void), array: ImmutableArray<a>) => Void
```

Iterates an array, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Void`|The iterator function to call with each element|
|`array`|`ImmutableArray<a>`|The array to iterate|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(["foo", "bar", "baz"])
let mut str = ""
Immutable.forEach(e => str = str ++ e, arr)
assert str == "foobarbaz"
```

#### Array.Immutable.**cycle**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
cycle : (fn: (a => Void), n: Number, array: ImmutableArray<a>) => Void
```

Iterates an array a given number of times, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Void`|The iterator function to call with each element|
|`n`|`Number`|The number of times to iterate the given array|
|`array`|`ImmutableArray<a>`|The array to iterate|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(["a", "b", "c"])
let mut str = ""
Immutable.cycle(e => str = str ++ e, 2, arr)
assert str == "abcabc"
```

#### Array.Immutable.**map**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
map : (fn: (a => b), array: ImmutableArray<a>) => ImmutableArray<b>
```

Produces a new array initialized with the results of a mapper function
called on each element of the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => b`|The mapper function to call on each element, where the value returned will be used to initialize the element in the new array|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<b>`|The new array with mapped values|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(["foo", "bar", "baz"])
let arr = Immutable.map(e => e ++ "_", arr)
assert arr == Immutable.fromList(["foo_", "bar_", "baz_"])
```

#### Array.Immutable.**reduce**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
reduce : (fn: ((a, b) => a), initial: a, array: ImmutableArray<b>) => a
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
|`fn`|`(a, b) => a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`a`|The initial value to use for the accumulator on the first iteration|
|`array`|`ImmutableArray<b>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 2, 3])
assert Immutable.reduce((acc, x) => acc + x, 0, arr) == 6
```

#### Array.Immutable.**reduceRight**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
reduceRight : (fn: ((a, b) => b), initial: b, array: ImmutableArray<a>) => b
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
|`fn`|`(a, b) => b`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`b`|The initial value to use for the accumulator on the first iteration|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`b`|The final accumulator returned from `fn`|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(["baz", "bar", "foo"])
assert Immutable.reduceRight((x, acc) => acc ++ x, "", arr) == "foobarbaz"
```

#### Array.Immutable.**flatMap**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
flatMap :
  (fn: (a => ImmutableArray<b>), array: ImmutableArray<a>) =>
   ImmutableArray<b>
```

Produces a new array by calling a function on each element
of the input array. Each iteration produces an intermediate
array, which are all appended to produce a "flattened" array
of all results.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => ImmutableArray<b>`|The function to be called on each element, where the value returned will be an array that gets appended to the new array|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<b>`|The new array|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 3, 5])
let arr = Immutable.flatMap(n => Immutable.fromList([n, n + 1]), arr)
assert arr == Immutable.fromList([1, 2, 3, 4, 5, 6])
```

#### Array.Immutable.**fromList**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
fromList : (list: List<a>) => ImmutableArray<a>
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

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 2, 3])
assert Immutable.get(1, arr) == 2
```

#### Array.Immutable.**toList**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
toList : (array: ImmutableArray<a>) => List<a>
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

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'b', 'c'])
let arr = Immutable.set(0, 'd', arr)
assert Immutable.toList(arr) == ['d', 'b', 'c']
```

#### Array.Immutable.**filter**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
filter : (fn: (a => Bool), array: ImmutableArray<a>) => ImmutableArray<a>
```

Produces a new array by calling a function on each element of
the input array and only including it in the result array if the element satisfies
the condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The new array containing elements where `fn` returned `true`|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'a', 'b', 'c'])
let arr = Immutable.filter(e => e == 'a', arr)
assert Immutable.toList(arr) == ['a', 'a']
```

#### Array.Immutable.**every**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
every : (fn: (a => Bool), array: ImmutableArray<a>) => Bool
```

Checks that the given condition is satisfied for all
elements in the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if all elements satify the condition or `false` otherwise|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'a'])
assert Immutable.every(e => e == 'a', arr) == true
```

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'a', 'b', 'c'])
assert Immutable.every(e => e == 'a', arr) == false
```

#### Array.Immutable.**some**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
some : (fn: (a => Bool), array: ImmutableArray<a>) => Bool
```

Checks that the given condition is satisfied **at least
once** by an element in the input array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if one or more elements satisfy the condition or `false` otherwise|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'a', 'b', 'c'])
assert Immutable.every(e => e == 'a', arr) == false
```

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['b', 'c'])
assert Immutable.some(e => e == 'a', arr) == false
```

#### Array.Immutable.**reverse**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
reverse : (array: ImmutableArray<a>) => ImmutableArray<a>
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

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'b', 'c'])
let arr = Immutable.reverse(arr)
assert Immutable.toList(arr) == ['c', 'b', 'a']
```

#### Array.Immutable.**contains**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
contains : (search: a, array: ImmutableArray<a>) => Bool
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

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'b', 'c'])
assert Immutable.contains('a', arr) == true
```

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['b', 'c'])
assert Immutable.contains('a', arr) == false
```

#### Array.Immutable.**find**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
find : (fn: (a => Bool), array: ImmutableArray<a>) => Option<a>
```

Finds the first element in an array that satisfies the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to search|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(element)` containing the first value found or `None` otherwise|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 2, 3])
assert Immutable.find(e => e == 2, arr) == Some(2)
```

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 3])
assert Immutable.find(e => e == 2, arr) == None
```

#### Array.Immutable.**findIndex**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
findIndex : (fn: (a => Bool), array: ImmutableArray<a>) => Option<Number>
```

Finds the first index in an array where the element satisfies the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to search|

Returns:

|type|description|
|----|-----------|
|`Option<Number>`|`Some(index)` containing the index of the first element found or `None` otherwise|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 2, 3])
assert Immutable.findIndex(e => e == 2, arr) == Some(1)
```

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 3])
assert Immutable.findIndex(e => e == 2, arr) == None
```

#### Array.Immutable.**product**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
product :
  (array1: ImmutableArray<a>, array2: ImmutableArray<b>) =>
   ImmutableArray<(a, b)>
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

Examples:

```grain
use Array.{ module Immutable }
let arr1 = Immutable.fromList([1, 2])
let arr2 = Immutable.fromList([3, 4])
assert Immutable.product(arr1, arr2) == Immutable.fromList([(1, 3), (1, 4), (2, 3), (2, 4)])
```

#### Array.Immutable.**count**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
count : (fn: (a => Bool), array: ImmutableArray<a>) => Number
```

Counts the number of elements in an array that satisfy the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a => Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`array`|`ImmutableArray<a>`|The array to iterate|

Returns:

|type|description|
|----|-----------|
|`Number`|The total number of elements that satisfy the condition|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 1, 2, 3, 4])
assert Immutable.count(e => e == 1, arr) == 2
```

#### Array.Immutable.**unique**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
unique : (array: ImmutableArray<a>) => ImmutableArray<a>
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

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 1, 2, 3, 2, 4])
assert Immutable.unique(arr) == Immutable.fromList([1, 2, 3, 4])
```

#### Array.Immutable.**zip**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
zip :
  (array1: ImmutableArray<a>, array2: ImmutableArray<b>) =>
   ImmutableArray<(a, b)>
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

Examples:

```grain
use Array.{ module Immutable }
let arr1 = Immutable.fromList([1, 2, 3])
let arr2 = Immutable.fromList([4, 5, 6])
assert Immutable.zip(arr1, arr2) == Immutable.fromList([(1, 4), (2, 5), (3, 6)])
```

#### Array.Immutable.**zipWith**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
zipWith :
  (fn: ((a, b) => c), array1: ImmutableArray<a>, array2: ImmutableArray<b>) =>
   ImmutableArray<c>
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
|`fn`|`(a, b) => c`|The function to apply to pairs of elements|
|`array1`|`ImmutableArray<a>`|The array whose elements will each be passed to the function as the first argument|
|`array2`|`ImmutableArray<b>`|The array whose elements will each be passed to the function as the second argument|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<c>`|The new array containing elements derived from applying the function to pairs of input array elements|

Examples:

```grain
use Array.{ module Immutable }
let arr1 = Immutable.fromList([1, 2, 3])
let arr2 = Immutable.fromList([4, 5, 6])
assert Immutable.zipWith((a, b) => a + b, arr1, arr2) == Immutable.fromList([5, 7, 9])
```

```grain
use Array.{ module Immutable }
let arr1 = Immutable.fromList([1, 2, 3])
let arr2 = Immutable.fromList([4, 5, 6])
assert Immutable.zipWith((a, b) => a * b, arr1, arr2) == Immutable.fromList([4, 10, 18])
```

#### Array.Immutable.**unzip**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
unzip :
  (array: ImmutableArray<(a, b)>) => (ImmutableArray<a>, ImmutableArray<b>)
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

Examples:

```grain
use Array.{ module Immutable }
let arr1 = Immutable.fromList([(1, 2), (3, 4), (5, 6)])
let arr2 = Immutable.fromList([1, 3, 5])
let arr3 = Immutable.fromList([2, 4, 6])
assert Immutable.unzip(arr1) == (arr2, arr3)
```

#### Array.Immutable.**join**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
join : (separator: String, array: ImmutableArray<String>) => String
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

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(["a", "b", "c"])
assert Immutable.join(", ", arr) == "a, b, c"
```

#### Array.Immutable.**slice**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
<tr><td><code>0.6.0</code></td><td>Default `end` to the Array length</td></tr>
</tbody>
</table>
</details>

```grain
slice :
  (start: Number, ?end: Number, array: ImmutableArray<a>) =>
   ImmutableArray<a>
```

Slices an array given zero-based start and end indexes. The value
at the end index will not be included in the result.

If either index is a negative number, it will be treated as a reverse index from
the end of the array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The index of the array where the slice will begin (inclusive)|
|`?end`|`Number`|The index of the array where the slice will end (exclusive)|
|`array`|`ImmutableArray<a>`|The array to be sliced|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The subset of the array that was sliced|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'b', 'c'])
assert Immutable.slice(0, end=2, arr) == Immutable.fromList(['a', 'b'])
```

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList(['a', 'b', 'c'])
assert Immutable.slice(1, end=-1, arr) == Immutable.fromList(['b'])
```

#### Array.Immutable.**sort**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module, with `compare` being a required argument</td></tr>
</tbody>
</table>
</details>

```grain
sort :
  (?compare: ((num1: a, num2: a) => Number), array: ImmutableArray<a>) =>
   ImmutableArray<a>
```

Sorts the given array based on a given comparator function.

Ordering is calculated using a comparator function which takes two array elements and must return 0 if both are equal, a positive number if the first is greater, and a negative number if the first is smaller.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?compare`|`(num1: a, num2: a) => Number`|The comparator function used to indicate sort order|
|`array`|`ImmutableArray<a>`|The array to be sorted|

Returns:

|type|description|
|----|-----------|
|`ImmutableArray<a>`|The sorted array|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([2, 3, 1, 4])
assert Immutable.sort(compare=(a, b) => a - b, arr) == Immutable.fromList([1, 2, 3, 4])
```

#### Array.Immutable.**rotate**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablearray"` module</td></tr>
</tbody>
</table>
</details>

```grain
rotate : (n: Number, array: ImmutableArray<a>) => ImmutableArray<a>
```

Rotates array elements by the specified amount to the left, such that the
`n`th element is the first in the new array.

If value is negative, array elements will be rotated by the
specified amount to the right. See examples.

Parameters:

|param|type|description|
|-----|----|-----------|
|`n`|`Number`|The number of elements to rotate by|
|`array`|`ImmutableArray<a>`|The array to be rotated|

Examples:

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 2, 3, 4, 5])
assert Immutable.rotate(2, arr) == Immutable.fromList([3, 4, 5, 1, 2])
```

```grain
use Array.{ module Immutable }
let arr = Immutable.fromList([1, 2, 3, 4, 5])
assert Immutable.rotate(-1, arr) == Immutable.fromList([5, 1, 2, 3, 4])
```

