---
title: List
---

Utilities for working with lists.

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally named `lists`</td></tr>
<tr><td><code>0.2.0</code></td><td>Renamed to `list`</td></tr>
</tbody>
</table>
</details>

```grain
import List from "list"
```

## Values

Functions for working with the List data type.

### List.**init**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
init : (Number, (Number -> a)) -> List<a>
```

Creates a new list of the specified length where each element is
initialized with the result of an initializer function. The initializer
is called with the index of each list element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`length`|`Number`|The length of the new list|
|`fn`|`Number -> a`|The initializer function to call with each index, where the value returned will be used to initialize the element|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list|

Examples:

```grain
List.init(5, n => n + 3) // [3, 4, 5, 6, 7]
```

### List.**length**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Made the function tail-recursive</td></tr>
</tbody>
</table>
</details>

```grain
length : List<a> -> Number
```

Computes the length of the input list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of elements in the list|

### List.**reverse**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
reverse : List<a> -> List<a>
```

Creates a new list with all elements in reverse order.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to reverse|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list|

### List.**append**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
append : (List<a>, List<a>) -> List<a>
```

Creates a new list with the elements of the first list followed by
the elements of the second list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list1`|`List<a>`|The list containing elements to appear first|
|`list2`|`List<a>`|The list containing elements to appear second|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list containing elements from `list1` followed by elements from `list2`|

### List.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
contains : (a, List<a>) -> Bool
```

Checks if the value is an element of the input list.
Uses the generic `==` structural equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`search`|`a`|The value to compare|
|`list`|`List<a>`|The list to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the value exists in the list or `false` otherwise|

### List.**reduce**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally named `foldLeft`</td></tr>
<tr><td><code>0.2.0</code></td><td>Renamed to `reduce`</td></tr>
</tbody>
</table>
</details>

```grain
reduce : (((a, b) -> a), a, List<b>) -> a
```

Combines all elements of a list using a reducer function,
starting from the "head", or left side, of the list.

In `List.reduce(fn, initial, list)`, `fn` is called with
an accumulator and each element of the list, and returns
a new accumulator. The final value is the last accumulator
returned. The accumulator starts with value `initial`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> a`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`a`|The initial value to use for the accumulator on the first iteration|
|`list`|`List<b>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

Examples:

```grain
List.reduce((a, b) => a + b, 0, [1, 2, 3]) // 6
```

### List.**reduceRight**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally named `foldRight`</td></tr>
<tr><td><code>0.2.0</code></td><td>Renamed to `reduceRight`</td></tr>
</tbody>
</table>
</details>

```grain
reduceRight : (((a, b) -> b), b, List<a>) -> b
```

Combines all elements of a list using a reducer function,
starting from the "end", or right side, of the list.

In `List.reduceRight(fn, initial, list)`, `fn` is called with
each element of the list and an accumulator, and returns
a new accumulator. The final value is the last accumulator
returned. The accumulator starts with value `initial`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> b`|The reducer function to call on each element, where the value returned will be the next accumulator value|
|`initial`|`b`|The initial value to use for the accumulator on the first iteration|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`b`|The final accumulator returned from `fn`|

Examples:

```grain
List.reduceRight((a, b) => b ++ a, "", ["baz", "bar", "foo"]) // "foobarbaz"
```

### List.**map**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
map : ((a -> b), List<a>) -> List<b>
```

Produces a new list initialized with the results of a mapper function
called on each element of the input list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> b`|The mapper function to call on each element, where the value returned will be used to initialize the element in the new list|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`List<b>`|The new list with mapped values|

### List.**mapi**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
mapi : (((a, Number) -> b), List<a>) -> List<b>
```

Produces a new list initialized with the results of a mapper function
called on each element of the input list and its index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, Number) -> b`|The mapper function to call on each element, where the value returned will be used to initialize the element in the new list|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`List<b>`|The new list with mapped values|

### List.**flatMap**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
flatMap : ((a -> List<b>), List<a>) -> List<b>
```

Produces a new list by calling a function on each element
of the input list. Each iteration produces an intermediate
list, which are all appended to produce a "flattened" list
of all results.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> List<b>`|The function to be called on each element, where the value returned will be a list that gets appended to the new list|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`List<b>`|The new list|

### List.**every**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
every : ((a -> Bool), List<a>) -> Bool
```

Checks that the given condition is satisfied for all
elements in the input list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The list to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if all elements satify the condition or `false` otherwise|

### List.**some**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
some : ((a -> Bool), List<a>) -> Bool
```

Checks that the given condition is satisfied **at least
once** by an element in the input list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if one or more elements satify the condition or `false` otherwise|

### List.**forEach**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
forEach : ((a -> Void), List<a>) -> Void
```

Iterates a list, calling an iterator function on each element.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Void`|The iterator function to call with each element|
|`list`|`List<a>`|The list to iterate|

### List.**forEachi**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
forEachi : (((a, Number) -> Void), List<a>) -> Void
```

Iterates a list, calling an iterator function on each element.
Also passes the index as the second argument to the function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, Number) -> Void`|The iterator function to call with each element|
|`list`|`List<a>`|The list to iterate|

### List.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
filter : ((a -> Bool), List<a>) -> List<a>
```

Produces a new list by calling a function on each element of
the input list and only including it in the result list if the element satisfies
the condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list containing elements where `fn` returned `true`|

### List.**filteri**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
filteri : (((a, Number) -> Bool), List<a>) -> List<a>
```

Produces a new list by calling a function on each element of
the input list and only including it in the result list if the element satisfies
the condition. Also passes the index to the function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, Number) -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list containing elements where `fn` returned `true`|

### List.**reject**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
reject : ((a -> Bool), List<a>) -> List<a>
```

Produces a new list by calling a function on each element of
the input list and excluding it from the result list if the element satisfies
the condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list containing elements where `fn` returned `false`|

### List.**head**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally named `hd`</td></tr>
<tr><td><code>0.2.0</code></td><td>Renamed to `head`</td></tr>
<tr><td><code>0.3.0</code></td><td>Return type converted to `Option` type</td></tr>
</tbody>
</table>
</details>

```grain
head : List<a> -> Option<a>
```

Provides `Some(element)` containing the first element, or "head", of
the input list or `None` if the list is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to access|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(firstElement)` if the list has elements or `None` otherwise|

### List.**tail**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally named `tl`</td></tr>
<tr><td><code>0.2.0</code></td><td>Renamed to `tail`</td></tr>
<tr><td><code>0.3.0</code></td><td>Return type converted to `Option` type</td></tr>
</tbody>
</table>
</details>

```grain
tail : List<a> -> Option<List<a>>
```

Provides `Some(tail)` containing all list items except the first element, or "tail", of
the input list or `None` if the list is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to access|

Returns:

|type|description|
|----|-----------|
|`Option<List<a>>`|`Some(tail)` if the list has elements or `None` otherwise|

### List.**nth**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally failed for index out-of-bounds or list empty</td></tr>
<tr><td><code>0.3.0</code></td><td>Return type converted to `Option` type</td></tr>
</tbody>
</table>
</details>

```grain
nth : (Number, List<a>) -> Option<a>
```

Provides `Some(element)` containing the element in the list at the specified index
or `None` if the index is out-of-bounds or the list is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`index`|`Number`|The index to access|
|`list`|`List<a>`|The list to access|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(element)` if the list contains an element at the index or `None` otherwise|

### List.**flatten**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
flatten : List<List<a>> -> List<a>
```

Flattens nested lists.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<List<a>>`|The list to flatten|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A new list containing all nested list elements combined|

Examples:

```grain
List.flatten([[1, 2], [3, 4]]) // [1, 2, 3, 4]
```

### List.**insert**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
insert : (a, Number, List<a>) -> List<a>
```

Inserts a new value into a list at the specified index.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value to insert|
|`index`|`Number`|The index to update|
|`list`|`List<a>`|The list to update|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list|

Throws:

`Failure(String)`

* When `index` is negative
* When `index` is more than 0 and greater than the list size

### List.**count**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Made the function tail-recursive</td></tr>
</tbody>
</table>
</details>

```grain
count : ((a -> Bool), List<a>) -> Number
```

Counts the number of elements in a list that satisfy the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The list to iterate|

Returns:

|type|description|
|----|-----------|
|`Number`|The total number of elements that satisfy the condition|

### List.**part**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
part : (Number, List<a>) -> (List<a>, List<a>)
```

Split a list into two, with the first list containing the required number of elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`count`|`Number`|The number of elements required|
|`list`|`List<a>`|The list to split|

Returns:

|type|description|
|----|-----------|
|`(List<a>, List<a>)`|Two lists where the first contains exactly the required amount of elements and the second contains any remaining elements|

Throws:

`Failure(String)`

* When `count` is negative
* When the list doesn't contain at least the required amount of elements

### List.**rotate**

<details disabled>
<summary tabindex="-1">Added in <code>0.1.0</code></summary>
No other changes yet.
</details>

```grain
rotate : (Number, List<a>) -> List<a>
```

Rotates list elements by the specified amount to the left.

If value is negative, list elements will be rotated by the
specified amount to the right. See examples.

Parameters:

|param|type|description|
|-----|----|-----------|
|`count`|`Number`|The number of elements to rotate by|
|`list`|`List<a>`|The list to be rotated|

Throws:

`Failure(String)`

* When the list doesn't contain at least the required amount of elements

Examples:

```grain
List.rotate(2, [1, 2, 3, 4, 5]) // [3, 4, 5, 1, 2]
```

```grain
List.rotate(-1, [1, 2, 3, 4, 5]) // [5, 1, 2, 3, 4]
```

### List.**unique**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.1.0</code></td><td>Originally named `uniq`</td></tr>
<tr><td><code>0.2.0</code></td><td>Renamed to `unique`</td></tr>
</tbody>
</table>
</details>

```grain
unique : List<a> -> List<a>
```

Produces a new list with any duplicates removed.
Uses the generic `==` structural equality operator.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to filter|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list with only unique values|

### List.**zip**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
zip : (List<a>, List<b>) -> List<(a, b)>
```

Produces a new list filled with tuples of elements from both given lists.
The first tuple will contain the first item of each list, the second tuple
will contain the second item of each list, and so on.

Calling this function with lists of different sizes will cause the returned
list to have the length of the smaller list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list1`|`List<a>`|The list to provide values for the first tuple element|
|`list2`|`List<b>`|The list to provide values for the second tuple element|

Returns:

|type|description|
|----|-----------|
|`List<(a, b)>`|The new list containing indexed pairs of `(a, b)`|

Examples:

```grain
List.zip([1, 2, 3], [4, 5, 6]) // [(1, 4), (2, 5), (3, 6)]
```

```grain
List.zip([1, 2, 3], [4, 5]) // [(1, 4), (2, 5)]
```

### List.**zipWith**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
zipWith : (((a, b) -> c), List<a>, List<b>) -> List<c>
```

Produces a new list filled with elements defined by applying a function on
pairs from both given lists. The first element will contain the result of
applying the function to the first elements of each list, the second element
will contain the result of applying the function to the second elements of
each list, and so on.

Calling this function with lists of different sizes will cause the returned
list to have the length of the smaller list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> c`|The function to apply to pairs of elements|
|`list1`|`List<a>`|The list whose elements will each be passed to the function as the first argument|
|`list2`|`List<b>`|The list whose elements will each be passed to the function as the second argument|

Returns:

|type|description|
|----|-----------|
|`List<c>`|The new list containing elements derived from applying the function to pairs of input list elements|

Examples:

```grain
List.zipWith((a, b) => a + b, [1, 2, 3], [4, 5, 6]) // [5, 7, 9]
```

```grain
List.zipWith((a, b) => a * b, [1, 2, 3], [4, 5]) // [4, 10]
```

### List.**unzip**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
unzip : List<(a, b)> -> (List<a>, List<b>)
```

Produces two lists by splitting apart a list of tuples.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<(a, b)>`|The list of tuples to split|

Returns:

|type|description|
|----|-----------|
|`(List<a>, List<b>)`|An list containing all elements from the first tuple element, and a list containing all elements from the second tuple element|

### List.**drop**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
drop : (Number, List<a>) -> List<a>
```

Produces a new list with the specified number of elements removed from
the beginning of the input list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`count`|`Number`|The amount of elements to remove|
|`list`|`List<a>`|The input list|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list without the dropped elements|

Throws:

`Failure(String)`

* When `count` is negative

### List.**dropWhile**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
dropWhile : ((a -> Bool), List<a>) -> List<a>
```

Produces a new list with the elements removed from the beginning
of the input list until they no longer satisfy the given condition.
Stops when the predicate function returns `false`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The input list|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list without the dropped elements|

### List.**take**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
take : (Number, List<a>) -> List<a>
```

Produces a new list with–at most—the specified amount elements from
the beginning of the input list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`count`|`Number`|The amount of elements to keep|
|`list`|`List<a>`|The input list|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list containing the taken elements|

Throws:

`Failure(String)`

* When `count` is negative

### List.**takeWhile**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
takeWhile : ((a -> Bool), List<a>) -> List<a>
```

Produces a new list with elements from the beginning of the input list
as long as they satisfy the given condition.
Stops when the predicate function returns `false`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The input list|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list containing the taken elements|

### List.**find**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally failed if the list was empty</td></tr>
<tr><td><code>0.3.0</code></td><td>Return type converted to `Option` type</td></tr>
</tbody>
</table>
</details>

```grain
find : ((a -> Bool), List<a>) -> Option<a>
```

Finds the first element in a list that satifies the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The list to search|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(element)` containing the first value found or `None` otherwise|

### List.**findIndex**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally failed if the list was empty</td></tr>
<tr><td><code>0.3.0</code></td><td>Return type converted to `Option` type</td></tr>
</tbody>
</table>
</details>

```grain
findIndex : ((a -> Bool), List<a>) -> Option<Number>
```

Finds the first index in a list where the element satifies the given condition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`a -> Bool`|The function to call on each element, where the returned value indicates if the element satisfies the condition|
|`list`|`List<a>`|The list to search|

Returns:

|type|description|
|----|-----------|
|`Option<Number>`|`Some(index)` containing the index of the first element found or `None` otherwise|

### List.**product**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
product : (List<a>, List<b>) -> List<(a, b)>
```

Combines two lists into a Cartesian product of tuples containing
all ordered pairs `(a, b)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list1`|`List<a>`|The list to provide values for the first tuple element|
|`list2`|`List<b>`|The list to provide values for the second tuple element|

Returns:

|type|description|
|----|-----------|
|`List<(a, b)>`|The new list containing all pairs of `(a, b)`|

### List.**sub**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
sub : (Number, Number, List<a>) -> List<a>
```

Provides the subset of a list given zero-based start index and amount of elements
to include.

Parameters:

|param|type|description|
|-----|----|-----------|
|`start`|`Number`|The index of the list where the subset will begin (inclusive)|
|`length`|`Number`|The amount of elements to be included in the subset|
|`list`|`List<a>`|The input list|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The subset of the list|

Throws:

`Failure(String)`

* When `start` is negative
* When `length` is negative

### List.**join**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.0</code></summary>
No other changes yet.
</details>

```grain
join : (String, List<String>) -> String
```

Combine the given list of strings into one string with the specified
separator inserted between each item.

Parameters:

|param|type|description|
|-----|----|-----------|
|`separator`|`String`|The separator to insert between elements|
|`list`|`List<String>`|The list to combine|

Returns:

|type|description|
|----|-----------|
|`String`|The combined elements with the separator between each|

### List.**revAppend**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.5</code></summary>
No other changes yet.
</details>

```grain
revAppend : (List<a>, List<a>) -> List<a>
```

Reverses the first list and appends the second list to the end.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list1`|`List<a>`|The list to reverse|
|`list2`|`List<a>`|The list to append|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The new list|

### List.**sort**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.5</code></summary>
No other changes yet.
</details>

```grain
sort : (((a, a) -> Number), List<a>) -> List<a>
```

Sorts the given list based on a given comparator function. The resulting list is sorted in increasing order.

Ordering is calculated using a comparator function which takes two list elements and must return 0 if both are equal, a positive number if the first is greater, and a negative number if the first is smaller.

Parameters:

|param|type|description|
|-----|----|-----------|
|`comp`|`(a, a) -> Number`|The comparator function used to indicate sort order|
|`list`|`List<a>`|The list to be sorted|

Returns:

|type|description|
|----|-----------|
|`List<a>`|The sorted list|

