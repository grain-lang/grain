---
title: PriorityQueue
---

A priority queue is a data structure that maintains elements in a priority order. Elements with higher priority are served before elements with lower priority when extracting from the priority queue.

An immutable priority queue implementation is available in the `Immutable` submodule.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
from "priorityqueue" include Priorityqueue
```

## Types

Type declarations included in the PriorityQueue module.

### PriorityQueue.**PriorityQueue**

```grain
type PriorityQueue<a>
```

Mutable data structure which maintains a priority order for its elements.

## Values

Functions and constants included in the PriorityQueue module.

### PriorityQueue.**make**

<details>
<summary>Added in <code>0.5.3</code></summary>
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
make : (?compare: ((a, a) => Number), ?size: Number) => PriorityQueue<a>
```

Creates a new priority queue with a given internal storage size and a
comparator function, which is used to determine priority of elements. The
comparator function takes two elements and must return 0 if both share
priority, a positive number if the first has greater priority, and a
negative number if the first has less priority.

Generally, you won't need to care about the storage size of your priority
queue and can use the default size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?compare`|`(a, a) => Number`|The comparator function used to indicate priority order|
|`?size`|`Number`|The initial storage size of the priority queue|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|An empty priority queue|

Examples:

```grain
PriorityQueue.make() // creates a min priority queue of numbers using the compare pervasive
```

```grain
PriorityQueue.make(compare=compare, size=32) // creates a min priority queue of numbers using the compare pervasive and an initial size of 32
```

```grain
PriorityQueue.make((a, b) => String.length(b) - String.length(a)) // creates a priority queue by string length (longest to shortest)
```

### PriorityQueue.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
size : (pq: PriorityQueue<a>) => Number
```

Gets the number of elements in a priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of elements in the priority queue|

### PriorityQueue.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
isEmpty : (pq: PriorityQueue<a>) => Bool
```

Determines if the priority queue contains no elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the priority queue is empty and `false` otherwise|

### PriorityQueue.**push**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
push : (val: a, pq: PriorityQueue<a>) => Void
```

Adds a new element to the priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`val`|`a`|The value to add into the priority queue|
|`pq`|`PriorityQueue<a>`|The priority queue to update|

### PriorityQueue.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
peek : (pq: PriorityQueue<a>) => Option<a>
```

Retrieves the highest priority element in the priority queue. It is not
removed from the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the highest priority element or `None` if the priority queue is empty|

### PriorityQueue.**pop**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
pop : (pq: PriorityQueue<a>) => Option<a>
```

Removes and retrieves the highest priority element in the priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the highest priority element or `None` if the priority queue is empty|

### PriorityQueue.**drain**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
drain : (pq: PriorityQueue<a>) => List<a>
```

Clears the priority queue and produces a list of all of the elements in the priority
queue in priority order.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to drain|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list of all elements in the priority in priority order|

### PriorityQueue.**fromArray**

<details>
<summary>Added in <code>0.5.4</code></summary>
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
fromArray :
  (array: Array<a>, ?compare: ((a, a) => Number)) => PriorityQueue<a>
```

Constructs a new priority queue initialized with the elements in the array
using a custom comparator function, which is used to determine priority of
elements. The comparator function takes two elements and must return 0 if
both share priority, a positive number if the first has greater priority,
and a negative number if the first has less priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|An array of values used to initialize the priority queue|
|`?compare`|`(a, a) => Number`|A comparator function used to assign priority to elements|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|A priority queue containing the elements from the array|

### PriorityQueue.**fromList**

<details>
<summary>Added in <code>0.5.3</code></summary>
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
fromList : (list: List<a>, ?compare: ((a, a) => Number)) => PriorityQueue<a>
```

Constructs a new priority queue initialized with the elements in the list
using a custom comparator function, which is used to determine priority of
elements. The comparator function takes two elements and must return 0 if
both share priority, a positive number if the first has greater priority,
and a negative number if the first has less priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|A list of values used to initialize the priority queue|
|`?compare`|`(a, a) => Number`|A comparator function used to assign priority to elements|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|A priority queue containing the elements from the list|

## PriorityQueue.Immutable

An immutable priority queue implementation.

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablepriorityqueue"` module</td></tr>
</tbody>
</table>
</details>

### Types

Type declarations included in the PriorityQueue.Immutable module.

#### PriorityQueue.Immutable.**PriorityQueue**

```grain
type PriorityQueue<a>
```

Immutable data structure which maintains a priority order for its elements.

### Values

Functions and constants included in the PriorityQueue.Immutable module.

#### PriorityQueue.Immutable.**empty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablepriorityqueue"` module</td></tr>
</tbody>
</table>
</details>

```grain
empty : PriorityQueue<a>
```

An empty priority queue with the default `compare` comparator.

#### PriorityQueue.Immutable.**make**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.3</code></td><td>Originally in `"immutablepriorityqueue"` module with `compare` being a required argument</td></tr>
</tbody>
</table>
</details>

```grain
make : (?compare: ((a, a) => Number)) => PriorityQueue<a>
```

Creates a new priority queue with a comparator function, which is used to
determine priority of elements. The comparator function takes two elements
and must return 0 if both share priority, a positive number if the first
has greater priority, and a negative number if the first has less priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?compare`|`(a, a) => Number`|The comparator function used to indicate priority order|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|An empty priority queue|

Examples:

```grain
PriorityQueue.Immutable.make(compare) // creates a min priority queue of numbers using the compare pervasive
```

```grain
PriorityQueue.Immutable.make((a, b) => String.length(b) - String.length(a)) // creates a priority queue by string length (longest to shortest)
```

#### PriorityQueue.Immutable.**size**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.3</code></td><td>Originally in `"immutablepriorityqueue"` module</td></tr>
</tbody>
</table>
</details>

```grain
size : (pq: PriorityQueue<a>) => Number
```

Gets the number of elements in a priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of elements in the priority queue|

#### PriorityQueue.Immutable.**isEmpty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.3</code></td><td>Originally in `"immutablepriorityqueue"` module</td></tr>
</tbody>
</table>
</details>

```grain
isEmpty : (pq: PriorityQueue<a>) => Bool
```

Determines if the priority queue contains no elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the priority queue is empty and `false` otherwise|

#### PriorityQueue.Immutable.**push**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.3</code></td><td>Originally in `"immutablepriorityqueue"` module</td></tr>
</tbody>
</table>
</details>

```grain
push : (val: a, pq: PriorityQueue<a>) => PriorityQueue<a>
```

Produces a new priority queue by inserting the given element into the given priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`val`|`a`|The value to add into the priority queue|
|`pq`|`PriorityQueue<a>`|The priority queue|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|A new priority queue with the given element inserted|

#### PriorityQueue.Immutable.**peek**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.3</code></td><td>Originally in `"immutablepriorityqueue"` module</td></tr>
</tbody>
</table>
</details>

```grain
peek : (pq: PriorityQueue<a>) => Option<a>
```

Retrieves the highest priority element in the priority queue. It is not
removed from the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the highest priority element or `None` if the priority queue is empty|

#### PriorityQueue.Immutable.**pop**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.3</code></td><td>Originally in `"immutablepriorityqueue"` module</td></tr>
</tbody>
</table>
</details>

```grain
pop : (pq: PriorityQueue<a>) => PriorityQueue<a>
```

Produces a new priority queue without the highest priority element in the
given priority queue. If the input priority queue is empty, this function will
return it.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|A new priority queue without the highest priority element|

#### PriorityQueue.Immutable.**drain**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.3</code></td><td>Originally in `"immutablepriorityqueue"` module</td></tr>
</tbody>
</table>
</details>

```grain
drain : (pq: PriorityQueue<a>) => List<a>
```

Produces a list of all elements in the priority queue in priority order.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to drain|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list of all elements in the priority in priority order|

#### PriorityQueue.Immutable.**fromList**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.3</code></td><td>Originally in `"immutablepriorityqueue"` module with `compare` being a required argument</td></tr>
</tbody>
</table>
</details>

```grain
fromList : (list: List<a>, ?compare: ((a, a) => Number)) => PriorityQueue<a>
```

Constructs a new priority queue initialized with the elements in the list
using a custom comparator function, which is used to determine priority of
elements. The comparator function takes two elements and must return 0 if
both share priority, a positive number if the first has greater priority,
and a negative number if the first has less priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|A list of values used to initialize the priority queue|
|`?compare`|`(a, a) => Number`|A comparator function used to assign priority to elements|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|A priority queue containing the elements from the list|

#### PriorityQueue.Immutable.**fromArray**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablepriorityqueue"` module with `compare` being a required argument</td></tr>
</tbody>
</table>
</details>

```grain
fromArray :
  (array: Array<a>, ?compare: ((a, a) => Number)) => PriorityQueue<a>
```

Constructs a new priority queue initialized with the elements in the array
using a custom comparator function, which is used to determine priority of
elements. The comparator function takes two elements and must return 0 if
both share priority, a positive number if the first has greater priority,
and a negative number if the first has less priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<a>`|An array of values used to initialize the priority queue|
|`?compare`|`(a, a) => Number`|A comparator function used to assign priority to elements|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|A priority queue containing the elements from the array|

