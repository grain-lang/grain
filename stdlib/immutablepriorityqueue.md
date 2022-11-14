---
title: ImmutablePriorityQueue
---

An immutable priority queue. A priority queue is a data structure that maintains elements in a priority order. Elements with higher priority are served before elements with lower priority when extracting from the priority queue.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
import ImmutablePriorityQueue from "immutablepriorityqueue"
```

## Types

Type declarations included in the ImmutablePriorityQueue module.

### ImmutablePriorityQueue.**ImmutablePriorityQueue**

```grain
type ImmutablePriorityQueue<a>
```

Immutable data structure which maintains a priority order for its elements.

## Values

Functions and constants for working with ImmutablePriorityQueues.

### ImmutablePriorityQueue.**empty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
empty : ImmutablePriorityQueue<a>
```

An empty priority queue with the default `compare` comparator.

### ImmutablePriorityQueue.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
make : ((a, a) -> Number) -> ImmutablePriorityQueue<a>
```

Creates a new priority queue with a comparator function, which is used to
determine priority of elements. The comparator function takes two elements
and must return 0 if both share priority, a positive number if the first
has greater priority, and a negative number if the first has less priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`comp`|`(a, a) -> Number`|The comparator function used to indicate priority order|

Returns:

|type|description|
|----|-----------|
|`ImmutablePriorityQueue<a>`|An empty priority queue|

Examples:

```grain
ImmutablePriorityQueue.make(compare) // creates a min priority queue of numbers using the compare pervasive
```

```grain
ImmutablePriorityQueue.make((a, b) => String.length(b) - String.length(a)) // creates a priority queue by string length (longest to shortest)
```

### ImmutablePriorityQueue.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
size : ImmutablePriorityQueue<a> -> Number
```

Gets the number of elements in a priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of elements in the priority queue|

### ImmutablePriorityQueue.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
isEmpty : ImmutablePriorityQueue<a> -> Bool
```

Determines if the priority queue contains no elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the priority queue is empty and `false` otherwise|

### ImmutablePriorityQueue.**push**

```grain
push : (a, ImmutablePriorityQueue<a>) -> ImmutablePriorityQueue<a>
```

Produces a new priority queue by inserting the given element into the given priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`val`|`a`|The value to add into the priority queue|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue|

Returns:

|type|description|
|----|-----------|
|`ImmutablePriorityQueue<a>`|A new priority queue with the given element inserted|

### ImmutablePriorityQueue.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
peek : ImmutablePriorityQueue<a> -> Option<a>
```

Retrieves the highest priority element in the priority queue. It is not
removed from the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the highest priority element or `None` if the priority queue is empty|

### ImmutablePriorityQueue.**pop**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
pop : ImmutablePriorityQueue<a> -> ImmutablePriorityQueue<a>
```

Produces a new priority queue without the highest priority element in the
given priority queue. If the input priority queue is empty, this function will
return it.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue|

Returns:

|type|description|
|----|-----------|
|`ImmutablePriorityQueue<a>`|A new priority queue without the highest priority element|

### ImmutablePriorityQueue.**drain**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
drain : ImmutablePriorityQueue<a> -> List<a>
```

Produces a list of all elements in the priority queue in priority order.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue to drain|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list of all elements in the priority in priority order|

### ImmutablePriorityQueue.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
fromList : (List<a>, ((a, a) -> Number)) -> ImmutablePriorityQueue<a>
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
|`comp`|`(a, a) -> Number`|A comparator function used to assign priority to elements|

Returns:

|type|description|
|----|-----------|
|`ImmutablePriorityQueue<a>`|A priority queue containing the elements from the list|

### ImmutablePriorityQueue.**fromArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
fromArray : (Array<a>, ((a, a) -> Number)) -> ImmutablePriorityQueue<a>
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
|`comp`|`(a, a) -> Number`|A comparator function used to assign priority to elements|

Returns:

|type|description|
|----|-----------|
|`ImmutablePriorityQueue<a>`|A priority queue containing the elements from the array|

