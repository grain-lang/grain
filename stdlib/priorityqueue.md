---
title: PriorityQueue
---

A mutable priority queue implementation. A priority queue is a data structure that maintains elements in a priority order. Elements with higher priority are served before elements with lower priority when extracting from the priority queue.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
import PriorityQueue from "priorityqueue"
```

## Types

Type declarations included in the PriorityQueue module.

### PriorityQueue.**PriorityQueue**

```grain
type PriorityQueue<a>
```

Mutable data structure which maintains a priority order for its elements.

## Values

Functions for working with PriorityQueues.

### PriorityQueue.**makeSized**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
makeSized : (Number, ((a, a) -> Number)) -> PriorityQueue<a>
```

Creates a new priority queue with a given internal storage size and a
comparator function, which is used to determine priority of elements. The
comparator function takes two elements and must return 0 if both share
priority, a positive number if the first has greater priority, and a
negative number if the first has less priority.

Generally, you won't need to care about the storage size of your priority
queue and can use `PriorityQueue.make()` instead.

Parameters:

|param|type|description|
|-----|----|-----------|
|`size`|`Number`|The initial storage size of the priority queue|
|`comp`|`(a, a) -> Number`|The comparator function used to indicate priority order|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|An empty priority queue|

### PriorityQueue.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
make : ((a, a) -> Number) -> PriorityQueue<a>
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
|`PriorityQueue<a>`|An empty priority queue|

Examples:

```grain
PriorityQueue.make(compare) // creates a min priority queue of numbers using the compare pervasive
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
size : PriorityQueue<a> -> Number
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
isEmpty : PriorityQueue<a> -> Bool
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
push : (a, PriorityQueue<a>) -> Void
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
peek : PriorityQueue<a> -> Option<a>
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
pop : PriorityQueue<a> -> Option<a>
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
drain : PriorityQueue<a> -> List<a>
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

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
fromArray : (Array<a>, ((a, a) -> Number)) -> PriorityQueue<a>
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
|`PriorityQueue<a>`|A priority queue containing the elements from the array|

### PriorityQueue.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.3</code></summary>
No other changes yet.
</details>

```grain
fromList : (List<a>, ((a, a) -> Number)) -> PriorityQueue<a>
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
|`PriorityQueue<a>`|A priority queue containing the elements from the list|

