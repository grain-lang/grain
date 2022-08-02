---
title: PriorityQueue
---

A mutable binary heap priority queue implementation. A priority queue is a data structure that maintains elements in a priority order. Elements with higher priority are served before elements with lower priority when extracting from the priority queue.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
import PriorityQueue from "priorityqueue"
```

## Types

Type declarations included in the PriorityQueue module.

### Priorityqueue.**PriorityQueue**

```grain
type PriorityQueue<a>
```

Mutable data structure which maintains a priority order for its elements.

## Values

Functions for working with PriorityQueues.

### Priorityqueue.**makeSized**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
makeSized : (Number, ((a, a) -> Number)) -> PriorityQueue<a>
```

Creates a new priority queue with a given internal storage size and a
comparator to use for determining element priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`size`|`Number`|The number of elements to initialize the internal storage array with|
|`comp`|`(a, a) -> Number`|The comparator function used to indicate priority order|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|An empty priority queue|

### Priorityqueue.**make**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
make : ((a, a) -> Number) -> PriorityQueue<a>
```

Creates a new priority queue with a comparator to use for determining element priority.

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
PriorityQueue.make((a, b) => a - b) // creates a min priority queue of numbers
```

### Priorityqueue.**size**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
size : PriorityQueue<a> -> Number
```

Returns the number of elements currently in the priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to get the size of|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of elements in the priority queue|

### Priorityqueue.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
isEmpty : PriorityQueue<a> -> Bool
```

Returns whether or not the priority queue is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to find the emptiness of|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the priority queue has no elements and `false` otherwise|

### Priorityqueue.**push**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
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
|`pq`|`PriorityQueue<a>`|The priority queue to add a new element to|

### Priorityqueue.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
peek : PriorityQueue<a> -> Option<a>
```

Returns the highest-priority element in the priority queue in a `Some`
variant, or `None` if the priority queue is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to get the highest-priority element of|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The element with the highest priority wrapped in a `Some` variant, or `None` if the priority queue is empty|

### Priorityqueue.**pop**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
pop : PriorityQueue<a> -> Option<a>
```

Removes and returns the highest-priority element in the priority queue in a `Some`
variant, or `None` if the priority queue is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to remove and get the highest-priority element of|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The element with the highest priority wrapped in a `Some` variant, or `None` if the priority queue is empty|

### Priorityqueue.**drain**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
drain : PriorityQueue<a> -> List<a>
```

Clears the priority queue, and returns all of the elements in the priority
queue in priority order.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`PriorityQueue<a>`|The priority queue to drain|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list of all elements in the priority in priority order|

### Priorityqueue.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromList : (List<a>, ((a, a) -> Number)) -> PriorityQueue<a>
```

Constructs a new priority queue initialized with the elements in the list, and
a custom comparator function used to assign priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|A list of values to initialize the priority queue with|
|`comp`|`(a, a) -> Number`|A comparator function used to assign priority to elements|

Returns:

|type|description|
|----|-----------|
|`PriorityQueue<a>`|A priority queue containing the elements in the list|

