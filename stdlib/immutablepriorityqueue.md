---
title: ImmutablePriorityQueue
---

An immutable priority queue implemented as a skew binomial queue. A priority queue is a data structure that maintains elements in a priority order. Elements with higher priority are served before elements with lower priority when extracting from the priority queue.

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
import ImmutablePriorityQueue from "immutablepriorityqueue"
```

## Types

Type declarations included in the ImmutablePriorityQueue module.

### Immutablepriorityqueue.**ImmutablePriorityQueue**

```grain
type ImmutablePriorityQueue<a>
```

Immutable data structure which maintains a priority order for its elements.

## Values

Functions for working with ImmutablePriorityQueues.

### Immutablepriorityqueue.**make**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
make : ((a, a) -> Number) -> ImmutablePriorityQueue<a>
```

Creates a new priority queue with a comparator to use for determining element priority.

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
ImmutablePriorityQueue.make((a, b) => a - b) // creates a min priority queue of numbers
```

### Immutablepriorityqueue.**size**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
size : ImmutablePriorityQueue<a> -> Number
```

Returns the number of elements currently in the priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue to get the size of|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of elements in the priority queue|

### Immutablepriorityqueue.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
isEmpty : ImmutablePriorityQueue<a> -> Bool
```

Returns whether or not the priority queue is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue to find the emptiness of|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the priority queue has no elements and `false` otherwise|

### Immutablepriorityqueue.**push**

```grain
push : (a, ImmutablePriorityQueue<a>) -> ImmutablePriorityQueue<a>
```

Returns a new priority queue with the given element inserted into the given priority queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`val`|`a`|The value to add into the priority queue|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue|

Returns:

|type|description|
|----|-----------|
|`ImmutablePriorityQueue<a>`|A new priority queue with the given element inserted into the given priority queue|

### Immutablepriorityqueue.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
peek : ImmutablePriorityQueue<a> -> Option<a>
```

Returns the highest-priority element in the priority queue in a `Some`
variant, or `None` if the priority queue is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue to get the highest-priority element of|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The element with the highest priority wrapped in a `Some` variant, or `None` if the priority queue is empty|

### Immutablepriorityqueue.**pop**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
pop : ImmutablePriorityQueue<a> -> ImmutablePriorityQueue<a>
```

Returns a new priority queue without the highest-priority element in the given priority queue
in a `Some` variant, or `None` if the priority queue is empty.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue|

Returns:

|type|description|
|----|-----------|
|`ImmutablePriorityQueue<a>`|A new priority queue without the highest-priority element in the input priority queue|

### Immutablepriorityqueue.**drain**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
drain : ImmutablePriorityQueue<a> -> List<a>
```

Returns all of the elements in the priority queue in priority order.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pq`|`ImmutablePriorityQueue<a>`|The priority queue to drain|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list of all elements in the priority in priority order|

### Immutablepriorityqueue.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fromList : (List<a>, ((a, a) -> Number)) -> ImmutablePriorityQueue<a>
```

Constructs a new priority queue initialized with the elements in the list, and
with a custom comparator function used to assign priority.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|A list of values to initialize the priority queue with|
|`comp`|`(a, a) -> Number`|A comparator function used to assign priority to elements|

Returns:

|type|description|
|----|-----------|
|`ImmutablePriorityQueue<a>`|A priority queue containing the elements in the list|

