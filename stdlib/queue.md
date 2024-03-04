---
title: Queue
---

A queue is a FIFO (first-in-first-out) data structure where new
values are added to the end and retrieved or removed from the beginning.

The default implementation is mutable, but an immutable queue
implementation is available in the `Immutable` submodule.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
from "queue" include Queue
```

## Types

Type declarations included in the Queue module.

### Queue.**Queue**

```grain
type Queue<a>
```

A mutable FIFO (first-in-first-out) data structure.

## Values

Functions and constants included in the Queue module.

### Queue.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
make : (?size: Number) => Queue<a>
```

Creates a new queue with an initial storage of the given size. As values are
added or removed, the internal storage may grow or shrink. Generally, you
won’t need to care about the storage size of your map and can use the
default size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?size`|`Number`|The initial storage size of the queue|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|An empty queue|

### Queue.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty : (queue: Queue<a>) => Bool
```

Checks if the given queue contains no items.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the queue has no items or `false` otherwise|

### Queue.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
size : (queue: Queue<a>) => Number
```

Computes the size of the input queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The count of the items in the queue|

### Queue.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
peek : (queue: Queue<a>) => Option<a>
```

Provides the value at the beginning of the queue, if it exists.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the value at the beginning of the queue or `None` otherwise.|

### Queue.**push**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
push : (value: a, queue: Queue<a>) => Void
```

Adds a new item to the end of the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The item to be added|
|`queue`|`Queue<a>`|The queue being updated|

### Queue.**pop**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
pop : (queue: Queue<a>) => Option<a>
```

Removes the item at the beginning of the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue being updated|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The element removed from the queue|

### Queue.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toList : (queue: Queue<a>) => List<a>
```

Converts a queue into a list of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all queue values|

### Queue.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromList : (list: List<a>) => Queue<a>
```

Creates a queue from a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to convert|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|A queue containing all list values|

### Queue.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
clear : (queue: Queue<a>) => Void
```

Clears the queue by removing all of its elements

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to clear|

### Queue.**copy**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
copy : (queue: Queue<a>) => Queue<a>
```

Produces a shallow copy of the input queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to copy|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|A new queue containing the elements from the input|

### Queue.**toArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toArray : (queue: Queue<a>) => Array<a>
```

Converts a queue into an array of its values.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to convert|

Returns:

|type|description|
|----|-----------|
|`Array<a>`|An array containing all values from the given queue|

### Queue.**fromArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromArray : (arr: Array<a>) => Queue<a>
```

Creates a queue from an array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`arr`|`Array<a>`|The array to convert|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|A queue containing all values from the array|

### Queue.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(==) : (queue1: Queue<a>, queue2: Queue<a>) => Bool
```

Checks if two queues are equivalent by value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue1`|`Queue<a>`|The first queue to compare|
|`queue2`|`Queue<a>`|The second queue to compare|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the queues are equivalent or `false` otherwise|

## Queue.Immutable

An immutable queue implementation.

### Types

Type declarations included in the Queue.Immutable module.

#### Queue.Immutable.**ImmutableQueue**

```grain
type ImmutableQueue<a>
```

An immutable FIFO (first-in-first-out) data structure.

### Values

Functions and constants included in the Queue.Immutable module.

#### Queue.Immutable.**empty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally a module root API</td></tr>
</tbody>
</table>
</details>

```grain
empty : ImmutableQueue<a>
```

An empty queue.

#### Queue.Immutable.**isEmpty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally a module root API</td></tr>
</tbody>
</table>
</details>

```grain
isEmpty : (queue: ImmutableQueue<a>) => Bool
```

Checks if the given queue contains any values.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`ImmutableQueue<a>`|The queue to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the given queue is empty or `false` otherwise|

#### Queue.Immutable.**peek**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `head`</td></tr>
<tr><td><code>0.3.2</code></td><td>Deprecated `head` function</td></tr>
<tr><td><code>0.3.2</code></td><td>Originally a module root API</td></tr>
<tr><td><code>0.4.0</code></td><td>Removed `head` function</td></tr>
</tbody>
</table>
</details>

```grain
peek : (queue: ImmutableQueue<a>) => Option<a>
```

Returns the value at the beginning of the queue. It is not removed from the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`ImmutableQueue<a>`|The queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the value at the beginning of the queue, or `None` if the queue is empty|

#### Queue.Immutable.**push**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `enqueue`</td></tr>
<tr><td><code>0.3.2</code></td><td>Deprecated `enqueue` function</td></tr>
<tr><td><code>0.3.2</code></td><td>Originally a module root API</td></tr>
<tr><td><code>0.4.0</code></td><td>Removed `enqueue` function</td></tr>
</tbody>
</table>
</details>

```grain
push : (value: a, queue: ImmutableQueue<a>) => ImmutableQueue<a>
```

Adds a value to the end of the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value to append|
|`queue`|`ImmutableQueue<a>`|The queue to update|

Returns:

|type|description|
|----|-----------|
|`ImmutableQueue<a>`|An updated queue|

#### Queue.Immutable.**pop**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `dequeue`</td></tr>
<tr><td><code>0.3.2</code></td><td>Deprecated `dequeue` function</td></tr>
<tr><td><code>0.3.2</code></td><td>Originally a module root API</td></tr>
<tr><td><code>0.4.0</code></td><td>Removed `dequeue` function</td></tr>
</tbody>
</table>
</details>

```grain
pop : (queue: ImmutableQueue<a>) => ImmutableQueue<a>
```

Dequeues the next value in the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`ImmutableQueue<a>`|The queue to change|

Returns:

|type|description|
|----|-----------|
|`ImmutableQueue<a>`|An updated queue|

#### Queue.Immutable.**size**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.2</code></td><td>Originally a module root API</td></tr>
</tbody>
</table>
</details>

```grain
size : (queue: ImmutableQueue<a>) => Number
```

Get the number of values in a queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`ImmutableQueue<a>`|The queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of values in the queue|

#### Queue.Immutable.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toList : (queue: ImmutableQueue<a>) => List<a>
```

Converts a queue into a list of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`ImmutableQueue<a>`|The queue to convert|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all queue values|

#### Queue.Immutable.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromList : (list: List<a>) => ImmutableQueue<a>
```

Creates a queue from a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<a>`|The list to convert|

Returns:

|type|description|
|----|-----------|
|`ImmutableQueue<a>`|A queue containing all list values|

