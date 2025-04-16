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

```grain
let queue = Queue.fromList([0, 1])
Queue.push(2, queue)
assert Queue.pop(queue) == Some(0)
assert Queue.pop(queue) == Some(1)
assert Queue.pop(queue) == Some(2)
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
make: (?size: Number) => Queue<a>
```

Creates a new queue with an initial storage of the given size. As values are
added or removed, the internal storage may grow or shrink. Generally, you
won’t need to care about the storage size of your queue and can use the
default size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?size`|`Number`|The initial storage size of the queue|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|An empty queue|

Examples:

```grain
Queue.make() // Creates a new queue
```

```grain
Queue.make(size=16) // Creates a new queue with an initial size of 16
```

### Queue.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty: (queue: Queue<a>) => Bool
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

Examples:

```grain
Queue.isEmpty(Queue.make()) == true
```

```grain
Queue.isEmpty(Queue.fromList([1, 2])) == false
```

### Queue.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
size: (queue: Queue<a>) => Number
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

Examples:

```grain
Queue.size(Queue.make()) == 0
```

```grain
Queue.size(Queue.fromList([1, 2])) == 2
```

### Queue.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
peek: (queue: Queue<a>) => Option<a>
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

Examples:

```grain
Queue.peek(Queue.make()) == None
```

```grain
let queue = Queue.make()
Queue.push(1, queue)
assert Queue.peek(queue) == Some(1)
```

### Queue.**push**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
push: (value: a, queue: Queue<a>) => Void
```

Adds a new item to the end of the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The item to be added|
|`queue`|`Queue<a>`|The queue being updated|

Examples:

```grain
let queue = Queue.make()
assert Queue.peek(queue) == None
Queue.push(1, queue)
assert Queue.peek(queue) == Some(1)
```

### Queue.**pop**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
pop: (queue: Queue<a>) => Option<a>
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

Examples:

```grain
let queue = Queue.make()
Queue.push(1, queue)
assert Queue.pop(queue) == Some(1)
assert Queue.pop(queue) == None
```

### Queue.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
clear: (queue: Queue<a>) => Void
```

Clears the queue by removing all of its elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to clear|

Examples:

```grain
let queue = Queue.make()
Queue.push(1, queue)
assert Queue.size(queue) == 1
Queue.clear(queue)
assert Queue.size(queue) == 0
```

### Queue.**copy**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
copy: (queue: Queue<a>) => Queue<a>
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

Examples:

```grain
let queue = Queue.make()
Queue.push(1, queue)
let copiedQueue = Queue.copy(queue)
Queue.push(2, queue) // Does not affect copiedQueue
assert Queue.pop(copiedQueue) == Some(1)
```

### Queue.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toList: (queue: Queue<a>) => List<a>
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

Examples:

```grain
let queue = Queue.make()
Queue.push(0, queue)
Queue.push(1, queue)
Queue.push(2, queue)
assert Queue.toList(queue) == [0, 1, 2]
```

### Queue.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromList: (list: List<a>) => Queue<a>
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

Examples:

```grain
let queue = Queue.fromList([0, 1])
assert Queue.pop(queue) == Some(0)
assert Queue.pop(queue) == Some(1)
```

### Queue.**toArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toArray: (queue: Queue<a>) => Array<a>
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

Examples:

```grain
let queue = Queue.make()
Queue.push(0, queue)
Queue.push(1, queue)
Queue.push(2, queue)
assert Queue.toArray(queue) == [> 0, 1, 2]
```

### Queue.**fromArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromArray: (arr: Array<a>) => Queue<a>
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

Examples:

```grain
let queue = Queue.fromArray([> 0, 1])
assert Queue.pop(queue) == Some(0)
assert Queue.pop(queue) == Some(1)
```

### Queue.**(==)**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
(==): (queue1: Queue<a>, queue2: Queue<a>) => Bool
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

Examples:

```grain
use Queue.{ (==) }
let queue1 = Queue.fromList([0, 1, 2])
let queue2 = Queue.fromList([0, 1, 2])
assert queue1 == queue2
```

```grain
use Queue.{ (==) }
let queue1 = Queue.fromList([0, 1, 2])
let queue2 = Queue.fromList([0, 1, 3])
assert !(queue1 == queue2)
```

## Queue.Immutable

An immutable queue implementation.

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
let queue = Immutable.Queue.fromList([0, 1])
let queue = Immutable.Queue.push(2, queue)
assert Immutable.Queue.peek(queue) == Some(0)
let queue = Immutable.Queue.pop(queue)
assert Immutable.Queue.peek(queue) == Some(1)
ignore(Queue.Immutable.pop(queue)) // Does not affect the original queue
assert Immutable.Queue.peek(queue) == Some(1)
```

### Types

Type declarations included in the Queue.Immutable module.

#### Queue.Immutable.**ImmutableQueue**

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
empty: ImmutableQueue<a>
```

An empty queue.

Examples:

```grain
let queue = Queue.Immutable.empty
assert Queue.Immutable.isEmpty(queue)
```

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
isEmpty: (queue: ImmutableQueue<a>) => Bool
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

Examples:

```grain
Queue.Immutable.isEmpty(Queue.Immutable.empty) == true
```

```grain
Queue.Immutable.isEmpty(Queue.Immutable.fromList([1, 2])) == false
```

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
peek: (queue: ImmutableQueue<a>) => Option<a>
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

Examples:

```grain
let queue = Queue.Immutable.fromList([1, 2, 3])
assert Queue.Immutable.peek(queue) == Some(1)
```

```grain
let queue = Queue.Immutable.empty
assert Queue.Immutable.peek(queue) == None
```

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
push: (value: a, queue: ImmutableQueue<a>) => ImmutableQueue<a>
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

Examples:

```grain
let queue = Queue.Immutable.fromList([1])
assert Queue.Immutable.size(queue) == 1
let queue = Queue.Immutable.push(2, queue)
assert Queue.Immutable.size(queue) == 2
```

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
pop: (queue: ImmutableQueue<a>) => ImmutableQueue<a>
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

Examples:

```grain
let queue = Queue.Immutable.fromList([1, 2, 3])
let queue = Queue.Immutable.pop(queue)
assert Queue.Immutable.peek(queue) == Some(2)
```

```grain
let queue = Queue.Immutable.empty
let queue = Queue.Immutable.pop(queue)
assert Queue.Immutable.isEmpty(queue)
```

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
size: (queue: ImmutableQueue<a>) => Number
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

Examples:

```grain
Queue.Immutable.size(Queue.Immutable.empty) == 0
```

```grain
Queue.Immutable.size(Queue.Immutable.fromList([1, 2])) == 2
```

#### Queue.Immutable.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
toList: (queue: ImmutableQueue<a>) => List<a>
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

Examples:

```grain
let queue = Queue.Immutable.empty
let queue = Queue.Immutable.push(1, queue)
let queue = Queue.Immutable.push(2, queue)
assert Queue.Immutable.toList(queue) == [1, 2]
```

```grain
let queue = Queue.Immutable.fromList([1, 2, 3])
assert Queue.Immutable.toList(queue) == [1, 2, 3]
```

```grain
let queue = Queue.Immutable.empty
assert Queue.Immutable.toList(queue) == []
```

#### Queue.Immutable.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
fromList: (list: List<a>) => ImmutableQueue<a>
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

Examples:

```grain
let queue = Queue.Immutable.fromList([1, 2, 3])
assert Queue.Immutable.peek(queue) == Some(1)
assert Queue.Immutable.size(queue) == 3
```

