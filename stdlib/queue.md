---
title: Queue
---

> **Deprecated:** This module will be renamed to ImmutableQueue in the v0.6.0 release of Grain.

An immutable queue implementation. A queue is a FIFO (first-in-first-out) data structure where new values are added to the end and retrieved or removed from the beginning.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
import Queue from "queue"
```

## Types

Type declarations included in the Queue module.

### Queue.**Queue**

```grain
type Queue<a>
```

## Values

Functions and constants for working with queues.

### Queue.**empty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
empty : Queue<a>
```

An empty queue.

### Queue.**make**

> **Deprecated:** This will be removed in the v0.6.0 release of Grain.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
make : () -> Queue<a>
```

Creates an empty queue.

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|An empty queue|

### Queue.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty : Queue<a> -> Bool
```

Checks if the given queue contains any values.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the given queue is empty or `false` otherwise|

### Queue.**peek**

<details>
<summary>Added in <code>0.3.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `head`</td></tr>
<tr><td><code>0.3.2</code></td><td>Deprecated `head` function</td></tr>
<tr><td><code>0.4.0</code></td><td>Removed `head` function</td></tr>
</tbody>
</table>
</details>

```grain
peek : Queue<a> -> Option<a>
```

Returns the value at the beginning of the queue. It is not removed from the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the value at the beginning of the queue, or `None` if the queue is empty|

### Queue.**push**

<details>
<summary>Added in <code>0.3.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `enqueue`</td></tr>
<tr><td><code>0.3.2</code></td><td>Deprecated `enqueue` function</td></tr>
<tr><td><code>0.4.0</code></td><td>Removed `enqueue` function</td></tr>
</tbody>
</table>
</details>

```grain
push : (a, Queue<a>) -> Queue<a>
```

Adds a value to the end of the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value to append|
|`queue`|`Queue<a>`|The queue to update|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|An updated queue|

### Queue.**pop**

<details>
<summary>Added in <code>0.3.2</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Originally named `dequeue`</td></tr>
<tr><td><code>0.3.2</code></td><td>Deprecated `dequeue` function</td></tr>
<tr><td><code>0.4.0</code></td><td>Removed `dequeue` function</td></tr>
</tbody>
</table>
</details>

```grain
pop : Queue<a> -> Queue<a>
```

Dequeues the next value in the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to change|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|An updated queue|

### Queue.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
size : Queue<a> -> Number
```

Get the number of values in a queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of values in the queue|

