---
title: Queue
---

An immutable queue implementation. A queue is a FIFO (first-in-first-out) data structure where new values are added to the end and retrieved or removed from the beginning.

```grain
import Queue from "queue"
```

## Values

Functions for working with queues.

### Queue.**make**

```grain
make : () -> Queue<a>
```

Creates an empty queue.

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|An empty queue|

### Queue.**isEmpty**

```grain
isEmpty : Queue<a> -> Bool
```

Checks if the given queue contains any elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the given queue is empty or `false` otherwise|

### Queue.**peek**

```grain
peek : Queue<a> -> Option<a>
```

Returns the element on the top of the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to peek into|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The element on the top of the queue, if the queue is empty it returns `None`|
|`Option<a>`|`Some(item)` containing the element on the top of the queue, if the queue is empty returns `None`|

### Queue.**push**

```grain
push : (a, Queue<a>) -> Queue<a>
```

Pushes a value to the end of the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value to queue|
|`queue`|`Queue<a>`|The queue to append the value too|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|The new queue|

### Queue.**pop**

```grain
pop : Queue<a> -> Queue<a>
```

Pops a value off the front of the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to pop from|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|The new queue|

### Queue.**size**

```grain
size : Queue<a> -> Number
```

Get the length of the provided queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to determine the size of.|

Returns:

|type|description|
|----|-----------|
|`Number`|The size of the queue|

