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

Creates a new queue.

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|A new empty queue|

### Queue.**isEmpty**

```grain
isEmpty : Queue<a> -> Bool
```

Checks if the given queue contains no elements.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing weather or not the queue is empty|

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

### Queue.**push**

```grain
push : (a, Queue<a>) -> Queue<a>
```

Pushes a new element into the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The value to push|
|`queue`|`Queue<a>`|The queue to push to|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|The queue with the addition of the new element|

### Queue.**pop**

```grain
pop : Queue<a> -> Queue<a>
```

Pops an element off the queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to pop from|

Returns:

|type|description|
|----|-----------|
|`Queue<a>`|The queue with the item removed|

### Queue.**size**

```grain
size : Queue<a> -> Number
```

Get the length of the provided queue.

Parameters:

|param|type|description|
|-----|----|-----------|
|`queue`|`Queue<a>`|The queue to get the length from|

Returns:

|type|description|
|----|-----------|
|`Number`|The size of the queue|

