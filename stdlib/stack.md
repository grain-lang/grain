---
title: Stack
---

> **Deprecated:** This module will be renamed to ImmutableStack in the v0.6.0 release of Grain.

An immutable stack implementation. A stack is a LIFO (last-in-first-out) data structure where new values are added, retrieved, and removed from the end.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
import Stack from "stack"
```

## Types

Type declarations included in the Stack module.

### Stack.**Stack**

```grain
type Stack<a>
```

Stacks are immutable data structures that store their data in a List.

## Values

Functions and constants included in the Stack module.

### Stack.**empty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
empty : Stack<a>
```

An empty stack.

### Stack.**make**

> **Deprecated:** This will be removed in the v0.6.0 release of Grain.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
make : () -> Stack<a>
```

Creates a new stack.

Returns:

|type|description|
|----|-----------|
|`Stack<a>`|An empty stack|

### Stack.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty : Stack<a> -> Bool
```

Checks if the given stack contains no items.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`Stack<a>`|The stack to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the stack has no items or `false` otherwise|

### Stack.**peek**

<details>
<summary>Added in <code>0.3.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.1</code></td><td>Rename from `head` to `peek`</td></tr>
</tbody>
</table>
</details>

```grain
peek : Stack<a> -> Option<a>
```

Provides the value at the top of the stack, if it exists.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`Stack<a>`|The stack to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the value at the top of the stack or `None` otherwise.|

### Stack.**push**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
push : (a, Stack<a>) -> Stack<a>
```

Adds a new item to the top of the stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The item to be added|
|`stack`|`Stack<a>`|The stack being updated|

Returns:

|type|description|
|----|-----------|
|`Stack<a>`|A new stack with the item added to the end|

### Stack.**pop**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
pop : Stack<a> -> Stack<a>
```

Removes the item at the top of the stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`Stack<a>`|The stack being updated|

Returns:

|type|description|
|----|-----------|
|`Stack<a>`|A new stack with the last item removed|

### Stack.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.2</code></summary>
No other changes yet.
</details>

```grain
size : Stack<a> -> Number
```

Computes the size of the input stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`Stack<a>`|The stack to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The count of the items in the stack|

