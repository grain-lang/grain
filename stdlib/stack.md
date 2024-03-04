---
title: Stack
---

A stack is a LIFO (last-in-first-out) data structure where new
values are added, retrieved, and removed from the end.

The default implementation is mutable, but an immutable stack
implementation is available in the `Immutable` submodule.

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
from "stack" include Stack
```

## Types

Type declarations included in the Stack module.

### Stack.**Stack**

```grain
type Stack<a>
```

A mutable LIFO (last-in-first-out) data structure.

## Values

Functions and constants included in the Stack module.

### Stack.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
make : (?size: Number) => Stack<a>
```

Creates a new stack with an initial storage of the given size. As values are
added or removed, the internal storage may grow or shrink. Generally, you
won’t need to care about the storage size of your map and can use the
default size.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?size`|`Number`|The initial storage size of the stack|

Returns:

|type|description|
|----|-----------|
|`Stack<a>`|An empty stack|

### Stack.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty : (stack: Stack<a>) => Bool
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

### Stack.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
size : (stack: Stack<a>) => Number
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

### Stack.**peek**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
peek : (stack: Stack<a>) => Option<a>
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
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
push : (value: a, stack: Stack<a>) => Void
```

Adds a new item to the top of the stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The item to be added|
|`stack`|`Stack<a>`|The stack being updated|

### Stack.**pop**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
pop : (stack: Stack<a>) => Option<a>
```

Removes the item at the top of the stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`Stack<a>`|The stack being updated|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|The element removed from the stack|

### Stack.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
clear : (stack: Stack<a>) => Void
```

Clears the stack by removing all of its elements

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`Stack<a>`|The stack to clear|

### Stack.**copy**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.0</code></summary>
No other changes yet.
</details>

```grain
copy : (stack: Stack<a>) => Stack<a>
```

Produces a shallow copy of the input stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`Stack<a>`|The stack to copy|

Returns:

|type|description|
|----|-----------|
|`Stack<a>`|A new stack containing the elements from the input|

## Stack.Immutable

An immutable stack implementation.

### Types

Type declarations included in the Stack.Immutable module.

#### Stack.Immutable.**ImmutableStack**

```grain
type ImmutableStack<a>
```

ImmutableStacks are immutable data structures that store their data in a List.

### Values

Functions and constants included in the Stack.Immutable module.

#### Stack.Immutable.**empty**

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
empty : ImmutableStack<a>
```

An empty stack.

#### Stack.Immutable.**isEmpty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.0</code></td><td>Originally a module root API</td></tr>
</tbody>
</table>
</details>

```grain
isEmpty : (stack: ImmutableStack<a>) => Bool
```

Checks if the given stack contains no items.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`ImmutableStack<a>`|The stack to check|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the stack has no items or `false` otherwise|

#### Stack.Immutable.**peek**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.0</code></td><td>Originally a module root API</td></tr>
<tr><td><code>0.3.1</code></td><td>Rename from `head` to `peek`</td></tr>
</tbody>
</table>
</details>

```grain
peek : (stack: ImmutableStack<a>) => Option<a>
```

Provides the value at the top of the stack, if it exists.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`ImmutableStack<a>`|The stack to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` containing the value at the top of the stack or `None` otherwise.|

#### Stack.Immutable.**push**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.0</code></td><td>Originally a module root API</td></tr>
</tbody>
</table>
</details>

```grain
push : (value: a, stack: ImmutableStack<a>) => ImmutableStack<a>
```

Adds a new item to the top of the stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`value`|`a`|The item to be added|
|`stack`|`ImmutableStack<a>`|The stack being updated|

Returns:

|type|description|
|----|-----------|
|`ImmutableStack<a>`|A new stack with the item added to the end|

#### Stack.Immutable.**pop**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.3.0</code></td><td>Originally a module root API</td></tr>
</tbody>
</table>
</details>

```grain
pop : (stack: ImmutableStack<a>) => ImmutableStack<a>
```

Removes the item at the top of the stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`ImmutableStack<a>`|The stack being updated|

Returns:

|type|description|
|----|-----------|
|`ImmutableStack<a>`|A new stack with the last item removed|

#### Stack.Immutable.**size**

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
size : (stack: ImmutableStack<a>) => Number
```

Computes the size of the input stack.

Parameters:

|param|type|description|
|-----|----|-----------|
|`stack`|`ImmutableStack<a>`|The stack to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The count of the items in the stack|

