---
title: ImmutableMap
---

An ImmutableMap holds key-value pairs. Any value may be used as a key or value. Operations on an ImmutableMap do not mutate the map's internal state.

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
import ImmutableMap from "immutablemap"
```

## Types

Type declarations included in the ImmutableMap module.

### ImmutableMap.**ImmutableMap**

```grain
type ImmutableMap<k, v>
```

## Values

Functions and constants for working with ImmutableMaps.

### ImmutableMap.**empty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
empty : ImmutableMap<a, b>
```

An empty map

### ImmutableMap.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
size : ImmutableMap<a, b> -> Number
```

Provides the count of key-value pairs stored within the map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`ImmutableMap<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The count of key-value pairs in the map|

### ImmutableMap.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
isEmpty : ImmutableMap<a, b> -> Bool
```

Determines if the map contains no key-value pairs.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`ImmutableMap<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the given map is empty or `false` otherwise|

### ImmutableMap.**set**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
set : (a, b, ImmutableMap<a, b>) -> ImmutableMap<a, b>
```

Produces a new map containing a new key-value pair. If the key already exists in the map, the value is replaced.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The unique key in the map|
|`value`|`b`|The value to store|
|`map`|`ImmutableMap<a, b>`|The base map|

Returns:

|type|description|
|----|-----------|
|`ImmutableMap<a, b>`|A new map containing the new key-value pair|

### ImmutableMap.**get**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
get : (a, ImmutableMap<a, b>) -> Option<b>
```

Retrieves the value for the given key.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key to access|
|`map`|`ImmutableMap<a, b>`|The map to access|

Returns:

|type|description|
|----|-----------|
|`Option<b>`|`Some(value)` if the key exists in the map or `None` otherwise|

### ImmutableMap.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
contains : (a, ImmutableMap<a, b>) -> Bool
```

Determines if the map contains the given key. In such a case, it will always contain a value for the given key.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key to search for|
|`map`|`ImmutableMap<a, b>`|The map to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the map contains the given key or `false` otherwise|

### ImmutableMap.**remove**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
remove : (a, ImmutableMap<a, b>) -> ImmutableMap<a, b>
```

Produces a new map without the key-value pair corresponding to the given
key. If the key doesn't exist in the map, the map will be returned unmodified.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key to exclude|
|`map`|`ImmutableMap<a, b>`|The map to exclude from|

Returns:

|type|description|
|----|-----------|
|`ImmutableMap<a, b>`|A new map without the given key|

### ImmutableMap.**update**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
update :
  (a, (Option<b> -> Option<b>), ImmutableMap<a, b>) -> ImmutableMap<a, b>
```

Produces a new map by calling an updater function that receives the
previously stored value as an `Option` and returns the new value to be
stored as an `Option`. If the key didn't exist previously, the value
will be `None`. If `None` is returned from the updater function, the
key-value pair is excluded.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The unique key in the map|
|`fn`|`Option<b> -> Option<b>`|The updater function|
|`map`|`ImmutableMap<a, b>`|The base map|

Returns:

|type|description|
|----|-----------|
|`ImmutableMap<a, b>`|A new map with the value at the given key modified according to the function's output|

### ImmutableMap.**forEach**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
forEach : (((a, b) -> Void), ImmutableMap<a, b>) -> Void
```

Iterates the map, calling an iterator function with each key and value.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> Void`|The iterator function to call with each key and value|
|`map`|`ImmutableMap<a, b>`|The map to iterate|

### ImmutableMap.**reduce**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
reduce : (((a, b, c) -> a), a, ImmutableMap<b, c>) -> a
```

Combines all key-value pairs of a map using a reducer function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b, c) -> a`|The reducer function to call on each key and value, where the value returned will be the next accumulator value|
|`init`|`a`|The initial value to use for the accumulator on the first iteration|
|`map`|`ImmutableMap<b, c>`|The map to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

### ImmutableMap.**keys**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
keys : ImmutableMap<a, b> -> List<a>
```

Enumerates all keys in the given map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`ImmutableMap<a, b>`|The map to enumerate|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list containing all keys from the given map|

### ImmutableMap.**values**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
values : ImmutableMap<a, b> -> List<b>
```

Enumerates all values in the given map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`ImmutableMap<a, b>`|The map to enumerate|

Returns:

|type|description|
|----|-----------|
|`List<b>`|A list containing all values from the given map|

### ImmutableMap.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
filter : (((a, b) -> Bool), ImmutableMap<a, b>) -> ImmutableMap<a, b>
```

Produces a new map excluding the key-value pairs where a predicate function returns `false`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> Bool`|The predicate function to indicate which key-value pairs to exclude from the map, where returning `false` indicates the key-value pair should be excluded|
|`map`|`ImmutableMap<a, b>`|The map to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableMap<a, b>`|A new map excluding the key-value pairs not fulfilling the predicate|

### ImmutableMap.**reject**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
reject : (((a, b) -> Bool), ImmutableMap<a, b>) -> ImmutableMap<a, b>
```

Produces a new map excluding the key-value pairs where a predicate function returns `true`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> Bool`|The predicate function to indicate which key-value pairs to exclude from the map, where returning `true` indicates the key-value pair should be excluded|
|`map`|`ImmutableMap<a, b>`|The map to iterate|

Returns:

|type|description|
|----|-----------|
|`ImmutableMap<a, b>`|A new map excluding the key-value pairs fulfilling the predicate|

### ImmutableMap.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
fromList : List<(a, b)> -> ImmutableMap<a, b>
```

Creates a map from a list.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<(a, b)>`|The list to convert|

Returns:

|type|description|
|----|-----------|
|`ImmutableMap<a, b>`|A map containing all key-value pairs from the list|

### ImmutableMap.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
toList : ImmutableMap<a, b> -> List<(a, b)>
```

Enumerates all key-value pairs in the given map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`ImmutableMap<a, b>`|The map to enumerate|

Returns:

|type|description|
|----|-----------|
|`List<(a, b)>`|A list containing all key-value pairs from the given map|

### ImmutableMap.**fromArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
fromArray : Array<(a, b)> -> ImmutableMap<a, b>
```

Creates a map from an array.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<(a, b)>`|The array to convert|

Returns:

|type|description|
|----|-----------|
|`ImmutableMap<a, b>`|A map containing all key-value pairs from the array|

### ImmutableMap.**toArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.4</code></summary>
No other changes yet.
</details>

```grain
toArray : ImmutableMap<a, b> -> Array<(a, b)>
```

Converts a map into an array of its key-value pairs.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`ImmutableMap<a, b>`|The map to convert|

Returns:

|type|description|
|----|-----------|
|`Array<(a, b)>`|An array containing all key-value pairs from the given map|

