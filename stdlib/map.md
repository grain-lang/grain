---
title: Map
---

A Map holds key-value pairs. Any value may be used as a key or value. Operations on a Map mutate the internal state, so it never needs to be re-assigned.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
import Map from "map"
```

## Values

Functions for working with the Map data type.

### Map.**Bucket**

```grain
record Bucket<k, v> {
  key: k,
  value: v,
  next: Option<Bucket<k, v>>,
}
```

### Map.**Map**

```grain
record Map<k, v> {
  size: Number,
  buckets: Array<Option<Bucket<k, v>>>,
}
```

### Map.**makeSized**

```grain
makeSized : Number -> Map<a, b>
```

### Map.**make**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
make : () -> Map<a, b>
```

Creates a new, empty map.

Returns:

|type|description|
|----|-----------|
|`Map<a, b>`|An empty map|

### Map.**set**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
set : (a, b, Map<a, b>) -> Void
```

Adds a new key pair to the map. If the key already exists in the map, the value is replaced.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key to add the value under|
|`value`|`a`|The value to set|
|`map`|`Map<a, b>`|The map to modify|

### Map.**get**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
get : (a, Map<a, b>) -> Option<b>
```

Returns the value for the given key.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key to look up|
|`map`|`Map<a, b>`|The map to get the value from|

Returns:

|type|description|
|----|-----------|
|`Option<a>`|`Some(value)` if the given key pair is in the map, `None` otherwise|

### Map.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
contains : (a, Map<a, b>) -> Bool
```

Determines if the map contains a value for the given key.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key to search for|
|`map`|`Map<a, b>`|The map to search|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the map contains the given key, `false` otherwise|

### Map.**remove**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
remove : (a, Map<a, b>) -> Void
```

Removes the given key pair from the map. If the key pair doesn't exist, nothing happens.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key to remove|
|`map`|`Map<a, b>`|The map to update|

### Map.**update**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
update : (a, (Option<b> -> Option<b>), Map<a, b>) -> Void
```

Applys a given function to a key pair in the map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`key`|`a`|The key indicating which key pair to update|
|`fn`|`Option<a> -> Option<a>`|The function to apply|
|`map`|`Map<a, b>`|The map to modify|

### Map.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
size : Map<a, b> -> Number
```

Returns the number of key pairs within the map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`Map<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`Number`|The number of key pairs in the map|

### Map.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty : Map<a, b> -> Bool
```

Determines if the map contains no key pairs.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`Map<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the given map is empty, `false` otherwise|

### Map.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
clear : Map<a, b> -> Void
```

Resets the map by removing all key pairs.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`Map<a, b>`|The map to reset|

### Map.**forEach**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
forEach : (((a, b) -> c), Map<a, b>) -> Void
```

Iterates the map, calling an iterator function on each key pair.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> c`|The iterator function to call with each key pair|
|`map`|`Map<a, b>`|The map to iterate|

### Map.**reduce**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
reduce : (((a, b, c) -> a), a, Map<b, c>) -> a
```

Combines all elements of a map using a reducer function.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b, c) -> a`|The reducer function to call on each key pair, where the value returned will be the next accumulator value|
|`init`|`a`|The initial value to use for the accumulator on the first iteration|
|`map`|`Map<a, b>`|The map to iterate|

Returns:

|type|description|
|----|-----------|
|`a`|The final accumulator returned from `fn`|

### Map.**keys**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
keys : Map<a, b> -> List<a>
```

Generates a list of all keys in the given map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`Map<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list of all keys in the given map|

### Map.**values**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
values : Map<a, b> -> List<b>
```

Generates a list of all values in the given map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`Map<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`List<a>`|A list of all values in the given map|

### Map.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toList : Map<a, b> -> List<(a, b)>
```

Generates a list of all key pairs in the given map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`Map<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`List<(a, b)>`|A list of all key pairs in the given map|

### Map.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromList : List<(a, b)> -> Map<a, b>
```

Generates a map given a list of key pairs.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list`|`List<(a, b)>`|The list to inspect|

Returns:

|type|description|
|----|-----------|
|`Map<a, b>`|A map made from the list of key pairs|

### Map.**toArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toArray : Map<a, b> -> Array<(a, b)>
```

Generates a array of all key pairs in the given map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`Map<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`Array<(a, b)>`|A array of all key pairs in the given map|

### Map.**fromArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromArray : Array<(a, b)> -> Map<a, b>
```

Generates a map given a array of key pairs.

Parameters:

|param|type|description|
|-----|----|-----------|
|`array`|`Array<(a, b)>`|The array to inspect|

Returns:

|type|description|
|----|-----------|
|`Map<a, b>`|A map made from the array of key pairs|

### Map.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
filter : (((a, b) -> Bool), Map<a, b>) -> Void
```

Removes key pairs from a map where a predicate function returns `false`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> Bool`|The predicate function to indicate which key pairs to remove from the map, where returning `false` indicates the key pair should be removed|
|`map`|`Map<a, b>`|The map to iterate|

### Map.**reject**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
reject : (((a, b) -> Bool), Map<a, b>) -> Void
```

Removes key pairs from a map where a predicate function returns `true`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fn`|`(a, b) -> Bool`|The predicate function to indicate which key pairs to remove from the map, where returning `true` indicates the key pair should be removed|
|`map`|`Map<a, b>`|The map to iterate|

### Map.**getInternalStats**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
getInternalStats : Map<a, b> -> (Number, Number)
```

Provides data representing the internal state state of the map.

Parameters:

|param|type|description|
|-----|----|-----------|
|`map`|`Map<a, b>`|The map to inspect|

Returns:

|type|description|
|----|-----------|
|`(Number, Number)`|The internal state of the map|

