---
title: Map
---

A Map holds key-value pairs. Any value may be used as a key or value. Operations on a Map mutate the internal state, so it never needs to be re-assigned.

An immutable map implementation is available in the `Immutable` submodule.

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
from "map" include Map
```

## Types

Type declarations included in the Map module.

### Map.**Map**

```grain
type Map<k, v>
```

### Map.**InternalMapStats**

```grain
record InternalMapStats {
  currentSize: Number,
  bucketCount: Number,
}
```

Represents the internal state of a map.

## Values

Functions and constants included in the Map module.

### Map.**make**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Merged with `makeSized`; modified signature to accept size</td></tr>
</tbody>
</table>
</details>

```grain
make: (?size: Number) => Map<a, b>
```

Creates a new empty map with an initial storage of the given size. As
values are added or removed, the internal storage may grow or shrink.
Generally, you won't need to care about the storage size of your map and
can use the default size.

Parameters:

| param   | type     | description                         |
| ------- | -------- | ----------------------------------- |
| `?size` | `Number` | The initial storage size of the map |

Returns:

| type        | description                                      |
| ----------- | ------------------------------------------------ |
| `Map<a, b>` | An empty map with the given initial storage size |

Throws:

`Failure(String)`

* If WASI random_get fails

### Map.**set**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
set: (key: a, value: b, map: Map<a, b>) => Void
```

Adds a new key-value pair to the map. If the key already exists in the map, the value is replaced.

Parameters:

| param   | type        | description               |
| ------- | ----------- | ------------------------- |
| `key`   | `a`         | The unique key in the map |
| `value` | `b`         | The value to store        |
| `map`   | `Map<a, b>` | The map to modify         |

### Map.**get**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
get: (key: a, map: Map<a, b>) => Option<b>
```

Retrieves the value for the given key.

Parameters:

| param | type        | description       |
| ----- | ----------- | ----------------- |
| `key` | `a`         | The key to access |
| `map` | `Map<a, b>` | The map to access |

Returns:

| type        | description                                                    |
| ----------- | -------------------------------------------------------------- |
| `Option<b>` | `Some(value)` if the key exists in the map or `None` otherwise |

### Map.**contains**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
contains: (key: a, map: Map<a, b>) => Bool
```

Determines if the map contains the given key. In such a case, it will always contain a value for the given key.

Parameters:

| param | type        | description           |
| ----- | ----------- | --------------------- |
| `key` | `a`         | The key to search for |
| `map` | `Map<a, b>` | The map to search     |

Returns:

| type   | description                                                   |
| ------ | ------------------------------------------------------------- |
| `Bool` | `true` if the map contains the given key or `false` otherwise |

### Map.**remove**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
remove: (key: a, map: Map<a, b>) => Void
```

Removes the given key from the map, which also removes the value. If the key pair doesn't exist, nothing happens.

Parameters:

| param | type        | description       |
| ----- | ----------- | ----------------- |
| `key` | `a`         | The key to remove |
| `map` | `Map<a, b>` | The map to update |

### Map.**update**

<details disabled>
<summary tabindex="-1">Added in <code>0.3.0</code></summary>
No other changes yet.
</details>

```grain
update: (key: a, fn: (Option<b> => Option<b>), map: Map<a, b>) => Void
```

Updates a value in the map by calling an updater function that receives the previously stored value as an `Option` and returns the new value to be stored as an `Option`. If the key didn't exist previously, the value will be `None`. If `None` is returned from the updater function, the key-value pair is removed.

Parameters:

| param | type                     | description               |
| ----- | ------------------------ | ------------------------- |
| `key` | `a`                      | The unique key in the map |
| `fn`  | `Option<b> => Option<b>` | The updater function      |
| `map` | `Map<a, b>`              | The map to modify         |

### Map.**size**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
size: (map: Map<a, b>) => Number
```

Provides the count of key-value pairs stored within the map.

Parameters:

| param | type        | description        |
| ----- | ----------- | ------------------ |
| `map` | `Map<a, b>` | The map to inspect |

Returns:

| type     | description                             |
| -------- | --------------------------------------- |
| `Number` | The count of key-value pairs in the map |

### Map.**isEmpty**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
isEmpty: (map: Map<a, b>) => Bool
```

Determines if the map contains no key-value pairs.

Parameters:

| param | type        | description        |
| ----- | ----------- | ------------------ |
| `map` | `Map<a, b>` | The map to inspect |

Returns:

| type   | description                                           |
| ------ | ----------------------------------------------------- |
| `Bool` | `true` if the given map is empty or `false` otherwise |

### Map.**clear**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
clear: (map: Map<a, b>) => Void
```

Resets the map by removing all key-value pairs.

Parameters:

| param | type        | description      |
| ----- | ----------- | ---------------- |
| `map` | `Map<a, b>` | The map to reset |

### Map.**forEach**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.0</code></td><td>Ensured the iterator function return type is always `Void`</td></tr>
</tbody>
</table>
</details>

```grain
forEach: (fn: ((a, b) => Void), map: Map<a, b>) => Void
```

Iterates the map, calling an iterator function with each key and value.

Parameters:

| param | type             | description                                           |
| ----- | ---------------- | ----------------------------------------------------- |
| `fn`  | `(a, b) => Void` | The iterator function to call with each key and value |
| `map` | `Map<a, b>`      | The map to iterate                                    |

### Map.**reduce**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
reduce: (fn: ((a, b, c) => a), init: a, map: Map<b, c>) => a
```

Combines all key-value pairs of a map using a reducer function.

Parameters:

| param  | type             | description                                                                                                     |
| ------ | ---------------- | --------------------------------------------------------------------------------------------------------------- |
| `fn`   | `(a, b, c) => a` | The reducer function to call on each key and value, where the value returned will be the next accumulator value |
| `init` | `a`              | The initial value to use for the accumulator on the first iteration                                             |
| `map`  | `Map<b, c>`      | The map to iterate                                                                                              |

Returns:

| type | description                              |
| ---- | ---------------------------------------- |
| `a`  | The final accumulator returned from `fn` |

### Map.**keys**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
keys: (map: Map<a, b>) => List<a>
```

Enumerates all keys in the given map.

Parameters:

| param | type        | description          |
| ----- | ----------- | -------------------- |
| `map` | `Map<a, b>` | The map to enumerate |

Returns:

| type      | description                                   |
| --------- | --------------------------------------------- |
| `List<a>` | A list containing all keys from the given map |

### Map.**values**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
values: (map: Map<a, b>) => List<b>
```

Enumerates all values in the given map.

Parameters:

| param | type        | description          |
| ----- | ----------- | -------------------- |
| `map` | `Map<a, b>` | The map to enumerate |

Returns:

| type      | description                                     |
| --------- | ----------------------------------------------- |
| `List<b>` | A list containing all values from the given map |

### Map.**toList**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toList: (map: Map<a, b>) => List<(a, b)>
```

Enumerates all key-value pairs in the given map.

Parameters:

| param | type        | description          |
| ----- | ----------- | -------------------- |
| `map` | `Map<a, b>` | The map to enumerate |

Returns:

| type           | description                                              |
| -------------- | -------------------------------------------------------- |
| `List<(a, b)>` | A list containing all key-value pairs from the given map |

### Map.**fromList**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromList: (list: List<(a, b)>) => Map<a, b>
```

Creates a map from a list.

Parameters:

| param  | type           | description         |
| ------ | -------------- | ------------------- |
| `list` | `List<(a, b)>` | The list to convert |

Returns:

| type        | description                                        |
| ----------- | -------------------------------------------------- |
| `Map<a, b>` | A map containing all key-value pairs from the list |

### Map.**toArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
toArray: (map: Map<a, b>) => Array<(a, b)>
```

Converts a map into an array of its key-value pairs.

Parameters:

| param | type        | description        |
| ----- | ----------- | ------------------ |
| `map` | `Map<a, b>` | The map to convert |

Returns:

| type            | description                                                |
| --------------- | ---------------------------------------------------------- |
| `Array<(a, b)>` | An array containing all key-value pairs from the given map |

### Map.**fromArray**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
fromArray: (array: Array<(a, b)>) => Map<a, b>
```

Creates a map from an array.

Parameters:

| param   | type            | description          |
| ------- | --------------- | -------------------- |
| `array` | `Array<(a, b)>` | The array to convert |

Returns:

| type        | description                                         |
| ----------- | --------------------------------------------------- |
| `Map<a, b>` | A map containing all key-value pairs from the array |

### Map.**filter**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
filter: (fn: ((a, b) => Bool), map: Map<a, b>) => Void
```

Removes key-value pairs from a map where a predicate function returns `false`.

Parameters:

| param | type             | description                                                                                                                                             |
| ----- | ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `fn`  | `(a, b) => Bool` | The predicate function to indicate which key-value pairs to remove from the map, where returning `false` indicates the key-value pair should be removed |
| `map` | `Map<a, b>`      | The map to iterate                                                                                                                                      |

### Map.**reject**

<details disabled>
<summary tabindex="-1">Added in <code>0.2.0</code></summary>
No other changes yet.
</details>

```grain
reject: (fn: ((a, b) => Bool), map: Map<a, b>) => Void
```

Removes key-value pairs from a map where a predicate function returns `true`.

Parameters:

| param | type             | description                                                                                                                                            |
| ----- | ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `fn`  | `(a, b) => Bool` | The predicate function to indicate which key-value pairs to remove from the map, where returning `true` indicates the key-value pair should be removed |
| `map` | `Map<a, b>`      | The map to iterate                                                                                                                                     |

### Map.**getInternalStats**

<details>
<summary>Added in <code>0.2.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Return `InternalMapStats` record instead of a tuple</td></tr>
</tbody>
</table>
</details>

```grain
getInternalStats: (map: Map<a, b>) => InternalMapStats
```

Provides data representing the internal state state of the map.

Parameters:

| param | type        | description        |
| ----- | ----------- | ------------------ |
| `map` | `Map<a, b>` | The map to inspect |

Returns:

| type               | description                   |
| ------------------ | ----------------------------- |
| `InternalMapStats` | The internal state of the map |

## Map.Immutable

An immutable map implementation.

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

### Types

Type declarations included in the Map.Immutable module.

#### Map.Immutable.**Map**

```grain
type Map<k, v>
```

### Values

Functions and constants included in the Map.Immutable module.

#### Map.Immutable.**empty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
empty: Map<a, b>
```

An empty map

#### Map.Immutable.**size**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
size: (map: Map<a, b>) => Number
```

Provides the count of key-value pairs stored within the map.

Parameters:

| param | type        | description        |
| ----- | ----------- | ------------------ |
| `map` | `Map<a, b>` | The map to inspect |

Returns:

| type     | description                             |
| -------- | --------------------------------------- |
| `Number` | The count of key-value pairs in the map |

#### Map.Immutable.**isEmpty**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
isEmpty: (map: Map<a, b>) => Bool
```

Determines if the map contains no key-value pairs.

Parameters:

| param | type        | description        |
| ----- | ----------- | ------------------ |
| `map` | `Map<a, b>` | The map to inspect |

Returns:

| type   | description                                           |
| ------ | ----------------------------------------------------- |
| `Bool` | `true` if the given map is empty or `false` otherwise |

#### Map.Immutable.**set**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
set: (key: a, value: b, map: Map<a, b>) => Map<a, b>
```

Produces a new map containing a new key-value pair. If the key already exists in the map, the value is replaced.

Parameters:

| param   | type        | description               |
| ------- | ----------- | ------------------------- |
| `key`   | `a`         | The unique key in the map |
| `value` | `b`         | The value to store        |
| `map`   | `Map<a, b>` | The base map              |

Returns:

| type        | description                                 |
| ----------- | ------------------------------------------- |
| `Map<a, b>` | A new map containing the new key-value pair |

#### Map.Immutable.**get**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
get: (key: a, map: Map<a, b>) => Option<b>
```

Retrieves the value for the given key.

Parameters:

| param | type        | description       |
| ----- | ----------- | ----------------- |
| `key` | `a`         | The key to access |
| `map` | `Map<a, b>` | The map to access |

Returns:

| type        | description                                                    |
| ----------- | -------------------------------------------------------------- |
| `Option<b>` | `Some(value)` if the key exists in the map or `None` otherwise |

#### Map.Immutable.**contains**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
contains: (key: a, map: Map<a, b>) => Bool
```

Determines if the map contains the given key. In such a case, it will always contain a value for the given key.

Parameters:

| param | type        | description           |
| ----- | ----------- | --------------------- |
| `key` | `a`         | The key to search for |
| `map` | `Map<a, b>` | The map to search     |

Returns:

| type   | description                                                   |
| ------ | ------------------------------------------------------------- |
| `Bool` | `true` if the map contains the given key or `false` otherwise |

#### Map.Immutable.**remove**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
remove: (key: a, map: Map<a, b>) => Map<a, b>
```

Produces a new map without the key-value pair corresponding to the given
key. If the key doesn't exist in the map, the map will be returned unmodified.

Parameters:

| param | type        | description             |
| ----- | ----------- | ----------------------- |
| `key` | `a`         | The key to exclude      |
| `map` | `Map<a, b>` | The map to exclude from |

Returns:

| type        | description                     |
| ----------- | ------------------------------- |
| `Map<a, b>` | A new map without the given key |

#### Map.Immutable.**update**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
update: (key: a, fn: (Option<b> => Option<b>), map: Map<a, b>) => Map<a, b>
```

Produces a new map by calling an updater function that receives the
previously stored value as an `Option` and returns the new value to be
stored as an `Option`. If the key didn't exist previously, the value
will be `None`. If `None` is returned from the updater function, the
key-value pair is excluded.

Parameters:

| param | type                     | description               |
| ----- | ------------------------ | ------------------------- |
| `key` | `a`                      | The unique key in the map |
| `fn`  | `Option<b> => Option<b>` | The updater function      |
| `map` | `Map<a, b>`              | The base map              |

Returns:

| type        | description                                                                           |
| ----------- | ------------------------------------------------------------------------------------- |
| `Map<a, b>` | A new map with the value at the given key modified according to the function's output |

#### Map.Immutable.**forEach**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
forEach: (fn: ((a, b) => Void), map: Map<a, b>) => Void
```

Iterates the map, calling an iterator function with each key and value.

Parameters:

| param | type             | description                                           |
| ----- | ---------------- | ----------------------------------------------------- |
| `fn`  | `(a, b) => Void` | The iterator function to call with each key and value |
| `map` | `Map<a, b>`      | The map to iterate                                    |

#### Map.Immutable.**reduce**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
reduce: (fn: ((a, b, c) => a), init: a, map: Map<b, c>) => a
```

Combines all key-value pairs of a map using a reducer function.

Parameters:

| param  | type             | description                                                                                                     |
| ------ | ---------------- | --------------------------------------------------------------------------------------------------------------- |
| `fn`   | `(a, b, c) => a` | The reducer function to call on each key and value, where the value returned will be the next accumulator value |
| `init` | `a`              | The initial value to use for the accumulator on the first iteration                                             |
| `map`  | `Map<b, c>`      | The map to iterate                                                                                              |

Returns:

| type | description                              |
| ---- | ---------------------------------------- |
| `a`  | The final accumulator returned from `fn` |

#### Map.Immutable.**keys**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
keys: (map: Map<a, b>) => List<a>
```

Enumerates all keys in the given map.

Parameters:

| param | type        | description          |
| ----- | ----------- | -------------------- |
| `map` | `Map<a, b>` | The map to enumerate |

Returns:

| type      | description                                   |
| --------- | --------------------------------------------- |
| `List<a>` | A list containing all keys from the given map |

#### Map.Immutable.**values**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
values: (map: Map<a, b>) => List<b>
```

Enumerates all values in the given map.

Parameters:

| param | type        | description          |
| ----- | ----------- | -------------------- |
| `map` | `Map<a, b>` | The map to enumerate |

Returns:

| type      | description                                     |
| --------- | ----------------------------------------------- |
| `List<b>` | A list containing all values from the given map |

#### Map.Immutable.**filter**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
filter: (fn: ((a, b) => Bool), map: Map<a, b>) => Map<a, b>
```

Produces a new map excluding the key-value pairs where a predicate function returns `false`.

Parameters:

| param | type             | description                                                                                                                                               |
| ----- | ---------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `fn`  | `(a, b) => Bool` | The predicate function to indicate which key-value pairs to exclude from the map, where returning `false` indicates the key-value pair should be excluded |
| `map` | `Map<a, b>`      | The map to iterate                                                                                                                                        |

Returns:

| type        | description                                                          |
| ----------- | -------------------------------------------------------------------- |
| `Map<a, b>` | A new map excluding the key-value pairs not fulfilling the predicate |

#### Map.Immutable.**reject**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
reject: (fn: ((a, b) => Bool), map: Map<a, b>) => Map<a, b>
```

Produces a new map excluding the key-value pairs where a predicate function returns `true`.

Parameters:

| param | type             | description                                                                                                                                              |
| ----- | ---------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `fn`  | `(a, b) => Bool` | The predicate function to indicate which key-value pairs to exclude from the map, where returning `true` indicates the key-value pair should be excluded |
| `map` | `Map<a, b>`      | The map to iterate                                                                                                                                       |

Returns:

| type        | description                                                      |
| ----------- | ---------------------------------------------------------------- |
| `Map<a, b>` | A new map excluding the key-value pairs fulfilling the predicate |

#### Map.Immutable.**fromList**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
fromList: (list: List<(a, b)>) => Map<a, b>
```

Creates a map from a list.

Parameters:

| param  | type           | description         |
| ------ | -------------- | ------------------- |
| `list` | `List<(a, b)>` | The list to convert |

Returns:

| type        | description                                        |
| ----------- | -------------------------------------------------- |
| `Map<a, b>` | A map containing all key-value pairs from the list |

#### Map.Immutable.**toList**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
toList: (map: Map<a, b>) => List<(a, b)>
```

Enumerates all key-value pairs in the given map.

Parameters:

| param | type        | description          |
| ----- | ----------- | -------------------- |
| `map` | `Map<a, b>` | The map to enumerate |

Returns:

| type           | description                                              |
| -------------- | -------------------------------------------------------- |
| `List<(a, b)>` | A list containing all key-value pairs from the given map |

#### Map.Immutable.**fromArray**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
fromArray: (array: Array<(a, b)>) => Map<a, b>
```

Creates a map from an array.

Parameters:

| param   | type            | description          |
| ------- | --------------- | -------------------- |
| `array` | `Array<(a, b)>` | The array to convert |

Returns:

| type        | description                                         |
| ----------- | --------------------------------------------------- |
| `Map<a, b>` | A map containing all key-value pairs from the array |

#### Map.Immutable.**toArray**

<details>
<summary>Added in <code>0.6.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.5.4</code></td><td>Originally in `"immutablemap"` module</td></tr>
</tbody>
</table>
</details>

```grain
toArray: (map: Map<a, b>) => Array<(a, b)>
```

Converts a map into an array of its key-value pairs.

Parameters:

| param | type        | description        |
| ----- | ----------- | ------------------ |
| `map` | `Map<a, b>` | The map to convert |

Returns:

| type            | description                                                |
| --------------- | ---------------------------------------------------------- |
| `Array<(a, b)>` | An array containing all key-value pairs from the given map |

