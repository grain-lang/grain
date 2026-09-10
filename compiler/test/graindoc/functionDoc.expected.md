---
title: FunctionGrainDoc
---

## Values

Functions and constants included in the FunctionGrainDoc module.

### FunctionGrainDoc.**get**

<details>
<summary>Added in <code>0.1.0</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.2.0</code></td><td>Argument order changed to data-last</td></tr>
</tbody>
</table>
</details>

```grain
get: (index: Number, array: Array<a>) => a
```

An alias for normal syntactic array access, i.e. `array[n]`.

Retrieves the element from the array at the specified index.
A negative index is treated as an offset from the end of the array.

Parameters:

| param   | type       | description         |
| ------- | ---------- | ------------------- |
| `index` | `Number`   | The index to access |
| `array` | `Array<a>` | The array to access |

Returns:

| type | description                |
| ---- | -------------------------- |
| `a`  | The element from the array |

### FunctionGrainDoc.**noAttributes**

```grain
noAttributes: (x: a, y: b) => Number
```

Description

### FunctionGrainDoc.**noReturn**

```grain
noReturn: (x: a, y: b) => Number
```

Description

Parameters:

| param | type | description         |
| ----- | ---- | ------------------- |
| `x`   | `a`  | The index to access |
| `y`   | `b`  | The array to access |

### FunctionGrainDoc.**noParams**

```grain
noParams: (x: a, y: b) => Number
```

Description

Returns:

| type     | description                |
| -------- | -------------------------- |
| `Number` | The element from the array |

### FunctionGrainDoc.**partialReturn**

```grain
partialReturn: (x: a, y: b) => Number
```

Description

Parameters:

| param | type | description         |
| ----- | ---- | ------------------- |
| `x`   | `a`  | The index to access |
| `y`   | `b`  |                     |

Returns:

| type     | description                |
| -------- | -------------------------- |
| `Number` | The element from the array |

