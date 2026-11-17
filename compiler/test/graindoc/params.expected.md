---
title: Params
---

## Values

Functions and constants included in the Params module.

### Params.**test1**

```grain
test1: (a: Number, b: Number) => Number
```

Fully documented labeled parameters test.

Parameters:

| param | type     | description        |
| ----- | -------- | ------------------ |
| `a`   | `Number` | This is the first  |
| `b`   | `Number` | This is the second |

### Params.**test2**

```grain
test2: ((Number, a), (Number, b)) => Number
```

Fully documented unlabeled parameters test.

Parameters:

| param | type          | description        |
| ----- | ------------- | ------------------ |
| `0`   | `(Number, a)` | This is the first  |
| `1`   | `(Number, b)` | This is the second |

### Params.**test3**

```grain
test3: (a: Number, (Number, a), c: Number) => Number
```

Fully documented mixed parameters test.

Parameters:

| param | type          | description        |
| ----- | ------------- | ------------------ |
| `a`   | `Number`      | This is the first  |
| `1`   | `(Number, a)` | This is the second |
| `c`   | `Number`      | This is the third  |

### Params.**test4**

```grain
test4: (a: Number, ?b: Number) => Number
```

Optional parameters test.

Parameters:

| param | type     | description   |
| ----- | -------- | ------------- |
| `a`   | `Number` | This is the a |
| `?b`  | `Number` | This is the b |

### Params.**test5**

```grain
test5: (a: Number, b: Number) => Number
```

No documentation test.

### Params.**test6**

```grain
test6: (a: Number, b: Number, c: Number) => Number
```

Partially documented parameters test.

Parameters:

| param | type     | description   |
| ----- | -------- | ------------- |
| `a`   | `Number` | This is the a |
| `b`   | `Number` |               |
| `c`   | `Number` | This is the c |

