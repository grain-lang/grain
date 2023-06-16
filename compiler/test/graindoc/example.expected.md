---
title: ExampleDoc
---

## Values

Functions and constants included in the ExampleDoc module.

### ExampleDoc.**singleLineExample**

```grain
singleLineExample : (index: a, array: b) => Number
```

SingleLineExample

Examples:

```grain
singleLineExample(1, [1, 2, 3])
```

### ExampleDoc.**multiLineExample**

```grain
multiLineExample : (index: a, array: b) => Number
```

MultiLineExample

Examples:

```grain
let x = multiLineExample(1, [1, 2, 3])
print(x) // 1
```

### ExampleDoc.**multipleSingleLineExamples**

```grain
multipleSingleLineExamples : (index: a, array: b) => Number
```

SingleLineExample

Examples:

```grain
singleLineExample(1, [1, 2, 3]) == 1
```

```grain
assert singleLineExample(1, [1, 2, 3]) == 1
```

```grain
print(singleLineExample(1, [1, 2, 3]))
```

### ExampleDoc.**multipleMultiLineExample**

```grain
multipleMultiLineExample : (index: a, array: b) => Number
```

MultiLineExample

Examples:

```grain
let x = multiLineExample(1, [1, 2, 3])
print(x) // 1
```

```grain
let x = multiLineExample(1, [1, 2, 3])
assert x == 1
```

```grain
let x = multiLineExample(1, [1, 2, 3])
x == 1
```

### ExampleDoc.**mixedLineExample**

```grain
mixedLineExample : (index: a, array: b) => Number
```

MultiLineExample

Examples:

```grain
let x = multiLineExample(1, [1, 2, 3])
print(x) // 1
```

```grain
let x = multiLineExample(1, [1, 2, 3])
assert x == 1
```

```grain
let x = multiLineExample(1, [1, 2, 3])
x == 1
```

```grain
singleLineExample(1, [1, 2, 3]) == 1
```

```grain
assert singleLineExample(1, [1, 2, 3]) == 1
```

```grain
print(singleLineExample(1, [1, 2, 3]))
```

