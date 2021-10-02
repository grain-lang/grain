---
title: Float64
---

Operations on the Float64 type.

```grain
import Float64 from "float64"
```

## Values

Functions for working with the `Float64` data type.

### Float64.**fromNumber**

```grain
fromNumber : Number -> Float64
```

Coverts the argument from a `Number` to a `Float64`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The `Number` to convert.|

Returns:

|type|description|
|----|-----------|
|`Float64`|The converted `Number`.|

### Float64.**toNumber**

```grain
toNumber : Float64 -> Number
```

Coverts the argument from a `Float64` to a `Number`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The `Float64` to convert.|

Returns:

|type|description|
|----|-----------|
|`Number`|The converted `Float64`.|

### Float64.**add**

```grain
add : (Float64, Float64) -> Float64
```

Floating-point addition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand.|
|`y`|`Float64`|The second operand.|

Returns:

|type|description|
|----|-----------|
|`Float64`|The sum of the two operands.|

### Float64.**sub**

```grain
sub : (Float64, Float64) -> Float64
```

Floating-point subtraction.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand.|
|`y`|`Float64`|The second operand.|

Returns:

|type|description|
|----|-----------|
|`Float64`|The difference of the two operands.|

### Float64.**mul**

```grain
mul : (Float64, Float64) -> Float64
```

Floating-point multiplication.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand.|
|`y`|`Float64`|The second operand.|

Returns:

|type|description|
|----|-----------|
|`Float64`|The product of the two operands.|

### Float64.**div**

```grain
div : (Float64, Float64) -> Float64
```

Floating-point division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand.|
|`y`|`Float64`|The second operand.|

Returns:

|type|description|
|----|-----------|
|`Float64`|The quotient of the two operands.|

## Comparisons

Comparison functions for working with floating points.

### Float64.**lt**

```grain
lt : (Float64, Float64) -> Bool
```

Checks if the first argument is strictly less than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first argument.|
|`y`|`Float64`|The second argument.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is less then the second argument.|

### Float64.**gt**

```grain
gt : (Float64, Float64) -> Bool
```

Checks if the first argument is strictly greater than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first argument.|
|`y`|`Float64`|The second argument.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is greater then the second argument.|

### Float64.**lte**

```grain
lte : (Float64, Float64) -> Bool
```

Checks if the first argument is less than or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first argument.|
|`y`|`Float64`|The second argument.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is less then or equal to the second argument.|

### Float64.**gte**

```grain
gte : (Float64, Float64) -> Bool
```

Checks if the first argument is greater than or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first argument.|
|`y`|`Float64`|The second argument.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is greater then or equal to the second argument.|

## Constants

Constants related to floating points.

### Float64.**infinity**

```grain
infinity : Float64
```

Infinity as a `Float64`.

### Float64.**nan**

```grain
nan : Float64
```

Nan as a `Float64`.

