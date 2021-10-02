---
title: Float32
---

Operations on the Float32 type.

```grain
import Float32 from "float32"
```

## Values

Functions for working with the `Float32` data type.

### Float32.**fromNumber**

```grain
fromNumber : Number -> Float32
```

Coverts the argument from a `Number` to a `Float32`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The `Number` to convert.|

Returns:

|type|description|
|----|-----------|
|`Float32`|The converted `Number`.|

### Float32.**toNumber**

```grain
toNumber : Float32 -> Number
```

Coverts the argument from a `Float32` to a `Number`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The `Float32` to convert.|

Returns:

|type|description|
|----|-----------|
|`Number`|The converted `Float32`.|

### Float32.**add**

```grain
add : (Float32, Float32) -> Float32
```

Floating-point addition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand.|
|`y`|`Float32`|The second operand.|

Returns:

|type|description|
|----|-----------|
|`Float32`|The sum of the two operands.|

### Float32.**sub**

```grain
sub : (Float32, Float32) -> Float32
```

Floating-point subtraction.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand.|
|`y`|`Float32`|The second operand.|

Returns:

|type|description|
|----|-----------|
|`Float32`|The difference of the two operands.|

### Float32.**mul**

```grain
mul : (Float32, Float32) -> Float32
```

Floating-point multiplication.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand.|
|`y`|`Float32`|The second operand.|

Returns:

|type|description|
|----|-----------|
|`Float32`|The product of the two operands.|

### Float32.**div**

```grain
div : (Float32, Float32) -> Float32
```

Floating-point division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first operand.|
|`y`|`Float32`|The second operand.|

Returns:

|type|description|
|----|-----------|
|`Float32`|The quotient of the two operands.|

## Comparisons

Comparison functions for working with floating points.

### Float32.**lt**

```grain
lt : (Float32, Float32) -> Bool
```

Checks if the first argument is strictly less than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first argument.|
|`y`|`Float32`|The second argument.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is less then the second argument.|

### Float32.**gt**

```grain
gt : (Float32, Float32) -> Bool
```

Checks if the first argument is strictly greater than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first argument.|
|`y`|`Float32`|The second argument.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is greater then the second argument.|

### Float32.**lte**

```grain
lte : (Float32, Float32) -> Bool
```

Checks if the first argument is less than or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first argument.|
|`y`|`Float32`|The second argument.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is less then or equal to the second argument.|

### Float32.**gte**

```grain
gte : (Float32, Float32) -> Bool
```

Checks if the first argument is greater than or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float32`|The first argument.|
|`y`|`Float32`|The second argument.|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is greater then or equal to the second argument.|

## Constants

Constants related to floating points.

### Float32.**infinity**

```grain
infinity : Float32
```

Infinity as a `Float32`.

### Float32.**nan**

```grain
nan : Float32
```

Nan as a `Float32`.

